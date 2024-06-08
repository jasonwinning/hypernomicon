/*
 * Copyright 2015-2024 Jason Winning
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.hypernomicon.previewWindow;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.previewWindow.PDFJSWrapper.PDFJSCommand;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;

import javafx.application.Platform;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tab;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public class PreviewWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class PreviewFile
  {
    private final FilePath filePath;
    private final HDT_RecordWithPath record;
    private final List<Integer> navList = new ArrayList<>();
    private int navNdx = -1;

    private PreviewFile(FilePath filePath, HDT_RecordWithPath record)
    {
      this.filePath = filePath;
      this.record = record;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath filePathShowing = null;
  private int fileNdx = -1, pageNum = -1, pageNumShowing = -1, workStartPageNum = -1, workEndPageNum = -1, numPages = 0;
  private final PreviewSource src;
  private final PreviewWindow window;
  private final Tab tab;
  private boolean viewerErrOccurred = false, needsRefresh = true, initialized = false, pdfIsShowing = false;
  private PDFJSWrapper jsWrapper;
  private Map<String, Integer> labelToPage;
  private Map<Integer, String> pageToLabel;
  private List<Integer> hilitePages;
  private final List<PreviewFile> fileList = new ArrayList<>();
  private PreviewFile curPrevFile;
  private final ToggleButton btn;
  private final AnchorPane ap;

  PreviewSource getSource()        { return src; }
  PDFJSWrapper getJSWrapper()      { return jsWrapper; }
  int getPageNum()                 { return pageNum; }
  int getNumPages()                { return numPages; }
  Tab getTab()                     { return tab; }
  FilePath getFilePath()           { return curPrevFile == null ? null : curPrevFile.filePath; }
  boolean needsRefresh()           { return needsRefresh; }
  int getWorkStartPageNum()        { return workStartPageNum; }
  int getWorkEndPageNum()          { return workEndPageNum; }
  HDT_RecordWithPath getRecord()   { return curPrevFile == null ? null : curPrevFile.record; }
  ToggleButton getToggleButton()   { return btn; }
  FilePath getFilePathShowing()    { return filePathShowing; }
  void prepareToHide()             { if (initialized) jsWrapper.prepareToHide(); }
  void prepareToShow()             { if (initialized) jsWrapper.prepareToShow(); }
  int lowestHilitePage()           { return collEmpty(hilitePages) ? -1 : hilitePages.get(0); }
  int highestHilitePage()          { return collEmpty(hilitePages) ? -1 : hilitePages.get(hilitePages.size() - 1); }
  int getPageByLabel(String label) { return collEmpty(labelToPage) ? parseInt(label, -1) : labelToPage.getOrDefault(label, -1); }
  String getLabelByPage(int page)  { return collEmpty(pageToLabel) ? String.valueOf(page) : pageToLabel.getOrDefault(page, ""); }
  boolean zoom(boolean zoomingIn)  { return (jsWrapper != null) && jsWrapper.zoom(zoomingIn); }
  void setNeedsRefresh()           { needsRefresh = true; }

  boolean enableFileNavButton(boolean isForward) { return (isForward ? getNextFileNdx() : getPreviousFileNdx()) != -1; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  PreviewWrapper(PreviewSource src, AnchorPane ap, Tab tab, ToggleButton btn, PreviewWindow window)
  {
    this.src = src;
    this.tab = tab;
    this.window = window;
    this.btn = btn;
    this.ap = ap;

    btn.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue)) window.tpPreview.getSelectionModel().select(tab);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void doneHndlr(PDFJSCommand cmd, boolean success, String errMessage)
  {
    switch (cmd)
    {
      case pjsOpen:

        if (curPrevFile == null) return;

        numPages = jsWrapper.getNumPages();
        Platform.runLater(() ->
        {
          if ((curPrevFile != null) && (curPrevFile.navNdx == -1))
            incrementNav();

          refreshControls();
        });

        break;

      case pjsClose:

        pdfIsShowing = false;
        break;

      default :
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pageChangeHndlr(int newPageNumShowing)
  {
    pageNumShowing = newPageNumShowing;

    if (pageNum == pageNumShowing) return;

    pageNum = pageNumShowing;

    incrementNav();

    if (window.curSource() == src)
      Platform.runLater(this::refreshControls);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void retrievedDataHndlr(Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel, List<Integer> hilitePages)
  {
    this.labelToPage = labelToPage;
    this.pageToLabel = pageToLabel;
    this.hilitePages = hilitePages;

    if (window.curSource() == src)
      Platform.runLater(this::refreshControls);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initJS()
  {
    if (jxBrowserDisabled) return;

    jsWrapper = new PDFJSWrapper(ap, this::doneHndlr, this::pageChangeHndlr, this::retrievedDataHndlr);

    if (jxBrowserDisabled) return;

    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setWorkPageNums(int start, int end)
  {
    workStartPageNum = start;
    workEndPageNum = end;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void go()
  {
    if (curPrevFile == null) return;

    if (curPrevFile.record != null)
      ui.goToRecord(curPrevFile.record, true);
    else if (FilePath.isEmpty(curPrevFile.filePath) == false)
      ui.goToRecord(HyperPath.getRecordFromFilePath(curPrevFile.filePath), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean enableNavButton(boolean isForward)
  {
    if (curPrevFile == null) return false;

    return isForward ?
      (curPrevFile.navNdx + 1) < curPrevFile.navList.size()
    :
      curPrevFile.navNdx >= 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int getPreviousFileNdx()
  {
    for (int ndx = fileNdx - 1; ndx >= 0; ndx--)
      if (useFileNavNdx(ndx)) return ndx;

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean useFileNavNdx(int ndx)
  {
    PreviewFile file = fileList.get(ndx);
    if ((file == null) || FilePath.isEmpty(file.filePath))
      return false;

    if ((curPrevFile == null) || FilePath.isEmpty(curPrevFile.filePath))
      return true;

    return curPrevFile.filePath.equals(file.filePath) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int getNextFileNdx()
  {
    for (int ndx = fileNdx + 1; ndx < fileList.size(); ndx++)
      if (useFileNavNdx(ndx)) return ndx;

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean addMenuItem(List<MenuItem> menu, int ndx)
  {
    menu.add(getMenuItemForNavNdx(ndx));

    return menu.size() == 15;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshNavMenu(List<MenuItem> menu, boolean isForward)
  {
    menu.clear();
    if (curPrevFile == null) return;

    if (isForward)
    {
      for (int ndx = curPrevFile.navNdx + 1; ndx < curPrevFile.navList.size(); ndx++)
        if (addMenuItem(menu, ndx)) return;
    }
    else
    {
      for (int ndx = curPrevFile.navNdx - 1; ndx >= 0; ndx--)
        if (addMenuItem(menu, ndx)) return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem getMenuItemForNavNdx(int ndx)
  {
    int page = curPrevFile.navList.get(ndx);
    String pageLabel = safeStr(getLabelByPage(page)), pageStr = String.valueOf(page);

    MenuItem item = new MenuItem("Page " + (pageLabel.isEmpty() || pageLabel.equals(pageStr) ? pageStr : (pageLabel + " (" + pageStr + ')')));

    item.setOnAction(event ->
    {
      curPrevFile.navNdx = ndx;
      setPreview(page, false);
    });

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void navClick(boolean isForward)
  {
    if (enableNavButton(isForward) == false) return;

    curPrevFile.navNdx += (isForward ? 1 : -1);

    setPreview(curPrevFile.navList.get(curPrevFile.navNdx), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void fileNavClick(boolean isForward)
  {
    fileNdx = isForward ? getNextFileNdx() : getPreviousFileNdx();

    PreviewFile prevFile = fileList.get(fileNdx);

    int newPageNum = prevFile.navNdx < 0 ? 1 : prevFile.navList.get(prevFile.navNdx);

    setPreview(prevFile.filePath, newPageNum, prevFile.record, false, prevFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
  {
    clearPreview();
    fileList.clear();
    fileNdx = -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearPreview()
  {
    filePathShowing = null;
    pageNum = -1;
    pageNumShowing = -1;
    workStartPageNum = -1;
    workEndPageNum = -1;
    curPrevFile = null;

    if (window.curSource() == src) window.clearControls();

    if (initialized == false) return;

    OfficePreviewer.stopPreview(jsWrapper);

    if (pdfIsShowing)
      jsWrapper.close();
    else
      jsWrapper.switchToPdfMode();

    pdfIsShowing = false;

    if (window.curSource() == src)
      needsRefresh = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setPreview(FilePath filePath, int pageNum, HDT_Record record)
  {
    setPreview(filePath, pageNum, record, true, null);
  }

  void setPreview(int pageNum, boolean incrementNav)
  {
    setPreview(getFilePath(), pageNum, getRecord(), incrementNav, null);
  }

  private void setPreview(FilePath filePath, int pageNum, HDT_Record record, boolean incrementNav, PreviewFile prevFile)
  {
    boolean fileChanged = true;

    if ((record != null) && (record.getType () != hdtWork    ) && (record.getType() != hdtMiscFile) &&
                            (record.getType () != hdtWorkFile) && (record.getType() != hdtPerson  ))
      record = null;

    if ((getRecord() == record) && (FilePath.isEmpty(getFilePath()) == false) && curPrevFile.filePath.equals(filePath))
    {
      fileChanged = false;

      if (this.pageNum == pageNum)
        return;
    }

    if (fileChanged)
    {
      if (prevFile != null)
        curPrevFile = prevFile;
      else
      {
        curPrevFile = new PreviewFile(filePath, (HDT_RecordWithPath) record);

        fileNdx++;
        while (fileList.size() > fileNdx)
          fileList.remove(fileNdx);

        fileList.add(curPrevFile);
      }
    }

    this.pageNum = pageNum;

    if (FilePath.isEmpty(filePath))
      clearPreview();
    else if ((window.curSource() == src) && window.getStage().isShowing())
      refreshPreview(false, incrementNav);
    else
    {
      if ((window.curSource() == src) && contentsWindow.getStage().isShowing())
        refreshControls();

      needsRefresh = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshControls()
  {
    FilePath filePath = getFilePath();

    if ((pageNum <= 0) || FilePath.isEmpty(filePath) || viewerErrOccurred || (filePath.exists() == false))
    {
      clearPreview();
      return;
    }

    window.refreshControls(pageNum, numPages, this);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void finishRefresh(boolean force, boolean incrementNav)
  {
    if ((pageNum <= 0) || FilePath.isEmpty(getFilePath()) || (initialized == false) || (curPrevFile.filePath.equals(filePathShowing) && viewerErrOccurred))
    {
      clearPreview();
      return;
    }

    viewerErrOccurred = false;

    if (force || (curPrevFile.filePath.equals(filePathShowing) == false))
    {
      if (curPrevFile.filePath.isDirectory())
      {
        clearPreview();
        return;
      }

      filePathShowing = null;
      pageNumShowing = -1;

      window.clearControls();
      needsRefresh = false;

      if (viewerErrOccurred) return;

      labelToPage = null;
      pageToLabel = null;
      hilitePages = null;

      String mimetypeStr = showFile(curPrevFile.filePath, pageNum, jsWrapper, this);  // This can set needsRefresh to true

      if (mimetypeStr.contains("pdf"))
      {
        pdfIsShowing = true;

        filePathShowing = curPrevFile.filePath;
        pageNumShowing = -1;

        return;
      }

      pdfIsShowing = false;
      numPages = 1;

      filePathShowing = curPrevFile.filePath;
      pageNumShowing = pageNum;
    }

    if (pageNum != pageNumShowing)
      jsWrapper.goToPage(pageNum);

    if (incrementNav)
      incrementNav();

    refreshControls();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static String showFile(FilePath filePath, int pageNum, PDFJSWrapper jsWrapper)
  {
    return showFile(filePath, pageNum, jsWrapper, null);
  }

  public static String showFile(FilePath filePath, int pageNum, PDFJSWrapper jsWrapper, PreviewWrapper previewWrapper)
  {
    String mimetypeStr = getMediaType(filePath).toString();

    OfficePreviewer.stopPreview(jsWrapper);

    // For PDF, no conversion is necessary. We display it as-is.

    if (mimetypeStr.contains("pdf"))
    {
      jsWrapper.loadPdf(filePath, pageNum);
      return mimetypeStr;
    }

    // Look for format that JodConverter can convert

    try
    {
      if (mimetypeStr.contains("openxmlformats-officedocument") ||  // docx (Microsoft Word XML)
          "application/msword".equalsIgnoreCase(mimetypeStr)    ||  // doc  (Microsoft Word)
          "application/rtf".equalsIgnoreCase(mimetypeStr)       ||  // rtf  (Rich Text format)
          mimetypeStr.contains("opendocument.text")             ||  // odt  (OpenDocument text), ott (OpenDocument test template)
          mimetypeStr.contains("sun.xml.writer")                ||  // sxw  (OpenOffice.org 1.0 text)
          mimetypeStr.contains("ms-powerpoint")                 ||  // ppt  (Microsoft PowerPoint)
          mimetypeStr.contains("opendocument.presentation")     ||  // odp  (OpenDocument presentation), otp (OpenDocument presentation template)
          mimetypeStr.contains("sun.xml.impress")               ||  // sxi  (OpenOffice.org 1.0 presentation)
          mimetypeStr.contains("vnd.wordperfect")               ||  // wpd  (WordPerfect)
          mimetypeStr.contains("ms-excel")                      ||  // xls  (Microsoft Excel)
          "text/csv".equalsIgnoreCase(mimetypeStr)              ||  // csv  (Comma-separated values)
          mimetypeStr.contains("tab-separated-values")          ||  // tsv  (Tab-separated values)
          mimetypeStr.contains("opendocument.spreadsheet")      ||  // ods  (OpenDocument spreadsheet), ots (OpenDocument spreadsheet template)
          mimetypeStr.contains("sun.xml.calc"))                     // sxc  (OpenOffice.org 1.0 spreadsheet)
      {
        OfficePreviewer.preview(mimetypeStr, filePath, pageNum, jsWrapper, previewWrapper);
      }
      else if (mimetypeStr.contains("html"))
        jsWrapper.loadFile(filePath, true);
      else if (mimetypeStr.contains("image")  || mimetypeStr.contains("plain") || mimetypeStr.contains("video") || mimetypeStr.contains("audio"))
        jsWrapper.loadFile(filePath, false);
      else
        jsWrapper.setUnable(filePath);
    }
    catch (IllegalStateException | IOException e)
    {
      jsWrapper.setUnable(filePath);
    }

    return mimetypeStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshPreview(boolean forceReload, boolean incrementNav)
  {
    if (initialized == false)
      initJS();

    if (window.disablePreviewUpdating) return;

    boolean neededRefresh = needsRefresh;
    needsRefresh = false;

    if (forceReload)
      jsWrapper.reloadBrowser(() -> Platform.runLater(() -> finishRefresh(true, incrementNav)));
    else
      finishRefresh(neededRefresh, incrementNav);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void incrementNav()
  {
    curPrevFile.navNdx++;

    while (curPrevFile.navList.size() > curPrevFile.navNdx)
      curPrevFile.navList.remove(curPrevFile.navNdx);

    curPrevFile.navList.add(pageNum);

    // Now remove adjacent duplicates

    Iterator<Integer> it = curPrevFile.navList.iterator();
    int ndx = 0, prevPage = -1;

    while (it.hasNext())
    {
      int page = it.next();
      if (page == prevPage)
      {
        it.remove();
        if (curPrevFile.navNdx >= ndx)
          curPrevFile.navNdx--;
      }
      else
      {
        ndx++;
        prevPage = page;
      }
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void updatePage(int newPageNum)
  {
    if ((newPageNum < 1) || (pageNum < 1) || FilePath.isEmpty(getFilePath()) || (newPageNum > numPages))
      return;

    setPreview(curPrevFile.filePath, newPageNum, curPrevFile.record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setWorkPageFromContentsWindow(int pageNum, boolean isStart)
  {
    if (isStart)
      workStartPageNum = pageNum;
    else
      workEndPageNum = pageNum;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean setCurPageAsWorkPage(boolean isStart)
  {
    if ((curPrevFile == null) || (curPrevFile.record == null) || (curPrevFile.record.getType() != hdtWork))
      return false;

    if (isStart)
      workStartPageNum = pageNum;
    else
      workEndPageNum = pageNum;

    HDT_Work work = (HDT_Work) curPrevFile.record;
    HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.getRecordFromFilePath(curPrevFile.filePath);

    if (isStart)
      work.setStartPageNum(workFile, pageNum);
    else
      work.setEndPageNum(workFile, pageNum);

    if ((ui.activeTabEnum() == workTabEnum) && (ui.activeTab().activeRecord() == work))
      ui.workHyperTab().setPageNum(workFile, pageNum, isStart);

    if (workFile == null)
      contentsWindow.update(curPrevFile.filePath, pageNum);
    else
      contentsWindow.update(workFile, pageNum);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int getPrevHilite(int curPage)
  {
    if (collEmpty(hilitePages)) return -1;

    int newPage = -1;

    for (Integer page : hilitePages)
      if ((page < curPage) && (page > newPage))
        newPage = page;

    return newPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int getNextHilite(int curPage)
  {
    if (collEmpty(hilitePages)) return -1;

    int newPage = numPages + 1;

    for (Integer page : hilitePages)
      if ((page > curPage) && (page < newPage))
        newPage = page;

    return newPage > numPages ? -1 : newPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void cleanup(Runnable disposeHndlr)
  {
    OfficePreviewer.cleanup();

    if (initialized)
      jsWrapper.cleanup(disposeHndlr);
    else
      disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
