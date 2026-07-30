/*
 * Copyright 2015-2026 Jason Winning
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
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.util.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.PDFJSWrapper.PDFJSOperation;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

/**
 * One preview pane's viewer host and window-facing state: owns the pane's
 * {@link PDFJSWrapper}, executes the load/page/hit commands its
 * {@link PreviewPaneHost} issues, and keeps everything the Preview Window's
 * controls read (current file and record, page and page-label metadata,
 * annotated pages, work start/end pages) plus the pane's navigation history
 * (the file back/forward list and each file's page history), fed by viewer
 * events and intent-driven loads. What to DISPLAY is never decided here; that
 * is the reconciler's job, and setting a pane's intent is the only mutation
 * path.
 */
final class PreviewWrapper
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

  private int fileNdx = -1, pageNum = -1, workStartPageNum = -1, workEndPageNum = -1, numPages = 0;
  private final PreviewSource src;
  private final PreviewWindow window;
  private final Tab tab;
  private boolean initialized = false;
  private PDFJSWrapper jsWrapper;
  private Map<String, Integer> labelToPage;
  private Map<Integer, String> pageToLabel;
  private List<Integer> hilitePages;
  private final List<PreviewFile> fileList = new ArrayList<>();
  private PreviewFile curPrevFile;
  private final ToggleButton btn;
  private final AnchorPane ap;

  /** File-history entry a back/forward file navigation is returning to;
   *  consumed by the next pane-driven load of that file, which then reuses the
   *  entry (keeping its page history and the forward file history) instead of
   *  appending a new one. See {@link #fileNavClick}. */
  private PreviewFile pendingHistoryNav = null;

  PreviewSource getSource()             { return src; }
  int getPageNum()                      { return pageNum; }
  int getNumPages()                     { return numPages; }
  Tab getTab()                          { return tab; }
  FilePath getFilePath()                { return curPrevFile == null ? null : curPrevFile.filePath; }
  int getWorkStartPageNum()             { return workStartPageNum; }
  int getWorkEndPageNum()               { return workEndPageNum; }
  HDT_RecordWithPath getRecord()        { return curPrevFile == null ? null : curPrevFile.record; }
  void prepareToHide()                  { if (initialized) jsWrapper.prepareToHide(); }
  void prepareToShow()                  { if (initialized) jsWrapper.prepareToShow(); }
  void clearAllHits()                   { if (initialized) jsWrapper.clearAllHits(); }
  void setNoOfficeInstallation()        { if (initialized) jsWrapper.setNoOfficeInstallation(); }
  void setStartingConverter()           { if (initialized) jsWrapper.setStartingConverter(); }
  void setUnable(FilePath filePath)     { if (initialized) jsWrapper.setUnable(filePath); }
  void setGenerating(FilePath filePath) { if (initialized) jsWrapper.setGenerating(filePath); }

  /** Shutdown-only: detach the browser view from the scene graph before the
   *  preview stage closes. See {@link PDFJSWrapper#detachBrowserView()}. */
  void detachBrowserView()              { if (initialized) jsWrapper.detachBrowserView(); }

  int lowestHilitePage()                { return collEmpty(hilitePages) ? -1 : hilitePages.getFirst(); }
  int highestHilitePage()               { return collEmpty(hilitePages) ? -1 : hilitePages.getLast(); }
  int getPageByLabel(String label)      { return collEmpty(labelToPage) ? parseInt(label, -1) : labelToPage.getOrDefault(label, -1); }
  String getLabelByPage(int page)       { return collEmpty(pageToLabel) ? String.valueOf(page) : pageToLabel.getOrDefault(page, ""); }
  boolean zoom(boolean zoomingIn)       { return (jsWrapper != null) && jsWrapper.zoom(zoomingIn); }

  void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage) { if (initialized) jsWrapper.scrollToHighlight(matchNdx, pageNum, ndxOnPage); }
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

  /**
   * Viewer lifecycle events forwarded to the reconciler driving this wrapper;
   * the wrapper's own bookkeeping (nav history, window controls) runs
   * alongside, fed by the same events.
   */
  interface PaneEventSink
  {
    /**
     * A document load completed (a pdf.js open, or a direct-content navigation
     * finishing). {@code file} is the document the load was for; the consumer
     * must match it against what it issued, because a superseded open still
     * reports here before the newest request's load has run.
     */
    void onOpened(FilePath file, boolean success);

    void onPageChanged(int pageNum);
  }

  private PaneEventSink paneEventSink = null;

  void setPaneEventSink(PaneEventSink sink) { paneEventSink = sink; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void doneHndlr(PDFJSOperation operation, FilePath file, boolean success, String errMessage)
  {
    switch (operation)
    {
      case pjsOpen: case pjsDirectLoad:

        if (paneEventSink != null)
          paneEventSink.onOpened(file, success);

        if (curPrevFile == null) return;

        if (operation == PDFJSOperation.pjsOpen)
          numPages = jsWrapper.getNumPages();

        Platform.runLater(() ->
        {
          if ((curPrevFile != null) && (curPrevFile.navNdx == -1))
            incrementNav();

          refreshControls();
        });

        break;

      case pjsClose:

        break;

      default :
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pageChangeHndlr(int newPageNum)
  {
    if (paneEventSink != null)
      paneEventSink.onPageChanged(newPageNum);

    // A change Java did not already know about is viewer-originated (the user
    // scrolled): it enters the page-nav history here. Java-initiated jumps
    // recorded at issue time (recordChromePageNav) and reconciler-driven ones
    // (which do not enter history) pre-set pageNum, so they only refresh the
    // window controls.

    if ((curPrevFile != null) && (pageNum != newPageNum))
    {
      pageNum = newPageNum;
      incrementNav();
    }

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

    // This pane's viewer attaches into the (non-modal) Preview Window. On macOS the
    // process's first BrowserView attach has to happen in a modal window or a later attach
    // in a modal dialog can kill the JVM, so make a throwaway modal attach first if nothing
    // else has. No-op elsewhere, and after the first time.

    BrowserEngine.primeModalAttach();

    jsWrapper = new PDFJSWrapper(ap, this::doneHndlr, this::pageChangeHndlr, this::retrievedDataHndlr);

    if (jxBrowserDisabled) return;

    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Lazy-initializes the underlying jsWrapper if it hasn't been created yet.
   *  Returns {@code true} if the wrapper is initialized after the call (which
   *  is always the case unless JxBrowser is disabled). */
  boolean ensureInitialized()
  {
    if (initialized == false)
      initJS();

    return initialized;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Reloads the embedded browser (recreating the viewer page), then runs
   *  {@code done}; the caller re-issues the display afterward. */
  void reloadViewer(Runnable done)
  {
    if (initialized)
      jsWrapper.reloadBrowser(done);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Load a paged document (a native PDF, or the PDF output of an office
   *  conversion) into the pdf.js viewer. */
  private void loadPagedDocument(FilePath displayPath, int pageNum)
  {
    jsWrapper.setContentToShowIsDirect(false);
    jsWrapper.loadPdf(displayPath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records {@code sourceFile} as the file this pane is showing: reuses the
   * file-history entry a back/forward navigation is returning to (keeping its
   * page history and the forward file history), else appends a new entry,
   * truncating any forward history. Also resets the per-document metadata the
   * previous document left behind.
   */
  private void trackFile(FilePath sourceFile, HDT_Record record)
  {
    if ((record != null) && (record.getType() != hdtWork    ) && (record.getType() != hdtMiscFile) &&
                            (record.getType() != hdtWorkFile) && (record.getType() != hdtPerson  ))
      record = null;

    if ((pendingHistoryNav != null) && pendingHistoryNav.filePath.equals(sourceFile))
    {
      curPrevFile = pendingHistoryNav;  // fileNdx was repositioned by fileNavClick
    }
    else
    {
      curPrevFile = new PreviewFile(sourceFile, (HDT_RecordWithPath) record);

      fileNdx++;

      while (fileList.size() > fileNdx)
        fileList.remove(fileNdx);

      fileList.add(curPrevFile);
    }

    pendingHistoryNav = null;

    labelToPage = null;
    pageToLabel = null;
    hilitePages = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pane-driven paged load: displays {@code displayPath} in the pdf.js viewer
   * at the given page, tracking {@code sourceFile} as the file being shown
   * (they differ for LibreOffice-converted office documents).
   *
   * @param sourceFile  the file the user asked to preview (used for file tracking)
   * @param displayPath the file the viewer actually loads (source itself, or converted artifact)
   * @param pageNum     1-based page to open at
   * @param record      record associated with the source file, or {@code null}
   */
  void paneShowPaged(FilePath sourceFile, FilePath displayPath, int pageNum, HDT_Record record)
  {
    if (ensureInitialized() == false) return;

    trackFile(sourceFile, record);

    this.pageNum = pageNum;

    loadPagedDocument(displayPath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pane-driven direct-content load: displays {@code displayPath} as direct
   * browser content (HTML, plain text, image, media), with the same
   * source-file tracking as {@link #paneShowPaged}.
   *
   * @return true if content was loaded; false if the file kind cannot be
   *         previewed or loading failed (the unable indicator is shown)
   */
  boolean paneShowDirect(FilePath sourceFile, FilePath displayPath, HDT_Record record)
  {
    if (ensureInitialized() == false) return false;

    trackFile(sourceFile, record);

    pageNum = 1;
    numPages = 1;

    try
    {
      if (jsWrapper.loadDirectContent(displayPath))
        return true;

      jsWrapper.setContentToShowIsDirect(false);
      jsWrapper.setUnable(sourceFile);
    }
    catch (IllegalStateException | IOException e)
    {
      jsWrapper.setUnable(sourceFile);
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Pane-driven page navigation within the currently-displayed document. */
  void paneGoToPage(int pageNum)
  {
    if (initialized == false) return;

    this.pageNum = pageNum;
    jsWrapper.goToPage(pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Push FTS hit JSON to the underlying jsWrapper. The reconciler delivers
   *  hits only after the intended document's load is confirmed, which implies
   *  the wrapper is initialized; a push before then is a caller bug. */
  void setAllHits(String allHitsJson)
  {
    if (initialized)
      jsWrapper.setAllHits(allHitsJson);
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
      jumpToHistoryPage(page);
    });

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void navClick(boolean isForward)
  {
    if (enableNavButton(isForward) == false) return;

    curPrevFile.navNdx += (isForward ? 1 : -1);

    jumpToHistoryPage(curPrevFile.navList.get(curPrevFile.navNdx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Navigates to a page reached through the back/forward history: the history
   * index was already repositioned, so no entry is recorded, and
   * {@link #pageNum} is pre-set so the resulting page-change event does not
   * re-enter history either.
   */
  private void jumpToHistoryPage(int page)
  {
    pageNum = page;

    PreviewWindow.hostFor(src).navigateToPage(page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records a page jump made with the window's controls (slider, page buttons,
   * page field, ContentsWindow) in this file's page-nav history at issue time;
   * the jump itself is then issued through the pane's intent. Also pre-sets
   * {@link #pageNum} so the resulting page-change event is recognized as
   * Java-initiated rather than user scrolling.
   */
  void recordChromePageNav(int page)
  {
    if (curPrevFile == null) return;

    pageNum = page;
    incrementNav();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Back/forward file navigation: repositions the file history and re-previews
   * that entry through the pane's intent. The entry itself is reused by the
   * resulting load (see {@link #trackFile}), preserving its page history and
   * the forward file history.
   */
  void fileNavClick(boolean isForward)
  {
    int newNdx = isForward ? getNextFileNdx() : getPreviousFileNdx();
    if (newNdx < 0) return;

    fileNdx = newNdx;

    PreviewFile prevFile = fileList.get(fileNdx);

    int newPageNum = prevFile.navNdx < 0 ? 1 : prevFile.navList.get(prevFile.navNdx);

    curPrevFile = prevFile;        // reflect the target immediately (controls, launch, go)
    pendingHistoryNav = prevFile;  // the load that follows reuses this entry

    PreviewWindow.hostFor(src).setPreviewAuto(prevFile.filePath, prevFile.record, newPageNum);
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
    pageNum = -1;
    workStartPageNum = -1;
    workEndPageNum = -1;
    curPrevFile = null;
    pendingHistoryNav = null;

    if (window.curSource() == src) window.clearControls();

    if (initialized)
      jsWrapper.reset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshControls()
  {
    FilePath filePath = getFilePath();

    if ((pageNum <= 0) || FilePath.isEmpty(filePath) || (filePath.exists() == false))
    {
      clearPreview();
      return;
    }

    window.refreshControls(pageNum, numPages, this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This pane's tab became the active one. The display itself needs nothing
   * (each tab has its own browser, and loads deferred while this source was
   * not the active, showing one replay through the intent path below); only
   * the window controls are brought up to date.
   */
  void activate()
  {
    btn.setSelected(true);

    if (curPrevFile == null)
      window.clearControls();
    else
      refreshControls();

    // This source is now active and showing, so any preview work a caller
    // deferred while it was not (record navigation with the window closed, the
    // FTS hit pipeline) can now run; the deferred caller has the last word on
    // what is displayed.

    PreviewWindow.fireActivation(getSource());
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
      ContentsWindow.instance().update(curPrevFile.filePath, pageNum);
    else
      ContentsWindow.instance().update(workFile, pageNum);

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

  private ConversionSession leasedArtifactSession = null;

  /**
   * Records that this pane is displaying the given session's artifact, leasing
   * it against cache eviction and releasing the lease on whatever artifact this
   * pane displayed before. Called on the FX thread (display callbacks).
   */
  void leaseArtifact(ConversionSession session)
  {
    if (leasedArtifactSession == session) return;

    if (leasedArtifactSession != null)
      leasedArtifactSession.release();

    leasedArtifactSession = session;
    session.lease();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void cleanup(Runnable disposeHndlr)
  {
    if (leasedArtifactSession != null)
    {
      leasedArtifactSession.release();
      leasedArtifactSession = null;
    }

    OfficePreviewer.cleanup();

    if (initialized)
      jsWrapper.cleanup(disposeHndlr);
    else
      disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
