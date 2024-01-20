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

package org.hypernomicon.dialogs;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.GUIBibData;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWrapper;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.view.populators.BibEntryPopulator;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import com.google.common.collect.Lists;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Screen;
import javafx.stage.Stage;

public class SelectWorkDlgCtrlr extends HyperDlg
{
  @FXML private ComboBox<HyperTableCell> cbAuthor, cbWork, cbBibEntry;
  @FXML private Button btnCreateNew, btnStop, btnLaunch;
  @FXML private Label lblBibEntry;
  @FXML private ToggleButton btnPreview;
  @FXML private TextField tfFile;
  @FXML private AnchorPane apMain;
  @FXML private ProgressBar progressBar;

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private final BibDataRetriever bibDataRetriever;
  private final HyperCB hcbAuthor, hcbWork, hcbBibEntry;
  private final boolean bibEntryIsConstant;

  private BibData bd = null;
  private AnchorPane apPreview;
  private FilePath filePath = null, previewFilePath = null;
  private PDFJSWrapper jsWrapper = null;
  private HDT_Work work = null;
  private HDT_Person author = null;
  private BibEntry<?, ?> bibEntry = null;
  private boolean createNewClicked = false, previewInitialized = false;

  public HDT_Work getWork()           { return work; }
  public BibEntry<?, ?> getBibEntry() { return HDT_Work.isUnenteredSet(work) ? null : bibEntry; }
  public BibData getBibData()         { return bd; }
  public HDT_Person getAuthor()       { return author; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SelectWorkDlgCtrlr(HDT_Person authorToUse, FilePath filePathToUse)
  {
    this(null, authorToUse, filePathToUse, true, null, false);
  }

  public SelectWorkDlgCtrlr(HDT_Work workToUse, BibEntry<?, ?> bibEntryToUse)
  {
    this(workToUse, null, null, false, bibEntryToUse, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SelectWorkDlgCtrlr(HDT_Work workToUse, HDT_Person authorToUse, FilePath       filePathToUse, boolean filePathIsConstant,
                                                                         BibEntry<?, ?> bibEntryToUse, boolean bibEntryIsConstant)
  {
    super("SelectWorkDlg", "Select a Work Record", true);

    bibEntry = bibEntryToUse;
    work = workToUse;

    lblBibEntry.setText(db.bibLibraryIsLinked() ? "Existing " + db.getBibLibrary().type().getUserFriendlyName() + " entry:" : "");

    if (db.bibLibraryIsLinked() == false)
      cbBibEntry.setDisable(true);

    if (work == null)
    {
      if ((authorToUse == null) && (bibEntry != null))
      {
        String doi = bibEntry.getStr(BibFieldEnum.bfDOI);

        if (doi.length() > 0)
        {
          for (HDT_Work curWork : db.works)
          {
            if (curWork.getDOI().equalsIgnoreCase(doi))
            {
              work = curWork;
              break;
            }
          }
        }

        if ((work == null) && (HDT_WorkType.getEnumVal(bibEntry.getWorkType()) == WorkTypeEnum.wtBook))
        {
          List<String> isbns = bibEntry.getMultiStr(BibFieldEnum.bfISBNs);

          if (isbns.isEmpty() == false)
            for (HDT_Work curWork : db.works)
              if (curWork.getWorkTypeEnum() == WorkTypeEnum.wtBook)
                if (curWork.getISBNs().stream().anyMatch(isbns::contains))
                  work = curWork;
        }

        if (work == null)
          authorToUse = findFirstHaving(bibEntry.getAuthors(), bibAuthor -> HDT_Person.lookUpByName(bibAuthor.getName()));
      }
    }
    else
      authorToUse = null;

    filePath = filePathToUse;
    author = authorToUse;

    this.bibEntryIsConstant = bibEntryIsConstant;

    btnStop.setOnAction(event -> stopRetrieving());

    if (FilePath.isEmpty(filePath) == false)
    {
      tfFile.setText(filePath.toString());
      btnLaunch.setOnAction(event -> launchFile(filePath));
    }

    onShown = this::addPreview; // set anchors after dialog has been rescaled

    hcbAuthor = new HyperCB(cbAuthor, ctDropDownList, new StandardPopulator(hdtPerson));
    hcbAuthor.dontCreateNewRecord = true;

    HybridSubjectPopulator workPop = new HybridSubjectPopulator(rtAuthorOfWork, id ->
    {
      if (bibEntryIsConstant == false) return true;

      HDT_Work curWork = db.works.getByID(id);

      return (HDT_Work.isUnenteredSet(curWork) == false) && curWork.getBibEntryKey().isBlank();
    });

    hcbWork = new HyperCB(cbWork, ctDropDownList, workPop);
    hcbBibEntry = new HyperCB(cbBibEntry, ctDropDownList, new BibEntryPopulator((row, force) ->
    {
      if (db.bibLibraryIsLinked() == false)
        return new ArrayList<>();

      if (bibEntryIsConstant)
        return Lists.newArrayList(bibEntry);

      String workBibEntryKey = nullSwitch(hcbWork.selectedRecord(), "", HDT_Work::getBibEntryKey);
      if (workBibEntryKey.isBlank() == false)
        return Lists.newArrayList(db.getBibEntryByKey(workBibEntryKey));

      return db.getBibLibrary().getNonTrashEntries().filter(entry -> entry.linkedToWork() == false)
                                                    .sorted(BibEntry.comparator())
                                                    .collect(Collectors.toCollection(ArrayList::new));
    }));

    btnCreateNew.setOnAction(event ->
    {
      createNewClicked = true;
      btnOkClick();
    });

    hcbAuthor.addListener((oldValue, newValue) ->
    {
      if ((newValue == null) || (getCellID(oldValue) == getCellID(newValue))) return;

      ((HybridSubjectPopulator)hcbWork.getPopulator()).setObj(getRecord(newValue));
      hcbWork.selectID(-1);
    });

    hcbWork.addListener((oldValue, newValue) ->
    {
      if ((newValue == null) || (getCellID(oldValue) == getCellID(newValue))) return;

      if (filePathIsConstant == false)
      {
        filePath = nullSwitch(newValue.getRecord(), null, HDT_Work::filePath);
        tfFile.setText(nullSwitch(filePath, "", FilePath::toString));
        updatePreview();
      }

      if (db.bibLibraryIsLinked() == false) return;

      if (bibEntryIsConstant)
      {
        hcbBibEntry.selectID(bibEntry.numericID());
        cbBibEntry.setDisable(true);
        return;
      }

      hcbBibEntry.populate(true);
      String workBibEntryKey = nullSwitch(newValue.getRecord(), "", HDT_Work::getBibEntryKey);
      if (workBibEntryKey.isBlank() == false)
      {
        hcbBibEntry.selectID(db.getBibLibrary().numericID(workBibEntryKey));
        cbBibEntry.setDisable(true);
        return;
      }

      hcbBibEntry.selectID(-1);

      cbBibEntry.setDisable(HDT_Work.isUnenteredSet(newValue.getRecord()));
    });

    if (filePathToUse != null)
    {
      setAllVisible(true, btnStop, progressBar);

      bibDataRetriever = new BibDataRetriever(httpClient, null, safeListOf(filePathToUse), (pdfBD, queryBD, messageShown) ->
      {
        setAllVisible(false, btnStop, progressBar);

        if (BibData.isEmpty(pdfBD))
          pdfBD = null;

        BibData bdToUse = queryBD == null ? pdfBD : queryBD;

        if (bdToUse == null)
        {
          bd = GUIBibData.NoneFoundBD;
          return;
        }

        bd = bdToUse;

        if (bibEntryIsConstant == false)
          bibEntry = null;

        String doi = bdToUse.getStr(BibFieldEnum.bfDOI);

        if (doi.length() > 0)
        {
          for (HDT_Work curWork : db.works)
          {
            if (curWork.getDOI().equalsIgnoreCase(doi))
            {
              work = curWork;
              updateFields();
              return;
            }
          }

          if ((bibEntry == null) && db.bibLibraryIsLinked())
          {
            db.getBibLibrary().getNonTrashEntries().forEach(entry ->
            {
              if (entry.getStr(BibFieldEnum.bfDOI).equalsIgnoreCase(doi))
                bibEntry = entry;
            });
          }
        }

        List<String> isbns = bdToUse.getMultiStr(BibFieldEnum.bfISBNs);

        if (isbns.isEmpty() == false)
        {
          for (HDT_Work curWork : db.works)
          {
            if (curWork.getWorkTypeEnum() == WorkTypeEnum.wtBook) for (String isbn : curWork.getISBNs())
            {
              if (isbns.contains(isbn))
              {
                work = curWork;
                updateFields();
                return;
              }
            }
          }

          if ((bibEntry == null) && db.bibLibraryIsLinked())
          {
            db.getBibLibrary().getNonTrashEntries().forEach(entry ->
            {
              if ((entry.linkedToWork() == false) && (HDT_WorkType.getEnumVal(entry.getWorkType()) == WorkTypeEnum.wtBook))
                for (String isbn : entry.getMultiStr(BibFieldEnum.bfISBNs))
                  if (isbns.contains(isbn))
                  {
                    bibEntry = entry;
                    break;
                  }
            });
          }
        }

        if (author == null) author = findFirstHaving(bdToUse.getAuthors(), bibAuthor ->
          bibAuthor.getPerson() != null ?
            bibAuthor.getPerson()
          :
            HDT_Person.lookUpByName(bibAuthor.getName()));

        List<String> titleList = bdToUse.getMultiStr(BibFieldEnum.bfTitle);
        if (titleList.isEmpty() == false)
        {
          String title = convertToEnglishChars(titleList.get(0).toLowerCase());

          if (title.length() > 0)
          {
            if (author != null)
            {
              for (HDT_Work curWork : author.works)
              {
                if (curWork.getNameEngChar().toLowerCase().startsWith(title))
                {
                  work = curWork;
                  updateFields();
                  return;
                }
              }
            }
            else
            {
              String fullTitle = convertToEnglishChars(strListToStr(titleList, false)).toLowerCase().replaceAll("[^a-z0-9 ]", "");
              HDT_Work firstMatch = null;
              boolean multipleMatches = false;

              for (HDT_Work curWork : db.works)
              {
                if (curWork.getNameEngChar().toLowerCase().replaceAll("[^a-z0-9 ]", "").equals(fullTitle))
                {
                  work = curWork;
                  updateFields();
                  return;
                }

                if ((multipleMatches == false) && curWork.getNameEngChar().toLowerCase().startsWith(title))
                {
                  if (firstMatch == null)
                    firstMatch = curWork;
                  else
                    multipleMatches = true;
                }
              }

              if ((multipleMatches == false) && (firstMatch != null))
              {
                work = firstMatch;
                updateFields();
                return;
              }
            }
          }
        }

        updateFields();
      });
    }
    else
    {
      this.bibDataRetriever = null;
    }

    updateFields();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateFields()
  {
    hcbAuthor.clear();
    hcbWork.clear();

    if ((author == null) && (work != null)) author = findFirstHaving(work.getBibData().getAuthors(), bibAuthor ->
      bibAuthor.getPerson() != null ?
        bibAuthor.getPerson()
      :
        HDT_Person.lookUpByName(bibAuthor.getName()));

    hcbAuthor.addAndSelectEntryOrBlank(author, HDT_Record::getCBText);

    if (author != null)
      hcbWork.selectID(nullSwitch(work, -1, HDT_Record::getID));
    else
      hcbWork.addAndSelectEntryOrBlank(work, HDT_Record::getCBText);

    if (((work == null) || bibEntryIsConstant) && (bibEntry != null))
      hcbBibEntry.selectID(bibEntry.numericID());

    if (bibEntryIsConstant)
      cbBibEntry.setDisable(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void stopRetrieving()
  {
    httpClient.stop();
    if (bibDataRetriever != null)
      bibDataRetriever.stop();

    setAllVisible(false, btnStop, progressBar);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addPreview()
  {
    apPreview = new AnchorPane();

    setAnchors(apPreview, apMain.getPrefHeight(), 0.0, 0.0, 0.0);
    setAnchors(apMain, 0.0, null, 0.0, 0.0);

    btnPreview.selectedProperty().addListener((obs, ov, nv) ->
    {
      if (ov.equals(Boolean.FALSE) && nv.equals(Boolean.TRUE))
      {
        height = dialogStage.getHeight();
        dialogStage.setMaxHeight(Double.MAX_VALUE);
        addToParent(apPreview, stagePane);
        accommodatePreview(dialogStage, apMain);
        updatePreview();
      }
      else if (ov.equals(Boolean.TRUE) && nv.equals(Boolean.FALSE))
      {
        removeFromParent(apPreview);
        dialogStage.setHeight(height);
        dialogStage.setMaxHeight(height);
      }
    });
  }

  private static Double height;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void accommodatePreview(Stage dialogStage, AnchorPane apMain)
  {
    List<Screen> screens = Screen.getScreensForRectangle(dialogStage.getX(), dialogStage.getY(), dialogStage.getWidth(), dialogStage.getHeight());
    double minHeight = screens.size() == 1 ? screens.get(0).getVisualBounds().getHeight() - 60.0 : 900.0;

    if (dialogStage.getHeight() < minHeight)
    {
      double diff = minHeight - dialogStage.getHeight();
      dialogStage.setY(dialogStage.getY() - (diff / 2.0));
      dialogStage.setHeight(minHeight);
      ensureVisible(dialogStage, apMain.getPrefWidth(), apMain.getPrefHeight());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updatePreview()
  {
    if (jxBrowserDisabled) return;

    if (previewInitialized == false) jsWrapper = new PDFJSWrapper(apPreview, null, null, null);

    if (jxBrowserDisabled) return;

    previewInitialized = true;

    if (FilePath.isEmpty(filePath) && (FilePath.isEmpty(previewFilePath) == false))
      jsWrapper.close();

    if (FilePath.isEmpty(filePath) || filePath.equals(previewFilePath))
      return;

    previewFilePath = filePath;

    PreviewWrapper.showFile(previewFilePath, 1, jsWrapper);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean showModal()
  {
    boolean rv = super.showModal();

    if (previewInitialized)
      jsWrapper.cleanup();

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if ((hcbWork.selectedID() < 1) && (createNewClicked == false))
      return falseWithInfoMessage("Select a work record.", cbWork);

    work = createNewClicked ? null : hcbWork.selectedRecord();
    author = hcbAuthor.selectedRecord();

    if (db.bibLibraryIsLinked())
    {
      int id = hcbBibEntry.selectedID();
      bibEntry = id < 1 ? null : db.getBibLibrary().getEntryByID(id);

      if ((bibEntry != null) && bibEntry.linkedToWork() && (work == null))
        bibEntry = null;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
