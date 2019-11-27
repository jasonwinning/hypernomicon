/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.dialogs;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.BibDataRetriever;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.view.populators.BibEntryPopulator;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.previewWindow.PDFJSWrapper;
import org.hypernomicon.view.previewWindow.PreviewWrapper;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import com.google.common.collect.Lists;

import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.TextField;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.HBox;
import javafx.stage.Screen;
import javafx.stage.Stage;

public class SelectWorkDlgCtrlr extends HyperDlg
{
  @FXML private ComboBox<HyperTableCell> cbAuthor, cbWork, cbBibEntry;
  @FXML private Button btnCreateNew, btnStop, btnLaunch;
  @FXML private ToggleButton btnPreview;
  @FXML private TextField tfFile;
  @FXML private AnchorPane apMain;
  @FXML private ProgressBar progressBar;
  @FXML private HBox hBox;

  private static final AsyncHttpClient httpClient = new AsyncHttpClient();

  private BibDataRetriever bibDataRetriever = null;
  private BibData bd = null;
  private AnchorPane apPreview;
  private FilePath filePath = null, previewFilePath = null;
  private PDFJSWrapper jsWrapper = null;
  private HyperCB hcbAuthor, hcbWork, hcbBibEntry;
  private HDT_Work work = null;
  private HDT_Person author = null;
  private BibEntry bibEntry = null;
  private boolean createNewClicked = false, previewInitialized = false, bibEntryIsConstant;

  public HDT_Work getWork()     { return work; }
  public BibEntry getBibEntry() { return bibEntry; }
  public BibData getBibData()   { return bd; }
  public HDT_Person getAuthor() { return author; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SelectWorkDlgCtrlr create(HDT_Person authorToUse, boolean enableCreateNew, FilePath filePathToUse)
  {
    SelectWorkDlgCtrlr swd = HyperDlg.create("SelectWorkDlg.fxml", "Select a Work Record", true);
    swd.init(authorToUse, enableCreateNew, filePathToUse, true, null, false);
    return swd;
  }

  public static SelectWorkDlgCtrlr create(HDT_Person authorToUse, boolean enableCreateNew, BibEntry bibEntryToUse)
  {
    SelectWorkDlgCtrlr swd = HyperDlg.create("SelectWorkDlg.fxml", "Select a Work Record", true);
    swd.init(authorToUse, enableCreateNew, null, false, bibEntryToUse, true);
    return swd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Person authorToUse, boolean enableCreateNew, FilePath filePathToUse, boolean filePathIsConstant,
                                                                     BibEntry bibEntryToUse, boolean bibEntryIsConstant)
  {
    filePath = filePathToUse;
    author = authorToUse;
    bibEntry = bibEntryToUse;

    cbBibEntry.setDisable(enableCreateNew == false);

    this.bibEntryIsConstant = bibEntryIsConstant;

    btnStop.setOnAction(event -> stopRetrieving());

    if (FilePath.isEmpty(filePath) == false)
    {
      tfFile.setText(filePath.toString());
      btnLaunch.setOnAction(event -> launchFile(filePath));
    }

    apPreview = new AnchorPane();
    addPreview(stagePane, apMain, apPreview, btnPreview);

    hcbAuthor = new HyperCB(cbAuthor, ctDropDownList, new StandardPopulator(hdtPerson));

    HybridSubjectPopulator workPop = new HybridSubjectPopulator(rtAuthorOfWork);
    workPop.setFilter(id ->
    {
      if (bibEntryIsConstant == false) return true;

      return db.works.getByID(id).getBibEntryKey().isBlank();
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

      return db.getBibLibrary().getNonTrashEntries().stream()
                                                    .filter(entry -> entry.linkedToWork() == false)
                                                    .sorted(BibEntry.comparator())
                                                    .collect(Collectors.toCollection(ArrayList::new));
    }));

    if (enableCreateNew == false)
      hBox.getChildren().remove(btnCreateNew);

    btnCreateNew.setOnAction(event ->
    {
      createNewClicked = true;
      btnOkClick();
    });

    cbAuthor.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (HyperTableCell.getCellID(oldValue) == HyperTableCell.getCellID(newValue))) return;

      ((HybridSubjectPopulator)hcbWork.getPopulator()).setObj(Populator.dummyRow, getRecord(newValue));
      hcbWork.selectID(-1);
    });

    cbWork.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (HyperTableCell.getCellID(oldValue) == HyperTableCell.getCellID(newValue))) return;

      if (filePathIsConstant == false)
      {
        filePath = nullSwitch(newValue.getRecord(), null, HDT_Work::filePath);
        tfFile.setText(nullSwitch(filePath, "", FilePath::toString));
        updatePreview();
      }

      if (bibEntryIsConstant)
      {
        hcbBibEntry.selectID(bibEntry.numericID());
        cbBibEntry.setDisable(true);
        return;
      }

      ((BibEntryPopulator)hcbBibEntry.getPopulator()).populate(Populator.dummyRow, true);
      String workBibEntryKey = nullSwitch(newValue.getRecord(), "", HDT_Work::getBibEntryKey);
      if (workBibEntryKey.isBlank() == false)
      {
        hcbBibEntry.selectID(db.getBibLibrary().numericID(workBibEntryKey));
        cbBibEntry.setDisable(true);
        return;
      }

      hcbBibEntry.selectID(-1);

      if (enableCreateNew)
        cbBibEntry.setDisable(false);
    });

    if (filePathToUse != null)
    {
      setAllVisible(true, btnStop, progressBar);

      bibDataRetriever = new BibDataRetriever(httpClient, null, WorkTypeEnum.wtNone, null, safeListOf(filePathToUse), (pdfBD, queryBD) ->
      {
        setAllVisible(false, btnStop, progressBar);

        if (BibData.isEmpty(pdfBD))
          pdfBD = null;

        BibData bdToUse = queryBD == null ? pdfBD : queryBD;

        if (bdToUse == null) return;

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
              {
                for (String isbn : entry.getMultiStr(BibFieldEnum.bfISBNs))
                {
                  if (isbns.contains(isbn))
                    bibEntry = entry;
                }
              }
            });
          }
        }

        if (author == null) author = findFirstHaving(bdToUse.getAuthors(), bibAuthor ->
        {
          if (bibAuthor.getPerson() != null)
            return bibAuthor.getPerson();

          return HDT_Person.lookUpByName(bibAuthor.getName());
        });

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
              for (HDT_Work curWork : db.works)
              {
                if (curWork.getNameEngChar().toLowerCase().startsWith(title))
                {
                  work = curWork;
                  updateFields();
                  return;
                }
              }
            }
          }
        }

        updateFields();
      });
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
    {
      if (bibAuthor.getPerson() != null)
        return bibAuthor.getPerson();

      return HDT_Person.lookUpByName(bibAuthor.getName());
    });

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

  private void addPreview(AnchorPane stagePane, AnchorPane apMain, AnchorPane apPreview, ToggleButton btnPreview)
  {
    setAnchors(apPreview, apMain.getPrefHeight(), 0.0, 0.0, 0.0);
    setAnchors(apMain, 0.0, null, 0.0, 0.0);

    btnPreview.selectedProperty().addListener((obs, ov, nv) ->
    {
      if (ov.equals(Boolean.FALSE) && nv.equals(Boolean.TRUE))
      {
        height = dialogStage.getHeight();
        dialogStage.setMaxHeight(Double.MAX_VALUE);
        stagePane.getChildren().add(apPreview);
        accommodatePreview(dialogStage, apMain);
        updatePreview();
      }
      else if (ov.equals(Boolean.TRUE) && nv.equals(Boolean.FALSE))
      {
        stagePane.getChildren().remove(apPreview);
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
    ObservableList<Screen> screens = Screen.getScreensForRectangle(dialogStage.getX(), dialogStage.getY(), dialogStage.getWidth(), dialogStage.getHeight());
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

    if ((filePath == null) && (previewFilePath != null))
      jsWrapper.close();

    if ((filePath == null) || filePath.equals(previewFilePath))
      return;

    previewFilePath = filePath;

    PreviewWrapper.showFile(previewFilePath, 1, jsWrapper);
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
