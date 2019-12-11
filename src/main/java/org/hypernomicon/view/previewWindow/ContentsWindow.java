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

package org.hypernomicon.view.previewWindow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.view.dialogs.HyperDlg;
import org.hypernomicon.view.tabs.WorkTabCtrlr;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.fxml.FXML;
import javafx.scene.control.TableView;
import javafx.stage.Modality;
import javafx.stage.StageStyle;

public class ContentsWindow extends HyperDlg
{
  @FXML private TableView<HyperTableRow> tvContents;

  public static final String dialogTitle = "Contents";
  private HyperTable htContents;
  private HDT_WorkFile curWorkFile;
  private boolean mouseAlreadyHere = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ContentsWindow create()
  {
    ContentsWindow contentsWindow = HyperDlg.createUsingFullPath("view/previewWindow/ContentsWindow.fxml", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);
    contentsWindow.init();
    return contentsWindow;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    htContents = new HyperTable(tvContents, 2, false, PREF_KEY_HT_CONTENTS_DLG, this);

    htContents.addCol(hdtPerson, ctNone);
    htContents.addCol(hdtWorkType, ctNone);
    htContents.addCol(hdtWork, ctNone);
    htContents.addCol(hdtWork, ctNone);

    htContents.addTextEditColWithUpdateHandler(hdtWork, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (previewWindow.disablePreviewUpdating) return;

      int pageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);

      nullSwitch(row.getRecord(), work -> setPageNum((HDT_Work)work, pageNum, true));
    });

    htContents.addCustomActionCol(4, "Go", (row, colNdx) ->
    {
      previewWindow.goToPage(parseInt(row.getText(4), -1));
      ui.openPreviewWindow(null);
    });

    htContents.addCustomActionCol(4, "Set", (row, colNdx) ->
    {
      HDT_Work work = row.getRecord();
      int num = previewWindow.curPage();

      if (num < 0) num = 1;

      row.setCellValue(4, work, String.valueOf(num));
      setPageNum(work, num, true);
    });

    htContents.addTextEditColWithUpdateHandler(hdtWork, false, true, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (previewWindow.disablePreviewUpdating) return;

      int pageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);

      nullSwitch(row.getRecord(), work -> setPageNum((HDT_Work) work, pageNum, false));
    });

    htContents.addCustomActionCol(7, "Go", (row, colNdx) ->
    {
      previewWindow.goToPage(parseInt(row.getText(7), -1));
      ui.openPreviewWindow(null);
    });

    htContents.addCustomActionCol(7, "Set", (row, colNdx) ->
    {
      HDT_Work work = row.getRecord();
      int num = previewWindow.curPage();

      if (num < 0) num = previewWindow.getMax();

      row.setCellValue(7, work, String.valueOf(num));
      setPageNum(work, num, false);
    });

    onShown = () -> ui.windows.push(dialogStage);

    dialogStage.getScene().setOnMouseEntered(event -> mouseAlreadyHere = true);  // Don't refresh when user clicks a button while dialog is out of focus
    dialogStage.getScene().setOnMouseExited (event -> mouseAlreadyHere = false);

    dialogStage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus()) return;

      if (Boolean.TRUE.equals(newValue) == false) return;

      if (!mouseAlreadyHere)
        update(curWorkFile, previewWindow.curPage(), false);

      ui.windows.push(dialogStage);
    });

    dialogStage.setOnHidden(event -> ui.windows.focusStage(app.getPrimaryStage()));

    htContents.setTooltip(5, ButtonAction.baCustom, "Jump to start page in preview window");
    htContents.setTooltip(8, ButtonAction.baCustom, "Jump to end page in preview window");
    htContents.setTooltip(6, ButtonAction.baCustom, "Assign page currently visible in preview window as start page");
    htContents.setTooltip(9, ButtonAction.baCustom, "Assign page currently visible in preview window as end page");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPageNum(HDT_Work work, int num, boolean isStart)
  {
    if ((ui.activeTabEnum() == workTabEnum) && (ui.activeTab().activeRecord() == work))
      ui.workHyperTab().setPageNum(curWorkFile, num, isStart);
    else
    {
      if (isStart)
        work.setStartPageNum(curWorkFile, num);
      else
        work.setEndPageNum(curWorkFile, num);
    }

    previewWindow.updatePageNumber(work, curWorkFile, num, isStart);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update(HDT_WorkFile workFile, int curPage, boolean setFocus)
  {
    Property<HyperTableRow> rowToSelect = new SimpleObjectProperty<>(null);

    clear();

    if (workFile == null) return;

    curWorkFile = workFile;

    dialogStage.setTitle(dialogTitle + " - " + workFile.getPath().getNameStr());

    List<HDT_Work> works = new ArrayList<>(workFile.works);

    works.sort((work1, work2) ->
    {
      int startPage1, startPage2;

      startPage1 = work1.getStartPageNum(workFile);
      if (startPage1 < 0) startPage1 = Integer.MAX_VALUE;

      startPage2 = work2.getStartPageNum(workFile);
      if (startPage2 < 0) startPage2 = Integer.MAX_VALUE;

      return startPage1 - startPage2;
    });

    htContents.buildRows(works, (row, work) ->
    {
      String authStr = work.getShortAuthorsStr(true), title = work.name(), year = work.getYear();
      HDT_WorkType workType = work.workType.get();
      int authorID = -1;
      WorkTabCtrlr wtc = null;

      if (work.authorRecords.size() > 0)
        authorID = work.authorRecords.get(0).getID();

      if ((ui.activeTabEnum() == workTabEnum) && (ui.activeTab().activeRecord() == work))
      {
        wtc = ui.workHyperTab();

        title = wtc.getTitle();
        year = wtc.tfYear.getText();
        workType = wtc.hcbType.selectedRecord();
        authStr = wtc.getShortAuthorsStr();
      }

      row.setCellValue(0, authorID, authStr, hdtPerson);

      if (workType != null)
        row.setCellValue(1, workType, workType.listName());

      row.setCellValue(2, work, title);
      row.setCellValue(3, work, year, HyperCellSortMethod.hsmNumeric);

      int pageNum = wtc == null ? -1 : wtc.getCurPageNum(work, workFile, true);

      if (pageNum == -1)
        pageNum = work.getStartPageNum(workFile);

      if (pageNum > -1)
      {
        row.setCellValue(4, work, String.valueOf(pageNum));

        if ((curPage == pageNum) && (rowToSelect.getValue() == null))
          rowToSelect.setValue(row);
      }

      pageNum = wtc == null ? -1 : wtc.getCurPageNum(work, workFile, false);

      if (pageNum == -1)
        pageNum = work.getEndPageNum(workFile);

      if (pageNum > -1)
      {
        row.setCellValue(7, work, String.valueOf(pageNum));

        if ((curPage == pageNum) && (rowToSelect.getValue() == null))
          rowToSelect.setValue(row);
      }
    });

    if (!setFocus) return;

    if (rowToSelect.getValue() != null)
      htContents.selectRow(rowToSelect.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    curWorkFile = null;
    dialogStage.setTitle(dialogTitle);
    htContents.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
