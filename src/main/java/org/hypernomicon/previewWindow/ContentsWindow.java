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

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.Const.TablePrefKey;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutAction;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.BibDateHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.tabs.WorkTabCtrlr;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public final class ContentsWindow extends NonmodalWindow
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TableView<HyperTableRow> tvContents;

  private static final String dialogTitle = "Contents";

  private static ContentsWindow instance;

  private final HyperTable htContents;

  private HDT_WorkFile curWorkFile;
  private FilePath curFilePath;
  private boolean mouseAlreadyHere = false;

//---------------------------------------------------------------------------

  @Override protected void getDividerPositions() { }
  @Override protected void setDividerPositions() { }

//---------------------------------------------------------------------------

  public static ContentsWindow instance()
  {
    if (instance == null) instance = new ContentsWindow();

    return instance;
  }

  public static void close(boolean exitingApp) { close(instance, exitingApp); }

//---------------------------------------------------------------------------

  private ContentsWindow()
  {
    super("previewWindow/ContentsWindow", dialogTitle, PrefKey.CONTENTS_WINDOW_X, PrefKey.CONTENTS_WINDOW_Y, PrefKey.CONTENTS_WINDOW_WIDTH, PrefKey.CONTENTS_WINDOW_HEIGHT);

    htContents = new HyperTable(tvContents, 2, false, TablePrefKey.CONTENTS_DLG, this);

    htContents.addLabelCol(hdtPerson);
    htContents.addLabelCol(hdtWorkType);
    htContents.addLabelCol(hdtWork);
    htContents.addLabelCol(hdtWork, smStandard);  // Date column

    htContents.addTextEditColWithUpdateHandler(hdtWork, false, smNumeric, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (PreviewWindow.disablePreviewUpdating) return;

      int pageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);

      nullSwitch(row.getRecord(), (HDT_Work work) -> setPageNum(work, pageNum, true));
    });

    htContents.addCustomActionCol(4, "Go", (row, colNdx) ->
    {
      PreviewWindow.instance().goToPage(parseInt(row.getText(4), -1));
      PreviewWindow.show();

    }).setButtonTooltip(ButtonAction.baCustom, "Jump to start page in preview window");

    htContents.addCustomActionCol(4, "Set", (row, colNdx) ->
    {
      HDT_Work work = row.getRecord();
      int num = PreviewWindow.instance().curPage();

      if (num < 0) num = 1;

      row.setCellValue(4, work, String.valueOf(num));
      setPageNum(work, num, true);

    }).setButtonTooltip(ButtonAction.baCustom, "Assign page currently visible in preview window as start page");

    htContents.addTextEditColWithUpdateHandler(hdtWork, false, smNumeric, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (PreviewWindow.disablePreviewUpdating) return;

      int pageNum = parseInt(HyperTableCell.getCellText(cellVal), -1);

      nullSwitch(row.getRecord(), (HDT_Work work) -> setPageNum(work, pageNum, false));
    });

    htContents.addCustomActionCol(7, "Go", (row, colNdx) ->
    {
      PreviewWindow.instance().goToPage(parseInt(row.getText(7), -1));
      PreviewWindow.show();

    }).setButtonTooltip(ButtonAction.baCustom, "Jump to end page in preview window");

    htContents.addCustomActionCol(7, "Set", (row, colNdx) ->
    {
      HDT_Work work = row.getRecord();
      int num = PreviewWindow.instance().curPage();

      if (num < 0) num = PreviewWindow.instance().getMax();

      row.setCellValue(7, work, String.valueOf(num));
      setPageNum(work, num, false);

    }).setButtonTooltip(ButtonAction.baCustom, "Assign page currently visible in preview window as end page");

    stage.getScene().setOnMouseEntered(event -> mouseAlreadyHere = true);  // Don't refresh when user clicks a button while dialog is out of focus
    stage.getScene().setOnMouseExited (event -> mouseAlreadyHere = false);

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus() || (Boolean.TRUE.equals(newValue) == false))
        return;

      if (mouseAlreadyHere == false)
        update(PreviewWindow.instance().curPage(), false);
    });

    registerShortcuts();

    app.shortcuts.addListener((obs, ov, nv) -> registerShortcuts());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerShortcuts()
  {
    Scene scene = stage.getScene();

    // Clear old accelerators first

    scene.getAccelerators().clear();

  //---------------------------------------------------------------------------

    // Hard-coded shortcuts

  //---------------------------------------------------------------------------

    // User-defined shortcuts

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToMainWindow   , () -> ui.windows.focusStage(ui.getStage()));
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToFileManager  , FileManager  ::show);
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToPreviewWindow, PreviewWindow::show);
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToBibManager   , () -> { if (db.bibLibraryIsLinked()) BibManager.show(true); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPageNum(HDT_Work work, int num, boolean isStart)
  {
    if ((ui.activeTabEnum() == workTabEnum) && (curWorkFile != null) && (ui.activeTab().activeRecord() == work))
      ui.workHyperTab().setPageNum(curWorkFile, num, isStart);
    else
    {
      if (isStart)
        work.setStartPageNum(curWorkFile, num);
      else
        work.setEndPageNum(curWorkFile, num);
    }

    PreviewWindow.instance().updatePageNumber(work, curWorkFile != null ? curWorkFile.filePath() : curFilePath, num, isStart);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(HDT_WorkFile workFile, int curPage)
  {
    clearDisplay();

    if (workFile == null) return;

    curWorkFile = workFile;
    curFilePath = null;

    update(curPage, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(FilePath filePath, int curPage)
  {
    clearDisplay();

    if (FilePath.isEmpty(filePath)) return;

    curWorkFile = null;
    curFilePath = filePath;

    update(curPage, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void update(int curPage, boolean setFocus)
  {
    Property<HyperTableRow> rowToSelect = new SimpleObjectProperty<>(null);

    clearDisplay();

    if ((curWorkFile == null) && (curFilePath == null)) return;

    List<HDT_Work> works;

    if (curWorkFile == null)
    {
      stage.setTitle(dialogTitle + " - " + curFilePath.getNameOnly());

      works = db.works.stream().filter(work -> curFilePath.equals(db.resolveExtFilePath(work.getURL())))
                               .collect(Collectors.toCollection(ArrayList::new));
    }
    else
    {
      stage.setTitle(dialogTitle + " - " + curWorkFile.getPath().getNameStr());
      works = new ArrayList<>(curWorkFile.works);
    }

    works.sort(Comparator.comparing(work ->
    {
      int startPage = work.getStartPageNum(curWorkFile);
      return startPage < 0 ? Integer.MAX_VALUE : startPage;
    }));

    htContents.buildRows(works, (row, work) ->
    {
      String authStr = work.getShortAuthorsStr(true), title = work.name();
      BibliographicDate bibDate = work.getBibDate();
      HDT_WorkType workType = work.workType.get();
      WorkTabCtrlr wtc = null;
      int authorID = work.authorRecords.stream().map(HDT_Record::getID).findFirst().orElse(-1);

      if ((ui.activeTabEnum() == workTabEnum) && (ui.activeTab().activeRecord() == work))
      {
        wtc = ui.workHyperTab();

        title = wtc.getTitle();
        bibDate = wtc.getDateFromUI();
        workType = wtc.hcbType.selectedRecord();
        authStr = wtc.getShortAuthorsStr();
      }

      row.setCellValue(0, authorID, authStr, hdtPerson);

      if (workType != null)
        row.setCellValue(1, workType);

      row.setCellValue(2, work, title);
      row.setCellValue(3, new BibDateHTC(work, bibDate));

      int pageNum = wtc == null ? -1 : wtc.getCurPageNum(work, curWorkFile, true);

      if (pageNum == -1)
        pageNum = work.getStartPageNum(curWorkFile);

      if (pageNum > -1)
      {
        row.setCellValue(4, work, String.valueOf(pageNum));

        if ((curPage == pageNum) && (rowToSelect.getValue() == null))
          rowToSelect.setValue(row);
      }

      pageNum = wtc == null ? -1 : wtc.getCurPageNum(work, curWorkFile, false);

      if (pageNum == -1)
        pageNum = work.getEndPageNum(curWorkFile);

      if (pageNum > -1)
      {
        row.setCellValue(7, work, String.valueOf(pageNum));

        if ((curPage == pageNum) && (rowToSelect.getValue() == null))
          rowToSelect.setValue(row);
      }
    });

    if (setFocus == false) return;

    if (rowToSelect.getValue() != null)
      htContents.selectRow(rowToSelect.getValue());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearDisplay()
  {
    stage.setTitle(dialogTitle);
    htContents.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void clear()
  {
    if (instance == null) return;

    instance.doClear();
  }

  private void doClear()
  {
    curWorkFile = null;
    curFilePath = null;
    clearDisplay();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show() { show(instance); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
