/*
 * Copyright 2015-2025 Jason Winning
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

package org.hypernomicon.view.tabs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.App;
import org.hypernomicon.dialogs.NewArgDlgCtrlr;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public final class ArgumentTabCtrlr extends HyperNodeTab<HDT_Argument, HDT_Argument>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ArgumentLowerPaneCtrlr lowerCtrlr;
  private final HyperTable htParents, htWhereMade, htResponses;

  private HDT_Argument curArgument;

//---------------------------------------------------------------------------

  public ArgumentTabCtrlr(Tab tab) throws IOException
  {
    super(argumentTabEnum, tab);

    AnchorPane aP = new AnchorPane();

    lblParentCaption.setText("Responds to:");

    gpToolBar.getChildren().set(0, aP);

    TableColumn<HyperTableRow, HyperTableCell> verdictCol = new TableColumn<>("Argues that");
    verdictCol.setPrefWidth(250.0);
    tvParents.getColumns().add(verdictCol);

    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/tabs/ArgumentLowerPane.fxml"));

    spMain.getItems().set(2, loader.load());

    lowerCtrlr = loader.getController();

    htParents = new HyperTable(tvParents, 3, true, TablePrefKey.ARG_PARENTS);

    htParents.addActionCol(ctGoBtn, 3)
      .setGoTooltipBasedOnTarget(record -> "Go to " + getTypeName(record.getType()) + ": " + record.listName());

    htParents.addActionCol(ctBrowseBtn, 3).setTooltip(ButtonAction.baBrowse, "Select a record from the Tree");

    RecordByTypePopulator verdictPopulator = new RecordByTypePopulator();

    RecordTypePopulator rtp = new RecordTypePopulator(hdtPosition, hdtArgument);

    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctEditableLimitedDropDown, rtp, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      RecordByTypePopulator rbtp = (RecordByTypePopulator)nextPopulator;

      RecordType parentType = HyperTableCell.getCellType(cellVal);
      rbtp.setRecordType(row, parentType);
      rbtp.setChanged(row);
      row.setCellValue(nextColNdx, "", parentType);

      if (parentType == hdtPosition)
        verdictPopulator.setRecordType(row, hdtPositionVerdict);
      else if (parentType == hdtArgument)
        verdictPopulator.setRecordType(row, hdtArgumentVerdict);
      else
        verdictPopulator.setRecordType(row, hdtNone);

      verdictPopulator.populate(row, true);
      row.setCellValue(nextColNdx + 1, "", verdictPopulator.getRecordType(row));
    });

    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctEditableLimitedDropDown, new RecordByTypePopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) < 1)
        row.setCellValue(nextColNdx, "", verdictPopulator.getRecordType(row));
    });

    htParents.addColAltPopulator(hdtNone, ctEditableLimitedDropDown, verdictPopulator);

    htWhereMade = new HyperTable(lowerCtrlr.tvWhereMade, 2, true, TablePrefKey.ARG_SRC);

    htWhereMade.addGoNewCol(hdtWork, 2);
    htWhereMade.addAuthorEditCol(null, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      HDT_Record obj = HyperTableCell.getRecord(cellVal);
      HybridSubjectPopulator hsPop = (HybridSubjectPopulator)nextPopulator;

      if (hsPop.getObj(row) == obj) return;

      hsPop.setObj(row, obj);
      row.setCellValue(nextColNdx, "", hsPop.getRecordType(row));

    }).setTextHndlr(row -> nullSwitch((HDT_Work)row.getRecord(2), HyperTableCell.getCellText(row.getCell(1)), HDT_RecordWithAuthors::getLongAuthorsStr));

    htWhereMade.addColAltPopulatorWithUpdateHandler(hdtWork, ctEditableLimitedDropDown, new HybridSubjectPopulator(rtAuthorOfWork), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) > 0)
      {
        HDT_Work work = db.works.getByID(HyperTableCell.getCellID(cellVal));
        row.setCellValue(3, work, curArgument.pagesInWork(work));
        row.setCellValue(4, work, work.getYearStr());
      }
      else
      {
        row.setCellValue(3, "", hdtWork);
        row.setCellValue(4, "", hdtWork);
      }
    });

    htWhereMade.addTextEditCol(hdtWork, false)  // Pages column
               .setValueType(cvtPageRange);

    htWhereMade.addLabelCol(hdtArgument);       // Date column

    htResponses = new HyperTable(lowerCtrlr.tvResponses, 3, true, TablePrefKey.ARG_RESPONSES);

    htResponses.addActionCol(ctGoNewBtn, 3)
      .setGoTooltipBasedOnTarget(record -> "Go to Argument: " + record.listName())
      .setTooltip(ButtonAction.baNew, "Add new Argument responding to this Argument");

    htResponses.addLabelCol(hdtPerson);
    htResponses.addLabelCol(hdtArgumentVerdict);
    htResponses.addLabelCol(hdtArgument);

    htWhereMade.getTV().focusedProperty().addListener((ob, oldValue, newValue) -> updateArgCounts());

    initContextMenus();
  }

//---------------------------------------------------------------------------

  @Override protected RecordType type()                { return hdtArgument; }
  @Override protected void setRecord(HDT_Argument arg) { curArgument = arg; }
  @Override protected HDT_Argument getNodeRecord()     { return curArgument; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    curArgument.addParentDisplayRecord();

    super.updateFromRecord();

    // Select parent records in ComboBoxes
    // -----------------------------------

    htParents.buildRows(curArgument.positions, (row, position) ->
    {
      row.setCellValue(2, getTypeName(hdtPosition), hdtPosition);
      row.setCellValue(3, position, position.listName());

      nullSwitch(curArgument.getPosVerdict(position), verdict -> row.setCellValue(4, verdict, verdict.getCBText()));
    });

    htParents.buildRows(curArgument.targetArgs, (row, targetArg) ->
    {
      row.setCellValue(2, getTypeName(hdtArgument), hdtArgument);
      row.setCellValue(3, targetArg, targetArg.listName());

      nullSwitch(curArgument.getArgVerdict(targetArg), verdict -> row.setCellValue(4, verdict, verdict.getCBText()));
    });

  // Populate the authors, works, and years
  // --------------------------------------

    htWhereMade.buildRows(curArgument.works, (row, work) ->
    {
      if (work.authorRecords.size() > 0)
        row.setCellValue(1, work.authorRecords.getFirst(), work.getLongAuthorsStr());
      else
        row.setCellValue(1, work.getLongAuthorsStr(), hdtPerson);

      row.setCellValue(2, work, work.getCBText());
      row.setCellValue(3, new PageRangeHTC(work, curArgument.pagesInWork(work)));
      row.setCellValue(4, new BibDateHTC(work, work.getBibDate()));
    });

  // Populate the response arguments
  // -------------------------------

    htResponses.buildRows(curArgument.responseArgs, (row, responseArg) ->
    {
      if (responseArg.works.size() > 0)
      {
        HDT_Work work = responseArg.works.getFirst();

        if (work.authorRecords.size() > 0)
          row.setCellValue(1, work.authorRecords.getFirst(), work.getLongAuthorsStr());
        else
          row.setCellValue(1, responseArg, work.getLongAuthorsStr());
      }

      nullSwitch(responseArg.getArgVerdict(curArgument), verdict -> row.setCellValue(2, responseArg, verdict.listName()));

      row.setCellValue(3, responseArg, responseArg.listName());
    });

    lowerCtrlr.tabResponses.setText("Counter/Response Arguments (" + curArgument.responseArgs.size() + ')');

  // Set active tab
  // --------------

    boolean noWorks     = curArgument.works       .isEmpty(),
            noResponses = curArgument.responseArgs.isEmpty();

    Tab tab = lowerCtrlr.tabPane.getSelectionModel().getSelectedItem();

    if (((tab == lowerCtrlr.tabWhereMade) && noWorks) ||
        ((tab == lowerCtrlr.tabResponses) && noResponses))
    {
      if      (noResponses == false) tab = lowerCtrlr.tabResponses;
      else if (noWorks     == false) tab = lowerCtrlr.tabWhereMade;

      lowerCtrlr.tabPane.getSelectionModel().select(tab);
    }

    updateArgCounts();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateArgCounts()
  {
    lowerCtrlr.tabWhereMade.setText("Where made (" + htWhereMade.dataRowCount() + ')');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initContextMenus()
  {
    htParents.addChangeOrderMenuItem();
    htParents.addRemoveMenuItem();

    htWhereMade.addDefaultMenuItems();

    htWhereMade.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htWhereMade.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htWhereMade.addChangeOrderMenuItem();
    htWhereMade.addRemoveMenuItem();

    htResponses.addContextMenuItem("Launch work", HDT_Argument.class,
      arg -> HDT_Work.hasLaunchableWork(arg.works),
      arg -> HDT_Work.getLaunchableWork(arg.works).launch(-1));

    htResponses.addContextMenuItem("Go to work record", HDT_Argument.class,
      arg -> arg.works.size() > 0,
      arg -> ui.goToRecord(nullSwitch(HDT_Work.getLaunchableWork(arg.works), arg.works.getFirst()), true));

    htResponses.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htResponses.addContextMenuItem("Go to argument record", HDT_Argument.class,
      argument -> ui.goToRecord(argument, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear(boolean resetRecord)
  {
    super.clear(resetRecord);

    curArgument = resetRecord ? null : HDT_Record.getCurrentInstance(curArgument);

    htParents  .clear();
    htWhereMade.clear();
    htResponses.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord(boolean saveNameIfBlank)
  {
    if (super.saveToRecord(saveNameIfBlank) == false)
      return false;

    boolean okToSave = true;

    for (HyperTableRow row : htParents.dataRows())
    {
      if (((row.getID(3) > 0) && (row.getID(4) < 1)) ||
          ((row.getID(4) > 0) && (row.getID(3) < 1)))

        okToSave = false;
    }

    if (okToSave == false)
      return falseWithErrorPopup("Unable to modify record: There must be a corresponding verdict for every position/argument targeted by this record.");

    if (htWhereMade.saveObjectsAndSingleNestedItem(curArgument, rtWorkOfArgument, tagPages, 2, 3) == false)
      return false;

    htParents.saveObjectsAndSingleNestedItem(curArgument, rtPositionOfArgument, tagPositionVerdict, 3, 4);
    htParents.saveObjectsAndSingleNestedItem(curArgument, rtTargetArgOfArg    , tagArgumentVerdict, 3, 4);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord()) return;

    switch (objType)
    {
      case hdtArgument :

        newResponseArgumentClick(curArgument);
        break;

      case hdtWork :

        HDT_Work work = db.createNewBlankRecord(hdtWork);
        nullSwitch(db.persons.getByID(row.getID(1)), work.getAuthors()::add);
        curArgument.works.add(work);
        ui.goToRecord(work, false);
        break;

      default:
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void newResponseArgumentClick(HDT_Argument target)
  {
    NewArgDlgCtrlr newArgDialog = new NewArgDlgCtrlr(target);

    if (newArgDialog.showModal() == false) return;

    ui.goToRecord(newArgDialog.getArgument(), false);

    lowerCtrlr.tabPane.getSelectionModel().select(lowerCtrlr.tabWhereMade);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDividerPositions()
  {
    setDividerPosition(spMain, DividerPositionPrefKey.ARG_TOP_VERT   , 0);
    setDividerPosition(spMain, DividerPositionPrefKey.ARG_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spMain, DividerPositionPrefKey.ARG_TOP_VERT   , 0);
    getDividerPosition(spMain, DividerPositionPrefKey.ARG_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}

