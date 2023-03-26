/*
 * Copyright 2015-2023 Jason Winning
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
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.App;
import org.hypernomicon.dialogs.NewArgDlgCtrlr;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.HyperTableCell.CellSortMethod;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.fxml.FXMLLoader;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public final class ArgumentTab extends HyperNodeTab<HDT_Argument, HDT_Argument>
{
  private final ArgumentLowerPaneCtrlr lowerCtrlr;
  private final HyperTable htParents, htWhereMade, htCounters;

  private HDT_Argument curArgument;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ArgumentTab(Tab tab) throws IOException
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

    htParents = new HyperTable(tvParents, 3, true, PREF_KEY_HT_ARG_PARENTS);

    htParents.addActionCol(ctGoBtn, 3);
    htParents.addActionCol(ctBrowseBtn, 3);

    RecordByTypePopulator verdictPopulator = new RecordByTypePopulator();

    RecordTypePopulator rtp = new RecordTypePopulator(hdtPosition, hdtArgument);

    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, rtp, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      RecordByTypePopulator rbtp = (RecordByTypePopulator)nextPopulator;

      RecordType parentType = cellVal.getType();
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

    htParents.addColAltPopulatorWithUpdateHandler(hdtNone, ctDropDownList, new RecordByTypePopulator(), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) < 1)
        row.setCellValue(nextColNdx, "", verdictPopulator.getRecordType(row));
    });

    htParents.addColAltPopulator(hdtNone, ctDropDownList, verdictPopulator);

    htParents.addRemoveMenuItem();
    htParents.addChangeOrderMenuItem(true);

    htWhereMade = new HyperTable(lowerCtrlr.tvWhereMade, 2, true, PREF_KEY_HT_ARG_SRC);

    htWhereMade.addActionCol(ctGoNewBtn, 2);
    HyperTableColumn col = htWhereMade.addColWithUpdateHandler(hdtPerson, ctDropDownList, (row, cellVal, nextColNdx, nextPopulator) ->
    {
      HDT_Record obj = HyperTableCell.getRecord(cellVal);
      HybridSubjectPopulator hsPop = (HybridSubjectPopulator)nextPopulator;

      if (hsPop.getObj(row) == obj) return;

      hsPop.setObj(row, obj);
      row.setCellValue(nextColNdx, "", hsPop.getRecordType(row));
    });

    col.textHndlr = row -> nullSwitch((HDT_Work)row.getRecord(2), HyperTableCell.getCellText(row.getCell(1)), work -> work.getLongAuthorsStr(true));

    htWhereMade.addColAltPopulatorWithUpdateHandler(hdtWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) > 0)
      {
        HDT_Work work = db.works.getByID(HyperTableCell.getCellID(cellVal));
        row.setCellValue(nextColNdx, work, work.getYear(), CellSortMethod.smNumeric);
      }
      else
        row.setCellValue(nextColNdx, "", hdtWork, CellSortMethod.smNumeric);
    });

    htWhereMade.addLabelCol(hdtWork);

    htWhereMade.addRemoveMenuItem();
    htWhereMade.addChangeOrderMenuItem(true);

    htCounters = new HyperTable(lowerCtrlr.tvCounters, 3, true, PREF_KEY_HT_ARG_COUNTERS);

    htCounters.addActionCol(ctGoNewBtn, 3);
    htCounters.addLabelCol(hdtPerson);
    htCounters.addLabelCol(hdtArgumentVerdict);
    htCounters.addLabelCol(hdtArgument);

    htWhereMade.getTV().focusedProperty().addListener((ob, oldValue, newValue) -> updateArgCounts());

    initContextMenus();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected RecordType type()             { return hdtArgument; }
  @Override public void setRecord(HDT_Argument arg) { curArgument = arg; }
  @Override protected HDT_Argument getNodeRecord()  { return curArgument; }

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

    htParents.buildRows(curArgument.counteredArgs, (row, counteredArg) ->
    {
      row.setCellValue(2, getTypeName(hdtArgument), hdtArgument);
      row.setCellValue(3, counteredArg, counteredArg.listName());

      nullSwitch(curArgument.getArgVerdict(counteredArg), verdict -> row.setCellValue(4, verdict, verdict.getCBText()));
    });

  // Populate the authors, works, and years
  // --------------------------------------

    htWhereMade.buildRows(curArgument.works, (row, work) ->
    {
      if (work.authorRecords.size() > 0)
        row.setCellValue(1, work.authorRecords.get(0), work.getLongAuthorsStr(true));
      else
        row.setCellValue(1, work.getLongAuthorsStr(true), hdtPerson);

      row.setCellValue(2, work, work.getCBText());
      row.setCellValue(3, work, work.getYear(), CellSortMethod.smNumeric);
    });

  // Populate the counterarguments
  // -----------------------------

    htCounters.buildRows(curArgument.counterArgs, (row, counterArg) ->
    {
      if (counterArg.works.size() > 0)
      {
        HDT_Work work = counterArg.works.get(0);

        if (work.authorRecords.size() > 0)
          row.setCellValue(1, work.authorRecords.get(0), work.getLongAuthorsStr(true));
        else
          row.setCellValue(1, counterArg, work.getLongAuthorsStr(true));
      }

      nullSwitch(counterArg.getArgVerdict(curArgument), verdict -> row.setCellValue(2, counterArg, verdict.listName()));

      row.setCellValue(3, counterArg, counterArg.listName());
    });

    lowerCtrlr.tabCounters.setText("Counterarguments (" + curArgument.counterArgs.size() + ')');

  // Set active tab
  // --------------

    boolean noWorks    = curArgument.works.isEmpty(),
            noCounters = curArgument.counterArgs.isEmpty();

    Tab tab = lowerCtrlr.tabPane.getSelectionModel().getSelectedItem();

    if (((tab == lowerCtrlr.tabWhereMade) && noWorks) ||
        ((tab == lowerCtrlr.tabCounters) && noCounters))
    {
      if (noCounters == false) tab = lowerCtrlr.tabCounters;
      else if (noWorks == false) tab = lowerCtrlr.tabWhereMade;

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
    htWhereMade.addDefaultMenuItems();

    htWhereMade.addContextMenuItem("Go to work record", HDT_Work.class,
      work -> ui.goToRecord(work, true));

    htWhereMade.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htCounters.addContextMenuItem("Launch work", HDT_Argument.class,
      arg -> HDT_Work.hasLaunchableWork(arg.works),
      arg -> HDT_Work.getLaunchableWork(arg.works).launch(-1));

    htCounters.addContextMenuItem("Go to work record", HDT_Argument.class,
      arg -> arg.works.size() > 0,
      arg -> ui.goToRecord(nullSwitch(HDT_Work.getLaunchableWork(arg.works), arg.works.get(0)), true));

    htCounters.addContextMenuItem("Go to person record", HDT_Person.class,
      person -> ui.goToRecord(person, true));

    htCounters.addContextMenuItem("Go to argument record", HDT_Argument.class,
      argument -> ui.goToRecord(argument, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    super.clear();

    htParents.clear();
    htWhereMade.clear();
    htCounters.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveToRecord()
  {
    if (super.saveToRecord() == false)
      return false;

    boolean okToSave = true;

    for (HyperTableRow row : htParents.dataRows())
    {
      if (((row.getID(3) > 0) && (row.getID(4) < 1)) ||
          ((row.getID(4) > 0) && (row.getID(3) < 1)))

        okToSave = false;
    }

    if (okToSave == false)
      return falseWithErrorMessage("Unable to modify record: There must be a corresponding verdict for every position/argument targeted by this record.");

    saveObjectGroups(tagPositionVerdict, rtPositionOfArgument);
    saveObjectGroups(tagArgumentVerdict, rtCounterOfArgument);

    return curArgument.setWorks(htWhereMade.saveToList(2, hdtWork));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void saveObjectGroups(Tag tag, RelationType relType)
  {
    Map<Integer, Tag> colNdxToTag = new HashMap<>();
    colNdxToTag.put(4, tag);

    List<ObjectGroup> tableGroups  = htParents.getObjectGroupList(curArgument, relType, 3, colNdxToTag);
    curArgument.updateObjectGroups(relType, tableGroups, colNdxToTag.values());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void newClick(RecordType objType, HyperTableRow row)
  {
    if (ui.cantSaveRecord()) return;

    switch (objType)
    {
      case hdtArgument :

        newCounterargumentClick(curArgument);
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

  public void newCounterargumentClick(HDT_Argument target)
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
    setDividerPosition(spMain, PREF_KEY_ARG_TOP_VERT, 0);
    setDividerPosition(spMain, PREF_KEY_ARG_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getDividerPositions()
  {
    getDividerPosition(spMain, PREF_KEY_ARG_TOP_VERT, 0);
    getDividerPosition(spMain, PREF_KEY_ARG_BOTTOM_VERT, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}

