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

package org.hypernomicon.view.tabs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.App;
import org.hypernomicon.dialogs.NewArgDlgCtrlr;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.view.cellValues.*;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.*;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.SequencedMap;

import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

public final class ArgumentTabCtrlr extends HyperNodeTab<HDT_Argument, HDT_Argument>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final AnchorPane apLowerTabs, apWhereMade, apResponses;
  private final ArgumentLowerPaneCtrlr lowerCtrlr;
  private final HyperTable htParents, htWhereMade, htResponses;
  private final ComboBox<Ternary> cbArgOrStance;

  private HDT_Argument curArgument;
  private Ternary isConfiguredForTabs = Ternary.Unset;

//---------------------------------------------------------------------------

  public ArgumentTabCtrlr(Tab tab) throws IOException
  {
    super(argumentTabEnum, tab);

    cbArgOrStance = new ComboBox<>();
    setupArgOrStance();

    lblParentCaption.setText("Responds to:");

    gpToolBar.getChildren().set(0, new AnchorPane());

    TableColumn<HyperTableRow, HyperTableCell> verdictCol = new TableColumn<>("Argues/Holds that");
    verdictCol.setPrefWidth(250.0);
    tvParents.getColumns().add(verdictCol);

    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/tabs/ArgumentLowerPane.fxml"));

    apLowerTabs = loader.load();
    lowerCtrlr = loader.getController();
    apWhereMade = (AnchorPane) lowerCtrlr.tvWhereMade.getParent();
    apResponses = (AnchorPane) lowerCtrlr.tvResponses.getParent();

    Platform.runLater(() -> configureLowerPaneBasedOnWidth(spMain.getWidth()));

    spMain.widthProperty().addListener((obs, oldWidth, newWidth) -> configureLowerPaneBasedOnWidth(newWidth));

    htParents = new HyperTable(tvParents, 3, true, TablePrefKey.ARG_PARENTS);

    htParents.addActionCol(ctGoBtn, 3)
      .setGoTooltipBasedOnTarget(record -> "Go to " + getTypeName(record.getType()) + ": " + record.defaultCellText());

    htParents.addActionCol(ctBrowseBtn, 3).setButtonTooltip(ButtonAction.baBrowse, "Select a record from the Tree");

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

    }).setTextHndlr(row -> nullSwitch((HDT_Work) row.getRecord(2), HyperTableCell.getCellText(row.getCell(1)), HDT_RecordWithAuthors::getLongAuthorsStr));

    htWhereMade.addColAltPopulatorWithUpdateHandler(hdtWork, ctEditableLimitedDropDown, new HybridSubjectPopulator(rtAuthorOfWork), (row, cellVal, nextColNdx, nextPopulator) ->
    {
      if (HyperTableCell.getCellID(cellVal) > 0)
      {
        HDT_Work work = db.works.getByID(HyperTableCell.getCellID(cellVal));
        row.setCellValue(3, work, curArgument.pagesInWork(work));
      }
      else
      {
        row.setCellValue(3, "", hdtWork);
      }

    }).setTextHndlr(row -> nullSwitch(row.getRecord(2), "", HDT_Work::yearTitleText))
      .setCellToolTipHndlr(row -> nullSwitch((HDT_Work) row.getRecord(2), null, work -> makeTooltip(work.defaultChoiceText())));

    htWhereMade.addTextEditCol(hdtWork, false)  // Pages column
               .setValueType(cvtPageRange);

    htResponses = new HyperTable(lowerCtrlr.tvResponses, 3, true, TablePrefKey.ARG_RESPONSES);

    htResponses.addActionCol(ctGoNewBtn, 3)
      .setGoTooltipBasedOnTarget(record -> "Go to Argument: " + record.defaultCellText())
      .setButtonTooltip(ButtonAction.baNew, "Add new Argument responding to this Argument");

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

  private void configureLowerPaneBasedOnWidth(Number newWidth)
  {
    double width = newWidth.doubleValue();

    if (width < scalePropertyValueForDPI(1000))
      configureLowerPaneForTabs();
    else
      configureLowerPaneForDivider();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void configureLowerPaneForDivider()
  {
    if (isConfiguredForTabs.isFalse()) return;

    lowerCtrlr.tabWhereMade.setContent(null);
    removeFromParent(apResponses);
    removeFromParent(apLowerTabs);

    spChildren.getItems().set(0, apWhereMade);
    spChildren.getItems().set(1, apResponses);
    spMain.getItems().set(2, apLowerPane);

    isConfiguredForTabs = Ternary.False;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void configureLowerPaneForTabs()
  {
    if (isConfiguredForTabs.isTrue()) return;

    removeFromParent(apWhereMade);
    removeFromParent(apResponses);
    removeFromParent(apLowerPane);

    lowerCtrlr.tabWhereMade.setContent(apWhereMade);
    lowerCtrlr.bpResponses.setCenter(apResponses);
    spMain.getItems().set(2, apLowerTabs);

    isConfiguredForTabs = Ternary.True;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setupArgOrStance()
  {
    AnchorPane ap = (AnchorPane) nameCtrl().getParent();

    Label lblArgOrStance = new Label("Argument or Stance:");
    AnchorPane.setTopAnchor(lblArgOrStance, 9.0);

    lblArgOrStance.widthProperty().addListener((obs, oldWidth, newWidth) ->
    {
      AnchorPane.setRightAnchor(nameCtrl(), cbArgOrStance.getWidth() + newWidth.doubleValue() + scalePropertyValueForDPI(5.0));
      AnchorPane.setRightAnchor(lblArgOrStance, cbArgOrStance.getWidth() + scalePropertyValueForDPI(6.0));
    });

    cbArgOrStance.widthProperty().addListener((obs, oldWidth, newWidth) ->
    {
      AnchorPane.setRightAnchor(nameCtrl(), newWidth.doubleValue() + lblArgOrStance.getWidth() + scalePropertyValueForDPI(5.0));
      AnchorPane.setRightAnchor(lblArgOrStance, cbArgOrStance.getWidth() + scalePropertyValueForDPI(6.0));
    });

    setupArgOrStanceSelector(cbArgOrStance);

    setHeights(cbArgOrStance, 25.0);
    setAnchors(cbArgOrStance, 5.0, null, null, 0.0);
    cbArgOrStance.setPrefWidth(150.0);

    ap.getChildren().addAll(lblArgOrStance, cbArgOrStance);

    lblArgOrStance.setPadding(new Insets(0.0, 0.0, 0.0, 20.0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setupArgOrStanceSelector(ComboBox<Ternary> cb)
  {
    SequencedMap<Ternary, String> strMap = new LinkedHashMap<>();

    strMap.put(Ternary.Unset, "");
    strMap.put(Ternary.False, "Stance");
    strMap.put(Ternary.True , "Argument");

    SimpleSelector.init(cb, strMap);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateFromRecord()
  {
    curArgument.addParentDisplayRecord();

    super.updateFromRecord();

    cbArgOrStance.getSelectionModel().select(curArgument.getIsArgument());

    getTab().setGraphic(imgViewForRecord(curArgument, hdtArgument));

    // Select parent records in ComboBoxes
    // -----------------------------------

    htParents.buildRows(curArgument.positions, (row, position) ->
    {
      row.setCellValue(2, getTypeName(hdtPosition), hdtPosition);
      row.setCellValue(3, position);

      nullSwitch(curArgument.getPosVerdict(position), verdict -> row.setCellValue(4, verdict, verdict.defaultChoiceText()));
    });

    htParents.buildRows(curArgument.targetArgs, (row, targetArg) ->
    {
      row.setCellValue(2, getTypeName(hdtArgument), hdtArgument);
      row.setCellValue(3, targetArg);

      nullSwitch(curArgument.getArgVerdict(targetArg), verdict -> row.setCellValue(4, verdict, verdict.defaultChoiceText()));
    });

  // Populate the authors, works, and years
  // --------------------------------------

    htWhereMade.buildRows(curArgument.works, (row, work) ->
    {
      if (work.authorRecords.size() > 0)
        row.setCellValue(1, work.authorRecords.getFirst(), work.getLongAuthorsStr());
      else
        row.setCellValue(1, work.getLongAuthorsStr(), hdtPerson);

      row.setCellValue(2, work, work.defaultChoiceText());
      row.setCellValue(3, new PageRangeHTC(work, curArgument.pagesInWork(work)));
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

      nullSwitch(responseArg.getArgVerdict(curArgument), verdict -> row.setCellValue(2, responseArg, verdict.defaultCellText()));

      row.setCellValue(3, responseArg);
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
    lowerCtrlr.tabWhereMade.setText("Where made/taken (" + htWhereMade.dataRowCount() + ')');
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

    cbArgOrStance.getSelectionModel().select(Ternary.Unset);

    getTab().setGraphic(imgViewForRecord(null, hdtArgument));
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

    curArgument.setIsArgument(cbArgOrStance.getSelectionModel().getSelectedItem());

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

