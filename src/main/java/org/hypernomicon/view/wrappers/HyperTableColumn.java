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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.List;
import java.util.function.Function;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.PersonTabController;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonCellHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.Tooltip;

//---------------------------------------------------------------------------

public class HyperTableColumn
{
  public static enum HyperCtrlType
  {
    ctNone,     ctIncremental, ctDropDownList, ctDropDown,
    ctEdit,     ctLinkBtn,     ctBrowseBtn,    ctGoBtn,
    ctGoNewBtn, ctEditNewBtn,  ctCustomBtn,    ctCheckbox,
    ctIcon,     ctInvSelect
  }

  final private TableColumn<HyperTableRow, HyperTableCell> tc;
  final private TableColumn<HyperTableRow, Boolean> chkCol;
  final private Populator populator;
  final private HDT_RecordType objType;
  final private HyperCtrlType ctrlType;
  final EnumMap<ButtonAction, String> tooltips = new EnumMap<>(ButtonAction.class);
  final CellUpdateHandler updateHandler;
  final private int colNdx;
  final private MutableBoolean canEditIfEmpty      = new MutableBoolean(true ),
                               isNumeric           = new MutableBoolean(false),
                               dontCreateNewRecord = new MutableBoolean(false);

  public Function<HyperTableRow, String> textHndlr = null;
  private boolean moreButtonClicked = false;

//---------------------------------------------------------------------------

  boolean wasMoreButtonClicked()                     { return moreButtonClicked; }
  public HyperCtrlType getCtrlType()                 { return ctrlType; }
  public int getColNdx()                             { return colNdx; }
  public String getHeader()                          { return tc.getText(); }
  HDT_RecordType getObjType()                        { return objType; }
  void setCanEditIfEmpty(boolean newVal)             { this.canEditIfEmpty.setValue(newVal); }
  void setNumeric(boolean newVal)                    { this.isNumeric.setValue(newVal); }
  public void setDontCreateNewRecord(boolean newVal) { this.dontCreateNewRecord.setValue(newVal); }
  void setTooltip(ButtonAction ba, String text)      { tooltips.put(ba, text); }

  @SuppressWarnings("unchecked") <PopType extends Populator> PopType getPopulator()     { return (PopType) populator; }

//---------------------------------------------------------------------------

  HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol) {
    this(table, objType, ctrlType, populator, targetCol, null, null, null, null); }

  HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, EventHandler<ActionEvent> onAction) {
    this(table, objType, ctrlType, populator, targetCol, null, onAction, null, null); }

  HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, CellUpdateHandler updateHandler) {
    this(table, objType, ctrlType, populator, targetCol, null, null, updateHandler, null); }

  HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol,
                          EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler) {
    this(table, objType, ctrlType, populator, targetCol, null, onAction, updateHandler, null); }

  HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, ButtonCellHandler btnHandler, String btnCaption) {
    this(table, objType, ctrlType, populator, targetCol, btnHandler, null, null, btnCaption); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private HyperTableColumn(HyperTable table, HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol,
                           ButtonCellHandler btnHandler, EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler, String btnCaption)
  {
    this.ctrlType = ctrlType;
    this.populator = populator;
    this.objType = objType;
    this.updateHandler = updateHandler;

    colNdx = table.getColumns().size();

    if (ctrlType == ctCheckbox)
    {
      chkCol = (TableColumn<HyperTableRow, Boolean>) table.getTV().getColumns().get(colNdx);
      tc = null;
    }
    else
    {
      tc = (TableColumn<HyperTableRow, HyperTableCell>) table.getTV().getColumns().get(colNdx);
      chkCol = null;
    }

    switch (ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn : case ctBrowseBtn : case ctLinkBtn : case ctCustomBtn :

        tc.setCellFactory(tableCol -> new ButtonCell(ctrlType, table, this, targetCol, btnHandler, btnCaption));
        break;

      case ctEdit :

        tc.setEditable(true);
        tc.setCellValueFactory(cellDataFeatures -> new SimpleObjectProperty<>(cellDataFeatures.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new TextFieldCell(table, canEditIfEmpty, isNumeric));

        tc.setOnEditCommit(event ->
        {
          HyperTableCell newCell = event.getNewValue().getCopyWithID(event.getOldValue().getID()); // preserve ID value
          event.getRowValue().setCellValue(colNdx, newCell);
        });

        break;

      case ctCheckbox :

        chkCol.setEditable(true);
        chkCol.setCellValueFactory(cellData ->
        {
          HyperTableCell cell = cellData.getValue().getCell(colNdx);
          int id = HyperTableCell.getCellID(cell);

          return new SimpleBooleanProperty(id == 1);
        });

        chkCol.setCellFactory(tableCol -> new CheckboxCell(table));

        break;

      case ctNone :

        tc.setEditable(false);
        tc.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new ReadOnlyCell(table, this, false));

        break;

      case ctIcon :

        tc.setEditable(false);
        tc.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new TableCell<>()
        {
          @Override public void updateItem(HyperTableCell cell, boolean empty)
          {
            super.updateItem(cell, empty);

            setText("");

            if (empty || (cell == null) || (getTableRow().getItem() == null)) { setGraphic(null); setTooltip(null); return; }

            HDT_RecordType type = HyperTableCell.getCellType(cell);

            if (type == hdtWork)
            {
              HDT_Work work = HyperTableCell.getRecord(cell);

              if (work.workType.isNotNull())
              {
                setGraphic(getImageViewForRelativePath(ui.getGraphicRelativePath(work)));
                setTooltip(new Tooltip(work.workType.get().getCBText()));
                return;
              }
            }

            setGraphic(getImageViewForRelativePath(ui.getGraphicRelativePathByType(type)));
            setTooltip(new Tooltip(db.getTypeName(type)));
          }
        });

        break;

      case ctIncremental :

        tc.setEditable(false);
        tc.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new ReadOnlyCell(table, this, true));

        break;

      case ctDropDownList : case ctDropDown :

        tc.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new ComboBoxCell(table, ctrlType, populator, onAction, dontCreateNewRecord, textHndlr));
        tc.setOnEditStart(event -> populator.populate(event.getRowValue(), false));

        break;

      case ctInvSelect :

        tc.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        tc.setCellFactory(tableCol -> new TableCell<>()
        {
          @Override public void startEdit()
          {
            super.startEdit();
            super.cancelEdit();

            PersonTabController personTabCtrlr = HyperTab.getHyperTab(personTab);
            personTabCtrlr.showInvSelectDialog(getTableRow().getItem());
          }

          @Override public void updateItem(HyperTableCell item, boolean empty)
          {
            super.updateItem(item, empty);

            if (empty) setText(null);
            else       setText(HyperTableCell.getCellText(getItem()));
          }
        });

        break;

      default :
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    if (populator != null)
      populator.clear();

    moreButtonClicked = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  List<HyperTableCell> getSelectedItems()
  {
    List<HyperTableCell> choices = new ArrayList<>();

    for (HyperTableRow row : tc.getTableView().getItems())
    {
      int recordID = row.getID(colNdx);
      if (recordID > 0)
        choices.add(new HyperTableCell(recordID, db.records(row.getType(colNdx)).getByID(recordID).getCBText(), row.getType(colNdx)));
    }
    choices.add(HyperTableCell.blankCell);

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static Button makeButton(TableCell<HyperTableRow, HyperTableCell> tableCell)
  {
    Button cellButton = new Button();

    cellButton.setMinHeight (18.0 * displayScale);
    cellButton.setPrefHeight(18.0 * displayScale);
    cellButton.setMaxHeight (18.0 * displayScale);
    cellButton.setPadding(new Insets(0.0, 7.0, 0.0, 7.0));

    tableCell.emptyProperty().addListener((observable, oldValue, newValue) -> cellButton.setVisible(newValue.booleanValue() == false));

    return cellButton;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
