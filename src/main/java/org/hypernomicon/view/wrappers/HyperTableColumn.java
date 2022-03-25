/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.EnumMap;
import java.util.function.Function;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonCellHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;

//---------------------------------------------------------------------------

public class HyperTableColumn
{
  public enum HyperCtrlType
  {
    ctNone,     ctIncremental, ctDropDownList, ctDropDown, ctEdit, ctUrlBtn,   ctBrowseBtn, ctGoBtn,
    ctGoNewBtn, ctEditNewBtn,  ctCustomBtn,    ctCheckbox, ctIcon, ctInvSelect
  }

  final private Populator populator;
  final private RecordType objType;
  final private HyperCtrlType ctrlType;
  final private TableColumn<HyperTableRow, ?> tc;
  final EnumMap<ButtonAction, String> tooltips = new EnumMap<>(ButtonAction.class);
  final CellUpdateHandler updateHandler;
  final private int colNdx;
  final private MutableBoolean canEditIfEmpty      = new MutableBoolean(true ),
                               isNumeric           = new MutableBoolean(false),
                               dontCreateNewRecord = new MutableBoolean(false);

  public Function<HyperTableRow, String> textHndlr = null;

//---------------------------------------------------------------------------

  public HyperCtrlType getCtrlType()                 { return ctrlType; }
  public int getColNdx()                             { return colNdx; }
  public String getHeader()                          { return tc.getText(); }
  RecordType getObjType()                            { return objType; }
  void setCanEditIfEmpty(boolean newVal)             { canEditIfEmpty.setValue(newVal); }
  void setNumeric(boolean newVal)                    { isNumeric.setValue(newVal); }
  public void setDontCreateNewRecord(boolean newVal) { dontCreateNewRecord.setValue(newVal); }
  void setTooltip(ButtonAction ba, String text)      { tooltips.put(ba, text); }
  void clear()                                       { if (populator != null) populator.clear(); }

  @SuppressWarnings("unchecked") <PopType extends Populator> PopType getPopulator()     { return (PopType) populator; }

//---------------------------------------------------------------------------

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol) {
    this(table, objType, ctrlType, populator, targetCol, null, null, null, null, null); }

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, EventHandler<ActionEvent> onAction) {
    this(table, objType, ctrlType, populator, targetCol, null, onAction, null, null, null); }

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, CellUpdateHandler updateHandler) {
    this(table, objType, ctrlType, populator, targetCol, null, null, updateHandler, null, null); }

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol,
                   EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler) {
    this(table, objType, ctrlType, populator, targetCol, null, onAction, updateHandler, null, null); }

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol, ButtonCellHandler btnHandler, String btnCaption) {
    this(table, objType, ctrlType, populator, targetCol, btnHandler, null, null, btnCaption, null); }

  HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol,
                   Function<HyperTableRow, Node> graphicProvider) {
    this(table, objType, ctrlType, populator, targetCol, null, null, null, null, graphicProvider); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private HyperTableColumn(HyperTable table, RecordType objType, HyperCtrlType ctrlType, Populator populator, int targetCol,
                           ButtonCellHandler btnHandler, EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler, String btnCaption,
                           Function<HyperTableRow, Node> graphicProvider)
  {
    this.ctrlType = ctrlType;
    this.populator = populator;
    this.objType = objType;
    this.updateHandler = updateHandler;

    colNdx = table.getColumns().size();
    tc = table.getTV().getColumns().get(colNdx);

    TableColumn<HyperTableRow, HyperTableCell> htcCol = ctrlType == ctCheckbox ? null : (TableColumn<HyperTableRow, HyperTableCell>) tc;

    switch (ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn : case ctBrowseBtn : case ctUrlBtn : case ctCustomBtn :

        htcCol.setCellFactory(tableCol -> new ButtonCell(ctrlType, table, this, targetCol, btnHandler, btnCaption));
        break;

      case ctEdit :

        htcCol.setEditable(true);
        htcCol.setCellValueFactory(cellDataFeatures -> new SimpleObjectProperty<>(cellDataFeatures.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new TextFieldCell(table, canEditIfEmpty, isNumeric));

        htcCol.setOnEditCommit(event ->
        {
          HyperTableCell newCell = event.getNewValue().getCopyWithID(event.getOldValue().getID()); // preserve ID value
          event.getRowValue().setCellValue(colNdx, newCell);
        });

        break;

      case ctCheckbox :

        TableColumn<HyperTableRow, Boolean> boolCol = (TableColumn<HyperTableRow, Boolean>) tc;

        boolCol.setEditable(true);
        boolCol.setCellValueFactory(cellData ->
        {
          HyperTableCell cell = cellData.getValue().getCell(colNdx);
          int id = HyperTableCell.getCellID(cell);

          return new SimpleBooleanProperty(id == 1);
        });

        boolCol.setCellFactory(tableCol -> new CheckboxCell(table));

        break;

      case ctNone :

        htcCol.setEditable(false);
        htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new ReadOnlyCell(table, false, graphicProvider));

        break;

      case ctIcon :

        htcCol.setEditable(false);
        htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new TableCell<>()
        {
          @Override public void updateItem(HyperTableCell cell, boolean empty)
          {
            super.updateItem(cell, empty);

            setText("");

            if (empty || (cell == null) || (getTableRow().getItem() == null)) { setGraphic(null); setTooltip(null); return; }

            HDT_Record record = HyperTableCell.getRecord(cell);
            RecordType type = HyperTableCell.getCellType(cell);

            setGraphic(record == null ? imgViewForRecordType(type) : imgViewForRecord(record));

            if ((type == hdtWork) && (record != null))
            {
              HDT_Work work = (HDT_Work)record;

              if (work.workType.isNotNull())
              {
                setToolTip(this, work.workType.get().getCBText());
                return;
              }
            }

            setToolTip(this, db.getTypeName(type));
          }
        });

        break;

      case ctIncremental :

        htcCol.setEditable(false);
        htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new ReadOnlyCell(table, true, graphicProvider));

        break;

      case ctDropDownList : case ctDropDown :

        htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new ComboBoxCell(table, ctrlType, populator, onAction, dontCreateNewRecord, textHndlr));
        htcCol.setOnEditStart(event -> populator.populate(event.getRowValue(), false));

        break;

      case ctInvSelect :

        htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));
        htcCol.setCellFactory(tableCol -> new TableCell<>()
        {
          @Override public void startEdit()
          {
            super.startEdit();
            super.cancelEdit();

            ui.personHyperTab().showInvSelectDialog(getTableRow().getItem());
          }

          @Override public void updateItem(HyperTableCell item, boolean empty)
          {
            super.updateItem(item, empty);

            setText(empty ? null : HyperTableCell.getCellText(getItem()));
          }
        });

        break;

      default :
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static Button makeButton(TableCell<HyperTableRow, HyperTableCell> tableCell)
  {
    Button cellButton = new Button();

    setHeights(cellButton, 18.0 * displayScale);
    cellButton.setPadding(new Insets(0.0, 7.0, 0.0, 7.0));

    tableCell.emptyProperty().addListener((ob, oldValue, newValue) -> cellButton.setVisible(Boolean.FALSE.equals(newValue)));

    return cellButton;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
