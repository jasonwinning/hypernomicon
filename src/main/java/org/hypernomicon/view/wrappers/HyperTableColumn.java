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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.EnumMap;
import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonCellHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;

import javafx.application.Platform;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.skin.TableColumnHeader;
import javafx.scene.control.TableColumn.SortType;

//---------------------------------------------------------------------------

public class HyperTableColumn
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum HyperCtrlType
  {
    /**
     * Read-only; just displays text without being editable
     */
    ctNone,

    /**
     * Same as ctNone, except bottom row has a "Show More" button to load more rows
     */
    ctIncremental,

    /**
     * ComboBox with editable TextField, entry limited to choices
     */
    ctEditableLimitedDropDown,

    /**
     * ComboBox with editable TextField, entry not limited to choices
     */
    ctEditableUnlimitedDropDown,

    /**
     * ComboBox without editable TextField
     */
    ctNoneditableDropDown,

    /**
     * TextField with free text entry
     */
    ctEdit,

    ctUrlBtn,
    ctBrowseBtn,
    ctGoBtn,
    ctGoNewBtn,
    ctEditNewBtn,

    /**
     * Noneditable Label with pencil button to open popup to edit
     */
    ctLabelEdit,

    /**
     * Button with customized caption and action handler
     */
    ctCustomBtn,

    ctCheckbox,

    /**
     * Displays only an icon, determined by calling HyperTableCell.getImgRelPath
     */
    ctIcon,

    ctInvSelect
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum CellSortMethod
  {
    smStandard, smTextSimple, smNumeric, smWork, smIcon
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Populator populator;
  private final RecordType objType;
  private final HyperCtrlType ctrlType;
  private final TableColumn<HyperTableRow, ?> tc;
  final EnumMap<ButtonAction, Function<HyperTableRow, String>> tooltips = new EnumMap<>(ButtonAction.class);
  final CellUpdateHandler updateHandler;
  private final int colNdx, targetCol;
  private final MutableBoolean canEditIfEmpty      = new MutableBoolean(true ),
                               dontCreateNewRecord = new MutableBoolean(false);

  private final Property<CellSortMethod> sortMethod = new SimpleObjectProperty<>();

  private Supplier<HDT_Work> workSupplier;
  private CellValueType cellValueType = cvtRecord;
  private Function<HyperTableRow, String> textHndlr;
  private Pos alignment = null;  // This is currently only respected by ReadOnlyCell

//---------------------------------------------------------------------------

  public HyperCtrlType getCtrlType() { return ctrlType; }
  public int getColNdx()             { return colNdx; }
  public String getHeader()          { return tc.getText(); }
  RecordType getObjType()            { return objType; }
  Pos getAlignment()                 { return alignment; }
  void clear()                       { if (populator != null) populator.clear(); }

  HyperTableColumn setCanEditIfEmpty(boolean newVal)         { canEditIfEmpty.setValue(newVal); return this; }
  HyperTableColumn setSortMethod(CellSortMethod newSM)       { sortMethod.setValue(newSM);      return this; }
  HyperTableColumn setWorkSupplier(Supplier<HDT_Work> newWS) { workSupplier = newWS;            return this; }
  HyperTableColumn setAlignment(Pos newAlignment)            { alignment = newAlignment;        return this; }

  public HyperTableColumn setValueType(CellValueType newCellValueType)            { cellValueType = newCellValueType;     return this; }
  public HyperTableColumn setTextHndlr(Function<HyperTableRow, String> newTH)     { textHndlr = newTH;                    return this; }
  public HyperTableColumn setDontCreateNewRecord(boolean newVal)                  { dontCreateNewRecord.setValue(newVal); return this; }

  @SuppressWarnings("unchecked") <PopType extends Populator> PopType getPopulator() { return (PopType) populator; }

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
    this.objType = objType;
    this.ctrlType = ctrlType;
    this.populator = populator;
    this.targetCol = targetCol;
    this.updateHandler = updateHandler;

    colNdx = table.getColumns().size();
    tc = table.getTV().getColumns().get(colNdx);

    tc.setComparator((obj1, obj2) ->
    {
      if ((obj1 == null) && (obj2 == null)) return 0;
      if (obj1 == null) return -1;
      if (obj2 == null) return 1;

      HyperTableCell cell1 = (HyperTableCell) obj1,
                     cell2 = (HyperTableCell) obj2;

      if (cell1.getSortToBottom())
        return tc.getSortType() == SortType.ASCENDING ? 1 : -1;

      if (cell2.getSortToBottom())
        return tc.getSortType() == SortType.ASCENDING ? -1 : 1;

      if (table.getCanAddRows())
      {
        HyperTableRow lastRow = table.getRows().getLast();

        if (lastRow.getCell(colNdx) == cell1)
          return tc.getSortType() == SortType.ASCENDING ? 1 : -1;

        if (lastRow.getCell(colNdx) == cell2)
          return tc.getSortType() == SortType.ASCENDING ? -1 : 1;
      }

      if (sortMethod.getValue() != null)
        return HyperTableCell.compareCells(cell1, cell2, sortMethod.getValue());

      if (objType == hdtWork)
        return HyperTableCell.compareCells(cell1, cell2, smWork);

      return cell1.compareTo(cell2);
    });

    TableColumn<HyperTableRow, HyperTableCell> htcCol = (TableColumn<HyperTableRow, HyperTableCell>) tc;

    htcCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getCell(colNdx)));

    switch (this.ctrlType)
    {
      case ctGoBtn : case ctGoNewBtn : case ctEditNewBtn : case ctBrowseBtn : case ctUrlBtn : case ctCustomBtn : case ctLabelEdit :

        htcCol.setCellFactory(tableCol -> new ButtonCell(this.ctrlType, table, this, this.targetCol, btnHandler, btnCaption));
        break;

      case ctEdit :

        htcCol.setEditable(true);
        htcCol.setCellFactory(tableCol -> new TextFieldCell(table, cellValueType, canEditIfEmpty, sortMethod));

        htcCol.setOnEditCommit(event ->
          event.getRowValue().setCellValue(colNdx, event.getNewValue().getCopyWithID(event.getOldValue().getID()))); // preserve ID value

        break;

      case ctCheckbox :

        htcCol.setEditable(true);
        htcCol.setCellFactory(tableCol -> new CheckboxCell(table));

        break;

      case ctNone : case ctIncremental :

        htcCol.setEditable(false);
        htcCol.setCellFactory(tableCol -> new ReadOnlyCell(table, this, graphicProvider));

        break;

      case ctIcon :

        sortMethod.setValue(smIcon);

        htcCol.setEditable(false);
        htcCol.setCellFactory(tableCol -> new TableCell<>()
        {
          @Override public void updateItem(HyperTableCell cell, boolean empty)
          {
            if (empty || (cell == null) || (getTableRow().getItem() == null))
            {
              super.updateItem(cell, empty);

              setText("");
              setGraphic(null);
              setTooltip(null);
              return;
            }

            HDT_Record record = HyperTableCell.getRecord(cell);
            RecordType type = HyperTableCell.getCellType(cell);

            String relPath = cell.getImgRelPath();

            super.updateItem(cell, false);
            setGraphic(imgViewFromRelPath(relPath));

            if ((type == hdtWork) && (record != null))
            {
              HDT_Work work = (HDT_Work)record;

              if (work.workType.isNotNull())
              {
                setToolTip(this, work.workType.get().getCBText());
                return;
              }
            }

            setToolTip(this, getTypeName(type));
          }
        });

        break;

      case ctNoneditableDropDown : case ctEditableLimitedDropDown : case ctEditableUnlimitedDropDown :

        htcCol.setCellFactory(tableCol -> new ComboBoxCell(table, this.ctrlType, populator, onAction, dontCreateNewRecord, textHndlr, workSupplier));
        htcCol.setOnEditStart(event -> populator.populate(event.getRowValue(), false));

        break;

      case ctInvSelect :

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

  public void setHeaderTooltip(String str)
  {
    setHeaderTooltip(makeTooltip(str));
  }

  public void setHeaderTooltip(Tooltip tooltip)
  {
    Platform.runLater(() ->
    {
      TableColumnHeader header = (TableColumnHeader) tc.getStyleableNode();
      Label label = (Label) header.lookup(".label");

      label.setTooltip(tooltip);

      // Makes the tooltip display, no matter where the mouse is inside the column header.
      label.setMaxSize(Double.MAX_VALUE, Double.MAX_VALUE);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static Button makeButton(TableCell<HyperTableRow, HyperTableCell> tableCell)
  {
    Button cellButton = new Button();

    setHeights(cellButton, scalePropertyValueForDPI(18));
    cellButton.setPadding(new Insets(0.0, 7.0, 0.0, 7.0));

    tableCell.emptyProperty().addListener((ob, oldValue, newValue) -> cellButton.setVisible(Boolean.FALSE.equals(newValue)));

    return cellButton;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn setTooltip(ButtonAction ba, String text)
  {
    tooltips.put(ba, row -> text);
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn setTooltip(ButtonAction ba, Function<HyperTableRow, String> supplier)
  {
    tooltips.put(ba, supplier);
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn setGoTooltipBasedOnTarget(Function<HDT_Record, String> supplier)
  {
    return setTooltip(ButtonAction.baGo, row -> nullSwitch(row.getRecord(targetCol), null, supplier));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
