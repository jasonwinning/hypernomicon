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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.dialogs.HyperDialog;
import org.hypernomicon.view.dialogs.ObjectOrderDialogController;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonCellHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.TreeWrapper.TreeTargetType;

import com.google.common.collect.HashBasedTable;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Predicate;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import javafx.stage.Modality;

//---------------------------------------------------------------------------

public class HyperTable implements RecordListView
{
  private int mainCol;
  HyperTableRow showMoreRow = null;
  private boolean canAddRows;
  private TableView<HyperTableRow> tv;
  private List<HyperTableColumn> cols;
  private ObservableList<HyperTableRow> rows;
  private SortedList<HyperTableRow> sortedRows;
  private FilteredList<HyperTableRow> filteredRows;
  final ArrayList<TableColumn<HyperTableRow, HyperTableCell>> tableCols = new ArrayList<TableColumn<HyperTableRow, HyperTableCell>>();
  private List<HyperMenuItem<? extends HDT_Base>> contextMenuItems;
  RecordHandler<? extends HDT_Base> dblClickHandler = null;
  Runnable onShowMore = null;
  public boolean disableRefreshAfterCellUpdate = false,
                 autoCommitListSelections = false;
  ComboBoxCell cellBeingEdited = null;

  private static HashMap<TableView<?>, Double> rowHeight = new HashMap<>();
  private static HashBasedTable<TableView<?>, Orientation, ScrollBar> sbMap = HashBasedTable.create();

  private static final HashMap<String, TableView<?>> registry = new HashMap<>();
  private static final HashMap<String, HyperDialog> dialogs = new HashMap<>();

//---------------------------------------------------------------------------

  public TableView<HyperTableRow> getTV()                          { return tv; }
  public List<HyperTableColumn> getColumns()                       { return Collections.unmodifiableList(cols); }
  public HyperTableColumn getColumn(int colNdx)                    { return cols.get(colNdx); }
  public <Pop extends Populator> Pop getPopulator(int colNdx)      { return cols.get(colNdx).getPopulator(); }
  public void clearFilter()                                        { filteredRows.setPredicate(row -> true); }
  public void setFilter(Predicate<HyperTableRow> filter)           { filteredRows.setPredicate(filter); }
  public HDT_RecordType getTypeByCol(int colNdx)                   { return cols.get(colNdx).getObjType(); }
  public List<HyperTableCell> getSelByCol(int colNdx)              { return cols.get(colNdx).getSelectedItems(); }
  public boolean getCanAddRows()                                   { return canAddRows; }
  public void setCanAddRows(boolean value)                         { canAddRows = value; tv.setEditable(value); }
  public void setOnShowMore(Runnable onShowMore)                   { this.onShowMore = onShowMore; }
  public int getMainColNdx()                                       { return mainCol; }
  public void setTooltip(int colNdx, ButtonAction ba, String text) { cols.get(colNdx).setTooltip(ba, text); }
  public ComboBoxCell getCellBeingEdited()                         { return cellBeingEdited; }
  public void removeRow(HyperTableRow row)                         { rows.remove(row); }
  public Iterable<HyperTableRow> getDataRows()                     { return new DataRowIterator(); }
  public int getDataRowCount()                                     { return canAddRows ? Math.max(rows.size() - 1, 0) : rows.size(); }

  @SuppressWarnings("unused")
  public <HDT_T extends HDT_Base> void setDblClickHandler(Class<HDT_T> klass, RecordHandler<HDT_T> hndlr)    { this.dblClickHandler = hndlr; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class DataRowIterator implements Iterable<HyperTableRow>, Iterator<HyperTableRow>
  {
    private int nextNdx = 0, dataRowCnt = getDataRowCount();
    private Iterator<HyperTableRow> rowIt = rows.iterator();

    @Override public Iterator<HyperTableRow> iterator() { return this; }
    @Override public boolean hasNext()                  { return nextNdx < dataRowCnt; }

    @Override public HyperTableRow next()
    {
      if (!hasNext()) throw new NoSuchElementException();
      nextNdx++;
      return rowIt.next();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface public static interface RowBuilder<T> { public void buildRow(HyperTableRow row, T object); }

  public <T> void buildRows(Iterable<T> objs, RowBuilder<T> bldr) { objs.forEach(obj -> bldr.buildRow(newDataRow(), obj)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void saveColWidthsToPrefs()
  {
    registry.forEach((prefID, tv) ->
    {
      HyperDialog dialog = dialogs.get(prefID);

      if ((dialog != null) && (dialog.shownAlready() == false))
        return;

      saveColWidthsForTable(tv, prefID, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void saveColWidthsForTable(TableView<RowType> tv, String prefID, boolean rescale)
  {
    for (int ndx = 0; ndx < tv.getColumns().size(); ndx++)
    {
      TableColumn<RowType, ?> col = tv.getColumns().get(ndx);
      double width = col.getWidth();

      if (rescale)
        width = width / displayScale;

      if (width > 0)
      {
        double oldWidth = appPrefs.getDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), -1);

        if (Math.abs(width - oldWidth) >= 1.0)
          appPrefs.putDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), width);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void loadColWidthsForTable(TableView<RowType> tv, String prefID)
  {
    int numCols = tv.getColumns().size();

    for (int ndx = 0; ndx < numCols; ndx++)
    {
      double width = appPrefs.getDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), -1);

      if (width > 0)
      {
        TableColumn<RowType, ?> col = tv.getColumns().get(ndx);

        if (col.isResizable())
          col.setPrefWidth(width);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void registerTable(TableView<RowType> tv, String prefID, HyperDialog dialog)
  {
    if (prefID.length() < 1) return;

    registry.put(prefID, tv);

    if (dialog != null)
      dialogs.put(prefID, dialog);

    loadColWidthsForTable(tv, prefID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID)
  {
    this(tv, mainCol, canAddRows, prefID, null);
  }

  @SuppressWarnings("unchecked")
  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID, HyperDialog dialog)
  {
    this.tv = tv;
    this.mainCol = mainCol;
    this.canAddRows = canAddRows;

    if (prefID.length() > 0)
      registerTable(tv, prefID, dialog);

    cols = new ArrayList<HyperTableColumn>();
    rows = FXCollections.observableArrayList();

    contextMenuItems = new ArrayList<>();

    filteredRows = new FilteredList<>(rows, row -> true);

    sortedRows = new SortedList<>(filteredRows);
    sortedRows.comparatorProperty().bind(tv.comparatorProperty());

    tv.setItems(sortedRows);
    tv.setPlaceholder(new Text(""));

    tv.getColumns().forEach(tc -> tableCols.add((TableColumn<HyperTableRow, HyperTableCell>) tc));

    tv.setOnKeyPressed(event ->
    {
      if (event.getCode() == KeyCode.ENTER)
      {
        for (HyperTableColumn col : cols)
        {
          switch (col.getCtrlType())
          {
            case ctCheckbox:     case ctDropDown:
            case ctDropDownList: case ctEdit:
              return;
            default:
              break;
          }
        }

        doRowAction();

        event.consume();
      }
    });

    tv.setRowFactory(theTV ->
    {
      final TableRow<HyperTableRow> row = new TableRow<>();

      row.itemProperty().addListener((observable, oldValue, newValue) ->
      {
        if (newValue == null)
          row.setContextMenu(null);
        else
          row.setContextMenu(createContextMenu(newValue));
      });

      return row;
    });

    HyperTable.preventMovingColumns(tv, tableCols);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDT_T extends HDT_Base> HDT_T selectedRecord()
  {
    return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, row -> row.getRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectRow(int ndx)
  {
    selectRow(rows.get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectRow(HyperTableRow row)
  {
    tv.getSelectionModel().select(row);
    scrollToSelection(tv, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void doRowAction()
  {
    if ((tv.getSelectionModel().getSelectedItem() == showMoreRow) && (showMoreRow != null) && (onShowMore != null))
      onShowMore.run();
    else
      ui.goToRecord(selectedRecord(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ContextMenu createContextMenu(HyperTableRow row)
  {
    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();

    for (HyperMenuItem<? extends HDT_Base> hItem : contextMenuItems)
    {
      MenuItem newItem = createContextMenuItem(hItem, row, rowMenu);
      rowMenu.getItems().add(newItem);

      if (newItem.isVisible()) noneVisible = false;
    }

    if (noneVisible) return null;
    return rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <HDT_T extends HDT_Base> MenuItem createContextMenuItem(HyperMenuItem<HDT_T> hItem, HyperTableRow row, ContextMenu rowMenu)
  {
    MenuItem newItem = new MenuItem(hItem.caption);

    newItem.setOnAction(event ->
    {
      HDT_T record;

      if (hItem.recordType == hdtNone)
        record = row.getRecord();
      else
        record = row.getRecordByType(hItem.recordType);

      rowMenu.hide();

      if (hItem.okayIfBlank)
      {
        if (hItem.recordHandler == null)
          hItem.rowHandler.handle(row);
        else
          hItem.recordHandler.handle(record);
      }
      else if (record != null)
        hItem.recordHandler.handle(record);
    });

    boolean visible = false;

    HDT_T record;

    if (hItem.recordType == hdtNone)
      record = row.getRecord();
    else
      record = row.getRecordByType(hItem.recordType);

    if (hItem.okayIfBlank)
    {
      if (hItem.condRecordHandler == null)
        visible = hItem.condRowHandler.handle(row);
      else
        visible = hItem.condRecordHandler.handle(record);
    }
    else if (record != null)
      visible = hItem.condRecordHandler.handle(record);

    newItem.setVisible(visible);

    return newItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType, ColType> void preventMovingColumns(TableView<RowType> tv, ArrayList<TableColumn<RowType, ColType>> colList)
  {
    // This handsome block of code is the only way within JavaFX to prevent the user from moving columns around
    tv.getColumns().addListener(new ListChangeListener<TableColumn<RowType, ?>>()
    {
      @Override public void onChanged(Change<? extends TableColumn<RowType, ?>> change)
      {
        change.next();
        if (change.wasReplaced())
        {
          tv.getColumns().clear();
          tv.getColumns().addAll(colList);
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface CellUpdateHandler { public void handle(HyperTableRow row, HyperTableCell cellVal, int nextColNdx, Populator nextPopulator); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface public interface RowHandler     { public void handle(HyperTableRow row); }
  @FunctionalInterface public interface CondRowHandler { public boolean handle(HyperTableRow row); }

  public static class HyperMenuItem<HDT_T extends HDT_Base>
  {
    public HDT_RecordType recordType = hdtNone;
    public RecordHandler<HDT_T> recordHandler;
    public CondRecordHandler<HDT_T> condRecordHandler;
    public RowHandler rowHandler;
    public CondRowHandler condRowHandler;

    public String caption;
    public boolean okayIfBlank = false;

    public HyperMenuItem(String caption) { this.caption = caption; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperMenuItem<HDT_Base> addCondRowBasedContextMenuItem(String caption, CondRowHandler condRowHandler, RowHandler rowHandler)
  {
    HyperMenuItem<HDT_Base> mnu;

    mnu = new HyperMenuItem<>(caption);
    mnu.recordType = hdtNone;
    mnu.condRecordHandler = null;
    mnu.recordHandler = null;
    mnu.condRowHandler = condRowHandler;
    mnu.rowHandler = rowHandler;
    mnu.okayIfBlank = true;

    contextMenuItems.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperMenuItem<HDT_Base> addCondContextMenuItemOkayIfBlank(String caption, CondRecordHandler<HDT_Base> condRecordHandler, RecordHandler<HDT_Base> recordHandler)
  {
    HyperMenuItem<HDT_Base> mnu;

    mnu = new HyperMenuItem<>(caption);
    mnu.recordType = hdtNone;
    mnu.condRecordHandler = condRecordHandler;
    mnu.recordHandler = recordHandler;
    mnu.okayIfBlank = true;

    contextMenuItems.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addContextMenuItem(String caption, Class<HDT_T> klass, RecordHandler<HDT_T> recordHandler)
  {
    return addCondContextMenuItem(caption, klass, record -> true, recordHandler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addCondContextMenuItem(String caption, Class<HDT_T> klass, CondRecordHandler<HDT_T> condRecordHandler, RecordHandler<HDT_T> recordHandler)
  {
    HyperMenuItem<HDT_T> mnu;

    mnu = new HyperMenuItem<>(caption);
    mnu.recordType = HDT_RecordType.typeByRecordClass(klass);
    mnu.condRecordHandler = condRecordHandler;
    mnu.recordHandler = recordHandler;

    contextMenuItems.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addCol(HDT_RecordType objType, HyperCtrlType ctrlType)
  {
    switch (ctrlType)
    {
      case ctDropDown: case ctDropDownList:

        return addColAltPopulator(objType, ctrlType, new StandardPopulator(objType));

      default:

        return addColAltPopulator(objType, ctrlType, new EmptyPopulator());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColWithUpdateHandler(HDT_RecordType objType, HyperCtrlType ctrlType, CellUpdateHandler updateHandler)
  {
    return addColAltPopulatorWithUpdateHandler(objType, ctrlType, new StandardPopulator(objType), updateHandler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColAltPopulator(HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctrlType, populator, -1);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColAltPopulatorWithUpdateHandler(HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, CellUpdateHandler updateHandler)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctrlType, populator, -1, updateHandler);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColAltPopulatorWithActionHandler(HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator, EventHandler<ActionEvent> onAction)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctrlType, populator, -1, onAction);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColAltPopulatorWithBothHandlers(HDT_RecordType objType, HyperCtrlType ctrlType, Populator populator,
                                                 EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctrlType, populator, -1, onAction, updateHandler);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addActionCol(HyperCtrlType ctrlType, int targetCol)
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addActionColWithButtonHandler(HyperCtrlType ctrlType, int targetCol, ButtonCellHandler handler)
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol, handler, null);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addCustomActionCol(int targetCol, String btnCaption, ButtonCellHandler handler)
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctCustomBtn, null, targetCol, handler, btnCaption);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addCheckboxCol()
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addCheckboxColWithUpdateHandler(CellUpdateHandler updateHandler)
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1, updateHandler);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditColWithUpdateHandler(HDT_RecordType objType, boolean canEditIfEmpty, boolean isNumeric, CellUpdateHandler updateHandler)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctEdit, null, -1, updateHandler);
    col.setCanEditIfEmpty(canEditIfEmpty);
    col.setNumeric(isNumeric);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditCol(HDT_RecordType objType, boolean canEditIfEmpty, boolean isNumeric)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctEdit, null, -1);
    col.setCanEditIfEmpty(canEditIfEmpty);
    col.setNumeric(isNumeric);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addIconCol()
  {
    HyperTableColumn col = new HyperTableColumn(this, hdtNone, ctIcon, null, -1);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clearKeepSortOrder()
  {
    showMoreRow = null;

    cols.forEach(HyperTableColumn::clear);

    rows.clear();
    clearFilter();

    if (canAddRows)
      rows.add(new HyperTableRow(cols.size(), this));
  }

  public void clear()
  {
    clearKeepSortOrder();
    tv.getSortOrder().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow newDataRow() { return newRow(true); }

  public HyperTableRow newRow(boolean dataRow)
  {
    HyperTableRow row = new HyperTableRow(cols.size(), this);

    if (dataRow && canAddRows)
      rows.add(getDataRowCount(), row);
    else
      rows.add(row);

    return row;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setDataRows(List<HyperTableRow> newRows)
  {
    showMoreRow = null;
    rows.setAll(newRows);

    if (canAddRows)
      newRow(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDataRows(List<HyperTableRow> newRows)
  {
    if (canAddRows)
      rows.addAll(getDataRowCount(), newRows);
    else
      rows.addAll(newRows);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectID(int colNdx, HyperTableRow row, int newID)
  {
    for (HyperTableCell cell : cols.get(colNdx).getPopulator().populate(row, false))
    {
      if (cell.getID() == newID)
      {
        row.setCellValue(colNdx, cell);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectType(int colNdx, HyperTableRow row, HDT_RecordType newType)
  {
    for (HyperTableCell cell : cols.get(colNdx).getPopulator().populate(row, false))
    {
      if (cell.getType() == newType)
      {
        row.setCellValue(colNdx, cell);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getColNdxByObjType(HDT_RecordType objType)
  {
    for (HyperTableColumn col : cols)
    {
      if (col.getObjType().equals(objType))
        return col.getColNdx();
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean containsRecord(HDT_Base record)
  {
    return getRowByRecord(record) != null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow getRowByRecord(HDT_Base record)
  {
    if (mainCol < 0) return null;

    for (HyperTableRow row : rows)
      if (row.getRecord() == record)
        return row;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void browseClick(HyperTableRow row, int colNdx)
  {
    Populator pop = null;
    RecordTypePopulator rtp = null;

    ui.treeTargetTypes.clear();

    if (colNdx > 0)
    {
      pop = cols.get(colNdx - 1).getPopulator();

      if (pop != null)
        if (pop.getValueType() == CellValueType.cvtRecordType)
        {
          rtp = (RecordTypePopulator) pop;

          rtp.getTypes().forEach(objType -> ui.treeTargetTypes.add(new TreeTargetType(getRelation(ui.activeType(), objType), objType)));
        }
    }

    if (rtp == null)
    {
      pop = cols.get(colNdx).getPopulator();
      HDT_RecordType objType = pop.getRecordType(row);
      ui.treeTargetTypes.add(new TreeTargetType(getRelation(ui.activeType(), objType), objType));
    }

// Determine start record and object record (to be replaced) for tree selection

    int startID = row.getID(colNdx);
    HDT_RecordType startType = row.getType(colNdx);
    if (startID > 0)
      ui.treeObjRecord = db.records(startType).getByID(startID);
    else
    {
      ui.treeObjRecord = null;

      if (pop.getRecordType(row) == hdtWorkLabel)
      {
        List<HyperTableCell> choices = pop.populate(row, false);
        HDT_Base record = null;

        if (choices != null)
          if (choices.size() > 0)
            record = HyperTableCell.getRecord(choices.get(0));

        startID = record == null ? 1 : record.getID();
        startType = hdtWorkLabel;
      }
      else
      {
        startID = ui.activeRecord().getID();
        startType = ui.activeRecord().getType();
      }
    }

    ui.treeSubjRecord = ui.activeRecord();

    ui.goToTreeRecord(db.records(startType).getByID(startID));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshCol(int colNdx)
  {
    tv.getColumns().get(colNdx).setVisible(false);  // Necessary workaround; tableview does not automatically refresh
    tv.getColumns().get(colNdx).setVisible(true);   // when you change values in the cell objects, just the row objects
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addRemoveMenuItem() { addRemoveMenuItem(null); }

  public void addRemoveMenuItem(Runnable handler)
  {
    addCondContextMenuItem("Remove this row", cols.get(mainCol).getObjType().getRecordClass(), record -> canAddRows, record ->
    {
      rows.remove(tv.getSelectionModel().getSelectedItem());
      if (handler != null) handler.run();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addChangeOrderMenuItem(boolean onlyIfCanAddRows) { addChangeOrderMenuItem(onlyIfCanAddRows, null); }

  public void addChangeOrderMenuItem(boolean onlyIfCanAddRows, Runnable handler)
  {
    addCondRowBasedContextMenuItem("Change order",
      row ->
      {
        if ((row.getRecord() == null) && (row.getText(mainCol).length() == 0)) return false;
        if (getDataRowCount() < 2) return false;
        return (onlyIfCanAddRows == false) || canAddRows;
      },

      row ->
      {
        if (onlyIfCanAddRows && (canAddRows == false)) return;

        if (ui.windows.getOutermostModality() == Modality.NONE)
          if (ui.cantSaveRecord(true)) return;

        boolean couldAddRows = canAddRows;

        canAddRows = false;

        if (ui.windows.getOutermostModality() == Modality.NONE)
          ui.update();

        ObjectOrderDialogController.create("Change order of rows", this, rows).showModal();

        if (handler != null)
          handler.run();
        else
        {
          if (ui.windows.getOutermostModality() == Modality.NONE)
            ui.cantSaveRecord(true);
        }

        canAddRows = couldAddRows;

        if (ui.windows.getOutermostModality() == Modality.NONE)
          ui.update();
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This assumes that the table is not currently being edited.

  public void edit(HyperTableRow row, int colNdx)
  {
    runDelayedInFXThread(1, 200, event -> tv.edit(tv.getItems().indexOf(row), tv.getColumns().get(colNdx)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ObjectGroup> getAuthorGroups(HDT_Work work, int authorCol, int inFileNameCol, int editorCol, int transCol)
  {
    HashMap<Integer, Tag> map = new HashMap<>();

    map.put(inFileNameCol, tagInFileName); // Sometimes this is -1, that's okay
    map.put(editorCol, tagEditor);
    map.put(transCol, tagTranslator);

    return getObjectGroupList(work, rtAuthorOfWork, authorCol, map);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ObjectGroup> getObjectGroupList(HDT_Base subj, RelationType relType, int primaryColNdx, Map<Integer, Tag> colNdxToTag)
  {
    ArrayList<ObjectGroup> list = new ArrayList<>();
    HDT_RecordType objType = db.getObjType(relType);

    db.getNestedTags(relType).forEach(tag ->
    {
      if (colNdxToTag.containsValue(tag) == false)
        colNdxToTag.put(-1, tag);
    });

    rows.forEach(row ->
    {
      if (row.getType(primaryColNdx) == objType)
      {
        int id = row.getID(primaryColNdx);
        if ((id > 0) || row.getText(primaryColNdx).length() > 0)
        {
          HDT_Base obj;
          ObjectGroup group;

          if (id < 1)
          {
            obj = null;
            group = new ObjectGroup(row.getText(primaryColNdx));
          }
          else
          {
            obj = db.records(objType).getByID(id);
            group = new ObjectGroup(obj);
          }

          colNdxToTag.forEach((colNdx, tag) ->
          {
            HDI_Schema schema = db.getNestedSchema(relType, tag);
            NestedValue val = new NestedValue(schema.getCategory());

            if (colNdx < 0)
            {
              if (subj != null)
              {
                if (id > 0)
                {
                  switch (schema.getCategory())
                  {
                    case hdcString        : val.str = db.getNestedString(subj, obj, tag); break;
                    case hdcBoolean       : val.bool = db.getNestedBoolean(subj, obj, tag); break;
                    case hdcTernary       : val.ternary = db.getNestedTernary(subj, obj, tag); break;
                    case hdcNestedPointer : val.target = db.getNestedPointer(subj, obj, tag); break;
                    default               : break;
                  }
                }
                else
                {
                  if (tag == Tag.tagInFileName)
                  {
                    Author author = HDT_Work.class.cast(subj).getAuthors().getAuthor(new PersonName(group.getPrimaryStr()));
                    if (author != null)
                      val.ternary = author.getInFileName();
                  }
                }
              }
            }
            else switch (schema.getCategory())
            {
              case hdcString        : val.str = row.getText(colNdx); break;
              case hdcBoolean       : val.bool = row.getCheckboxValue(colNdx); break;
              case hdcTernary       : val.ternary = row.getCheckboxValue(colNdx) ? Ternary.True : Ternary.False; break;
              case hdcNestedPointer : val.target = row.getRecord(colNdx); break;
              default               : break;
            }

            group.addNestedEntry(tag, val);
          });

          list.add(group);
        }
      }
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_Base> ArrayList<HDT_T> saveToList(int colNdx, HDT_RecordType objType)
  {
    ArrayList<HDT_T> list = new ArrayList<>();

    rows.forEach(row ->
    {
      if (row.getType(colNdx) == objType)
      {
        int id = row.getID(colNdx);
        if (id > 0)
        {
          HDT_T record = (HDT_T) db.records(objType).getByID(id);
          if (record != null)
            list.add(record);
        }
      }
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <RowType> ScrollBar getScrollBar(TableView<RowType> tv, Orientation o)
  {
    ScrollBar sb = sbMap.get(tv, o);
    if (sb != null) return sb;

    for (Node n: tv.lookupAll(".scroll-bar"))
    {
      if (n instanceof ScrollBar)
      {
        sb = (ScrollBar) n;
        if (sb.getOrientation() == o)
        {
          sbMap.put(tv, o, sb);
          return sb;
        }
      }
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void scrollToSelection() { scrollToSelection(tv, false); }

  public static <RowType> void scrollToSelection(TableView<RowType> tv, boolean delay)
  {
    if (delay)
      Platform.runLater(() -> scrollToNdx(tv, tv.getSelectionModel().getSelectedIndex()));
    else
      scrollToNdx(tv, tv.getSelectionModel().getSelectedIndex());
  }

  // The way this works is better than TableView.scrollTo
  // scrollTo changes the scroll position even if the row in question was already in view

  private static <RowType> void scrollToNdx(TableView<RowType> tv, int ndx)
  {
    ScrollBar sb = getScrollBar(tv, Orientation.VERTICAL);
    if (sb == null) return;

    double rHeight = getRowHeight(tv);

    double allRowsHeight = rHeight * (tv.getItems().size() + 1);
    double dataRowsHeight = rHeight * tv.getItems().size();
    double vpHeight = allRowsHeight * sb.getVisibleAmount();
    double vpTop = (dataRowsHeight - vpHeight) * sb.getValue();
    double vpBottom = vpTop + vpHeight;

    double y1 = ndx * rHeight;
    double y2 = (ndx + 1) * rHeight;

    if (y1 < vpTop)
      sb.setValue(y1 / (dataRowsHeight - vpHeight));
    else if (y2 > vpBottom)
      sb.setValue((y2 - vpHeight) / (dataRowsHeight - vpHeight));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <RowType> double getRowHeight(TableView<RowType> tv)
  {
    Double heightObj = rowHeight.get(tv);
    if (heightObj != null) return heightObj.doubleValue();

    for (Node rowNode : tv.lookupAll(".indexed-cell"))
    {
      if (rowNode instanceof TableRow)
      {
        double height = ((Region) rowNode).getHeight();
        rowHeight.put(tv, height);
        return height;
      }
    }

    return 0.0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
