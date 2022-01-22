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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.ObjectOrderDlgCtrlr;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonCellHandler;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.populators.Populator.CellValueType;

import com.google.common.collect.HashBasedTable;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Orientation;
import javafx.scene.Node;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumnBase;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.Region;
import javafx.scene.text.Text;
import javafx.stage.Modality;

//---------------------------------------------------------------------------

public class HyperTable extends HasRightClickableRows<HyperTableRow>
{
  final private int mainCol;
  final private TableView<HyperTableRow> tv;
  final private List<HyperTableColumn> cols = new ArrayList<>();
  final private ObservableList<HyperTableRow> rows = FXCollections.observableArrayList();
  final private FilteredList<HyperTableRow> filteredRows;
  final List<TableColumn<HyperTableRow, ?>> tableCols = new ArrayList<>();

  Consumer<? extends HDT_Record> dblClickHandler = null;
  Runnable onShowMore = null;
  HyperTableRow showMoreRow = null;
  private Runnable refreshHandler = null;
  private boolean canAddRows;
  public boolean disableRefreshAfterCellUpdate = false,
                 autoCommitListSelections = false;

  private static final Map<TableView<?>, Double> rowHeight = new HashMap<>();
  private static final HashBasedTable<TableView<?>, Orientation, ScrollBar> sbMap = HashBasedTable.create();

  private static final Map<String, TableView<?>> registry = new HashMap<>();
  private static final Map<String, HyperDlg> dialogs = new HashMap<>();
  private static final Map<String, List<Double>> colWidthMap = new HashMap<>();

//---------------------------------------------------------------------------

  public TableView<HyperTableRow> getTV()                          { return tv; }
  public List<HyperTableColumn> getColumns()                       { return Collections.unmodifiableList(cols); }
  public HyperTableColumn getColumn(int colNdx)                    { return cols.get(colNdx); }
  public <Pop extends Populator> Pop getPopulator(int colNdx)      { return cols.get(colNdx).getPopulator(); }
  public void clearFilter()                                        { filteredRows.setPredicate(row -> true); }
  public void setFilter(Predicate<HyperTableRow> filter)           { filteredRows.setPredicate(filter); }
  public RecordType getTypeByCol(int colNdx)                       { return cols.get(colNdx).getObjType(); }
  public List<HyperTableCell> getSelByCol(int colNdx)              { return cols.get(colNdx).getSelectedItems(); }
  boolean getCanAddRows()                                          { return canAddRows; }
  public void setCanAddRows(boolean value)                         { canAddRows = value; tv.setEditable(value); }
  public void setOnShowMore(Runnable onShowMore)                   { this.onShowMore = onShowMore; }
  int getMainColNdx()                                              { return mainCol; }
  public void setTooltip(int colNdx, ButtonAction ba, String text) { cols.get(colNdx).setTooltip(ba, text); }
  public void removeRow(HyperTableRow row)                         { rows.remove(row); }
  public Iterable<HyperTableRow> dataRows()                        { return new DataRowIterator(); }
  public Stream<HyperTableRow> dataRowStream()                     { return StreamSupport.stream(new DataRowIterator().spliterator(), false); }
  public int dataRowCount()                                        { return canAddRows ? Math.max(rows.size() - 1, 0) : rows.size(); }
  public void addRefreshHandler(Runnable hndlr)                    { refreshHandler = hndlr; }
  public HyperTableRow selectRowByRecord(HDT_Record record)        { return nullSwitch(getRowByRecord(record), null, this::selectRow); }
  public boolean removeRowsIf(Predicate<HyperTableRow> filter)     { return rows.removeIf(filter); }

  @SuppressWarnings("unused")
  public <HDT_T extends HDT_Record> void setDblClickHandler(Class<HDT_T> klass, Consumer<HDT_T> hndlr) { dblClickHandler = hndlr; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private class DataRowIterator implements Iterable<HyperTableRow>, Iterator<HyperTableRow>
  {
    private int nextNdx = 0;
    final private int dataRowCnt = dataRowCount();
    final private Iterator<HyperTableRow> rowIt = rows.iterator();

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

  @FunctionalInterface public static interface RowBuilder<T> { void buildRow(HyperTableRow row, T object); }

  public <T> void buildRows(Iterable<T> objs, RowBuilder<T> bldr) { objs.forEach(obj -> bldr.buildRow(newDataRow(), obj)); }
  public <T> void buildRows(Stream<T>   objs, RowBuilder<T> bldr) { objs.forEach(obj -> bldr.buildRow(newDataRow(), obj)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // If the tableview is embedded in a context where layout requests aren't getting propagated correctly,
  // this method can be used with a user-supplied handler to manually trigger parent layout requests

  void doExternalRefresh()
  {
    if (refreshHandler != null)
      refreshHandler.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refresh()
  {
    tv.refresh();
    doExternalRefresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void saveColWidthsToPrefs()
  {
    registry.forEach((prefID, tv) ->
    {
      HyperDlg dialog = dialogs.get(prefID);

      if ((dialog == null) || (dialog.shownAlready()))
        saveColWidthsForTable(tv.getColumns(), prefID, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void saveColWidthsForTable(List<? extends TableColumnBase<RowType, ?>> columns, String prefID, boolean rescale)
  {
    int numCols = columns.size();

    List<Double> oldWidths = colWidthMap.get(prefID), curWidths = columns.stream()
      .map(TableColumnBase::getWidth)
      .map(width -> ((width > 0.0) && rescale) ? width / displayScale : width)
      .collect(Collectors.toList());

    boolean save = false;

    if ((oldWidths == null) || (oldWidths.size() != curWidths.size()))
      save = true;
    else
    {
      for (int ndx = 0; ndx < numCols; ndx++)
        if ((curWidths.get(ndx) > 0.0) && (Math.abs(curWidths.get(ndx) - oldWidths.get(ndx)) >= 1.0))
          save = true;
    }

    if (save == false) return;

    for (int ndx = 0; ndx < numCols; ndx++)
    {
      double width = curWidths.get(ndx);

      if (width > 0.0)
      {
        double oldWidth = appPrefs.getDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), -1.0);

        if (Math.abs(width - oldWidth) >= 1.0)
          appPrefs.putDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), width);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void loadColWidthsForTable(List<? extends TableColumnBase<RowType, ?>> columns, String prefID)
  {
    List<Double> newWidths = new ArrayList<>();
    colWidthMap.put(prefID, newWidths);
    int numCols = columns.size();

    for (int ndx = 0; ndx < numCols; ndx++)
    {
      double width = appPrefs.getDouble(prefID + "ColWidth" + String.valueOf(ndx + 1), -1.0);
      newWidths.add(width);

      if (width > 0.0)
      {
        TableColumnBase<RowType, ?> col = columns.get(ndx);

        if (col.isResizable())
          col.setPrefWidth(width);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void registerTable(TableView<RowType> tv, String prefID, HyperDlg dialog)
  {
    if (prefID.isEmpty()) return;

    registry.put(prefID, tv);

    if (dialog != null)
      dialogs.put(prefID, dialog);

    loadColWidthsForTable(tv.getColumns(), prefID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final EnumSet<HyperCtrlType> editableCtrlTypes = EnumSet.of(ctCheckbox, ctDropDown, ctDropDownList, ctEdit);

  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID)
  {
    this(tv, mainCol, canAddRows, prefID, null);
  }

  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID, HyperDlg dialog)
  {
    this.tv = tv;
    this.mainCol = mainCol;
    this.canAddRows = canAddRows;

    if (prefID.length() > 0)
      registerTable(tv, prefID, dialog);

    filteredRows = new FilteredList<>(rows, row -> true);

    SortedList<HyperTableRow> sortedRows = new SortedList<>(filteredRows);
    sortedRows.comparatorProperty().bind(tv.comparatorProperty());

    tv.setItems(sortedRows);
    tv.setPlaceholder(new Text(""));

    tv.getColumns().forEach(tableCols::add);

    tv.setOnKeyPressed(event ->
    {
      if ((event.getCode() != KeyCode.ENTER) || cols.stream().map(HyperTableColumn::getCtrlType).anyMatch(editableCtrlTypes::contains))
        return;

      doRowAction();
      event.consume();
    });

    tv.setRowFactory(theTV ->
    {
      TableRow<HyperTableRow> row = new TableRow<>();

      row.itemProperty().addListener((ob, ov, nv) -> row.setContextMenu(createContextMenu(nv)));

      return row;
    });

    HyperTable.preventMovingColumns(tv, tableCols);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDT_T extends HDT_Record> HDT_T selectedRecord()
  {
    return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, HyperTableRow::getRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell selectedCell()
  {
    return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, HyperTableRow::getCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow selectRow(int ndx)
  {
    return selectRow(rows.get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow selectRow(HyperTableRow row)
  {
    tv.getSelectionModel().select(row);
    scrollToSelection(tv, false);
    return row;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void doRowAction()
  {
    if ((tv.getSelectionModel().getSelectedItem() == showMoreRow) && (showMoreRow != null) && (onShowMore != null))
      onShowMore.run();
    else
      ReadOnlyCell.handleRecord(dblClickHandler, selectedRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void preventMovingColumns(TableView<RowType> tv, List<TableColumn<RowType, ?>> colList)
  {
    // This handsome block of code is the only way within JavaFX to prevent the user from moving columns around
    tv.getColumns().addListener((Change<? extends TableColumn<RowType, ?>> change) ->
    {
      change.next();
      if (change.wasReplaced())
      {
        tv.getColumns().clear();
        tv.getColumns().addAll(colList);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface CellUpdateHandler { void handle(HyperTableRow row, HyperTableCell cellVal, int nextColNdx, Populator nextPopulator); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableColumn addCol(HyperTableColumn col)
  {
    cols.add(col);
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addCol(RecordType objType, HyperCtrlType ctrlType)
  {
    return addColAltPopulator(objType, ctrlType, (ctrlType == ctDropDown) || (ctrlType == ctDropDownList) ?
      new StandardPopulator(objType)
    :
      Populator.create(cvtRecord));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addColWithUpdateHandler(RecordType objType, HyperCtrlType ctrlType, CellUpdateHandler updateHandler) {
    return addColAltPopulatorWithUpdateHandler(objType, ctrlType, new StandardPopulator(objType), updateHandler); }

  public HyperTableColumn addColAltPopulator(RecordType objType, HyperCtrlType ctrlType, Populator populator) {
    return addCol(new HyperTableColumn(this, objType, ctrlType, populator, -1)); }

  public HyperTableColumn addColAltPopulatorWithUpdateHandler(RecordType objType, HyperCtrlType ctrlType, Populator populator, CellUpdateHandler updateHandler) {
    return addCol(new HyperTableColumn(this, objType, ctrlType, populator, -1, updateHandler)); }

  public HyperTableColumn addColAltPopulatorWithActionHandler(RecordType objType, HyperCtrlType ctrlType, Populator populator, EventHandler<ActionEvent> onAction) {
    return addCol(new HyperTableColumn(this, objType, ctrlType, populator, -1, onAction)); }

  public HyperTableColumn addColAltPopulatorWithBothHandlers(RecordType objType, HyperCtrlType ctrlType, Populator populator,
                                                             EventHandler<ActionEvent> onAction, CellUpdateHandler updateHandler) {
    return addCol(new HyperTableColumn(this, objType, ctrlType, populator, -1, onAction, updateHandler)); }

  public HyperTableColumn addActionCol(HyperCtrlType ctrlType, int targetCol) {
    return addCol(new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol)); }

  public HyperTableColumn addActionColWithButtonHandler(HyperCtrlType ctrlType, int targetCol, ButtonCellHandler handler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol, handler, null)); }

  public HyperTableColumn addCustomActionCol(int targetCol, String btnCaption, ButtonCellHandler handler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctCustomBtn, null, targetCol, handler, btnCaption)); }

  public HyperTableColumn addCheckboxCol() {
    return addCol(new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1)); }

  public HyperTableColumn addCheckboxColWithUpdateHandler(CellUpdateHandler updateHandler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1, updateHandler)); }

  public HyperTableColumn addIconCol() {
    return addCol(new HyperTableColumn(this, hdtNone, ctIcon, null, -1));  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditColWithUpdateHandler(RecordType objType, boolean canEditIfEmpty, boolean isNumeric, CellUpdateHandler updateHandler)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctEdit, null, -1, updateHandler);
    col.setCanEditIfEmpty(canEditIfEmpty);
    col.setNumeric(isNumeric);
    cols.add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditCol(RecordType objType, boolean canEditIfEmpty, boolean isNumeric)
  {
    HyperTableColumn col = new HyperTableColumn(this, objType, ctEdit, null, -1);
    col.setCanEditIfEmpty(canEditIfEmpty);
    col.setNumeric(isNumeric);
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

  HyperTableRow newRow(boolean dataRow)
  {
    HyperTableRow row = new HyperTableRow(cols.size(), this);

    if (dataRow && canAddRows)
      rows.add(dataRowCount(), row);
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
      rows.addAll(dataRowCount(), newRows);
    else
      rows.addAll(newRows);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectID(int colNdx, HyperTableRow row, int newID)
  {
    nullSwitch(findFirst(cols.get(colNdx).getPopulator().populate(row, false), cell -> cell.getID() == newID), cell -> row.setCellValue(colNdx, cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectType(int colNdx, HyperTableRow row, RecordType newType)
  {
    nullSwitch(findFirst(cols.get(colNdx).getPopulator().populate(row, false), cell -> cell.getType() == newType), cell -> row.setCellValue(colNdx, cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean containsRecord(HDT_Record record)
  {
    return getRowByRecord(record) != null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow getRowByRecord(HDT_Record record)
  {
    return mainCol < 0 ? null : findFirst(rows, row -> row.getRecord() == record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void browseClick(HyperTableRow row, int colNdx)
  {
    Populator pop = null;
    RecordTypePopulator rtp = null;

    if (row.getRecordType() == hdtGlossary)
    {
      HDT_Term term = HDT_Term.class.cast(ui.activeRecord());
      HDT_Concept concept = nullSwitch(row.getRecord(), null, term::getConcept);

      ui.treeSelector.reset(concept == null ? term : concept, true);
    }
    else
      ui.treeSelector.reset(ui.activeRecord(), true);

    if (colNdx > 0)
    {
      pop = cols.get(colNdx - 1).getPopulator();

      if ((pop != null) && (pop.getValueType() == CellValueType.cvtRecordType))
      {
        rtp = (RecordTypePopulator) pop;

        rtp.getTypes().forEach(objType -> ui.treeSelector.addTargetType(objType));
      }
    }

    if (rtp == null)
    {
      pop = cols.get(colNdx).getPopulator();
      ui.treeSelector.addTargetType(pop.getRecordType(row));
    }

// Determine start record and object record (to be replaced) for tree selection

    int startID = row.getID(colNdx);
    RecordType startType = row.getType(colNdx);
    if (startID > 0)
      ui.treeSelector.setTarget(db.records(startType).getByID(startID));
    else
    {
      if (pop.getRecordType(row) == hdtWorkLabel)
      {
        List<HyperTableCell> choices = pop.populate(row, false);
        HDT_Record record = null;

        if (collEmpty(choices) == false)
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

    ui.goToTreeRecord(db.records(startType).getByID(startID));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addRemoveMenuItem(Predicate<HyperTableRow> condRowHandler)
  {
    addContextMenuItem("Remove this row", condRowHandler, row ->
    {
      rows.remove(row);
      doExternalRefresh();
    });
  }

  public void addRemoveMenuItem() { addRemoveMenuItem((Runnable)null); }

  public void addRemoveMenuItem(Runnable handler)
  {
    addContextMenuItem("Remove this row", cols.get(mainCol).getObjType().getRecordClass(), record -> canAddRows, record ->
    {
      rows.remove(tv.getSelectionModel().getSelectedItem());
      doExternalRefresh();
      if (handler != null) handler.run();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addChangeOrderMenuItem(boolean onlyIfCanAddRows) { addChangeOrderMenuItem(onlyIfCanAddRows, null); }

  public void addChangeOrderMenuItem(boolean onlyIfCanAddRows, Runnable handler)
  {
    addContextMenuItem("Change order",
      row ->
      {
        if ((row.getRecord() == null) && row.getText(mainCol).isEmpty()) return false;
        if (dataRowCount() < 2) return false;
        return (onlyIfCanAddRows == false) || canAddRows;
      },

      row ->
      {
        if (onlyIfCanAddRows && (canAddRows == false)) return;

        if (ui.windows.getOutermostModality() == Modality.NONE)
          if (ui.cantSaveRecord()) return;

        boolean couldAddRows = canAddRows;

        canAddRows = false;

        if (ui.windows.getOutermostModality() == Modality.NONE)
          ui.update();

        ObjectOrderDlgCtrlr.build(this, rows).showModal();

        if (handler != null)
          handler.run();
        else
        {
          if (ui.windows.getOutermostModality() == Modality.NONE)
            ui.cantSaveRecord();
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
    runDelayedInFXThread(1, 200, () -> tv.edit(tv.getItems().indexOf(row), tv.getColumns().get(colNdx)));
  }

  public void cancelEditing()
  {
    runDelayedInFXThread(1, 200, () -> tv.edit(-1, null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ObjectGroup> getAuthorGroups(HDT_Work work, int authorCol, int inFileNameCol, int editorCol, int transCol)
  {
    Map<Integer, Tag> map = new HashMap<>();

    map.put(inFileNameCol, tagInFileName); // Sometimes this is -1, that's okay
    map.put(editorCol    , tagEditor);
    map.put(transCol     , tagTranslator);

    return getObjectGroupList(work, rtAuthorOfWork, authorCol, map);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<ObjectGroup> getObjectGroupList(HDT_Record subj, RelationType relType, int primaryColNdx, Map<Integer, Tag> colNdxToTag)
  {
    List<ObjectGroup> list = new ArrayList<>();
    RecordType objType = db.getObjType(relType);

    db.getNestedTags(relType).stream().filter(Predicate.not(colNdxToTag::containsValue)).forEach(tag -> colNdxToTag.put(-1, tag));

    rows.forEach(row ->
    {
      if (row.getType(primaryColNdx) == objType)
      {
        int id = row.getID(primaryColNdx);
        if ((id > 0) || (row.getText(primaryColNdx).length() > 0))
        {
          HDT_Record obj;
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
            HyperDataCategory hdc = db.getNestedSchema(relType, tag).getCategory();
            NestedValue val = new NestedValue(hdc);

            if (colNdx < 0)
            {
              if (subj != null)
              {
                if (id > 0)
                {
                  switch (hdc)
                  {
                    case hdcString        : val.str     = db.getNestedString (subj, obj, tag); break;
                    case hdcBoolean       : val.bool    = db.getNestedBoolean(subj, obj, tag); break;
                    case hdcTernary       : val.ternary = db.getNestedTernary(subj, obj, tag); break;
                    case hdcNestedPointer : val.target  = db.getNestedPointer(subj, obj, tag); break;
                    default               : break;
                  }
                }
                else
                {
                  if (tag == Tag.tagInFileName)
                  {
                    Author author = ((HDT_Work)subj).getAuthors().getAuthor(new PersonName(group.getPrimaryStr()));
                    if (author != null)
                      val.ternary = author.getInFileName();
                  }
                }
              }
            }
            else switch (hdc)
            {
              case hdcString        : val.str     = row.getText(colNdx); break;
              case hdcBoolean       : val.bool    = row.getCheckboxValue(colNdx); break;
              case hdcTernary       : val.ternary = row.getCheckboxValue(colNdx) ? Ternary.True : Ternary.False; break;
              case hdcNestedPointer : val.target  = row.getRecord(colNdx); break;
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
  public <HDT_T extends HDT_Record> List<HDT_T> saveToList(int colNdx, RecordType objType)
  {
    return rows.stream().filter(row -> row.getType(colNdx) == objType)
                        .map(row -> row.getID(colNdx))
                        .filter(id -> id > 0)
                        .map(id -> (HDT_T) db.records(objType).getByID(id))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toCollection(ArrayList::new));
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

    double rHeight = getRowHeight(tv),

           allRowsHeight = rHeight * (tv.getItems().size() + 1),
           dataRowsHeight = rHeight * tv.getItems().size(),
           vpHeight = allRowsHeight * sb.getVisibleAmount(),
           vpTop = (dataRowsHeight - vpHeight) * sb.getValue(),
           vpBottom = vpTop + vpHeight,

           y1 = ndx * rHeight,
           y2 = (ndx + 1) * rHeight;

    if      (y1 < vpTop)    sb.setValue(y1 / (dataRowsHeight - vpHeight));
    else if (y2 > vpBottom) sb.setValue((y2 - vpHeight) / (dataRowsHeight - vpHeight));
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

  public void changeIDs(RecordType changedType, int oldID, int newID)
  {
    dataRows().forEach(row -> row.changeIDs(changedType, oldID, newID));
    refresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
