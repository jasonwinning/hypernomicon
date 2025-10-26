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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.*;
import java.util.function.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.dialogs.ObjectOrderDlgCtrlr;
import org.hypernomicon.dialogs.base.DialogBase;
import org.hypernomicon.model.HDI_Schema.HyperDataCategory;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.HyperTableColumn.*;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.text.Text;
import javafx.stage.Modality;

//---------------------------------------------------------------------------

public class HyperTable extends HasRightClickableRows<HyperTableRow>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final int mainCol;
  private final TableView<HyperTableRow> tv;
  private final List<HyperTableColumn> cols = new ArrayList<>();
  private final ObservableList<HyperTableRow> rows = FXCollections.observableArrayList();
  private final List<HyperTableRow> unmodRows = Collections.unmodifiableList(rows);
  private final FilteredList<HyperTableRow> filteredRows;
  private final Map<Integer, HyperTableCell> colNdxToDefaultValue = new HashMap<>();

  Consumer<? extends HDT_Record> dblClickHandler = null;
  Runnable onShowMore = null;
  HyperTableRow showMoreRow = null;
  private Runnable refreshHandler = null;
  private boolean canAddRows;
  public boolean disableRefreshAfterCellUpdate = false,
                 autoCommitListSelections = false;

  private static final Map<String, TableView<?>> registry = new HashMap<>();
  private static final Map<String, DialogBase> dialogs = new HashMap<>();

//---------------------------------------------------------------------------

  public TableView<HyperTableRow> getTV()                       { return tv; }
  public List<HyperTableColumn> getColumns()                    { return Collections.unmodifiableList(cols); }
  public List<HyperTableRow> getRows()                          { return unmodRows; }
  public HyperTableColumn getColumn(int colNdx)                 { return cols.get(colNdx); }
  public <Pop extends Populator> Pop getPopulator(int colNdx)   { return cols.get(colNdx).getPopulator(); }
  public void clearFilter()                                     { filteredRows.setPredicate(row -> true); }
  public void setFilter(Predicate<HyperTableRow> filter)        { filteredRows.setPredicate(filter); }
  public RecordType getTypeByCol(int colNdx)                    { return cols.get(colNdx).getObjType(); }
  boolean getCanAddRows()                                       { return canAddRows; }
  public void setCanAddRows(boolean value)                      { canAddRows = value; tv.setEditable(value); }
  public void setOnShowMore(Runnable onShowMore)                { this.onShowMore = onShowMore; }
  int getMainColNdx()                                           { return mainCol; }
  public void removeRow(HyperTableRow row)                      { rows.remove(row); }
  public void swapRows(int ndx1, int ndx2)                      { Collections.swap(rows, ndx1, ndx2); }
  public Iterable<HyperTableRow> dataRows()                     { return new DataRowIterator(); }
  public Stream<HyperTableRow> dataRowStream()                  { return iterableToStream(dataRows()); }
  public int dataRowCount()                                     { return canAddRows ? Math.max(rows.size() - 1, 0) : rows.size(); }
  public void addRefreshHandler(Runnable handler)               { refreshHandler = handler; }
  public HyperTableRow selectRowByRecord(HDT_Record record)     { return nullSwitch(getRowByRecord(record), null, this::selectRow); }
  public boolean removeRowsIf(Predicate<HyperTableRow> filter)  { return rows.removeIf(filter); }
  public void setDefaultValue(int colNdx, HyperTableCell value) { colNdxToDefaultValue.put(colNdx, value); }

  @SuppressWarnings("unused")
  public <HDT_T extends HDT_Record> void setDblClickHandler(Class<HDT_T> klass, Consumer<HDT_T> handler) { dblClickHandler = handler; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private class DataRowIterator implements Iterable<HyperTableRow>, Iterator<HyperTableRow>
  {
    private int nextNdx = 0;
    private final int dataRowCnt = dataRowCount();
    private final Iterator<HyperTableRow> rowIt = rows.iterator();

    @Override public Iterator<HyperTableRow> iterator() { return this; }
    @Override public boolean hasNext()                  { return nextNdx < dataRowCnt; }

    @Override public HyperTableRow next()
    {
      if (hasNext() == false)
        throw new NoSuchElementException("No more rows available at index " + nextNdx);

      nextNdx++;
      return rowIt.next();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface public interface RowBuilder<T> { void buildRow(HyperTableRow row, T object); }

  public <T> void buildRows(Iterable<T> objs, RowBuilder<T> bldr) { objs.forEach       (obj -> bldr.buildRow(newDataRow(), obj)); }
  public <T> void buildRows(Stream<T>   objs, RowBuilder<T> bldr) { objs.forEachOrdered(obj -> bldr.buildRow(newDataRow(), obj)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final EnumSet<HyperCtrlType> editableCtrlTypes = EnumSet.of(ctCheckbox, ctNoneditableDropDown, ctEditableUnlimitedDropDown, ctEditableLimitedDropDown, ctEdit);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID)
  {
    this(tv, mainCol, canAddRows, prefID, null);
  }

  public HyperTable(TableView<HyperTableRow> tv, int mainCol, boolean canAddRows, String prefID, DialogBase dialog)
  {
    this.tv = tv;
    this.mainCol = mainCol;
    this.canAddRows = canAddRows;

    if (safeStr(prefID).length() > 0)
      registerTable(tv, prefID, dialog);

    filteredRows = new FilteredList<>(rows, row -> true);

    SortedList<HyperTableRow> sortedRows = new SortedList<>(filteredRows);
    sortedRows.comparatorProperty().bind(tv.comparatorProperty());

    tv.setItems(sortedRows);
    tv.setPlaceholder(new Text(""));

    tv.addEventHandler(KeyEvent.KEY_PRESSED, event ->
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

    preventMovingColumns(tv, List.copyOf(tv.getColumns()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * If the tableview is embedded in a context where layout requests aren't getting propagated correctly,
   * this method can be used with a user-supplied handler to manually trigger parent layout requests.
   */
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

  /**
   * This regenerates the cell values for a column, assuming they were originally generated based on IDs.
   * <p>
   * Cells where the <code>id == -1</code> are ignored.
   * <p>
   * This assumes that the column has already been repopulated, so normally you have to call <code>populate(true)</code> first.
   * @param colNdx Column index
   */
  public void regenerateCellsBasedOnIDs(int colNdx)
  {
    dataRows().forEach(row ->
    {
      if (row.getID(colNdx) >= 0)
        row.setCellValue(colNdx, row.getPopulator(colNdx).getChoiceByID(row, row.getID(colNdx)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void saveColWidthsToPrefs()
  {
    registry.forEach((prefID, tv) ->
    {
      DialogBase dialog = dialogs.get(prefID);

      if ((dialog == null) || dialog.shownAlready())
        saveColWidthsForTable(tv, tv.getColumns(), prefID);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record ColumnSettings(int oldNdx, double oldWidth, boolean defVisible) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Widths are saved to preferences as scaled

  public static <RowType, ColType extends TableColumnBase<RowType, ?>> void saveColWidthsForTable(Control tv, Iterable<ColType> columns, String prefID)
  {
    assert(getIsScaled(tv));

    columns.forEach(col ->
    {
      double newWidth = col.getWidth();

      ColumnSettings colSettings = (ColumnSettings) col.getUserData();

      if ((newWidth > 0.0) && (Math.abs(newWidth - colSettings.oldWidth) >= 1.0))
      {
        app.prefs.putDouble(prefID + "ColWidth" + colSettings.oldNdx, newWidth);
        col.setUserData(new ColumnSettings(colSettings.oldNdx, newWidth, colSettings.defVisible));
      }

      String prefKey = prefID + "ColVisible" + colSettings.oldNdx;
      if (col.isVisible())
      {
        if (colSettings.defVisible) // Indicates that it was visible by default
          app.prefs.remove(prefKey);
        else
          app.prefs.putBoolean(prefKey, true); // Since it was hidden by default, explicit true value needs to be saved
      }
      else
        app.prefs.putBoolean(prefKey, false);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Widths are saved to preferences as scaled

  public static <RowType, ColType extends TableColumnBase<RowType, ?>> void loadColWidthsForTable(Control tv, List<ColType> columns, String prefID)
  {
    boolean unscale = getIsScaled(tv) == false;

    for (int numCols = columns.size(), ndx = 1; ndx <= numCols; ndx++)
    {
      ColType col = columns.get(ndx - 1);
      double width = app.prefs.getDouble(prefID + "ColWidth" + ndx, -1.0);

      col.setUserData(new ColumnSettings(ndx, width, col.isVisible())); // ColumnSettings always has scaled value

      if (unscale) width = width / displayScale;

      if ((width > 0.0) && col.isResizable())
        col.setPrefWidth(width);

      col.setVisible(app.prefs.getBoolean(prefID + "ColVisible" + ndx, col.isVisible()));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <RowType> void registerTable(TableView<RowType> tv, String prefID, DialogBase dialog)
  {
    if (prefID.isEmpty()) return;

    registry.put(prefID, tv);

    if (dialog != null)
      dialogs.put(prefID, dialog);

    loadColWidthsForTable(tv, tv.getColumns(), prefID);
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

  public static <RowType> void preventMovingColumns(TableView<RowType> tv, Collection<TableColumn<RowType, ?>> columns)
  {
    // This handsome block of code is the only way within JavaFX to prevent the user from moving columns around

    tv.getColumns().addListener((Change<? extends TableColumn<RowType, ?>> change) ->
    {
      change.next();

      if (change.wasReplaced())
      {
        tv.getColumns().clear();
        tv.getColumns().addAll(columns);
      }
    });
  }

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
    return addColAltPopulator(objType, ctrlType, (ctrlType == ctEditableUnlimitedDropDown) || (ctrlType == ctNoneditableDropDown) || (ctrlType == ctEditableLimitedDropDown) ?
      new StandardPopulator(objType)
    :
      Populator.emptyPopulator(cvtRecord));
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

  public HyperTableColumn addActionColWithButtonHandler(HyperCtrlType ctrlType, int targetCol, CellClickHandler handler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol, handler, null)); }

  public HyperTableColumn addCustomActionCol(int targetCol, String btnCaption, CellClickHandler handler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctCustomBtn, null, targetCol, handler, btnCaption)); }

  public HyperTableColumn addLabelCol(RecordType objType) {
    return addLabelColWithAlignment(objType, null); }

  public HyperTableColumn addLabelCol(RecordType objType, CellSortMethod sortMethod) {
    return addLabelCol(objType).setSortMethod(sortMethod); }

  public HyperTableColumn addLabelColWithAlignment(RecordType objType, Pos alignment) {
    return addColAltPopulator(objType, ctNone, Populator.emptyPopulator(cvtRecord)).setAlignment(alignment); }

  public HyperTableColumn addLabelEditCol(CellClickHandler handler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctLabelEdit, null, -1, handler, null)); }

  public HyperTableColumn addIconCol() {
    return addCol(new HyperTableColumn(this, hdtNone, ctIcon, null, -1)); }

  public HyperTableColumn addCheckboxCol() {
    return addCol(new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1)); }

  public HyperTableColumn addCheckboxColWithUpdateHandler(CellUpdateHandler updateHandler) {
    return addCol(new HyperTableColumn(this, hdtNone, ctCheckbox, null, -1, updateHandler)); }

  public HyperTableColumn addReadOnlyColWithCustomGraphic(RecordType objType, Function<HyperTableRow, Node> graphicProvider) {
    return addCol(new HyperTableColumn(this, objType, ctNone, null, -1, graphicProvider)); }

  public HyperTableColumn addGoNewCol(RecordType recordType, int targetCol) {
    return addActionCol(ctGoNewBtn, targetCol).setTooltip(ButtonAction.baNew, "Add new " + getTypeName(recordType) + " record"); }

  public HyperTableColumn addClickToEditCol(RecordType objType, Tooltip headerTooltip, CellClickHandler handler) {
    return addCol(new HyperTableColumn(this, objType, ctLabelClickToEdit, null, -1, handler, null).setHeaderTooltip(headerTooltip)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addActionCol(HyperCtrlType ctrlType, int targetCol)
  {
    HyperTableColumn col = addCol(new HyperTableColumn(this, hdtNone, ctrlType, null, targetCol));

    if ((ctrlType == ctGoNewBtn) || (ctrlType == ctGoBtn))
      col.setTooltip(ButtonAction.baGo, row -> nullSwitch(row.getRecord(targetCol), null, (HDT_Record record) -> "Go to this " + getTypeName(record.getType()) + " record"));

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addAuthorEditCol(Supplier<HDT_Work> workSupplier, CellUpdateHandler updateHandler)
  {
    return addCol(new HyperTableColumn(this, hdtPerson, ctEditableLimitedDropDown, new StandardPopulator(hdtPerson), -1, updateHandler))
      .setWorkSupplier(workSupplier);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditColWithUpdateHandler(RecordType objType, boolean canEditIfEmpty, CellUpdateHandler updateHandler)
  {
    return addTextEditColWithUpdateHandler(objType, canEditIfEmpty, null, updateHandler);
  }

  public HyperTableColumn addTextEditColWithUpdateHandler(RecordType objType, boolean canEditIfEmpty, CellSortMethod sortMethod, CellUpdateHandler updateHandler)
  {
    return addCol(new HyperTableColumn(this, objType, ctEdit, null, -1, updateHandler))
      .setCanEditIfEmpty(canEditIfEmpty)
      .setSortMethod(sortMethod);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableColumn addTextEditCol(RecordType objType, boolean canEditIfEmpty)
  {
    return addTextEditCol(objType, canEditIfEmpty, null);
  }

  public HyperTableColumn addTextEditCol(RecordType objType, boolean canEditIfEmpty, CellSortMethod sortMethod)
  {
    return addCol(new HyperTableColumn(this, objType, ctEdit, null, -1))
      .setCanEditIfEmpty(canEditIfEmpty)
      .setSortMethod(sortMethod);
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
      newRow(false);
  }

  public void clear()
  {
    clearKeepSortOrder();
    tv.getSortOrder().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void sortAscending(int colNdx)
  {
    tv.getSortOrder().setAll(List.of(tv.getColumns().get(colNdx)));
    tv.getColumns().get(colNdx).setSortType(TableColumn.SortType.ASCENDING);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean settingDefaultValue = false;

  public HyperTableRow newDataRow() { return newRow(true); }

  HyperTableRow newRow(boolean dataRow)
  {
    if (settingDefaultValue) return null;

    HyperTableRow row = new HyperTableRow(cols.size(), this);

    if (dataRow && canAddRows)
      rows.add(dataRowCount(), row);
    else
    {
      rows.add(row);
      settingDefaultValue = true;
      colNdxToDefaultValue.forEach(row::setCellValue);
      settingDefaultValue = false;
    }

    return row;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow newShowMoreRow()
  {
    HyperTableRow row = newDataRow();

    for (int colNdx = 0; colNdx < cols.size(); colNdx++)
      row.setCellValue(colNdx, new GenericNonRecordHTC("", cols.get(colNdx).getCtrlType() == ctIncremental ? hdtAuxiliary : hdtNone, true));

    return row;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setDataRows(Collection<HyperTableRow> newRows)
  {
    showMoreRow = null;
    rows.setAll(newRows);

    if (canAddRows)
      newRow(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDataRows(Collection<HyperTableRow> newRows)
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
    nullSwitch(findFirst(cols.get(colNdx).getPopulator().populate(row, false), cell -> HyperTableCell.getCellType(cell) == newType), cell -> row.setCellValue(colNdx, cell));
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

    ui.treeSelector.reset(ui.activeRecord(), true);

    if (colNdx > 0)
    {
      pop = cols.get(colNdx - 1).getPopulator();

      if ((pop != null) && (pop.getValueType(row) == cvtRecordType))
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
    RecordType startType = row.getRecordType(colNdx);
    if (startID > 0)
      ui.treeSelector.setTarget(db.records(startType).getByID(startID));
    else
    {
      if (pop.getRecordType(row) == hdtWorkLabel)
      {
        List<? extends HyperTableCell> choices = pop.populate(row, false);
        HDT_Record record = null;

        if (collEmpty(choices) == false)
          record = HyperTableCell.getRecord(choices.getFirst());

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

  public void addRemoveMenuItem(Predicate<HyperTableRow> condRowHandler) { addRemoveMenuItem(condRowHandler, null); }

  public void addRemoveMenuItem(Predicate<HyperTableRow> condRowHandler, Consumer<HyperTableRow> completeHandler)
  {
    addContextMenuItem("Remove this row", condRowHandler, row ->
    {
      rows.remove(row);
      doExternalRefresh();
      if (completeHandler != null) completeHandler.accept(row);
    });
  }

  public void addRemoveMenuItem() { addRemoveMenuItem((Consumer<HyperTableRow>)null); }

  public void addRemoveMenuItem(Consumer<HyperTableRow> completeHandler)
  {
    addContextMenuItem("Remove this row", cols.get(mainCol).getObjType().getRecordClass(), record -> canAddRows, record ->
    {
      HyperTableRow row = tv.getSelectionModel().getSelectedItem();
      rows.remove(row);
      doExternalRefresh();
      if (completeHandler != null) completeHandler.accept(row);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addChangeOrderMenuItem() { addChangeOrderMenuItem(true, null); }

  public void addChangeOrderMenuItem(boolean onlyIfCanAddRows, Runnable completeHandler)
  {
    addContextMenuItem("Change order",
      row ->
      {
        if ((row.getRecord() == null) && row.getText(mainCol).isEmpty()) return false;
        if (dataRowCount() < 2) return false;
        return (onlyIfCanAddRows == false) || canAddRows;
      },

      row -> triggerChangeOrder(onlyIfCanAddRows, completeHandler));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void triggerChangeOrder(boolean onlyIfCanAddRows, Runnable completeHandler)
  {
    if (onlyIfCanAddRows && (canAddRows == false)) return;

    if (ui.windows.getOutermostModality() == Modality.NONE)
      if (ui.cantSaveRecord()) return;

    boolean couldAddRows = canAddRows;

    canAddRows = false; // Blank row at the bottom is removed while the dialog is open

    if (ui.windows.getOutermostModality() == Modality.NONE)
      ui.update();
    else if (couldAddRows)
      rows.removeLast();

    new ObjectOrderDlgCtrlr(this, rows).showModal();

    if (completeHandler != null)
      completeHandler.run();
    else
    {
      if (ui.windows.getOutermostModality() == Modality.NONE)
        ui.cantSaveRecord();
    }

    canAddRows = couldAddRows;

    if (ui.windows.getOutermostModality() == Modality.NONE)
      ui.update();
    else if (canAddRows)
      newRow(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This assumes that the table is not currently being edited.
   * @param row the row
   * @param colNdx the column index
   */
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

  /**
   * Saves data for 2 columns: An object column and a nested item column.
   * Nothing if saved if saving the objects will result in a cycle; in that case,
   * it shows an error popup and returns false.
   *
   * @param subj The subject record to save to
   * @param relType The relation
   * @param tag The nested item tag
   * @param objColNdx The object column index
   * @param nestedColNdx The nested item column index
   * @return True if they saved successfully
   */
  public boolean saveObjectsAndSingleNestedItem(HDT_Record subj, RelationType relType, Tag tag, int objColNdx, int nestedColNdx)
  {
    Map<Integer, Tag> colNdxToTag = new HashMap<>();
    colNdxToTag.put(nestedColNdx, tag);

    List<ObjectGroup> newGroups = getObjectGroupList(subj, relType, objColNdx, colNdxToTag);

    return subj.updateObjectGroups(relType, newGroups, colNdxToTag.values());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<ObjectGroup> getObjectGroupList(HDT_Record subj, RelationType relType, int objColNdx, Map<Integer, Tag> colNdxToTag)
  {
    List<ObjectGroup> list = new ArrayList<>();
    RecordType objType = db.getObjType(relType);

    db.getNestedTags(relType).stream().filter(Predicate.not(colNdxToTag::containsValue)).forEach(tag -> colNdxToTag.put(-1, tag));

    Set<HDT_Record> existingObjects = new HashSet<>();

    rows.stream().filter(row -> row.getRecordType(objColNdx) == objType).forEach(row ->
    {
      int id = row.getID(objColNdx);
      if ((id < 1) && row.getText(objColNdx).isEmpty())
        return;

      HDT_Record obj;
      ObjectGroup group;

      if (id < 1)
      {
        obj = null;
        group = new ObjectGroup(row.getText(objColNdx));
      }
      else
      {
        obj = db.records(objType).getByID(id);

        if (existingObjects.add(obj) == false) return;  // Filter out duplicate object records

        group = new ObjectGroup(obj);
      }

      colNdxToTag.forEach((colNdx, tag) ->
      {
        HyperDataCategory hdc = db.getNestedSchema(relType, tag).category();
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
              if (tag == tagInFileName)
              {
                RecordAuthor author = ((HDT_Work) subj).getAuthors().getAuthor(new PersonName(group.getPrimaryStr()));
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
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_Record> List<HDT_T> saveToList(int colNdx, RecordType objType)
  {
    return rows.stream().filter(row -> row.getRecordType(colNdx) == objType)
                        .map(row -> row.getID(colNdx))
                        .filter(id -> id > 0)
                        .map(id -> (HDT_T) db.records(objType).getByID(id))
                        .filter(Objects::nonNull)
                        .collect(Collectors.toCollection(ArrayList::new));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void scrollToSelection() { scrollToSelection(tv, false); }

  public static <RowType> void scrollToSelection(TableView<RowType> tv, boolean delay)
  {
    if (delay)
      Platform.runLater(() -> scrollToNdxInTable(tv, tv.getSelectionModel().getSelectedIndex()));
    else
      scrollToNdxInTable(tv, tv.getSelectionModel().getSelectedIndex());
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

  /**
   * This can be called immediately after the constructor if the TableView is using a constrained resize policy
   * and any of the columns are not intended to be resizable. Sometimes in that situation the table won't display correctly due to bugs
   * in JavaFX; this function prevents that.
   */
  public void initConstrainedResize()
  {
    boolean colZeroVisible = tv.getColumns().getFirst().isVisible();

    Platform.runLater(() ->  // This has to be done because of bugginess of constrained resize policies. The column widths may not be correctly initialized until you force it to be redrawn.
    {
      tv.getColumns().getFirst().setVisible(!colZeroVisible);

      Platform.runLater(() -> tv.getColumns().getFirst().setVisible(colZeroVisible));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
