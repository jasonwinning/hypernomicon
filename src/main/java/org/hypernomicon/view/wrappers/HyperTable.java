/*
 * Copyright 2015-2018 Jason Winning
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
import static org.hypernomicon.model.HyperDB.Tag.tagEditor;
import static org.hypernomicon.model.HyperDB.Tag.tagInFileName;
import static org.hypernomicon.model.HyperDB.Tag.tagTranslator;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.rtAuthorOfWork;

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
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.TreeWrapper.TreeTargetType;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.function.Predicate;

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
  public TableView<HyperTableRow> tv;
  private List<HyperTableColumn> cols;
  private ObservableList<HyperTableRow> rows;
  private SortedList<HyperTableRow> sortedRows;
  private FilteredList<HyperTableRow> filteredRows;
  final ArrayList<TableColumn<HyperTableRow, HyperTableCell>> tableCols = new ArrayList<TableColumn<HyperTableRow, HyperTableCell>>();
  private List<HyperMenuItem> contextMenuItems;
  RecordListView.RecordHandler dblClickHandler = null;
  Runnable onShowMore = null;
  public boolean disableRefreshAfterCellUpdate = false,
                 autoCommitListSelections = false;
  ComboBoxCell cellBeingEdited = null;
  
  private double rowHeight = 0;
  private HashMap<Orientation, ScrollBar> sbMap = new HashMap<>();
  
  private static final HashMap<String, TableView<?>> registry = new HashMap<>();
  private static final HashMap<String, HyperDialog> dialogs = new HashMap<>();

//---------------------------------------------------------------------------  

  public List<HyperTableColumn> getColumns()                       { return Collections.unmodifiableList(cols); }
  public HyperTableColumn getColumn(int colNdx)                    { return cols.get(colNdx); }
  public Populator getPopulator(int colNdx)                        { return cols.get(colNdx).getPopulator(); }
  public void clearFilter()                                        { filteredRows.setPredicate(row -> true); }
  public void setFilter(Predicate<HyperTableRow> filter)           { filteredRows.setPredicate(filter); }
  public int getID(int colNdx, int rowNdx)                         { return rows.size() <= rowNdx ? -1 : rows.get(rowNdx).getID(colNdx); }
  public HDT_RecordType getType(int colNdx, int rowNdx)            { return rows.size() <= rowNdx ? hdtNone : rows.get(rowNdx).getType(colNdx); }
  public String getText(int colNdx, int rowNdx)                    { return rows.size() <= rowNdx ? "" : rows.get(rowNdx).getText(colNdx); }
  public HDT_RecordType getTypeByCol(int colNdx)                   { return cols.get(colNdx).getObjType(); }
  public List<HyperTableCell> getSelByCol(int colNdx)              { return cols.get(colNdx).getSelectedItems(); }
  public boolean getCanAddRows()                                   { return canAddRows; }
  public void setCanAddRows(boolean value)                         { canAddRows = value; tv.setEditable(value); }
  public void setDblClickHandler(RecordHandler dblClickHandler)    { this.dblClickHandler = dblClickHandler; }
  public void setOnShowMore(Runnable onShowMore)                   { this.onShowMore = onShowMore; }
  public int getMainColNdx()                                       { return mainCol; }
  public void setTooltip(int colNdx, ButtonAction ba, String text) { cols.get(colNdx).setTooltip(ba, text); }
  public ComboBoxCell getCellBeingEdited()                         { return cellBeingEdited; }
  public HyperTableRow getRowByRowNdx(int rowNdx)                  { return rows.get(rowNdx); }
  public void removeRow(HyperTableRow row)                         { rows.remove(row); }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public static void saveColWidthsToPrefs()
  {
    registry.entrySet().forEach(entry -> 
    {
      String prefID = entry.getKey();      
      HyperDialog dialog = dialogs.get(prefID);
      
      if (dialog != null)
        if (dialog.shownAlready() == false)
          return;
      
      saveColWidthsForTable(entry.getValue(), entry.getKey(), true); 
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

  public HDT_Base selectedRecord()                                   
  { 
    HyperTableRow row = tv.getSelectionModel().getSelectedItem();    
    return row == null ? null : row.getRecord(); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectRow(int ndx)                                     
  { 
    tv.getSelectionModel().select(rows.get(ndx));
    scrollToSelection();
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
    boolean visible, noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();
    
    for (HyperMenuItem hItem : contextMenuItems)
    {
      MenuItem newItem = new MenuItem(hItem.caption);
     
      rowMenu.getItems().add(newItem);
      
      newItem.setOnAction(event ->
      {
        HDT_Base record;
        
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

      visible = false;
      
      HDT_Base record;
      
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
      if (visible) noneVisible = false;
    }
    
    if (noneVisible) return null;
    return rowMenu;
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
  
  public static class HyperMenuItem
  {
    public HDT_RecordType recordType = hdtNone;
    public RecordListView.RecordHandler recordHandler;
    public RecordListView.CondRecordHandler condRecordHandler;    
    public RowHandler rowHandler;
    public CondRowHandler condRowHandler;
 
    public String caption;
    public boolean okayIfBlank = false;
    
    public HyperMenuItem(String caption) { this.caption = caption; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperMenuItem addCondRowBasedContextMenuItem(String caption, CondRowHandler condRowHandler, RowHandler rowHandler)
  {
    HyperMenuItem mnu;
    
    mnu = new HyperMenuItem(caption);
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

  public HyperMenuItem addCondContextMenuItemOkayIfBlank(String caption, RecordListView.CondRecordHandler condRecordHandler, RecordListView.RecordHandler recordHandler)
  {
    HyperMenuItem mnu;
    
    mnu = new HyperMenuItem(caption);
    mnu.recordType = hdtNone;
    mnu.condRecordHandler = condRecordHandler;
    mnu.recordHandler = recordHandler;
    mnu.okayIfBlank = true;
    
    contextMenuItems.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperMenuItem addContextMenuItem(HDT_RecordType recordType, String caption, RecordListView.RecordHandler recordHandler)
  {
    return addCondContextMenuItem(recordType, caption, record -> true, recordHandler);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperMenuItem addCondContextMenuItem(HDT_RecordType recordType, String caption, RecordListView.CondRecordHandler condRecordHandler, RecordListView.RecordHandler recordHandler)
  {
    HyperMenuItem mnu;
       
    mnu = new HyperMenuItem(caption);
    mnu.recordType = recordType;
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
    
    cols.forEach(col -> col.clear());
    
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
  
  public void setCheckboxValue(int colNdx, HyperTableRow row, boolean boolVal)
  {
    HyperTableCell cell = HyperTableCell.fromBoolean(boolVal);
    row.updateCell(colNdx, cell);
  } 
  
  public void setCheckboxValue(int colNdx, int rowNdx, boolean boolVal)
  {
    HyperTableCell cell = HyperTableCell.fromBoolean(boolVal);
    setDataItem(colNdx, rowNdx, cell.getID(), cell.getText(), cell.getType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean getCheckboxValue(int colNdx, int rowNdx)
  {
    return getID(colNdx, rowNdx) == HyperTableCell.trueCell.getID();
  }
  
  public boolean getCheckboxValue(int colNdx, HyperTableRow row)
  {
    return row.getID(colNdx) == HyperTableCell.trueCell.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void setDataItem(int colNdx, HyperTableRow row, int newID, String text, HDT_RecordType newType)
  {
    setDataItem(colNdx, row, newID, text, newType, HyperCellSortMethod.hsmStandard);
  }
  
  public void setDataItem(int colNdx, HyperTableRow row, int newID, String text, HDT_RecordType newType, HyperCellSortMethod newSortMethod)
  {
    row.updateCell(colNdx, new HyperTableCell(newID, text, newType, newSortMethod));
  }
  
  public void setDataItem(int colNdx, int rowNdx, int newID, String text, HDT_RecordType newType)
  {
    setDataItem(colNdx, rowNdx, newID, text, newType, HyperCellSortMethod.hsmStandard);
  }

  public void setDataItem(int colNdx, int rowNdx, int newID, String text, HDT_RecordType newType, HyperCellSortMethod newSortMethod)
  {
    while (rows.size() <= rowNdx)
      rows.add(new HyperTableRow(cols.size(), this));

    rows.get(rowNdx).updateCell(colNdx, new HyperTableCell(newID, text, newType, newSortMethod));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  public void setDataRows(List<HyperTableRow> newRows)
  {
    showMoreRow = null;
    rows.setAll(newRows);
  }

  public void addDataRows(List<HyperTableRow> newRows)
  {
    rows.addAll(newRows);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectID(int colNdx, HyperTableRow row, int newID)
  {
    int ndx;
    HyperTableCell cell;
    
    if (row == null)
    {
      if (rows.size() == 0)
        rows.add(new HyperTableRow(cols.size(), this));
        
      row = rows.get(0);
    }
    
    List<HyperTableCell> list = cols.get(colNdx).getPopulator().populate(row, false);
    
    for (ndx = 0; ndx < list.size(); ndx++)
    {
      cell = list.get(ndx);
      if (HyperTableCell.getCellID(cell) == newID)
      {
        row.updateCell(colNdx, cell);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectType(int colNdx, HyperTableRow row, HDT_RecordType newType)
  {
    int ndx;
    HyperTableCell cell;
    
    if (row == null)
    {
      if (rows.size() == 0)
        rows.add(new HyperTableRow(cols.size(), this));
        
      row = rows.get(0);
    }
        
    List<HyperTableCell> list = cols.get(colNdx).getPopulator().populate(row, false);
    
    for (ndx = 0; ndx < list.size(); ndx++)
    {
      cell = list.get(ndx);
      if (HyperTableCell.getCellType(cell) == newType)
      {
        row.updateCell(colNdx, cell);
        return;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getDataRowCount()
  {
    if (mainCol >= 0)
    {
      int cnt = 0;
      
      for (HyperTableRow row : rows)
        if ((row.getID(mainCol) > 0) || (row.getText(mainCol).length() > 0))
          cnt++;
      
      return cnt;
    }
    
    if (canAddRows)
      return rows.size() - 1;
    
    return rows.size();
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

  public boolean containsRecord(int id, HDT_RecordType type)
  {   
    if (mainCol < 0) return false;
    
    for (HyperTableRow row : rows)
    {
      if (row.getID(mainCol) == id)
        if (row.getType(mainCol) == type)
          return true;
    }
    
    return false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public HyperTableRow getRowByRecord(HDT_Base record)
  {
    for (HyperTableRow row : rows)
    {
      if (row.getRecord() == record)
        return row;
    }
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
 
  public void browseClick(HyperTableRow row, int colNdx)
  {
    HDT_RecordType startType;
    Populator pop = null;
    RecordTypePopulator rtp = null;
    int startID;
    
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

    startID = row.getID(colNdx);
    startType = row.getType(colNdx);
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

        if (record == null)
          startID = 1;
        else
          startID = record.getID();
        
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
    addCondContextMenuItem(cols.get(mainCol).getObjType(), "Remove this row", record -> canAddRows, record -> 
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

  public void addBlankRow()
  {
    rows.add(new HyperTableRow(tv.getColumns().size(), this));
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
    HDT_Base obj;
    
    db.getNestedTags(relType).forEach(tag ->
    {
      if (colNdxToTag.containsValue(tag) == false)
        colNdxToTag.put(-1, tag);
    });
    
    for (HyperTableRow row : rows)
    {
      if (row.getType(primaryColNdx) == objType)
      {
        int id = row.getID(primaryColNdx);
        if ((id > 0) || row.getText(primaryColNdx).length() > 0)
        {
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
          
          for (Entry<Integer, Tag> entry : colNdxToTag.entrySet())
          {
            Tag tag = entry.getValue();
            HDI_Schema schema = db.getNestedSchema(relType, tag);
            NestedValue val = new NestedValue(schema.getCategory());
            int colNdx = entry.getKey();
            
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
                    HDT_Work work = (HDT_Work)subj;
                    Author author = work.getAuthors().getAuthor(new PersonName(group.getPrimaryStr()));
                    if (author != null)
                      val.ternary = author.getInFileName();
                  }
                }
              }
            }
            else switch (schema.getCategory())
            {
              case hdcString        : val.str = row.getText(colNdx); break;
              case hdcBoolean       : val.bool = getCheckboxValue(colNdx, row); break;
              case hdcTernary       : val.ternary = getCheckboxValue(colNdx, row) ? Ternary.True : Ternary.False; break;
              case hdcNestedPointer : val.target = row.getRecord(colNdx); break;
              default               : break;
            }
            
            group.addNestedEntry(tag, val);
          }
          
          list.add(group);
        }
      }
    }
    
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
  
  private ScrollBar getScrollBar(Orientation o)
  {
    ScrollBar sb = sbMap.get(o);
    if (sb != null) return sb;
    
    for (Node n: tv.lookupAll(".scroll-bar")) 
    {
      if (n instanceof ScrollBar)
      {
        sb = (ScrollBar) n;        
        if (sb.getOrientation() == o)
        {
          sbMap.put(o, sb);
          return sb;
        }
      }
    }
    
    return null;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void scrollToSelection()
  {
    ScrollBar sb = getScrollBar(Orientation.VERTICAL);
    if (sb == null) return;
    
    int ndx = tv.getSelectionModel().getSelectedIndex();
              
    double rHeight = getRowHeight();
    
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

  private double getRowHeight()
  {
    if (rowHeight != 0)
      return rowHeight;
    
    for (Node rowNode : tv.lookupAll( ".indexed-cell"))
    {        
      if (rowNode instanceof TableRow) 
      {            
        rowHeight = ((Region) rowNode).getHeight();
        return rowHeight;
      }
    }
    
    return rowHeight;
  }
    
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
}
