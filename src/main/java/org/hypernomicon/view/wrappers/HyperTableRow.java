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

import java.util.HashMap;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.HyperTable.CellUpdateHandler;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;

//---------------------------------------------------------------------------

public class HyperTableRow
{
  private ObservableList<HyperTableCell> cells;
  private HyperTable table;
  private HashMap<Integer, Populator> populators = new HashMap<Integer, Populator>();

//---------------------------------------------------------------------------
  
  public int getCount()                       { return cells.size(); }
  public HyperTableCell getCell(int ndx)      { return cells.get(ndx); }  
  public int getID(int ndx)                   { return cells.size() > ndx ? HyperTableCell.getCellID(cells.get(ndx)) : -1; }
  public String getText(int ndx)              { return cells.size() > ndx ? HyperTableCell.getCellText(cells.get(ndx)) : ""; }
  public HDT_RecordType getType(int ndx)      { return cells.size() > ndx ? HyperTableCell.getCellType(cells.get(ndx)) : hdtNone; }
  public boolean getCheckboxValue(int colNdx) { return getID(colNdx) == HyperTableCell.trueCell.getID(); }

  public <HDT_T extends HDT_Base> HDT_T getRecord()        { return HyperTableCell.getRecord(cells.get(table.getMainColNdx())); }
  public <HDT_T extends HDT_Base> HDT_T getRecord(int ndx) { return HyperTableCell.getRecord(cells.get(ndx)); }
  
//---------------------------------------------------------------------------

  public HyperTableRow(int colCount, HyperTable table)
  {
    this.table = table;
    cells = FXCollections.observableArrayList();
    
    for (int colNdx = 0; colNdx < colCount; colNdx++)
    {
      cells.add(new HyperTableCell(-1, "", HDT_RecordType.hdtNone));
      populators.put(colNdx, table.getPopulator(colNdx));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow(ObservableList<HyperTableCell> cells, HyperTable table)
  {
    this.cells = cells;
    this.table = table;
    
    if (cells == null) return;  // this occurs in the case of Populator.dummyRow
    
    for (int colNdx = 0; colNdx < cells.size(); colNdx++)
      populators.put(colNdx, table.getPopulator(colNdx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public <HDT_T extends HDT_Base> HDT_T getRecordByType(HDT_RecordType type)
  {
    if (type == hdtNone)
      return getRecord();
            
    for (HyperTableCell cell : cells)
      if (HyperTableCell.getCellType(cell) == type)
        return HyperTableCell.getRecord(cell);
    
    return null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void setCheckboxValue(int colNdx, boolean boolVal) {
    setCellValue(colNdx, HyperTableCell.fromBoolean(boolVal)); } 
  
  public boolean setCellValue(int colNdx, HDT_Base record, String text) {
    return setCellValue(colNdx, new HyperTableCell(record, text)); }

  public boolean setCellValue(int colNdx, int id, String text, HDT_RecordType type) {
    return setCellValue(colNdx, new HyperTableCell(id, text, type)); }

  public boolean setCellValue(int colNdx, HDT_Base record, String text, HyperCellSortMethod newSortMethod) {
    return setCellValue(colNdx, new HyperTableCell(record, text, newSortMethod)); }

  public boolean setCellValue(int colNdx, int id, String text, HDT_RecordType type, HyperCellSortMethod newSortMethod) {
    return setCellValue(colNdx, new HyperTableCell(id, text, type, newSortMethod)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean setCellValue(int colNdx, HyperTableCell newCell)  // return true if changed
  {
    HyperTableCell cell = cells.get(colNdx);
    VariablePopulator vp;
    boolean restricted;
    HyperTableColumn col = table.getColumn(colNdx);
    
    if ((cell != null) && (newCell != null))
      if (cell.equals(newCell))
      {
        table.refreshCol(colNdx);
        return false;
      }

    if (col.getObjType() == hdtNone)
      newCell = HyperTableCell.simpleSortValue(newCell);
    
    if (col.getPopulator() != null)
    {
      if (col.getPopulator().getValueType() == cvtVaries)
      {
        vp = (VariablePopulator)col.getPopulator();
        restricted = vp.getRestricted(this);
      }
      else
        restricted = (col.getCtrlType() == ctDropDownList);
      
      if (restricted)
      {
        Populator populator = col.getPopulator();
        HyperTableCell matchedCell = populator.match(this, newCell);
        
        if (matchedCell != null)
          newCell = matchedCell;
        else if (HyperTableCell.getCellText(newCell).length() > 0)
        {
          table.refreshCol(colNdx);
          return false;
        }
      }
    }
       
    cells.set(colNdx, newCell);
    if (table.getCanAddRows())
    {
      if (table.getTV().getItems().get(table.getTV().getItems().size() - 1) == this)      
        table.newRow(false);
    }

    if (table.disableRefreshAfterCellUpdate == false)
      table.refreshCol(colNdx);                 // Necessary workaround; tableview does not automatically refresh
                                                // when you change values in the cell objects, just the row objects
      
    CellUpdateHandler handler = col.updateHandler;
    final HyperTableCell finalCell = newCell.clone();
    
    if (handler != null)
    {
      Populator nextPop = null;
      if (table.getColumns().size() > (colNdx + 1))
        nextPop = populators.get(colNdx + 1);
      
      handler.handle(this, finalCell, colNdx + 1, nextPop);
      
      if (table.getColumns().size() > (colNdx + 1))
        table.refreshCol(colNdx + 1);           // Necessary workaround; tableview does not automatically refresh
    }                                           // when you change values in the cell objects, just the row objects
     
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}
