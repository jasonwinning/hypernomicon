/*
 * Copyright 2015-2024 Jason Winning
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

import java.util.Collections;
import java.util.Objects;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.populators.*;

//---------------------------------------------------------------------------

public class HyperTableRow extends AbstractRow<HDT_Record, HyperTableRow>
{
  private final ObservableList<HyperTableCell> cells;
  private final HyperTable table;

//---------------------------------------------------------------------------

  public HyperTableCell getCell(int ndx)      { return cells.get(ndx); }
  public HyperTableCell getCell()             { return cells.get(table.getMainColNdx()); }
  public int getID(int ndx)                   { return cells.size() > ndx ? HyperTableCell.getCellID  (cells.get(ndx)) : -1; }
  public String getText(int ndx)              { return cells.size() > ndx ? HyperTableCell.getCellText(cells.get(ndx)) : ""; }
  public RecordType getRecordType(int ndx)    { return cells.size() > ndx ? HyperTableCell.getCellType(cells.get(ndx)) : hdtNone; }
  public boolean getCheckboxValue(int colNdx) { return getID(colNdx) == 1; }

  @Override public RecordType getRecordType() { return getRecordType(table.getMainColNdx()); }
  @Override public int getRecordID()          { return getID(table.getMainColNdx()); }

  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return HyperTableCell.getRecord(getCell()); }
  public <HDT_T extends HDT_Record> HDT_T getRecord(int ndx)    { return HyperTableCell.getRecord(getCell(ndx)); }

  public <Pop extends Populator> Pop getPopulator(int colNdx)   { return table.getPopulator(colNdx); }

//---------------------------------------------------------------------------

  HyperTableRow(int colCount, HyperTable table)
  {
    this(FXCollections.observableArrayList(Collections.nCopies(colCount, HyperTableCell.blankCell)), table);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableRow(ObservableList<HyperTableCell> cells, HyperTable table)
  {
    this.cells = cells;
    this.table = table;

    if (cells == null) return;  // this occurs in the case of Populator.dummyRow

    for (int colNdx = 0; colNdx < cells.size(); colNdx++)
    {
      Populator populator = table.getPopulator(colNdx);
      if (nullSwitch(populator, null, Populator::getValueType) == cvtVaries)
        ((VariablePopulator)populator).initRow(this);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override <HDT_T extends HDT_Record> HDT_T getRecordByType(RecordType type)
  {
    HDT_T record = super.getRecordByType(type);

    return record != null ? record : findFirst(cells, cell -> HyperTableCell.getCellType(cell) == type, cell -> HyperTableCell.getRecord(cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setCheckboxValue(int colNdx, boolean boolVal) {
    setCellValue(colNdx, HyperTableCell.fromBoolean(boolVal)); }

  public boolean setCellValue(int colNdx, HDT_Record record, String text) {
    return setCellValue(colNdx, new HyperTableCell(record, text)); }

  public boolean setCellValue(int colNdx, String text, RecordType type) {
    return setCellValue(colNdx, new HyperTableCell(text, type)); }

  public boolean setCellValue(int colNdx, int id, String text, RecordType type) {
    return setCellValue(colNdx, new HyperTableCell(id, text, type)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean setCellValue(int colNdx, HyperTableCell newCell)  // return true if changed
  {
    HyperTableCell cell = cells.get(colNdx);
    HyperTableColumn col = table.getColumn(colNdx);
    boolean isNotCheckBox = col.getCtrlType() != ctCheckbox;

    if (Objects.equals(cell, newCell))
    {
      if (isNotCheckBox) table.refresh();
      return false;
    }

    Populator populator = col.getPopulator();

    if ((populator != null) &&
        ((col.getCtrlType() == ctDropDownList) ||
         (((populator.getValueType() == cvtVaries) && ((VariablePopulator)populator).getRestricted(this)))))
    {
      HyperTableCell matchedCell = populator.match(this, newCell);

      if (HyperTableCell.isEmpty(matchedCell) == false)
        newCell = matchedCell;
      else if (HyperTableCell.getCellText(newCell).length() > 0)
      {
        if (isNotCheckBox) table.refresh();
        return false;
      }
    }

    if (newCell != null)
      newCell = newCell.clone();  // Cells in a TableView always need to be unique so we can determine whether a cell is in the bottom row and if so, make sure it sorts to the bottom

    cells.set(colNdx, newCell);

    if (table.getCanAddRows() && (table.getTV().getItems().get(table.getTV().getItems().size() - 1) == this))
      table.newRow(false);

    if ((table.disableRefreshAfterCellUpdate == false) && isNotCheckBox)
      table.refresh();

    if (col.updateHandler != null)
    {
      boolean isNotLastColumn = table.getColumns().size() > (colNdx + 1);

      col.updateHandler.handle(this, newCell, colNdx + 1, isNotLastColumn ? table.getPopulator(colNdx + 1) : null);

      if (isNotLastColumn && isNotCheckBox)
        table.refresh();
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void changeIDs(RecordType changedType, int oldID, int newID)
  {
    for (int colCount = cells.size(), colNdx = 0; colNdx < colCount; colNdx++)
    {
      HyperTableCell cell = cells.get(colNdx);
      if ((HyperTableCell.getCellID(cell) == oldID) && (HyperTableCell.getCellType(cell) == changedType))
        cells.set(colNdx, new HyperTableCell(newID, cell.text, changedType));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
