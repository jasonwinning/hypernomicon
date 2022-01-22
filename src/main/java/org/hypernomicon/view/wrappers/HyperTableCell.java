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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.CellSortMethod.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------

public final class HyperTableCell implements Comparable<HyperTableCell>, Cloneable
{
  public static enum CellSortMethod
  {
    smStandard, smTextSimple, smNumeric, smLast, smWork
  }

  public static final HyperTableCell trueCheckboxCell  = new HyperTableCell(1 , "", hdtNone),
                                     falseCheckboxCell = new HyperTableCell(0 , "", hdtNone),
                                     blankCell = new HyperTableCell("", hdtNone);

  private int id;
  private String text;
  private RecordType type;
  private CellSortMethod sortMethod = smStandard;

  public int getID()          { return id; }
  public String getText()     { return text; }
  public RecordType getType() { return type; }

  public <HDT_T extends HDT_Record> HDT_T getRecord()            { return HyperTableCell.getRecord(this); }

  static HyperTableCell checkboxCellFromBoolean(boolean boolVal) { return boolVal ? trueCheckboxCell : falseCheckboxCell; }
  public static int getCellID(HyperTableCell cell)               { return cell == null ? -1 : cell.id; }
  public static String getCellText(HyperTableCell cell)          { return cell == null ? "" : safeStr(cell.text); }
  public static RecordType getCellType(HyperTableCell cell)      { return (cell == null) || (cell.type == null) ? hdtNone : cell.type; }
  public static boolean isEmpty(HyperTableCell cell)             { return cell == null ? true : cell.equals(HyperTableCell.blankCell); }

  @Override public HyperTableCell clone()
  { try { return (HyperTableCell) super.clone(); } catch (CloneNotSupportedException ex) { throw new RuntimeException(ex); }}

//---------------------------------------------------------------------------

  public HyperTableCell(           String newText, RecordType newType)         { this(-1   , newText, newType, smStandard); }
  public HyperTableCell(int newID, String newText, RecordType newType)         { this(newID, newText, newType, smStandard); }
  public HyperTableCell(HDT_Record record, String newText)                     { this(record.getID(), newText, record.getType(), smStandard); }

  public HyperTableCell(HDT_Record record, String newText, CellSortMethod sm)  { this(record.getID(), newText, record.getType(), sm); }
  public HyperTableCell(String newText, RecordType newType, CellSortMethod sm) { this(-1, newText, newType, sm); }

  public HyperTableCell(int newID, String newText, RecordType newType, CellSortMethod newSortMethod)
  {
    id = newID;
    text = newText;
    type = newType;
    sortMethod = newSortMethod;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + id;
    result = prime * result + (text == null ? 0 : text.hashCode());
    result = prime * result + (type == null ? 0 : type.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    HyperTableCell other = (HyperTableCell) obj;

    if (getCellType(this) != getCellType(other))
    {
      if ((getCellType(this) == hdtAuxiliary) || (getCellType(other) == hdtAuxiliary))
        return false;

      if ((id < 0) && (other.id < 0))
        if (safeStr(text).isEmpty() && safeStr(other.text).isEmpty())
          return true;

      return false;
    }

    if (((id >= 0) || (other.id >= 0)) && (id != other.id)) return false;

    return safeStr(text).equals(safeStr(other.text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell getCopyWithID(int newID)
  {
    HyperTableCell newCell = clone();
    newCell.id = newID;
    return newCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static HyperTableCell simpleSortValue(HyperTableCell cell)
  {
    HyperTableCell newCell = cell.clone();
    newCell.sortMethod = smTextSimple;
    return newCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(HyperTableCell otherCell)
  {
    if (sortMethod == smLast)
      return Integer.MAX_VALUE;
    else if (otherCell.sortMethod == smLast)
      return Integer.MIN_VALUE + 1;

    if (sortMethod == smTextSimple)
      return text.compareTo(otherCell.text);
    else if (sortMethod == smNumeric)
    {
      return parseInt(text, Integer.MAX_VALUE) - parseInt(HyperTableCell.getCellText(otherCell), Integer.MAX_VALUE);
    }
    else if (sortMethod == smWork)
    {
      HDT_Work thisWork = getRecord(), otherWork = otherCell.getRecord();

      int cResult, numAuthors = Math.max(thisWork.getAuthors().size(), otherWork.getAuthors().size());

      for (int ndx = 0; ndx < numAuthors; ndx++)
      {
        if ((ndx >= thisWork.getAuthors().size()) || (ndx >= otherWork.getAuthors().size()))
          return thisWork.getAuthors().size() - otherWork.getAuthors().size();

        cResult = thisWork.getAuthors().get(ndx).compareTo(otherWork.getAuthors().get(ndx));
        if (cResult != 0) return cResult;
      }

      cResult = thisWork.getYear().compareTo(otherWork.getYear());
      if (cResult != 0) return cResult;

      return thisWork.getSortKey().compareTo(otherWork.getSortKey());
    }

    String thisKey = "", otherKey = "";

    if ((id > 0) && (type != null) && (type != hdtNone))
      thisKey = db.records(type).getByID(id).getSortKey();

    if (thisKey.isEmpty()) thisKey = makeSortKeyByType(text, type);

    if ((otherCell.id > 0) && (otherCell.type != null) && (otherCell.type != hdtNone))
      otherKey = db.records(otherCell.type).getByID(otherCell.id).getSortKey();

    if (otherKey.isEmpty()) otherKey = makeSortKeyByType(otherCell.text, otherCell.type);

    return thisKey.compareTo(otherKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <HDT_T extends HDT_Record> HDT_T getRecord(HyperTableCell cell)
  {
    int id = getCellID(cell);
    if (id < 1) return null;

    RecordType type = getCellType(cell);
    return type == hdtNone ? null : (HDT_T)db.records(type).getByID(id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
