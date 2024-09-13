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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.Comparator;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.model.items.BibliographicYear;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.MediaUtil;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod;
import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------

public class HyperTableCell implements Comparable<HyperTableCell>, Cloneable
{
  private int id;
  private String imgRelPath;  // should only ever be accessed by getImgRelPath

  public final String text;
  public final RecordType type;
  public final boolean sortToBottom;

  public int getID()                                             { return id; }
  public <HDT_T extends HDT_Record> HDT_T getRecord()            { return getRecord(this); }

  static HyperTableCell checkboxCellFromBoolean(boolean boolVal) { return boolVal ? trueCheckboxCell() : falseCheckboxCell(); }
  public static int getCellID(HyperTableCell cell)               { return cell == null ? -1 : cell.id; }
  public static String getCellText(HyperTableCell cell)          { return cell == null ? "" : safeStr(cell.text); }
  public static RecordType getCellType(HyperTableCell cell)      { return (cell == null) || (cell.type == null) ? hdtNone : cell.type; }
  public static boolean isEmpty(HyperTableCell cell)             { return (cell == null) || cell.equals(blankCell()); }
  public static HyperTableCell blankCell()                       { return new HyperTableCell(    "", hdtNone); }
  public static HyperTableCell trueCheckboxCell()                { return new HyperTableCell(1 , "", hdtNone); }
  public static HyperTableCell falseCheckboxCell()               { return new HyperTableCell(0 , "", hdtNone); }

  @Override public HyperTableCell clone()
  { try { return (HyperTableCell) super.clone(); } catch (CloneNotSupportedException e) { throw new AssertionError(e); }}

//---------------------------------------------------------------------------

  public HyperTableCell(                   String text, RecordType type)         { this(-1            , text, type); }
  public HyperTableCell(int id           , String text, RecordType type)         { this(id            , text, type, false); }
  public HyperTableCell(HDT_Record record, String text                 )         { this(record.getID(), text, record.getType()); }

  public HyperTableCell(String text, RecordType type, boolean sortToBottom)      { this(-1, text, type, sortToBottom); }

  private HyperTableCell(int id, String text, RecordType type, boolean sortToBottom)
  {
    this.id = id;
    this.text = text;
    this.type = type;
    this.sortToBottom = sortToBottom;
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
        return safeStr(text).isEmpty() && safeStr(other.text).isEmpty();

      return false;
    }

    if (((id >= 0) || (other.id >= 0)) && (id != other.id)) return false;

    return safeStr(text).equals(safeStr(other.text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getImgRelPath()
  {
    if (imgRelPath != null)
      return imgRelPath;

    return imgRelPath = safeStr(nullSwitch(getRecord(this), imgRelPathByType(type), MediaUtil::imgRelPath));
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

  public static int compareCells(HyperTableCell cell1, HyperTableCell cell2, CellSortMethod sortMethod)
  {
    if ((cell1 == null) && (cell2 == null)) return 0;
    if (cell1 == null) return -1;
    if (cell2 == null) return 1;

    if (cell1.sortToBottom)
      return 1;

    if (cell2.sortToBottom)
      return -1;

    if (sortMethod == smIcon)
    {
      int result = compareImgRelPaths(cell1.getImgRelPath(), cell2.getImgRelPath());

      if (result == 0)
      {
        result = cell1.type.compareTo(cell2.type);

        if (result == 0) result = cell1.id - cell2.id;
      }

      return result;
    }

    if (sortMethod == smYear)
    {
      return ObjectUtils.compare(BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC(cell1.text), BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC(cell2.text));
    }

    if (sortMethod == smNumeric)
    {
      MutableInt result = new MutableInt();

      if (compareNumberStrings(cell1.text, cell2.text, result))
        return result.getValue();

      sortMethod = smTextSimple;
    }

    if (sortMethod == smTextSimple)
    {
      int result = cell1.text.compareToIgnoreCase(cell2.text);
      if (result == 0) result = cell1.id - cell2.id;
      return result;
    }

    if (sortMethod == smWork)
    {
      HDT_Work work1 = cell1.getRecord(), work2 = cell2.getRecord();

      if ((work1 == null) && (work2 != null))
        return -1;

      if ((work2 == null) && (work1 != null))
        return 1;

      if (work1 != null)
        return work1.compareTo(work2);
    }

    String key1 = "", key2 = "";

    if ((cell1.id > 0) && (cell1.type != null) && (cell1.type != hdtNone))
      key1 = db.records(cell1.type).getByID(cell1.id).getSortKey();

    if (key1.isEmpty()) key1 = makeSortKeyByType(cell1.text, cell1.type);

    if ((cell2.id > 0) && (cell2.type != null) && (cell2.type != hdtNone))
      key2 = db.records(cell2.type).getByID(cell2.id).getSortKey();

    if (key2.isEmpty()) key2 = makeSortKeyByType(cell2.text, cell2.type);

    return key1.compareTo(key2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Comparator<HyperTableCell> leadingNumberComparator()
  {
    return(cell1, cell2) ->
    {
      String text1 = ultraTrim(cell1.text), text2 = ultraTrim(cell2.text);
      int num1 = extractLeadingNumber(text1), num2 = extractLeadingNumber(text2);
      if ((num1 < 0) && (num2 < 0))
        return text1.compareTo(text2);

      if (num1 < 0)
        return -1;

      if (num2 < 0)
        return 1;

      return Integer.compare(num1, num2);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(HyperTableCell otherCell)
  {
    if (sortToBottom)
      return Integer.MAX_VALUE;

    if (otherCell.sortToBottom)
      return Integer.MIN_VALUE + 1;

    return compareCells(this, otherCell, smStandard);
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
