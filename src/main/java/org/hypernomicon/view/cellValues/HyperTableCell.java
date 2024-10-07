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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.RecordHTC.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.Comparator;

import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod;

//---------------------------------------------------------------------------

public abstract class HyperTableCell implements Comparable<HyperTableCell>, Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final int TRUE_ID  = 1,
                          FALSE_ID = 2,
                          UNSET_ID = 3;

  public final boolean sortToBottom;

  public static HyperTableCell fromBoolean(boolean boolVal) { return boolVal ? trueCell : falseCell; }
  public static int getCellID(HyperTableCell cell)          { return cell == null ? -1 : cell.getID(); }
  public static String getCellText(HyperTableCell cell)     { return cell == null ? "" : safeStr(cell.getText()); }
  public static RecordType getCellType(HyperTableCell cell) { return (cell == null) || (cell.getRecordType() == null) ? hdtNone : cell.getRecordType(); }
  public static boolean isEmpty(HyperTableCell cell)        { return (cell == null) || blankCell.equals(cell); }

//---------------------------------------------------------------------------

  HyperTableCell(boolean sortToBottom)
  {
    this.sortToBottom = sortToBottom;
  }

//---------------------------------------------------------------------------

  public abstract int getID();
  public abstract String getText();
  public abstract RecordType getRecordType();
  public abstract String getImgRelPath();
  public abstract HyperTableCell getCopyWithID(int newID);

  public <HDT_T extends HDT_Record> HDT_T getRecord() { return getRecord(this); }

  @Override public HyperTableCell clone()
  { try { return (HyperTableCell) super.clone(); } catch (CloneNotSupportedException e) { throw new AssertionError(e); }}

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

  public static Comparator<HyperTableCell> leadingNumberComparator()
  {
    return(cell1, cell2) ->
    {
      String text1 = ultraTrim(cell1.getText()), text2 = ultraTrim(cell2.getText());
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

  public static int compareCells(HyperTableCell cell1, HyperTableCell cell2, CellSortMethod sortMethod)
  {
    if ((cell1 == null) && (cell2 == null)) return 0;
    if (cell1 == null) return -1;
    if (cell2 == null) return 1;

    if (cell1.sortToBottom)
      return 1;

    if (cell2.sortToBottom)
      return -1;

    if ((cell1 instanceof BibDateHTC) || (cell2 instanceof BibDateHTC))
    {
      return cell1 instanceof BibDateHTC bibDateHTC1 ?
        (cell2 instanceof BibDateHTC bibDateHTC2 ?
           bibDateHTC1.bibDate.compareTo(bibDateHTC2.bibDate)
         : 1)
      : -1;
    }

    if (sortMethod == smIcon)
    {
      int result = compareImgRelPaths(cell1.getImgRelPath(), cell2.getImgRelPath());

      if (result == 0)
      {
        result = getCellType(cell1).compareTo(getCellType(cell2));

        if (result == 0) result = cell1.getID() - cell2.getID();
      }

      return result;
    }

    if (sortMethod == smNumeric)
    {
      MutableInt result = new MutableInt();

      if (compareNumberStrings(cell1.getText(), cell2.getText(), result))
        return result.getValue();

      sortMethod = smTextSimple;
    }

    if (sortMethod == smTextSimple)
    {
      int result = cell1.getText().compareToIgnoreCase(cell2.getText());
      if (result == 0) result = cell1.getID() - cell2.getID();
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

    if ((cell1.getID() > 0) && (cell1.getRecordType() != null) && (cell1.getRecordType() != hdtNone))
      key1 = db.records(cell1.getRecordType()).getByID(cell1.getID()).getSortKey();

    if (key1.isEmpty()) key1 = makeSortKeyByType(cell1.getText(), getCellType(cell1));

    if ((cell2.getID() > 0) && (cell2.getRecordType() != null) && (cell2.getRecordType() != hdtNone))
      key2 = db.records(cell2.getRecordType()).getByID(cell2.getID()).getSortKey();

    if (key2.isEmpty()) key2 = makeSortKeyByType(cell2.getText(), getCellType(cell2));

    return key1.compareTo(key2);
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

}
