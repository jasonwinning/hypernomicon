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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.mutable.MutableInt;

import org.hypernomicon.model.records.*;
import org.hypernomicon.query.ui.ResultRow;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod;

//---------------------------------------------------------------------------

public interface HyperTableCell extends Cloneable, Comparable<HyperTableCell>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int TRUE_ID  = 1,
      FALSE_ID = 2,
      UNSET_ID = 3;

  static int getCellID(HyperTableCell cell)          { return cell == null ? -1 : cell.getID(); }
  static String getCellText(HyperTableCell cell)     { return cell == null ? "" : safeStr(cell.getText()); }
  static RecordType getCellType(HyperTableCell cell) { return (cell == null) || (cell.getRecordType() == null) ? hdtNone : cell.getRecordType(); }
  static boolean isEmpty(HyperTableCell cell)        { return cell == null ? true : cell.isEmpty(); }

  static <HDT_T extends HDT_Record> HDT_T getRecord(HyperTableCell cell) { return cell == null ? null : cell.getRecord(); }

//---------------------------------------------------------------------------

  int getID();
  String getText();
  RecordType getRecordType();
  HyperTableCell getCopyWithID(int newID);
  boolean getSortToBottom();
  boolean isEmpty();
  <HDT_T extends HDT_Record> HDT_T getRecord();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static HyperTableCell clone(HyperTableCell cell)
  {
    if (cell instanceof AbstractHTC abstractHTC)
      return abstractHTC.clone();

    if (cell instanceof ResultRow resultRow)
      return resultRow.clone();

    throw new AssertionError();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static int compareCells(HyperTableCell cell1, HyperTableCell cell2, CellSortMethod sortMethod)
  {
    if ((cell1 == null) && (cell2 == null)) return 0;
    if (cell1 == null) return -1;
    if (cell2 == null) return  1;

    if (cell1.getSortToBottom()) return  1;
    if (cell2.getSortToBottom()) return -1;

    if (anyIsInstanceOf(BibDateHTC   .class, cell1, cell2)) return BibDateHTC   .compareCells(cell1, cell2);
    if (anyIsInstanceOf(PageRangeHTC .class, cell1, cell2)) return PageRangeHTC .compareCells(cell1, cell2);
    if (anyIsInstanceOf(RecordIconHTC.class, cell1, cell2)) return RecordIconHTC.compareCells(cell1, cell2);

    if (allAreInstancesOf(GenericNonRecordHTC.class, cell1, cell2))
      sortMethod = smTextSimple;

    if (sortMethod == smNumeric   ) return compareNumericCells   (cell1, cell2);
    if (sortMethod == smTextSimple) return compareSimpleTextCells(cell1, cell2);

    if (sortMethod == smWork)
    {
      HDT_Work work1 = cell1.getRecord(), work2 = cell2.getRecord();

      if ((work1 != null) || (work2 != null))
        return ObjectUtils.compare(work1, work2);
    }

    String key1 = nullSwitch(cell1.getRecord(), "", HDT_Record::getSortKey),
           key2 = nullSwitch(cell2.getRecord(), "", HDT_Record::getSortKey);

    if (key1.isEmpty()) key1 = makeSortKeyByType(cell1.getText(), getCellType(cell1));
    if (key2.isEmpty()) key2 = makeSortKeyByType(cell2.getText(), getCellType(cell2));

    return key1.compareTo(key2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int compareNumericCells(HyperTableCell cell1, HyperTableCell cell2)
  {
    MutableInt result = new MutableInt();

    return compareNumberStrings(cell1.getText(), cell2.getText(), result) ?
      result.intValue()
    :
      compareSimpleTextCells(cell1, cell2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int compareSimpleTextCells(HyperTableCell cell1, HyperTableCell cell2)
  {
    int result = cell1.getText().compareToIgnoreCase(cell2.getText());

    return result != 0 ? result : (cell1.getID() - cell2.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
