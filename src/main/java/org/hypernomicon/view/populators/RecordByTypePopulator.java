/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.view.populators;

import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class RecordByTypePopulator extends Populator
{
  private final Map<HyperTableRow, HDT_RecordType> rowToRecordType = new HashMap<>();
  private final Map<HyperTableRow, Boolean> rowToChanged = new HashMap<>();
  private final Map<HyperTableRow, List<HyperTableCell>> rowToChoices = new HashMap<>();
  private final boolean nameOnly;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RecordByTypePopulator()                          { this(null, false); }
  public RecordByTypePopulator(boolean nameOnly)          { this(null, nameOnly); }
  public RecordByTypePopulator(Predicate<Integer> filter) { this(filter, false); }

  public RecordByTypePopulator(Predicate<Integer> filter, boolean nameOnly)
  {
    this.filter = filter;
    this.nameOnly = nameOnly;
  }

  @Override public CellValueType getValueType() { return cvtRecord; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setRecordType(HyperTableRow row, HDT_RecordType newType)
  {
    if (row == null) row = dummyRow;

    if (rowToRecordType.put(row, newType) != newType)
      rowToChanged.put(row, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record getNextRecord(Iterator<? extends HDT_Record> it)
  {
    while (it.hasNext())
    {
      HDT_Record record = it.next();

      if ((filter == null) || filter.test(record.getID()))
        if (isUnstoredRecord(record.getID(), record.getType()) == false)
          return record;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // If num = 5, returns a list of the 5 most recently viewed records

  private List<Integer> getRecent(HDT_RecordType recordType, int num)
  {
    Instant dates[] = new Instant[num];
    int ids[] = new int[num], pos[] = new int[num], revPos[] = new int[num];

    for (int ndx = 0; ndx < num; ndx++)
    {
      dates[ndx] = Instant.MIN;
      ids[ndx] = -1;
      pos[ndx] = ndx;
      revPos[ndx] = ndx;
    }

    Iterator<? extends HDT_Record> it = db.records(recordType).keyIterator();
    HDT_Record record;

    while ((record = getNextRecord(it)) != null)
    {
      Instant curDate = record.getViewDate();
      int slotNdx = -1;

      for (int ndx = num - 1; ndx >= 0; ndx--)
      {
        if (dates[revPos[ndx]].compareTo(curDate) >= 0)  // Most of the time this happens on the first iteration so
          break;                                         // the containing loop moves quickly to its next iteration

        slotNdx = revPos[ndx];
      }

      if (slotNdx > -1)
      {
        int insertPos = pos[slotNdx];

        for (int ndx = 0; ndx < num; ndx++)
        {
          if (pos[ndx] == (num - 1))
          {
            pos[ndx] = insertPos;
            ids[ndx] = record.getID();
            dates[ndx] = curDate;
          }

          else if (pos[ndx] >= insertPos)
            pos[ndx]++;

          revPos[pos[ndx]] = ndx;
        }
      }
    }

    List<Integer> recent = new ArrayList<>();

    for (int ndx = 0; ndx < num; ndx++)
    {
      int slotNdx = revPos[ndx];
      if (ids[slotNdx] > 0)
        recent.add(ids[slotNdx]);
    }

    return recent;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if (row == null) row = dummyRow;

    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    List<HyperTableCell> choices = rowToChoices.get(row), recentChoices = new ArrayList<>();

    if ((hasChanged(row) == false) && (force == false))
      return choices;

    HDT_RecordType recordType = rowToRecordType.get(row);
    choices.clear();
    choices.add(HyperTableCell.blankCell);

    if ((recordType == hdtNone) || (db.isLoaded() == false)) return choices;

    Map<Integer, Boolean> map = new HashMap<>();
    boolean firstAdd = true;

    if (recordType.getDisregardDates() == false)
    {
      for (Integer id : getRecent(recordType, 5))
      {
        HDT_Record record = db.records(recordType).getByID(id.intValue());

        if (firstAdd)
        {
          choices.clear();
          firstAdd = false;
        }

        recentChoices.add(new HyperTableCell(record.getID(), nameOnly ? record.name() : record.getCBText(), recordType));

        map.put(id, true);
      }
    }

    for (HDT_Record record : db.records(recordType).keyIterable())
    {
      if (map.containsKey(record.getID()) == false)
      {
        HyperTableCell choice = getCell(record);

        if (choice != null)
        {
          if (firstAdd)
          {
            choices.clear();
            firstAdd = false;
          }

          addToSortedList(choices, choice);
        }
      }
    }

    if (db.records(recordType).size() == 0)
      choices.clear();
    else if (recentChoices.size() > 0)
    {
      for (int ndx = 0; ndx < recentChoices.size(); ndx++)
        choices.add(ndx, recentChoices.get(ndx));
    }

    choices.add(HyperTableCell.blankCell);

    rowToChanged.put(row, false);
    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableCell getCell(HDT_Record record)
  {
    if ((filter != null) && (filter.test(record.getID()) == false))
      return null;

    if (nameOnly)
      return new HyperTableCell(record.getID(), record.name(), record.getType());
    else if (record.getType() == hdtWork)
      return new HyperTableCell(record.getID(), record.getCBText(), record.getType(), hsmWork);
    else
      return new HyperTableCell(record.getID(), record.getCBText(), record.getType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    if (row == null) row = dummyRow;
    if (hasChanged(row) == false) return equalMatch(row, cell);

    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    HDT_RecordType recordType = rowToRecordType.get(row);

    if ((recordType == hdtNone) || (HyperTableCell.getCellType(cell) == hdtNone) || (HyperTableCell.getCellID(cell) < 1))
      return HyperTableCell.blankCell;

    if (recordType != HyperTableCell.getCellType(cell))
      return null;

    return getCell(cell.getRecord());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    if (row == null) row = dummyRow;
    if (hasChanged(row) == false) return super.getChoiceByID(row, id);

    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    HDT_RecordType recordType = rowToRecordType.get(row);

    if ((recordType == hdtNone) || (id < 1))
      return HyperTableCell.blankCell;

    return getCell(db.records(recordType).getByID(id));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasChanged(HyperTableRow row)
  {
    if (row == null) row = dummyRow;

    rowToChanged.putIfAbsent(row, true);
    return rowToChanged.get(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setChanged(HyperTableRow row)
  {
    rowToChanged.put(nullSwitch(row, dummyRow), true);
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_RecordType getRecordType(HyperTableRow row)
  {
    return rowToRecordType.getOrDefault(nullSwitch(row, dummyRow), hdtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    rowToChanged.clear();
    rowToChoices.clear();
    rowToRecordType.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value)
  {
    if (row == null) row = dummyRow;

    HDT_RecordType type = ((id > 0) || (safeStr(value).length() > 0)) ? rowToRecordType.getOrDefault(row, hdtNone) : hdtNone;

    HyperTableCell cell = new HyperTableCell(id, value, type);

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).add(cell);
    return cell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
