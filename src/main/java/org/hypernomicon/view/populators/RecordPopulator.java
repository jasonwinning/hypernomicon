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

package org.hypernomicon.view.populators;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.cellValues.RecordHTC;

//---------------------------------------------------------------------------

public abstract class RecordPopulator extends Populator
{
  private final DisplayKind displayKind;
  private final Predicate<Integer> idFilter;

//---------------------------------------------------------------------------

  protected RecordPopulator(Predicate<Integer> idFilter, DisplayKind displayKind)
  {
    this.idFilter = idFilter;
    this.displayKind = displayKind;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int NUM_RECENTS = 5;

  protected void populateRecordCells(List<HyperTableCell> choices, Iterable<? extends HDT_Record> recordIt, RecordType recordType)
  {
    choices.clear();

    List<HDT_Record> recordsSortedByViewDate = new ArrayList<>(),
                     recordsSortedByKey = new ArrayList<>();

    Comparator<HyperTableCell> comparator = (recordType == hdtWork) && (displayKind == DisplayKind.cbText) ?
      (cell1, cell2) -> HyperTableCell.compareCells(cell1, cell2, smWork)
    :
      HyperTableCell::compareTo;

    for (HDT_Record record : recordIt)
    {
      HyperTableCell choice = generateCell(record);

      if (HyperTableCell.getCellType(choice) != hdtNone)
      {
        int ndx = addToSortedList(choices, choice, comparator);

        if (recordType.getDisregardDates() == false)
        {
          recordsSortedByKey.add(ndx, record);
          addToSortedList(recordsSortedByViewDate, record, Comparator.comparing(HDT_Record::getViewDate).reversed());

          if (recordsSortedByViewDate.size() > NUM_RECENTS)
            recordsSortedByViewDate.remove(NUM_RECENTS);
        }
      }
    }

    if ((recordType.getDisregardDates() == false) && (choices.size() > 25))
    {
      int numRecents = Math.min(NUM_RECENTS, recordsSortedByViewDate.size());

      for (int newNdx = 0; newNdx < numRecents; newNdx++)
      {
        HDT_Record record = recordsSortedByViewDate.get(newNdx);
        int oldNdx = recordsSortedByKey.indexOf(record);
        recordsSortedByKey.add(newNdx, recordsSortedByKey.remove(oldNdx));
        choices.add(newNdx, choices.remove(oldNdx));
      }
    }

    choices.add(RecordHTC.blankCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected HyperTableCell generateCell(HDT_Record record)
  {
    return (record == null) || ((idFilter != null) && (idFilter.test(record.getID()) == false)) ?
      RecordHTC.blankCell
    :
      new RecordHTC(record, getCellText(record));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getCellText(HDT_Record record)
  {
    return record == null ? "" : switch (displayKind)
    {
      case cbText   -> record.getCBText();
      case listName -> record.listName();
      default       -> record.name();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
