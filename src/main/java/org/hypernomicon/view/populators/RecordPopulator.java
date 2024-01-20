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

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;

//---------------------------------------------------------------------------

public abstract class RecordPopulator extends Populator
{
  private final DisplayKind displayKind;
  private final Predicate<Integer> idFilter;
  private final boolean nullOkay;

//---------------------------------------------------------------------------

  protected RecordPopulator(Predicate<Integer> idFilter, DisplayKind displayKind, boolean nullOkay)
  {
    this.idFilter = idFilter;
    this.displayKind = displayKind;
    this.nullOkay = nullOkay;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void populateRecordCells(List<HyperTableCell> choices, Iterable<? extends HDT_Record> recordIt, RecordType recordType)
  {
    choices.clear();

    List<HDT_Record> recordsSortedByViewDate = new ArrayList<>(),
                     recordsSortedByKey = new ArrayList<>();

    for (HDT_Record record : recordIt)
    {
      HyperTableCell choice = getCell(record, recordType);

      if (choice != null)
      {
        int ndx = addToSortedList(choices, choice);

        if (recordType.getDisregardDates() == false)
        {
          recordsSortedByKey.add(ndx, record);
          addToSortedList(recordsSortedByViewDate, record, Comparator.comparing(HDT_Record::getViewDate).reversed());

          if (recordsSortedByViewDate.size() > 5)
            recordsSortedByViewDate.remove(5);
        }
      }
    }

    if ((recordType.getDisregardDates() == false) && (choices.size() > 25))
    {
      for (int newNdx = 0; newNdx < 5; newNdx++)
      {
        HDT_Record record = recordsSortedByViewDate.get(newNdx);
        int oldNdx = recordsSortedByKey.indexOf(record);
        recordsSortedByKey.add(newNdx, recordsSortedByKey.remove(oldNdx));
        choices.add(newNdx, choices.remove(oldNdx));
      }
    }

    choices.add(HyperTableCell.blankCell());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected HyperTableCell getCell(HDT_Record record, RecordType recordType)
  {
    if (record == null) return nullOkay ? new HyperTableCell("", recordType) : null;

    if ((idFilter != null) && (idFilter.test(record.getID()) == false))
      return null;

    String text;

    switch (displayKind)
    {
      case cbText   : text = record.getCBText(); break;
      case listName : text = record.listName();  break;
      default       : text = record.name();
    }

    return new HyperTableCell(record, text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
