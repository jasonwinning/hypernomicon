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

package org.hypernomicon.view.populators;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class RecordByTypePopulator extends RecordPopulator
{
  private final Map<HyperTableRow, RecordType> rowToRecordType = new HashMap<>();
  private final Map<HyperTableRow, Boolean> rowToChanged = new HashMap<>();
  private final Map<HyperTableRow, List<HyperTableCell>> rowToChoices = new HashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RecordByTypePopulator()
  {
    super(null, DisplayKind.cbText);
  }

  public RecordByTypePopulator(Predicate<Integer> idFilter, Function<HDT_Record, String> textFunction)
  {
    super(idFilter, textFunction);
  }

  public RecordByTypePopulator(Predicate<Integer> idFilter, DisplayKind displayKind)
  {
    super(idFilter, displayKind);
  }

  @Override public CellValueType getValueType() { return cvtRecord; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void setRecordType(RecordType newType)
  {
    setRecordType(dummyRow, newType);
  }

  public void setRecordType(HyperTableRow row, RecordType newType)
  {
    if (rowToRecordType.put(row, newType) != newType)
      rowToChanged.put(row, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    rowToRecordType.putIfAbsent(row, hdtNone);

    rowToChoices.putIfAbsent(row, new ArrayList<>());

    List<HyperTableCell> choices = rowToChoices.get(row);

    if ((hasChanged(row) == false) && (force == false))
      return choices;

    RecordType recordType = rowToRecordType.get(row);
    choices.clear();

    if ((recordType == hdtNone) || (db.isLoaded() == false))
    {
      choices.add(GenericNonRecordHTC.blankCell);
      return choices;
    }

    populateRecordCells(choices, db.records(recordType).keyIterable(), recordType);

    rowToChanged.put(row, false);
    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    if (hasChanged(row) == false) return matchFromList(row, cell);

    return match(row, HyperTableCell.getCellID(cell), HyperTableCell.getCellType(cell), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    if (hasChanged(row) == false) return super.getChoiceByID(row, id);

    return match(row, id, hdtNone, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HyperTableCell match(HyperTableRow row, int id, RecordType recordType, boolean ignoreRecordType)
  {
    rowToRecordType.putIfAbsent(row, hdtNone);

    RecordType rowRecordType = rowToRecordType.get(row);

    return ((rowRecordType == hdtNone) ||
            (id < 1) ||
            ((ignoreRecordType == false) && (rowRecordType != recordType))) ?
      GenericNonRecordHTC.blankCell
    :
      generateCell(db.records(rowRecordType).getByID(id));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean hasChanged(HyperTableRow row)
  {
    rowToChanged.putIfAbsent(row, true);
    return rowToChanged.get(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setChanged(HyperTableRow row)
  {
    rowToChanged.put(row, true);
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordType getRecordType(HyperTableRow row)
  {
    return rowToRecordType.getOrDefault(row, hdtNone);
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

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String text)
  {
    RecordType type = ((id > 0) || strNotNullOrEmpty(text)) ? rowToRecordType.getOrDefault(row, hdtNone) : hdtNone;

    return createAndAddCell(row, rowToChoices, id, text, type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
