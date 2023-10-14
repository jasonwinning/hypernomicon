/*
 * Copyright 2015-2023 Jason Winning
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
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
    super(null, DisplayKind.cbText, false);
  }

  public RecordByTypePopulator(Predicate<Integer> idFilter, DisplayKind displayKind)
  {
    super(idFilter, displayKind, false);
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
    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    List<HyperTableCell> choices = rowToChoices.get(row);

    if ((hasChanged(row) == false) && (force == false))
      return choices;

    RecordType recordType = rowToRecordType.get(row);
    choices.clear();

    if ((recordType == hdtNone) || (db.isLoaded() == false))
    {
      choices.add(HyperTableCell.blankCell());
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
    if (hasChanged(row) == false) return equalMatch(row, cell);

    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    RecordType recordType = rowToRecordType.get(row);

    if ((recordType == hdtNone) || (HyperTableCell.getCellType(cell) == hdtNone) || (HyperTableCell.getCellID(cell) < 1))
      return HyperTableCell.blankCell();

    if (recordType != HyperTableCell.getCellType(cell))
      return null;

    return getCell(cell.getRecord(), recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    if (hasChanged(row) == false) return super.getChoiceByID(row, id);

    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);

    RecordType recordType = rowToRecordType.get(row);

    return (recordType == hdtNone) || (id < 1) ?
      HyperTableCell.blankCell()
    :
      getCell(db.records(recordType).getByID(id), recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasChanged(HyperTableRow row)
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
    RecordType type = ((id > 0) || (safeStr(text).length() > 0)) ? rowToRecordType.getOrDefault(row, hdtNone) : hdtNone;

    HyperTableCell cell = new HyperTableCell(id, text, type);

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).add(cell);
    return cell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
