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
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class RecordTypePopulator extends Populator
{
  private Set<RecordType> types;
  private boolean changed = true;
  private final List<HyperTableCell> choices = new ArrayList<>();

  public RecordTypePopulator(boolean noHubs)
  {
    types = EnumSet.allOf(RecordType.class);
    types.removeAll(noHubs ? EnumSet.of(hdtNone, hdtAuxiliary, hdtHub) : EnumSet.of(hdtNone, hdtAuxiliary));
  }

  public RecordTypePopulator(Collection<RecordType> set) { setTypes(set); }

  public RecordTypePopulator(RecordType... types)        { setTypes(Arrays.asList(types)); }

  public Set<RecordType> getTypes()                      { return types; }

  @Override public boolean hasChanged(HyperTableRow row) { return changed; }
  @Override public void setChanged(HyperTableRow row)    { changed = true; }
  @Override public CellValueType getValueType()          { return cvtRecordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setTypes(Collection<RecordType> collection)
  {
    if (collection == null)
      types = EnumSet.noneOf(RecordType.class);
    else if (collection instanceof Set)
      types = (Set<RecordType>)collection;
    else
      types = EnumSet.copyOf(collection);

    changed = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HyperTableCell getChoiceByType(RecordType type)
  {
    return getChoiceByType(dummyRow, type);
  }

  public HyperTableCell getChoiceByType(HyperTableRow row, RecordType type)
  {
    return findFirst(populate(row, false), cell -> HyperTableCell.getCellType(cell) == type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if ((force == false) && (changed == false)) return choices;

    choices.clear();

    types.forEach(type ->
    {
      HyperTableCell cell = new HyperTableCell(getTypeName(type), type);
      addToSortedList(choices, cell, Comparator.comparing(cel -> cel.text));
    });

    changed = false;
    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    return findFirst(populate(row, false), choice -> HyperTableCell.getCellType(choice) == HyperTableCell.getCellType(cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    choices.clear();
    changed = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
