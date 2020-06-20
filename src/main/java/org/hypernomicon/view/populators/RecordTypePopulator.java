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

import java.util.ArrayList;
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
  final List<HyperTableCell> choices = new ArrayList<>();

  public RecordTypePopulator()                           { this(null); }
  public RecordTypePopulator(Set<RecordType> set)        { setTypes(set); }

  public Set<RecordType> getTypes()                      { return types; }
  public void setTypes(Set<RecordType> set)              { types = set; changed = true; }

  @Override public boolean hasChanged(HyperTableRow row) { return changed; }
  @Override public void setChanged(HyperTableRow row)    { changed = true; }
  @Override public CellValueType getValueType()          { return cvtRecordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if ((force == false) && (changed == false)) return choices;

    choices.clear();

    if (types == null)
      types = EnumSet.noneOf(RecordType.class);

    if (types.isEmpty())
    {
      for (RecordType type : RecordType.values())
        if ((type != hdtNone) && (type != hdtAuxiliary))
          types.add(type);
    }

    types.forEach(type ->
    {
      HyperTableCell cell = new HyperTableCell(db.getTypeName(type), type);
      addToSortedList(choices, cell, (c1, c2) -> c1.getText().compareTo(c2.getText()));
    });

    changed = false;
    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    for (HyperTableCell choice : populate(row, false))
      if (HyperTableCell.getCellType(choice) == HyperTableCell.getCellType(cell))
        return choice;

    return null;
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
