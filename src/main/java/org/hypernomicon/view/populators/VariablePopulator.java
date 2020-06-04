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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class VariablePopulator extends Populator
{
  private final Map<HyperTableRow, Populator> rowToPop = new HashMap<>();
  private final Map<HyperTableRow, Boolean> rowToRestricted = new HashMap<>();

//---------------------------------------------------------------------------

  public void setPopulator(HyperTableRow row, Populator populator) { rowToPop.put(row, populator); rowToRestricted.put(row, true); }
  public void setRestricted(HyperTableRow row, boolean restrict)   { rowToRestricted.put(row, restrict); }
  public boolean getRestricted(HyperTableRow row)                  { return rowToRestricted.getOrDefault(row, true); }

  @SuppressWarnings("unchecked")
  public <PopType extends Populator> PopType getPopulator(HyperTableRow row)        { return (PopType) rowToPop.get(row); }

  @Override public CellValueType getValueType()                                     { return cvtVaries; }
  @Override public void clear()                                                     { rowToPop.clear(); rowToRestricted.clear(); }
  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value) { return rowToPop.get(row).addEntry(row, id, value); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return nullSwitch(rowToPop.get(row), new ArrayList<>(), pop -> pop.populate(row, force));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    return nullSwitch(rowToPop.get(row), null, pop -> pop.match(row, cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    return nullSwitch(rowToPop.get(row), null, pop -> pop.getChoiceByID(row, id));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasChanged(HyperTableRow row)
  {
    return nullSwitch(rowToPop.get(row), true, pop -> pop.hasChanged(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setChanged(HyperTableRow row)
  {
    nullSwitch(rowToPop.get(row), pop -> pop.setChanged(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordType getRecordType(HyperTableRow row)
  {
    return nullSwitch(rowToPop.get(row), hdtNone, pop -> pop.getRecordType(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
