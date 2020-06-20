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

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.List;
import java.util.function.Predicate;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class StandardPopulator extends Populator
{
  private final RecordByTypePopulator rtp;
  private final RecordType objType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public StandardPopulator(RecordType objType) { this(objType, null, false); }

  public StandardPopulator(RecordType objType, Predicate<Integer> filter, boolean nameOnly)
  {
    this.objType = objType;

    rtp = new RecordByTypePopulator(filter, nameOnly);

    rtp.setRecordType(dummyRow, objType);
  }

//---------------------------------------------------------------------------

  @Override public CellValueType getValueType()                                    { return cvtRecord; }
  @Override public boolean hasChanged(HyperTableRow row)                           { return rtp.hasChanged(dummyRow); }
  @Override public void setChanged(HyperTableRow row)                              { rtp.setChanged(dummyRow); }
  @Override public RecordType getRecordType(HyperTableRow row)                     { return rtp.getRecordType(dummyRow); }
  @Override public void clear()                                                    { rtp.clear(); rtp.setRecordType(dummyRow, objType); }
  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force) { return rtp.populate(dummyRow, force); }
  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)    { return rtp.match(dummyRow, cell); }
  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)         { return rtp.getChoiceByID(dummyRow, id); }
  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String text) { return rtp.addEntry(dummyRow, id, text); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFilter(Predicate<Integer> filter)
  {
    super.setFilter(filter);
    rtp.setFilter(filter);
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
