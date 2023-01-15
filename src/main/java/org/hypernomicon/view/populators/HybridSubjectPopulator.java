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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class HybridSubjectPopulator extends Populator
{
  private final StandardPopulator standardPop;
  private final SubjectPopulator subjPop;
  private final Map<HyperTableRow, Populator> rowToPop;
  private final Map<HyperTableRow, Boolean> rowToChanged;
  private final RelationType relType;

//---------------------------------------------------------------------------

  @Override public CellValueType getValueType()                                 { return cvtRecord; }
  @Override public RecordType getRecordType(HyperTableRow row)                  { return db.getSubjType(relType); }
  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell) { return equalMatch(row, cell); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HybridSubjectPopulator(RelationType relType)
  {
    this(relType, null);
  }

  public HybridSubjectPopulator(RelationType relType, Predicate<Integer> idFilter)
  {
    rowToChanged = new HashMap<>();
    rowToPop = new HashMap<>();

    standardPop = new StandardPopulator(db.getSubjType(relType), idFilter, DisplayKind.cbText);
    subjPop = new SubjectPopulator(relType, true, idFilter);

    this.relType = relType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Record getObj(HyperTableRow row)
  {
    if (row == null) row = dummyRow;

    if (rowToPop.get(row) == subjPop) return subjPop.getObj(row);

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setObj(HyperTableRow row, HDT_Record obj)
  {
    if (row == null) row = dummyRow;

    rowToPop.putIfAbsent(row, standardPop);

    if (obj == null)
    {
      if (rowToPop.get(row) != standardPop)
      {
        rowToPop.put(row, standardPop);
        rowToChanged.put(row, true);
      }
    }
    else
    {
      subjPop.setObj(row, obj);

      if (rowToPop.get(row) == subjPop)
        rowToChanged.put(row, subjPop.hasChanged(row));
      else
      {
        rowToChanged.put(row, true);
        rowToPop.put(row, subjPop);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if (row == null) row = dummyRow;

    rowToChanged.put(row, false);

    rowToPop.putIfAbsent(row, standardPop);
    return rowToPop.get(row).populate(row, force);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasChanged(HyperTableRow row)
  {
    if (row == null) row = dummyRow;

    rowToChanged.putIfAbsent(row, true);
    rowToPop.putIfAbsent(row, standardPop);

    return rowToChanged.get(row) || rowToPop.get(row).hasChanged(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setChanged(HyperTableRow row)
  {
    if (row == null) row = dummyRow;

    rowToChanged.put(row, true);
    rowToPop.putIfAbsent(row, standardPop);
    rowToPop.get(row).setChanged(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    rowToChanged.clear();
    rowToPop.clear();

    subjPop.clear();
    standardPop.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String text)
  {
    if (row == null) row = dummyRow;

    standardPop.addEntry(row, id, text);
    return subjPop.addEntry(row, id, text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
