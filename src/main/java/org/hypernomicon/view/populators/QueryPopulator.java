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

import static org.hypernomicon.query.ui.QueriesTabCtrlr.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.*;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.Query;
import org.hypernomicon.query.QueryType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class QueryPopulator extends Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Map<HyperTableRow, QueryType> rowToQueryType;
  private final Map<HyperTableRow, List<HyperTableCell>> rowToChoices;

  @Override public CellValueType getValueType() { return cvtQuery; }

//---------------------------------------------------------------------------

  public QueryPopulator()
  {
    rowToChoices   = new HashMap<>();
    rowToQueryType = new HashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return rowToChoices.computeIfAbsent(row, _ -> new ArrayList<>());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordType getRecordType(HyperTableRow row)
  {
    return rowToQueryType.getOrDefault(row, QueryType.qtAllRecords).getRecordType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setQueryType(HyperTableRow row, QueryType newType)
  {
    if (newType == null)
    {
      rowToQueryType.remove(row);
      rowToChoices.remove(row);
      return;
    }

    QueryType oldType = rowToQueryType.put(row, newType);

    if (newType == oldType) return;

    rowToChoices.computeIfAbsent(row, _ -> new ArrayList<>()).clear();

    addQueriesToPopulator(this, row, newType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    rowToChoices  .clear();
    rowToQueryType.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class QueryCell extends GenericNonRecordHTC
  {
    private final Query<?> query;

    private QueryCell(Query<?> query, RecordType newType)
    {
      super(query.getID(), query.getDescription(), newType);
      this.query = query;
    }

    private QueryCell(int id, String text, RecordType newType)
    {
      super(id, text, newType);
      this.query = null;
    }

    public Query<?> getQuery() { return query; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String text)
  {
    return addEntryToList(rowToChoices.computeIfAbsent(row, _ -> new ArrayList<>()), new QueryCell(id, text, getRecordType(row)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryCell addQuery(HyperTableRow row, Query<?> query)
  {
    return (QueryCell) addEntryToList(rowToChoices.computeIfAbsent(row, _ -> new ArrayList<>()), new QueryCell(query, getRecordType(row)));

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
