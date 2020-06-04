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

import static org.hypernomicon.query.QueryTabCtrlr.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.QueryTabCtrlr;
import org.hypernomicon.query.QueryTabCtrlr.QueryView;
import org.hypernomicon.query.engines.QueryEngine.QueryType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryPopulator extends Populator
{
  private final Map<HyperTableRow, QueryType> rowToQueryType;
  private final Map<HyperTableRow, List<HyperTableCell>> rowToChoices;

  @Override public CellValueType getValueType() { return cvtQuery; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryPopulator()
  {
    rowToChoices = new HashMap<>();
    rowToQueryType = new HashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    return rowToChoices.get(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordType getRecordType(HyperTableRow row)
  {
    return rowToQueryType.getOrDefault(row, QueryType.qtAllRecords).getRecordType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setQueryType(HyperTableRow row, QueryType newType, QueryView qV)
  {
    QueryType oldType = rowToQueryType.put(row, newType);

    if ((newType == null) || newType.equals(oldType)) return;

    curQuery = row.getID(1);

    switch (curQuery)
    {
      case QUERY_WITH_NAME_CONTAINING : case QUERY_ANY_FIELD_CONTAINS :
      case QUERY_LIST_ALL             : case QUERY_WHERE_FIELD        :
        break;

      default :
        qV.clearOperands(row, 1);
    }

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).clear();
    QueryTabCtrlr.addQueries(this, row, newType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    rowToChoices.clear();
    rowToQueryType.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value)
  {
    HyperTableCell cell = new HyperTableCell(id, value, getRecordType(row));

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).add(cell);
    return cell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
