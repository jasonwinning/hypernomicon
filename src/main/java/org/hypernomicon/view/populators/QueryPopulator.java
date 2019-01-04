/*
 * Copyright 2015-2019 Jason Winning
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

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.QueriesTabController.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.queryEngines.QueryEngine.QueryType;
import org.hypernomicon.view.tabs.QueriesTabController;
import org.hypernomicon.view.tabs.QueriesTabController.QueryView;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryPopulator extends Populator
{
  private final HashMap<HyperTableRow, QueryType> rowToQueryType;
  private final HashMap<HyperTableRow, List<HyperTableCell>> rowToChoices;

  @Override public CellValueType getValueType()          { return cvtQuery; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryPopulator()
  {
    rowToChoices = new HashMap<HyperTableRow, List<HyperTableCell>>();
    rowToQueryType = new HashMap<HyperTableRow, QueryType>();
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

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    for (HyperTableCell choice : populate(nullSwitch(row, dummyRow), false))
      if (HyperTableCell.getCellID(choice) == HyperTableCell.getCellID(cell))
        return choice;

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_RecordType getRecordType(HyperTableRow row)
  {
    switch (rowToQueryType.getOrDefault(row, QueryType.qtNone))
    {
      case qtAllRecords :    return hdtNone;
      case qtArguments:      return hdtArgument;
      case qtDebates:        return hdtDebate;
      case qtFiles:          return hdtMiscFile;
      case qtInstitutions:   return hdtInstitution;
      case qtInvestigations: return hdtInvestigation;
      case qtNotes:          return hdtNote;
      case qtPersons:        return hdtPerson;
      case qtPositions:      return hdtPosition;
      case qtConcepts:       return hdtConcept;
      case qtWorks:          return hdtWork;

      default: break;
    }

    return hdtNone;
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
      case QUERY_LIST_ALL : case QUERY_WHERE_FIELD :
        break;

      default :
        qV.clearOperands(row, 1);
    }

    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).clear();
    QueriesTabController.addQueries(this, row, newType);
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
