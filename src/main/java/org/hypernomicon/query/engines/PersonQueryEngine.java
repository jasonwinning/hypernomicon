/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.query.engines;

import org.hypernomicon.model.records.HDT_Institution;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryTabCtrlr.*;

public class PersonQueryEngine extends QueryEngine<HDT_Person>
{

  private static final int QUERY_SET_DECEASED_AS_PAST = QUERY_FIRST_NDX + 1,
                           QUERY_MULTIPLE_INST        = QUERY_FIRST_NDX + 2;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    if (app.debugging())
      pop.addEntry(row, QUERY_SET_DECEASED_AS_PAST, "Set deceased people as past members of institutions");

    pop.addEntry(row, QUERY_MULTIPLE_INST, "with multiple affiliations");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    switch (query)
    {

    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Person person, boolean firstCall, boolean lastCall)
  {
    switch (curQuery)
    {
      case QUERY_SET_DECEASED_AS_PAST :

        if (person.status.getID() == 5)  // Deceased
        {
          boolean foundOne = false;
          for (HDT_Institution inst : person.institutions)
          {
            if (person.instIsPast(inst) == false)
            {
              foundOne = true;
              db.updateNestedBoolean(person, inst, tagPast, true);
            }
          }

          return foundOne;
        }

        return false;

      case QUERY_MULTIPLE_INST :

        return person.institutions.size() > 1;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QueryType getQueryType()
  {
    return QueryType.qtPersons;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    switch (query)
    {
      default :
        break;
    }

    return new DatasetQuerySource(hdtPerson);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean needsMentionsIndex(int query)
  {
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasOperand(int query, int opNum, HyperTableCell prevOp)
  {
    switch (query)
    {
      case QUERY_SET_DECEASED_AS_PAST :
      case QUERY_MULTIPLE_INST :

        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
