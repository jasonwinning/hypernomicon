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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryTabCtrlr.*;

import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class ConceptQueryEngine extends QueryEngine<HDT_Concept>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QueryType getQueryType()
  {
    return QueryType.qtConcepts;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {

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

  @Override public boolean evaluate(HDT_Concept concept, boolean firstCall, boolean lastCall)
  {
    switch (curQuery)
    {

    }

    return false;
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

    return new DatasetQuerySource(hdtConcept);
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
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
