/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.queryEngines;

import org.hypernomicon.querySources.DatasetQuerySource;
import org.hypernomicon.querySources.FilteredQuerySource;
import org.hypernomicon.querySources.QuerySource;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.tabs.QueriesTabController.*;

import org.hypernomicon.model.records.HDT_Institution;

public class InstitutionQueryEngine extends QueryEngine<HDT_Institution>
{  
  public static final int QUERY_CONVERT_INST           = QUERY_FIRST_NDX + 8;

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    pop.addEntry(row, QUERY_CONVERT_INST, "convert department records to institution records");
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

  @Override public boolean evaluate(HDT_Institution inst, boolean firstCall, boolean lastCall)
  {
    switch (curQuery)
    {
      case QUERY_CONVERT_INST :
        
//        if (firstCall)
//        {
//          db.runningConversion = true;
//          db.stopIndexingMentions();
//        }
//        
//        inst.instType.setID(2);
//        
//        for (HDT_Department dept : inst.departments)
//        {
//          HDT_Institution subInst = db.createNewBlankRecord(hdtInstitution);
//          
//          subInst.setName(dept.name());
//          subInst.parentInst.set(inst);
//          subInst.country.set(inst.country.get());
//          subInst.state.set(inst.state.get());
//          subInst.setCity(inst.getCity());
//          subInst.setLink(dept.getLink());
//          
//          for (HDT_Person person : dept.persons)
//            db.getObjectList(rtInstOfPerson, person, false).add(subInst);
//          
//          if (dept.name().toLowerCase().contains("school"))
//            subInst.instType.setID(5);
//          else if (dept.name().toLowerCase().contains("college"))
//            subInst.instType.setID(7);
//          else if (dept.name().toLowerCase().contains("institute"))
//            subInst.instType.setID(11);
//          else if (dept.name().toLowerCase().contains("faculty"))
//            subInst.instType.setID(9);
//          else if (dept.name().toLowerCase().contains("division"))
//            subInst.instType.setID(4);
//          else
//            subInst.instType.setID(10);
//          
//          subInst.setDates(dept.getCreationDate(), dept.getModifiedDate(), dept.getViewDate());
//        }
//        
//        if (lastCall)
//        {
//          db.runningConversion = false;
//          db.rebuildMentions();
//        }
//        
//        return true;
    }
    
    return false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public QueryType getQueryType()
  {
    return QueryType.qtInstitutions;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    switch (query)
    {
      case QUERY_CONVERT_INST :
        return new FilteredQuerySource(getQueryType(), query)
        {
          @Override protected void runFilter()
          {
            for (HDT_Institution inst : db.institutions) 
              list.add(inst);
          }          
        };
      
      default :
        break;
    }
    
    return new DatasetQuerySource(hdtInstitution);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public boolean needsMentionsIndex(int query)
  {
    return false;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
