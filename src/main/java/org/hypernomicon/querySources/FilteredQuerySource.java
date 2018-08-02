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

package org.hypernomicon.querySources;

import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.queryEngines.QueryEngine.QueryType;

public abstract class FilteredQuerySource implements QuerySource
{
  protected List<HDT_Base> list = new ArrayList<>();
  private boolean generated = false;
  protected HyperTableCell op1, op2, op3;
  protected int query;
  protected QueryType queryType;
  protected HyperDB db;
  
  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { init(queryType, query, op1, op2, op3); }
  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1, HyperTableCell op2)                     { init(queryType, query, op1, op2, null); }
  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1)                                         { init(queryType, query, op1, null, null); }
  public FilteredQuerySource(QueryType queryType, int query)                                                             { init(queryType, query, null, null, null); }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  private void init(QueryType queryType, int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    this.queryType = queryType;
    this.query = query;
    this.op1 = op1;
    this.op2 = op2;
    this.op3 = op3;
    this.db = HyperDB.db;
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  protected abstract void runFilter();
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  protected void ensureGenerated()
  {
    if (!generated)
    {
      runFilter();
      generated = true;
    }   
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public int count()
  {
    ensureGenerated();
    return list.size();
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void addAllTo(Set<HDT_Base> filteredRecords)
  {
    ensureGenerated();
    filteredRecords.addAll(list);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public HyperTableCell getCell(int ndx)
  {
    ensureGenerated();
    return new HyperTableCell(list.get(ndx));
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public HDT_Base getRecord(int ndx)
  {
    ensureGenerated();
    return list.get(ndx);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public boolean containsCell(HyperTableCell cell)
  {
    ensureGenerated();
    return list.contains(HyperTableCell.getRecord(cell));
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public boolean containsRecord(HDT_Base record)
  {
    ensureGenerated();
    return list.contains(record);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public HDT_RecordType recordType()
  {
    switch (queryType)
    {
      case qtPersons :       return hdtPerson;
      case qtAllRecords:     return hdtNone;
      case qtArguments:      return hdtArgument;
      case qtDebates:        return hdtDebate;
      case qtFiles:          return hdtMiscFile;
      case qtInstitutions:   return hdtInstitution;
      case qtInvestigations: return hdtInvestigation;
      case qtNotes:          return hdtNote;
      case qtPositions:      return hdtPosition;
      case qtConcepts:       return hdtConcept;
      case qtWorks:          return hdtWork;
      default: break;
    }
    
    return null;
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public QuerySourceType sourceType()
  {
    return QuerySourceType.QST_filteredRecords;
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

}
