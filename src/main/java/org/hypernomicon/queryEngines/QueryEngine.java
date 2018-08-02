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

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.querySources.QuerySource;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

@SuppressWarnings("unused")
public abstract class QueryEngine<HDT_T extends HDT_Base>
{
  public static enum QueryType
  {
    qtAllRecords(1),
    qtPersons(2),
    qtWorks(3),
    qtInstitutions(4),
    qtInvestigations(5),
    qtDebates(6),
    qtPositions(7),
    qtArguments(8),
    qtNotes(9),
    qtFiles(10),
    qtConcepts(11),
    qtReport(12), 
    qtNone(13);
    
    private int code;  
  
    private QueryType(int code) { this.code = code; }  
  
    public int getCode() { return code; }
    
    public static QueryType codeToVal(int num)
    {
      for (QueryType val : QueryType.values()) if (val.getCode() == num) return val;
      return null;
    }
    
    public static QueryType fromRecordType(HDT_RecordType recordType)
    {
      if (recordType == null) return qtAllRecords;
      
      switch (recordType)
      {
        case hdtArgument:       return qtArguments;
        case hdtDebate:         return qtDebates;
        case hdtMiscFile:       return qtFiles;
        case hdtInstitution:    return qtInstitutions;
        case hdtInvestigation:  return qtInvestigations;
        case hdtNote:           return qtNotes;
        case hdtPerson:         return qtPersons;
        case hdtPosition:       return qtPositions;
        case hdtConcept:        return qtConcepts;
        case hdtWork:           return qtWorks;
        default:                return qtAllRecords;        
      }
    }
  }
  
  public abstract QueryType getQueryType();
  
  public abstract void addQueries(QueryPopulator pop, HyperTableRow row);
  
  public abstract void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3);
  
  public abstract boolean evaluate(HDT_T record, boolean firstCall, boolean lastCall);
    
  public void op1Change(int query, HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { }
  
  public void op2Change(int query, HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { }
  
  public abstract QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3);

  public abstract boolean needsMentionsIndex(int query);
}
