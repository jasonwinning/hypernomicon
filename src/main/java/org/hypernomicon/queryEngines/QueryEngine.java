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

package org.hypernomicon.queryEngines;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.querySources.QuerySource;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.queryEngines.QueryEngine.QueryType.*;

@SuppressWarnings("unused")
public abstract class QueryEngine<HDT_T extends HDT_Record>
{
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum QueryType
  {
    qtAllRecords    (1,  "Any records"           , hdtNone         ),
    qtPersons       (2,  "Person records"        , hdtPerson       ),
    qtWorks         (3,  "Work records"          , hdtWork         ),
    qtInstitutions  (4,  "Institution records"   , hdtInstitution  ),
    qtInvestigations(5,  "Investigation records" , hdtInvestigation),
    qtDebates       (6,  "Problem/debate records", hdtDebate       ),
    qtPositions     (7,  "Position records"      , hdtPosition     ),
    qtArguments     (8,  "Argument records"      , hdtArgument     ),
    qtNotes         (9,  "Note records"          , hdtNote         ),
    qtFiles         (10, "Misc. file records"    , hdtMiscFile     ),
    qtConcepts      (11, "Concept records"       , hdtConcept      ),
    qtReport        (12, "Report"                , hdtNone         );

    //---------------------------------------------------------------------------

    private final int code;
    private final String caption;
    private final HDT_RecordType recordType;
    private static final Map<Integer, QueryType> codeToValMap = new HashMap<>();
    private static final EnumMap<QueryType, HDT_RecordType> queryTypeToRecordType;
    private static final EnumMap<HDT_RecordType, QueryType> recordTypeToQueryType;

    private QueryType(int code, String caption, HDT_RecordType recordType)
    {
      this.code = code;
      this.caption = caption;
      this.recordType = recordType;
    }

    static
    {
      queryTypeToRecordType = new EnumMap<>(QueryType.class);
      recordTypeToQueryType = new EnumMap<>(HDT_RecordType.class);

      EnumSet.allOf(QueryType.class).forEach(type ->
      {
        queryTypeToRecordType.put(type, type.recordType);
        if (type != qtReport)
          recordTypeToQueryType.put(type.recordType, type);
      });
    }

    public int getCode()                  { return code; }
    public String getCaption()            { return caption; }
    public HDT_RecordType getRecordType() { return recordType; }

    //---------------------------------------------------------------------------

    public static QueryType codeToVal(int num)
    {
      if (codeToValMap.isEmpty())
        EnumSet.allOf(QueryType.class).forEach(val -> codeToValMap.put(val.code, val));

      return codeToValMap.get(num);
    }

  //---------------------------------------------------------------------------

    public static QueryType fromRecordType(HDT_RecordType recordType)
    {
      return recordType == null ? qtAllRecords : recordTypeToQueryType.getOrDefault(recordType, qtAllRecords);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract QueryType getQueryType();

  public abstract void addQueries(QueryPopulator pop, HyperTableRow row);

  public abstract void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3);

  public abstract boolean evaluate(HDT_T record, boolean firstCall, boolean lastCall);

  public void cancelled() { }

  public void op1Change(int query, HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { }

  public void op2Change(int query, HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3) { }

  public abstract QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3);

  public abstract boolean needsMentionsIndex(int query);

  public abstract int numOperands(int query);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
