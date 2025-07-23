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

package org.hypernomicon.query;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.*;

import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public enum QueryType
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  qtAllRecords    (1,  "Any records"           , hdtNone         ),
  qtReport        (12, "Report"                , hdtNone         ),
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
  qtFolders       (13, "Folders"               , hdtFolder       );

//---------------------------------------------------------------------------

  private final int code;
  private final String caption;
  private final RecordType recordType;
  private static final Map<Integer, QueryType> codeToValMap = new HashMap<>();
  private static final EnumMap<RecordType, QueryType> recordTypeToQueryType;

//---------------------------------------------------------------------------

  QueryType(int code, String caption, RecordType recordType)
  {
    this.code = code;
    this.caption = caption;
    this.recordType = recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static
  {
    recordTypeToQueryType = new EnumMap<>(RecordType.class);

    EnumSet.allOf(QueryType.class).forEach(type ->
    {
      if (type != qtReport)
        recordTypeToQueryType.put(type.recordType, type);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getCode()              { return code; }
  public String getCaption()        { return caption; }
  public RecordType getRecordType() { return recordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static QueryType codeToVal(int num)
  {
    if (codeToValMap.isEmpty())
      EnumSet.allOf(QueryType.class).forEach(val -> codeToValMap.put(val.code, val));

    return codeToValMap.get(num);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static QueryType fromRecordType(RecordType recordType)
  {
    return recordType == null ? qtAllRecords : recordTypeToQueryType.getOrDefault(recordType, qtAllRecords);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
