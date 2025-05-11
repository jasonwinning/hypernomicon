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

package org.hypernomicon.model.records;

import java.util.List;

import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.*;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;

//---------------------------------------------------------------------------

public final class SimpleRecordTypes
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SimpleRecordTypes() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  static abstract class HDT_SimpleRecord extends HDT_RecordBase
  {
    HDT_SimpleRecord(RecordState xmlState, DatasetAccessor<? extends HDT_SimpleRecord> dataset)
    {
      super(xmlState, dataset);
    }

    @Override public String listName() { return name(); }

    @Override public void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError
    {
      setNameInternal(backupState.simpleName, false);

      super.restoreTo(backupState, rebuildMentions);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static abstract class HDT_Verdict extends HDT_SimpleRecord
  {
    private HDT_Verdict(RecordState xmlState, DatasetAccessor<? extends HDT_Verdict> dataset)      { super(xmlState, dataset); }

    @Override public String getCBText() { return name(); }
    @Override public String listName()  { return getTagString(tagListName); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class HDT_Country extends HDT_SimpleRecord
  { public HDT_Country(RecordState xmlState, DatasetAccessor<HDT_Country> dataset)                 { super(xmlState, dataset);  } }

  public static final class HDT_Rank extends HDT_SimpleRecord
  { public HDT_Rank(RecordState xmlState, DatasetAccessor<HDT_Rank> dataset)                       { super(xmlState, dataset);  } }

  public static final class HDT_PersonStatus extends HDT_SimpleRecord
  { public HDT_PersonStatus(RecordState xmlState, DatasetAccessor<HDT_PersonStatus> dataset)       { super(xmlState, dataset);  } }

  public static final class HDT_Field extends HDT_SimpleRecord
  { public HDT_Field(RecordState xmlState, DatasetAccessor<HDT_Field> dataset)                     { super(xmlState, dataset);  } }

  public static final class HDT_PositionVerdict extends HDT_Verdict
  { public HDT_PositionVerdict(RecordState xmlState, DatasetAccessor<HDT_PositionVerdict> dataset) { super(xmlState, dataset);  } }

  public static final class HDT_ArgumentVerdict extends HDT_Verdict
  { public HDT_ArgumentVerdict(RecordState xmlState, DatasetAccessor<HDT_ArgumentVerdict> dataset) { super(xmlState, dataset);  } }

  public static final class HDT_InstitutionType extends HDT_SimpleRecord
  { public HDT_InstitutionType(RecordState xmlState, DatasetAccessor<HDT_InstitutionType> dataset) { super(xmlState, dataset);  } }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class HDT_FileType extends HDT_SimpleRecord
  {
    public final List<HDT_MiscFile> miscFiles;

    public HDT_FileType(RecordState xmlState, DatasetAccessor<HDT_FileType> dataset)
    {
      super(xmlState, dataset);
      miscFiles = getSubjList(rtTypeOfFile);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class HDT_ConceptSense extends HDT_SimpleRecord
  {
    public final List<HDT_Concept> concepts;

    public HDT_ConceptSense(RecordState xmlState, DatasetAccessor<HDT_ConceptSense> dataset)
    {
      super(xmlState, dataset);
      concepts = getSubjList(rtSenseOfConcept);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum WorkTypeEnum
  {
    wtNone, wtPaper, wtBook, wtWebPage, wtChapter, wtRecording, wtUnenteredSet, wtThesis
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class HDT_WorkType extends HDT_SimpleRecord
  {
    private static final EnumHashBiMap<WorkTypeEnum, Integer> enumMap = initEnumMap();

    private static EnumHashBiMap<WorkTypeEnum, Integer> initEnumMap()
    {
      EnumHashBiMap<WorkTypeEnum, Integer> map = EnumHashBiMap.create(WorkTypeEnum.class);

      map.put(wtNone   , -1); map.put(wtPaper    , 1); map.put(wtBook        , 2); map.put(wtWebPage, 3);
      map.put(wtChapter,  4); map.put(wtRecording, 5); map.put(wtUnenteredSet, 6); map.put(wtThesis , 7);

      return map;
    }

    public HDT_WorkType(RecordState xmlState, DatasetAccessor<HDT_WorkType> dataset) { super(xmlState, dataset); }

    public WorkTypeEnum enumVal() { return getEnumVal(this); }

    public static WorkTypeEnum getEnumVal(HDT_WorkType wt)   { return wt == null ? wtNone : workTypeIDToEnumVal(wt.getID()); }
    public static HDT_WorkType get(WorkTypeEnum enumVal)     { return db.workTypes.getByID(enumMap.getOrDefault(enumVal, -1)); }
    public static int getIDbyEnum(WorkTypeEnum enumVal)      { return enumMap.getOrDefault(enumVal, -1); }
    public static WorkTypeEnum workTypeIDToEnumVal(int wtID) { return enumMap.inverse().getOrDefault(wtID, wtNone); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
