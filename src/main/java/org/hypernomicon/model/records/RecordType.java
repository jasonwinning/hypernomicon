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

package org.hypernomicon.model.records;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithConnector;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public enum RecordType
{
  hdtNone           (HDT_Record         .class),

  hdtFolder         (HDT_Folder         .class),  // Folders must be brought online first. See HyperPath.assignNameInternal
  hdtPerson         (HDT_Person         .class),
  hdtPersonStatus   (HDT_PersonStatus   .class),
  hdtPersonGroup    (HDT_PersonGroup    .class),
  hdtRank           (HDT_Rank           .class),
  hdtInstitution    (HDT_Institution    .class),
  hdtInstitutionType(HDT_InstitutionType.class),
  hdtInvestigation  (HDT_Investigation  .class),
  hdtDebate         (HDT_Debate         .class),
  hdtArgument       (HDT_Argument       .class),
  hdtPosition       (HDT_Position       .class),
  hdtTerm           (HDT_Term           .class),
  hdtConcept        (HDT_Concept        .class),
  hdtField          (HDT_Field          .class),
  hdtSubfield       (HDT_Subfield       .class),
  hdtWorkFile       (HDT_WorkFile       .class),
  hdtWork           (HDT_Work           .class),
  hdtMiscFile       (HDT_MiscFile       .class),
  hdtWorkType       (HDT_WorkType       .class),
  hdtWorkLabel      (HDT_WorkLabel      .class),
  hdtRegion         (HDT_Region         .class),
  hdtCountry        (HDT_Country        .class),
  hdtPositionVerdict(HDT_PositionVerdict.class),
  hdtArgumentVerdict(HDT_ArgumentVerdict.class),
  hdtFileType       (HDT_FileType       .class),
  hdtNote           (HDT_Note           .class),
  hdtGlossary       (HDT_Glossary       .class),

  hdtAuxiliary      (HDT_Record         .class),

  hdtHub            (HDT_Hub            .class);

//---------------------------------------------------------------------------

  private final Class<? extends HDT_Record> klass;
  private final boolean simple, gotConnector, disregardDates;
  private final static Map<Class<? extends HDT_Record>, RecordType> classToType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  <T extends HDT_Record> RecordType(Class<T> klass)
  {
    this.klass = klass;
    simple = HDT_SimpleRecord.class.isAssignableFrom(klass);
    gotConnector = HDT_RecordWithConnector.class.isAssignableFrom(klass);
    disregardDates = simple || (klass == HDT_Subfield.class) || (klass == HDT_Region.class) || (klass == HDT_Hub.class);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static
  {
    classToType = new HashMap<>();
    EnumSet.allOf(RecordType.class).forEach(type -> classToType.put(type.klass, type));
    classToType.put(HDT_Record.class, hdtNone);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Class<? extends HDT_Record> getRecordClass() { return klass; }
  public boolean isSimple()                           { return simple; }
  public boolean hasConnector()                       { return gotConnector; }
  public boolean getDisregardDates()                  { return disregardDates; }

  public static RecordType typeByRecordClass(Class<? extends HDT_Record> klass) { return classToType.getOrDefault(klass, hdtNone); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}

