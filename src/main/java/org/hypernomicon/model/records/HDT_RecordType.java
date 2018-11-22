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

package org.hypernomicon.model.records;

import org.hypernomicon.model.records.SimpleRecordTypes.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

public enum HDT_RecordType
{
  hdtNone           (HDT_Base.class),
  
  hdtFolder         (HDT_Folder.class),  // Folders must be brought online first. See HyperPath.assignNameInternal
  hdtPerson         (HDT_Person.class),
  hdtPersonStatus   (HDT_PersonStatus.class),
  hdtPersonGroup    (HDT_PersonGroup.class),
  hdtRank           (HDT_Rank.class),
  hdtInstitution    (HDT_Institution.class),
  hdtInstitutionType(HDT_InstitutionType.class),
  hdtInvestigation  (HDT_Investigation.class),
  hdtDebate         (HDT_Debate.class),
  hdtArgument       (HDT_Argument.class),
  hdtPosition       (HDT_Position.class),
  hdtTerm           (HDT_Term.class),
  hdtConcept        (HDT_Concept.class),
  hdtField          (HDT_Field.class),
  hdtSubfield       (HDT_Subfield.class),
  hdtWorkFile       (HDT_WorkFile.class),
  hdtMiscFile       (HDT_MiscFile.class),
  hdtWork           (HDT_Work.class),
  hdtWorkType       (HDT_WorkType.class),
  hdtWorkLabel      (HDT_WorkLabel.class),
  hdtState          (HDT_State.class),
  hdtCountry        (HDT_Country.class),
  hdtPositionVerdict(HDT_PositionVerdict.class),
  hdtArgumentVerdict(HDT_ArgumentVerdict.class),
  hdtFileType       (HDT_FileType.class),
  hdtNote           (HDT_Note.class),
  hdtGlossary       (HDT_Glossary.class),
  
  hdtAuxiliary      (HDT_Base.class),

  hdtHub            (HDT_Hub.class);

//---------------------------------------------------------------------------

  private final Class<? extends HDT_Base> klass;
  private final boolean simple, gotConnector, disregardDates;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private <T extends HDT_Base> HDT_RecordType(Class<T> klass)
  {
    this.klass = klass;
    simple = HDT_SimpleRecord.class.isAssignableFrom(klass);
    gotConnector = HDT_RecordWithConnector.class.isAssignableFrom(klass);
    disregardDates = simple || (klass == HDT_Subfield.class) || (klass == HDT_Hub.class);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public Class<? extends HDT_Base> getRecordClass() { return klass; }  
  public boolean isSimple()                         { return simple; }
  public boolean hasConnector()                     { return gotConnector; }
  public boolean getDisregardDates()                { return disregardDates; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

}

