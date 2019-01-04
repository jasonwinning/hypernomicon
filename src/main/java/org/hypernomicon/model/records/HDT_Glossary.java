/*
 * Copyright 2015-2019 Jason Winning
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

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.relations.HyperSubjList;

public class HDT_Glossary extends HDT_Record
{
  public final List<HDT_Glossary> parentGlossaries;
  public final HyperSubjList<HDT_Glossary, HDT_Concept> concepts;

  public HDT_Glossary(HDT_RecordState xmlState, HyperDataset<HDT_Glossary> dataset)
  {
    super(xmlState, dataset, tagName);

    parentGlossaries = getObjList(rtParentGlossaryOfGlossary);
    concepts = getSubjList(rtGlossaryOfConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()        { return name(); }
  @Override public HDT_RecordType getType() { return hdtGlossary; }

  public boolean getActive()              { return getTagBoolean(tagActive); }
  public void setActive(boolean newValue) { updateTagBoolean(tagActive, newValue); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
