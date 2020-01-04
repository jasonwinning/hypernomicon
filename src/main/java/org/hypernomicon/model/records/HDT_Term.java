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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.MainText;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.relations.HyperObjList;

public class HDT_Term extends HDT_RecordBase implements HDT_RecordWithDescription
{
  public final HyperObjList<HDT_Term, HDT_Concept> concepts;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Term(HDT_RecordState xmlState, HyperDataset<HDT_Term> dataset)
  {
    super(xmlState, dataset, tagTerm);

    concepts = getObjList(rtConceptOfTerm);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()  { return name(); }
  @Override public MainText getDesc() { return concepts.get(0).getMainText(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    Iterator<HDT_Concept> it = concepts.iterator();  // This algorithm is different from
                                                     // HDT_Institution.expire() because
    while (it.hasNext())                             // concepts are the objects in the
    {                                                // rtConceptOfTerm relation
      HDT_Concept concept = it.next();
      it.remove();
      db.deleteRecord(hdtConcept, concept.getID());
    }

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<HDT_Glossary> getGlossaries()
  {
    return concepts.stream().map(concept -> concept.glossary.get()).collect(Collectors.toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Concept getConcept(HDT_Glossary glossary)
  {
    return findFirst(concepts, concept -> concept.glossary.get() == glossary);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
