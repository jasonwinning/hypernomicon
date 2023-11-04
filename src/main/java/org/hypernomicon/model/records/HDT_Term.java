/*
 * Copyright 2015-2023 Jason Winning
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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;
import java.util.Iterator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HDI_OfflinePointerMulti;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.unities.MainText;

public class HDT_Term extends HDT_RecordBase implements HDT_RecordWithDescription
{
  public final HyperObjList<HDT_Term, HDT_Concept> concepts;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Term(RecordState xmlState, HyperDataset<HDT_Term> dataset)
  {
    super(xmlState, dataset, tagTerm);

    concepts = getObjList(rtConceptOfTerm);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()       { return name(); }
  @Override public final boolean hasDesc() { return true; }
  @Override public MainText getDesc()      { return concepts.get(0).getMainText(); }

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
      db.deleteRecord(concept);
    }

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<HDT_Glossary> getGlossaries()
  {
    return concepts.stream().map(concept -> concept.glossary.get()).distinct().collect(Collectors.toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean hasMultipleSensesInGlossary(HDT_Glossary glossary)
  {
    return concepts.stream().filter(concept -> concept.glossary.get() == glossary).count() > 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Instant getModifiedDate()
  {
    return concepts.stream().map(HDT_Concept::getModifiedDate)
                            .reduce(super.getModifiedDate(), BinaryOperator.maxBy(Instant::compareTo));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Concept getConcept(HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    return findFirst(concepts, concept -> (concept.glossary.get() == glossary) && (concept.sense.get() == sense));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Term create(HDT_Glossary glossary)
  {
    HDT_Term term = db.createNewBlankRecord(hdtTerm);
    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);
    term.concepts.add(concept);
    concept.glossary.set(glossary);

    return term;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError
  {
    if (isOnline())
    {
      Set<Integer> offlineIDs = new HashSet<>(((HDI_OfflinePointerMulti)backupState.items.get(tagConcept)).getObjIDs()),
                   onlineIDs = concepts.stream().map(HDT_Record::getID).collect(Collectors.toCollection(HashSet::new));

      if ((offlineIDs.containsAll(onlineIDs) && onlineIDs.containsAll(offlineIDs)) == false)
        throw new ConceptChangedException();
    }

    super.restoreTo(backupState, rebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
