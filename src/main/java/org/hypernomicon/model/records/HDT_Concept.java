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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.Collections;
import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.model.Exceptions.DuplicateSearchKeyException;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.SearchKeyTooShortException;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjPointer;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

public class HDT_Concept extends HDT_RecordWithMainText
{
  public final List<HDT_Concept> subConcepts, parentConcepts;
  public final HyperSubjPointer<HDT_Term, HDT_Concept> term;
  public final HyperObjPointer<HDT_Concept, HDT_Glossary> glossary;
  public final HyperObjPointer<HDT_Concept, HDT_ConceptSense> sense;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Concept(RecordState xmlState, HyperDataset<HDT_Concept> dataset)
  {
    super(xmlState, dataset, tagName);

    term = getSubjPointer(rtConceptOfTerm);
    glossary = getObjPointer(rtGlossaryOfConcept);
    sense = getObjPointer(rtSenseOfConcept);
    parentConcepts = Collections.unmodifiableList(getObjList(rtParentConceptOfConcept));
    subConcepts = getSubjList(rtParentConceptOfConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String name()                           { return term.isNull() ? "" : term.get().name(); }
  @Override public String getSearchKey()                   { return term.get().getSearchKey(); }
  @Override public Iterable<SearchKeyword> getSearchKeys() { return db.getKeysByRecord(term.get()); }
  @Override public String getCBText()                      { return extendedName(); }
  @Override public String getXMLObjectName()               { return extendedName(); }
  @Override public String getNameEngChar()                 { return term.get().getNameEngChar(); }
  @Override public String firstActiveKeyWord()             { return term.get().firstActiveKeyWord(); }
  @Override public void setName(String str)                { term.get().setName(str); }
  @Override public String listName()                       { return sense.isNull() ? name() : (name() + " (" + sense.get().name() + ')'); }
  @Override public final boolean isUnitable()              { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setNameInternal(String str, boolean update)
  {
    if (term.isNotNull())
      term.get().setNameInternal(str, update);

    super.setNameInternal(str, update);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String extendedName()
  {
    String glossaryText = glossary.isNull() || ((glossary.get().getID() == 1) && (term.get().getGlossaries().size() == 1)) ?
      ""
    :
      glossary.get().name();

    String senseText = sense.isNull() ? "" : sense.get().name();

    return glossaryText.isBlank() ?
      (senseText.isBlank() ? name() : name() + " (" + senseText + ')')
    :
      (name() + " (" + glossaryText + (senseText.isBlank() ? ")" : (", " + senseText + ')')));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setSearchKey(String newKey) throws DuplicateSearchKeyException, SearchKeyTooShortException
  {
    if (term.isNotNull())
      term.get().setSearchKey(newKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setSearchKey(String newKey, boolean noMod, boolean rebuildMentions) throws DuplicateSearchKeyException, SearchKeyTooShortException
  {
    if (term.isNotNull())
      term.get().setSearchKey(newKey, noMod, rebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeParent(HDT_Concept parentConcept)
  {
    HyperObjList<HDT_Concept, HDT_Concept> modifiableParents = getObjList(rtParentConceptOfConcept);
    modifiableParents.remove(parentConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addParentConcept(HDT_Concept parentConcept) throws RelationCycleException
  {
    HyperObjList<HDT_Concept, HDT_Concept> modifiableParents = getObjList(rtParentConceptOfConcept);
    if (modifiableParents.add(parentConcept) == false)
      modifiableParents.throwLastException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Concept addNewSubConcept()
  {
    HDT_Term newTerm = HDT_Term.create(glossary.get());
    HDT_Concept childConcept = newTerm.getConcept(glossary.get(), null);

    try { childConcept.addParentConcept(this); } catch (RelationCycleException e) { throw new AssertionError(e.getMessage(), e); }

    return childConcept;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
