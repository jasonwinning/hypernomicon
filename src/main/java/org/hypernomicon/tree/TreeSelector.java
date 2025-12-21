/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.tree;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.view.MainCtrlr;

//---------------------------------------------------------------------------

public class TreeSelector
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record TreeTargetType(RelationType relType, RecordType targetType) { }

//---------------------------------------------------------------------------

  private HDT_Record base, target;
  private final List<TreeTargetType> targetTypes = new ArrayList<>();
  private boolean baseIsSubj = true;

  public TreeSelector()        { clear(); }

  public HDT_Record getBase()  { return base; }
  private HDT_Record getSubj() { return baseIsSubj ? base : target; }

  public void setTarget(HDT_Record target) { this.target = target; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    base = null;
    target = null;
    targetTypes.clear();
    baseIsSubj = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset(HDT_Record base, boolean baseIsSubj)
  {
    clear();

    this.base = base;
    this.baseIsSubj = baseIsSubj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addTargetType(RecordType targetType)
  {
    RelationType relType = (base.getType() == hdtTerm) && (targetType == hdtGlossary) ?
      rtGlossaryOfConcept
    :
      (baseIsSubj ? getRelation(base.getType(), targetType, true) : getRelation(targetType, base.getType(), true));

    targetTypes.add(new TreeTargetType(relType, targetType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void uniting(HDT_Record base, RecordType targetType)
  {
    clear();

    this.base = base;

    targetTypes.add(new TreeTargetType(rtUnited, targetType));
    if (targetType == hdtDebate)
      targetTypes.add(new TreeTargetType(rtUnited, hdtPosition));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RelationType getRelTypeForTargetType(RecordType targetType)
  {
    return findFirst(targetTypes, ttType -> ttType.targetType == targetType, rtNone, ttType -> ttType.relType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTypesStr()
  {
    int lastNdx = targetTypes.size() - 1;
    String msg = "";

    for (int ndx = 0; ndx <= lastNdx; ndx++)
    {
      String typeName = getTypeName(targetTypes.get(ndx).targetType);

      if (typeName.endsWith("Label"))
        typeName = "Label";

      msg += typeName;

      if      ((ndx == 0) && (lastNdx == 1)) msg += " or ";
      else if (ndx == (lastNdx - 1))         msg += ", or ";
      else if (ndx < lastNdx)                msg += ", ";
    }

    return msg;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void attach(HDT_Record subj, HDT_Record obj)
  {
    HDT_Folder folder = ((base != null) && (base.getType() == hdtFolder) && (subj.getType() == hdtNote) && (obj.getType() == hdtNote)) ?
      (HDT_Folder)base
    :
      null;

    reset(subj, true);
    addTargetType(obj.getType());
    if (select(obj) == false)
      return;

    if ((folder != null) && ((HDT_Note) subj).folder.isNull())
      ui.noteHyperTab().assignFolder(folder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean select(HDT_Record record)
  {
    if (base == null)
      return falseWithInternalErrorPopup(91827);

    if (record == null)
      return false;

    switch (getRelTypeForTargetType(record.getType()))
    {
      case rtUnited                 : return selectToUnite      ((HDT_RecordWithMainText) record);
      case rtGlossaryOfConcept      : return selectGlossary     ((HDT_Glossary          ) record);
      case rtParentConceptOfConcept : return selectParentConcept((HDT_Concept           ) record);

      case rtNone                   : return falseWithErrorPopup("You must select a record of type: " + getTypesStr() + '.');

      default : break;
    }

    RecordTreeEdge newEdge = baseIsSubj ? new RecordTreeEdge(record, base) : new RecordTreeEdge(base, record),
                   oldEdge = ((target == null) || (base == null)) ?
                     null
                   :
                     baseIsSubj ? new RecordTreeEdge(target, base) : new RecordTreeEdge(base, target);

    if (newEdge.attach(oldEdge) == false)
      return false;

    target = record;

    ui.goToRecord(getSubj(), false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean selectParentConcept(HDT_Concept parentConcept)
  {
    HDT_Concept childConcept = (HDT_Concept) base;

    if (childConcept.parentConcepts.contains(parentConcept) == false)
    {
      if (childConcept.glossary.get() != parentConcept.glossary.get())
        return falseWithErrorPopup("A concept can only be another concept's parent if they are in the same glossary.");

      try { childConcept.addParentConcept(parentConcept); }
      catch (RelationCycleException e)
      {
        return falseWithErrorPopup("Unable to add parent concept: A cycle would result.");
      }
    }

    ui.goToRecord(childConcept, false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean selectGlossary(HDT_Glossary glossary)
  {
    if (base instanceof HDT_Term term)
    {
      // The user clicked Browse in the Terms tab on a blank row

      ui.goToRecord(term, false);
      ui.termHyperTab().addGlossaryToNextBlankRow(glossary);
      return true;
    }

    if ((base instanceof HDT_Concept) == false)
      return false;

    HDT_Concept concept = (HDT_Concept) base,
                otherConcept = concept.term.get().getConcept(glossary, concept.sense.get());

    if (glossary != concept.glossary.get())
    {
      if ((otherConcept != null) && (concept != otherConcept))
        return falseWithErrorPopup("The term is already in that glossary.");

      if (concept.replaceGlossaryInteractive(glossary) == false)
        return false;
    }

    ui.goToRecord(concept, false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean selectToUnite(HDT_RecordWithMainText record2)
  {
    StringBuilder sb = new StringBuilder();

    HDT_RecordWithMainText record1 = (HDT_RecordWithMainText) base;

    if (HDT_Hub.canUnite(record1, record2, sb) == false)
      return falseWithErrorPopup(sb.toString());

    try
    {
      if (MainCtrlr.uniteRecords(record1, record2) == false)
        return false;
    }
    catch (HyperDataException e)
    {
      return falseWithErrorPopup(e.getMessage());
    }

    ui.goToRecord(record1, false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
