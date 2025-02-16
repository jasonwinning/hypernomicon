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

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class TreeSelector
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record base, target;
  private final List<TreeTargetType> targetTypes = new ArrayList<>();
  private HyperTableRow tableRow;
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
    tableRow = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset(HDT_Record base, boolean baseIsSubj)
  {
    reset(base, baseIsSubj, null);
  }

  public void reset(HDT_Record base, boolean baseIsSubj, HyperTableRow tableRow)
  {
    clear();

    this.base = base;
    this.baseIsSubj = baseIsSubj;
    this.tableRow = tableRow;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addTargetType(RecordType targetType)
  {
    RelationType relType;

    if ((base.getType() == hdtTerm) && (targetType == hdtGlossary))
      relType = rtGlossaryOfConcept;
    else if ((base.getType() == hdtTerm) && (targetType == hdtConcept))
      relType = rtParentConceptOfConcept;
    else
      relType = (baseIsSubj ? getRelation(base.getType(), targetType, true) : getRelation(targetType, base.getType(), true));

    targetTypes.add(new TreeTargetType(relType, targetType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void linking(HDT_Record base, RecordType targetType)
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

  private static final class TreeTargetType
  {
    private TreeTargetType(RelationType relType, RecordType objType)
    {
      this.relType = relType;
      this.targetType = objType;
    }

    private final RelationType relType;
    private final RecordType targetType;
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
      return falseWithErrorPopup(new HDB_InternalError(91827));

    if (record == null) return false;

    RelationType relType = getRelTypeForTargetType(record.getType());

    if (relType == rtNone)
      return falseWithErrorPopup("You must select a record of type: " + getTypesStr() + '.');

    if (relType == rtUnited)
      return selectToUnite((HDT_RecordWithMainText) record);

    if (relType == rtGlossaryOfConcept)
    {
      if (glossaryChecks((HDT_Glossary) record) == false)
        return false;

      ui.termHyperTab().selectFromTree(tableRow, (HDT_Glossary) record, ((HDT_Concept) base).sense.get(), null);
      ui.goToRecord(ui.termHyperTab().viewRecord(), false);
      return true;
    }

    if (relType == rtParentConceptOfConcept)
    {
      HDT_Concept parentConcept = (HDT_Concept) record;

      ui.termHyperTab().selectFromTree(tableRow, parentConcept.glossary.get(), ((HDT_Concept) base).sense.get(), parentConcept);
      ui.goToRecord(ui.termHyperTab().viewRecord(), false);
      return true;
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

  private boolean glossaryChecks(HDT_Glossary glossary)
  {

    HDT_Concept concept = (HDT_Concept) base,
                otherConcept = concept.term.get().getConcept(glossary, concept.sense.get());

    if ((otherConcept != null) && (concept != otherConcept))
      return falseWithErrorPopup("The term is already in that glossary.");

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
      if (ui.uniteRecords(record1, record2) == false)
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
