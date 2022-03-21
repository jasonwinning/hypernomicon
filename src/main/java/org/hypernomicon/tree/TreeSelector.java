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

package org.hypernomicon.tree;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Term;
import org.hypernomicon.model.unities.HDT_RecordWithConnector;

public class TreeSelector
{
  private HDT_Record base, target;
  private final List<TreeTargetType> targetTypes = new ArrayList<>();
  private boolean baseIsSubj = true;

  public TreeSelector()        { reset(); }

  public HDT_Record getBase()  { return base; }
  private HDT_Record getSubj() { return baseIsSubj ? base : target; }

  public void setTarget(HDT_Record target) { this.target = target; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset()
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
    reset();
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
      (baseIsSubj ? getRelation(base.getType(), targetType) : getRelation(targetType, base.getType()));

    targetTypes.add(new TreeTargetType(relType, targetType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void linking(HDT_Record base, RecordType targetType)
  {
    reset();

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

  private static class TreeTargetType
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

  private String getTypesStr()
  {
    int lastNdx = targetTypes.size() - 1;
    String msg = "";

    for (int ndx = 0; ndx <= lastNdx; ndx++)
    {
      msg += db.getTypeName(targetTypes.get(ndx).targetType);

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
    if (select(obj, true) == false)
      return;

    if ((folder != null) && ((HDT_Note) subj).folder.isNull())
      ui.noteHyperTab().assignFolder(folder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean select(HDT_Record record, boolean showErrMsg)
  {
    if (base == null)
      return falseWithErrMsgCond(showErrMsg, "Internal error #91827");

    if (record == null) return false;

    RelationType relType = getRelTypeForTargetType(record.getType());

    if (relType == rtNone)
      return falseWithErrMsgCond(showErrMsg, "You must select a record of type: " + getTypesStr() + ".");

    if (relType == rtUnited)
      return selectToUnite((HDT_RecordWithConnector) record, showErrMsg);

    if (relType == rtGlossaryOfConcept)
    {
      if (glossaryChecks((HDT_Glossary) record, showErrMsg) == false)
        return false;

      if (base.getType() == hdtTerm)
      {
        ui.goToRecord(base, false);
        ui.termHyperTab().addGlossary((HDT_Glossary) record);
        ui.update();
        return true;
      }
    }

    RecordTreeEdge newEdge = baseIsSubj ? new RecordTreeEdge(record, base) : new RecordTreeEdge(base, record),
                   oldEdge = ((target == null) || (base == null)) ?
                     null
                   :
                     baseIsSubj ? new RecordTreeEdge(target, base) : new RecordTreeEdge(base, target);

    if (newEdge.attach(oldEdge, showErrMsg) == false)
      return false;

    target = record;

    ui.goToRecord(getSubj(), false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean glossaryChecks(HDT_Glossary glossary, boolean showErrMsg)
  {
    if (base.getType() == hdtTerm)
    {
      HDT_Term term = (HDT_Term) base;

      if (term.getConcept(glossary) != null)
        return falseWithErrMsgCond(showErrMsg, "The term is already in that glossary.");

      return true;
    }

    HDT_Concept concept = (HDT_Concept) base,
                otherConcept = concept.term.get().getConcept(glossary);

    if ((otherConcept != null) && (concept != otherConcept))
      return falseWithErrMsgCond(showErrMsg, "The term is already in that glossary.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean selectToUnite(HDT_RecordWithConnector record2, boolean showErrMsg)
  {
    HDT_RecordWithConnector record1 = (HDT_RecordWithConnector) base;

    if (record2.getType() == record1.getType())
      return falseWithErrMsgCond(showErrMsg, "You cannot connect records of the same type.");

    if (HyperDB.isUnstoredRecord(record1.getID(), record1.getType()))
      return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " record cannot be connected to another record.");

    if (HyperDB.isUnstoredRecord(record2.getID(), record2.getType()))
      return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " record cannot be connected to another record.");

    if (record2.hasHub())
    {
      if (record2.getHub().getSpoke(record1.getType()) != null)
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " record is already connected to a " + db.getTypeName(record1.getType()) + " record.");

      if ((record1.getType() == hdtDebate) && (record2.getHub().getSpoke(hdtPosition) != null))
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " record is already connected to a " + db.getTypeName(hdtPosition) + " record.");

      if ((record1.getType() == hdtPosition) && (record2.getHub().getSpoke(hdtDebate) != null))
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " record is already connected to a " + db.getTypeName(hdtDebate) + " record.");

      if (record1.hasHub())
        return falseWithErrMsgCond(showErrMsg, "Both records are already united with other records.");
    }

    if (record1.hasHub())
    {
      if (record1.getHub().getSpoke(record2.getType()) != null)
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " record is already connected to a " + db.getTypeName(record2.getType()) + " record.");

      if ((record2.getType() == hdtDebate) && (record1.getHub().getSpoke(hdtPosition) != null))
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " record is already connected to a " + db.getTypeName(hdtPosition) + " record.");

      if ((record2.getType() == hdtPosition) && (record1.getHub().getSpoke(hdtDebate) != null))
        falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " record is already connected to a " + db.getTypeName(hdtDebate) + " record.");
    }

    ui.uniteRecords(record1, record2, showErrMsg == false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
