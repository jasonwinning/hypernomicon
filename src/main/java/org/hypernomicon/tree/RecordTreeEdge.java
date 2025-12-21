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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.dialogs.VerdictDlgCtrlr;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText;

//---------------------------------------------------------------------------

class RecordTreeEdge
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final HDT_Record parent, child;
  private final HDT_Record subj, obj;
  final RelationType relType;

//---------------------------------------------------------------------------

  RecordTreeEdge(HDT_Record parent, HDT_Record child)
  {
    this.parent = parent;
    this.child = child;

    RelationType tempRelType = getRelation(child.getType(), parent.getType(), true);

    if (tempRelType != rtNone)
    {
      subj = child;
      obj = parent;
    }
    else
    {
      tempRelType = getRelation(parent.getType(), child.getType(), true);

      if (tempRelType == rtNone)
      {
        subj = null;
        obj = null;
      }
      else
      {
        subj = parent;
        obj = child;
      }
    }

    relType = tempRelType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean mustDetachIfAttaching(RecordTreeEdge otherEdge)
  {
    if ((relType == rtNone) || (relType == rtKeyWork))
      return false;

    if (db.relationIsMulti(relType))
      return false;

    if (otherEdge.relType != relType)
      return false;

    return subj == otherEdge.subj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  RecordTreeEdge edgeToDetach()
  {
    if ((relType == rtNone) || (relType == rtKeyWork) || db.relationIsMulti(relType))
      return null;

    HDT_Record currentObj = db.getObjPointer(relType, subj).get();

    return (currentObj == null) || (currentObj == obj) ? null : new RecordTreeEdge(currentObj, subj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean attach(RecordTreeEdge detaching)
  {
    if (canAttach() == false)
      return false;

    RecordTreeEdge otherDetaching = edgeToDetach();
    if ((otherDetaching != null) && otherDetaching.equals(detaching))
      otherDetaching = null;

    try
    {
      if ((relType == rtPositionOfArgument) || (relType == rtTargetArgOfArg))
      {
        HDT_Argument childArg = (HDT_Argument) subj;

        VerdictDlgCtrlr vdc = new VerdictDlgCtrlr("Select Verdict for " + childArg.name(), obj);

        if (vdc.showModal() == false)
          return false;

        if (obj.getType() == hdtPosition)
          childArg.addPosition((HDT_Position)obj, vdc.hcbVerdict.selectedRecord());
        else if (obj.getType() == hdtArgument)
          childArg.addTargetArg((HDT_Argument)obj, vdc.hcbVerdict.selectedRecord());
      }
      else if (relType == rtKeyWork)
      {
        HDT_RecordWithAuthors<? extends RecordAuthors> kwRecord = (HDT_RecordWithAuthors<? extends RecordAuthors>) subj;

        Set<HDT_RecordWithMainText> mentioners = db.keyWorkMentionerStream(kwRecord, obj.getType()).collect(Collectors.toSet());
        mentioners.add((HDT_RecordWithMainText) obj);
        MainText.setKeyWorkMentioners(kwRecord, mentioners, obj.getType());
      }
      else
      {
        HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, subj, true);
        objList.add(obj);
        objList.throwLastException();
      }

      if (detaching != null)
        detaching.detach();

      if (otherDetaching != null)
        otherDetaching.detach();

      if ((relType == rtWorkOfMiscFile) && ((HDT_MiscFile) subj).work.isNotNull())
        db.getObjectList(rtAuthorOfFile, subj, false).clear();
    }
    catch (RelationCycleException e)
    {
      return falseWithErrorPopup(e);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canAttach()
  {
    if ((relType == rtFolderOfNote) && HyperDB.isUnstoredRecord(subj.getID(), hdtNote))
      return falseWithErrorPopup("A folder cannot be assigned to that record.");

    if ((obj.getID() == subj.getID()) && (obj.getType() == subj.getType()))
      return falseWithErrorPopup("A record cannot be its own parent. Please select another record.");

    if (relType == rtKeyWork)
    {
      HDT_RecordWithAuthors<? extends RecordAuthors> kwRecord = (HDT_RecordWithAuthors<? extends RecordAuthors>) subj;

      if (db.keyWorkMentionerStream(kwRecord, false).anyMatch(mentioner -> mentioner == obj))
        return falseWithErrorPopup("Unable to associate the records as requested: They are already associated in the requested way.");
    }
    else if (db.getObjectList(relType, subj, true).contains(obj))
      return falseWithErrorPopup("Unable to associate the records as requested: They are already associated in the requested way.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canDetach()
  {
    return (relType != rtNone) && (relType != rtGlossaryOfConcept);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void detach()
  {
    if (relType == rtKeyWork)
    {
      HDT_RecordWithAuthors<? extends RecordAuthors> kwRecord = (HDT_RecordWithAuthors<? extends RecordAuthors>) subj;

      Set<HDT_RecordWithMainText> mentioners = db.keyWorkMentionerStream(kwRecord, obj.getType()).collect(Collectors.toSet());
      mentioners.remove(obj);
      MainText.setKeyWorkMentioners(kwRecord, mentioners, obj.getType());
      return;
    }

    db.getObjectList(relType, subj, true).remove(obj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canDetachWithoutAttaching(boolean doDetach)
  {
    if ((canDetach() == false) || HyperDB.isUnstoredRecord(obj))
      return false;

    if (doDetach)
    {
      detach();
      db.attachOrphansToRoots();
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;

    if (relType == rtNone)
    {
      result = prime * result + ((parent == null) ? 0 : parent.hashCode());
      result = prime * result + ((child  == null) ? 0 : child .hashCode());
    }
    else
    {
      result = prime * result + ((obj  == null) ? 0 : obj .hashCode());
      result = prime * result + ((subj == null) ? 0 : subj.hashCode());
    }
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object o)
  {
    if (this == o) return true;
    if (o == null) return false;
    if (getClass() != o.getClass()) return false;

    RecordTreeEdge other = (RecordTreeEdge) o;

    if (relType != other.relType)
      return false;

    if (relType == rtNone)
      return (other.parent == parent) && (other.child == child);

    return (other.obj == obj) && (other.subj == subj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isConceptsInSameGlossary()
  {
    return (relType == rtParentConceptOfConcept) && (((HDT_Concept)parent).glossary.get() == ((HDT_Concept)child).glossary.get());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
