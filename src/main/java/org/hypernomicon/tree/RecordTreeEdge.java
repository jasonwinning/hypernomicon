/*
 * Copyright 2015-2021 Jason Winning
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
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.dialogs.VerdictDlgCtrlr;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.relations.HyperObjList;

public class RecordTreeEdge
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final HDT_Record parent, child;
  private final HDT_Record subj, obj;
  final RelationType relType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  RecordTreeEdge(HDT_Record parent, HDT_Record child)
  {
    this.parent = parent;
    this.child = child;

    RelationType relType = getRelation(child.getType(), parent.getType());

    if (relType != rtNone)
    {
      subj = child;
      obj = parent;
    }
    else
    {
      relType = getRelation(parent.getType(), child.getType());

      if (relType == rtNone)
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

    this.relType = relType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean mustDetachIfAttaching(RecordTreeEdge otherEdge)
  {
    if (relType == rtNone)
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
    if ((relType == rtNone) || db.relationIsMulti(relType))
      return null;

    HDT_Record currentObj = db.getObjPointer(relType, subj).get();

    return (currentObj == null) || (currentObj == obj) ? null : new RecordTreeEdge(currentObj, subj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean attach(RecordTreeEdge detaching, boolean showErrMsg)
  {
    if (canAttach(showErrMsg) == false)
      return false;

    RecordTreeEdge otherDetaching = edgeToDetach();
    if ((otherDetaching != null) && otherDetaching.equals(detaching))
      otherDetaching = null;

    try
    {
      if ((relType == rtPositionOfArgument) || (relType == rtCounterOfArgument))
      {
        HDT_Argument childArg = (HDT_Argument) subj;

        VerdictDlgCtrlr vdc = VerdictDlgCtrlr.build("Select Verdict for " + childArg.getCBText(), obj);

        if (vdc.showModal() == false)
          return false;

        if (obj.getType() == hdtPosition)
          childArg.addPosition((HDT_Position)obj, vdc.hcbVerdict.selectedRecord());
        else if (obj.getType() == hdtArgument)
          childArg.addCounteredArg((HDT_Argument)obj, vdc.hcbVerdict.selectedRecord());
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

      if ((relType == rtWorkOfMiscFile) && HDT_MiscFile.class.cast(subj).work.isNotNull())
        db.getObjectList(rtAuthorOfFile, subj, false).clear();
    }
    catch (RelationCycleException e)
    {
      return falseWithErrMsgCond(showErrMsg, e.getMessage());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canAttach(boolean showErrMsg)
  {
    if ((relType == rtFolderOfNote) && HyperDB.isUnstoredRecord(subj.getID(), hdtNote))
      return falseWithErrMsgCond(showErrMsg, "A folder cannot be assigned to that record.");

    if ((obj.getID() == subj.getID()) && (obj.getType() == subj.getType()))
      return falseWithErrMsgCond(showErrMsg, "A record cannot be its own parent. Please select another record.");

    if (db.getObjectList(relType, subj, true).contains(obj))
      return falseWithErrMsgCond(showErrMsg, "Unable to associate the records as requested: They are already associated in the requested way.");

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
    db.getObjectList(relType, subj, true).remove(obj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canDetachWithoutAttaching(boolean doDetach)
  {
    if ((canDetach() == false) || HyperDB.isUnstoredRecord(obj.getID(), obj.getType()))
      return false;

    if (doDetach)
    {
      detach();
      ui.attachOrphansToRoots();
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

}
