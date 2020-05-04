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

package org.hypernomicon.view;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public class TreeSelector
{
  private HDT_Record base, target;
  private final List<TreeTargetType> targetTypes = new ArrayList<>();
  private boolean baseIsSubj = true;

  TreeSelector()              { reset(); }

  public HDT_Record getBase() { return base; }
  HDT_Record getSubj()        { return baseIsSubj ? base : target; }

  public void setTarget(HDT_Record target) { this.target = target; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
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

  public void addTargetType(HDT_RecordType targetType)
  {
    RelationType relType = baseIsSubj ? getRelation(base.getType(), targetType) : getRelation(targetType, base.getType());

    targetTypes.add(new TreeTargetType(relType, targetType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void linking(HDT_Record base, HDT_RecordType targetType)
  {
    reset();

    this.base = base;

    targetTypes.add(new TreeTargetType(rtUnited, targetType));
    if (targetType == hdtDebate)
      targetTypes.add(new TreeTargetType(rtUnited, hdtPosition));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RelationType getRelTypeForTargetType(HDT_RecordType targetType)
  {
    return findFirst(targetTypes, ttType -> ttType.targetType == targetType, rtNone, ttType -> ttType.relType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class TreeTargetType
  {
    private TreeTargetType(RelationType relType, HDT_RecordType objType)
    {
      this.relType = relType;
      this.targetType = objType;
    }

    private RelationType relType;
    private HDT_RecordType targetType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTypesStr()
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

  public boolean select(HDT_Record record, boolean showErrMsg)
  {
    final RelationType relType = getRelTypeForTargetType(record.getType());
    RelationType oldRelType = null;
    int ndx = -1;
    HyperObjList<HDT_Record, HDT_Record> objList;
    final HDT_Record subj = getSubj(),
                     obj  = baseIsSubj ? target : base;

    if (target != null)
    {
      oldRelType = getRelation(subj.getType(), obj.getType());
      objList = db.getObjectList(oldRelType, subj, true);
      ndx = objList.indexOf(obj);
      objList.remove(ndx);
    }

    if (baseIsSubj)
    {
      objList = db.getObjectList(relType, base, true);
      objList.add(record);
    }
    else
    {
      objList = db.getObjectList(relType, record, true);
      objList.add(base);
    }

    try { objList.throwLastException(); }
    catch (RelationCycleException e)
    {
      if (showErrMsg)
        messageDialog("Cannot use selected record: Records would be organized in a cycle as a result.", mtError);

      if (target != null)
        db.getObjectList(oldRelType, subj, true).add(ndx, obj);

      return false;
    }

    target = record;
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean falseWithErrMsgCond(boolean showErrMsg, String errMsg)
  {
    return showErrMsg ? falseWithErrorMessage(errMsg) : false;
  }

  public boolean selectToUnite(HDT_RecordWithConnector record2, boolean showErrMsg)
  {
    HDT_RecordWithConnector record1 = (HDT_RecordWithConnector) base;

    if (record2.getType() == record1.getType())
      return falseWithErrMsgCond(showErrMsg, "You cannot connect records of the same type.");

    if (record2.isLinked())
    {
      if (record2.getLink().getSpoke(record1.getType()) != null)
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(record1.getType()) + ".");

      if (record1.getType() == hdtDebate)
        if (record2.getLink().getSpoke(hdtPosition) != null)
          return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(hdtPosition) + ".");

      if (record1.getType() == hdtPosition)
        if (record2.getLink().getSpoke(hdtDebate) != null)
          return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record2.getType()) + " is already connected to a " + db.getTypeName(hdtDebate) + ".");

      if (record1.isLinked())
        return falseWithErrMsgCond(showErrMsg, "Both records are already linked to other records.");
    }

    if (record1.isLinked())
    {
      if (record1.getLink().getSpoke(record2.getType()) != null)
        return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(record2.getType()) + ".");

      if (record2.getType() == hdtDebate)
        if (record1.getLink().getSpoke(hdtPosition) != null)
          return falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(hdtPosition) + ".");

      if (record2.getType() == hdtPosition)
        if (record1.getLink().getSpoke(hdtDebate) != null)
          falseWithErrMsgCond(showErrMsg, "The selected " + db.getTypeName(record1.getType()) + " is already connected to a " + db.getTypeName(hdtDebate) + ".");
    }

    ui.uniteRecords(record1, record2, showErrMsg == false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
