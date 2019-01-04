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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;

import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public class HDI_OnlinePointerMulti extends HDI_OnlineBase<HDI_OfflinePointerMulti>
{
  private RelationType relType;

  public HDI_OnlinePointerMulti(HDI_Schema newSchema, HDT_Base newRecord)
  {
    super(newSchema, newRecord);
    relType = schema.getRelType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    db.getObjectList(relType, record, false).clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    if (relType != rtNone)
      db.resolvePointersByRelation(relType, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflinePointerMulti val, Tag tag) throws RelationCycleException
  {
    HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(relType, record, false);
    ArrayList<HDT_Base> newList = new ArrayList<>();

    HDT_RecordType objType = db.getObjType(relType);

    val.objIDs.forEach(objID ->
    {
      HDT_Base obj = db.records(objType).getByID(objID.intValue());
      if (obj != null)
        newList.add(obj);
    });

    for (HDT_Base obj : newList)
    {
      if (objList.contains(obj) == false)
      {
        objList.add(obj);
        objList.throwLastException();
      }

      Map<Tag, HDI_OfflineBase> tagToNestedItem = val.objIDtoMaps.get(obj.getID());
      if (tagToNestedItem != null)
        for (Entry<Tag, HDI_OfflineBase> entry : tagToNestedItem.entrySet())
          db.setNestedItemFromOfflineValue(record, obj, entry.getKey(), entry.getValue());
    }

    for (HDT_Base obj : objList)
      if (newList.contains(obj) == false)
      {
        objList.remove(obj);
        objList.throwLastException();
      }

    objList.reorder(newList);
    objList.throwLastException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (!searchLinkedRecords) return;

    HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(relType, record, false);

    for (HDT_Base objRecord : objList)
      list.add(objRecord.listName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    String allStr = "", oneStr;

    HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(relType, record, false);

    for (HDT_Base objRecord : objList)
    {
      oneStr = objRecord.listName();
      if (oneStr.length() > 0)
        allStr = allStr.length() == 0 ? oneStr : (allStr + "; " + oneStr);
    }

    return allStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflinePointerMulti val, Tag tag)
  {
    HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(relType, record, false);

    val.objIDs.clear();

    for (HDT_Base objRecord : objList)
    {
      val.objIDs.add(objRecord.getID());

      if (db.relationHasNestedValues(relType))
      {
        Map<Tag, HDI_OfflineBase> tagToNestedItem = val.objIDtoMaps.get(objRecord.getID());
        if (tagToNestedItem == null)
        {
          tagToNestedItem = new LinkedHashMap<>();
          val.objIDtoMaps.put(objRecord.getID(), tagToNestedItem);
        }

        db.saveNestedValuesToOfflineMap(record, objRecord, tagToNestedItem, val.recordState);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
