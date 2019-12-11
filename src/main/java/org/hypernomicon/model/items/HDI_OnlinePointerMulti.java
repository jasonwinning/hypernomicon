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
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;

import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public class HDI_OnlinePointerMulti extends HDI_OnlineBase<HDI_OfflinePointerMulti>
{
  private final RelationType relType;

  public HDI_OnlinePointerMulti(HDI_Schema newSchema, HDT_Record newRecord)
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
    HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, record, false);
    List<HDT_Record> newList = new ArrayList<>();

    HDT_RecordType objType = db.getObjType(relType);

    val.objIDs.forEach(objID -> nullSwitch((HDT_Record)db.records(objType).getByID(objID.intValue()), newList::add));

    for (HDT_Record obj : newList)
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

    for (HDT_Record obj : objList)
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

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (searchLinkedRecords)
      db.getObjectList(relType, record, false).forEach(objRecord -> list.add(objRecord.listName()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    return db.getObjectList(relType, record, false).stream().map(HDT_Record::listName)
                                                            .filter(oneStr -> oneStr.length() > 0)
                                                            .limit(20)
                                                            .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflinePointerMulti val, Tag tag)
  {
    val.objIDs.clear();

    db.getObjectList(relType, record, false).forEach(objRecord ->
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
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
