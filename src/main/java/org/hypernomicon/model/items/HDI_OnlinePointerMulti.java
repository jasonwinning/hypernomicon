/*
 * Copyright 2015-2024 Jason Winning
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
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public class HDI_OnlinePointerMulti extends HDI_OnlineBase<HDI_OfflinePointerMulti>
{
  private final RelationType relType;

  public HDI_OnlinePointerMulti(HDI_Schema schema, HDT_Record record)
  {
    super(schema, record);
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

  @Override public void setFromOfflineValue(HDI_OfflinePointerMulti val, Tag tag) throws RelationCycleException, HDB_InternalError
  {
    HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, record, false);
    List<HDT_Record> newList = new ArrayList<>();

    RecordType objType = db.getObjType(relType);

    val.objIDs.forEach(objID -> nullSwitch((HDT_Record)db.records(objType).getByID(objID), newList::add));

    boolean wasEmpty = objList.isEmpty();

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

    if (wasEmpty) return;

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
    return recordStreamResultText(db.getObjType(relType), db.getObjectList(relType, record, false).stream());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String recordStreamResultText(RecordType objType, Stream<HDT_Record> stream)
  {
    Function<? super HDT_Record, String> strFunction = objType == RecordType.hdtWork ? HDT_Record::getCBText : HDT_Record::listName;

    return stream.map(strFunction).filter(Predicate.not(String::isBlank)).limit(20).reduce((s1, s2) -> s1 + "; " + s2).orElse("");
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
        Map<Tag, HDI_OfflineBase> tagToNestedItem = val.objIDtoMaps.computeIfAbsent(objRecord.getID(), objRecordID -> new LinkedHashMap<>());

        db.saveNestedValuesToOfflineMap(record, objRecord, tagToNestedItem, val.recordState);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
