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

package org.hypernomicon.model.items;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map.Entry;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;

//---------------------------------------------------------------------------

public class HDI_OnlinePointerSingle extends HDI_OnlineBase<HDI_OfflinePointerSingle>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final RelationType relType;

//---------------------------------------------------------------------------

  public HDI_OnlinePointerSingle(HDI_Schema schema, HDT_Record record)
  {
    super(schema, record);
    relType = schema.relType();
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

  @Override public void setFromOfflineValue(HDI_OfflinePointerSingle val, Tag tag) throws RelationCycleException, HDB_InternalError
  {
    HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, record, false);
    RecordType objType = db.getObjType(relType);

    int objID = val.getObjID(); if (objID < 1) return;

    HDT_Record obj = db.records(objType).getByID(objID); if (obj == null) return;

    if (objList.contains(obj)) return;

    objList.clear();

    if ((val.ord > -1) && db.isLoading())  // This should only be done when records are first being brought online during initial loading
      objList.initObjWithSubjOrd(obj, val.ord);
    else
    {
      objList.add(obj);
      objList.throwLastException();
    }

    if (val.tagToNestedItem != null)
    {
      for (Entry<Tag, HDI_OfflineBase> entry : val.tagToNestedItem.entrySet())
        db.setNestedItemFromOfflineValue(record, obj, entry.getKey(), entry.getValue());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * {@inheritDoc}
   */
  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords, boolean engChar)
  {
    if (searchLinkedRecords)
      db.getObjectList(relType, record, false).forEach(objRecord -> list.add(engChar ? convertToEnglishChars(objRecord.defaultCellText()) : objRecord.defaultCellText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items)
  {
    HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, record, false);

    if (objList.isEmpty()) return "";

    return db.getObjType(relType) == RecordType.hdtWork ? objList.getFirst().defaultChoiceText() : objList.getFirst().defaultCellText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getResultCount(Tag tag)
  {
    return db.getObjectList(relType, record, false).size();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflinePointerSingle val, Tag tag)
  {
    HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, record, false);

    if (objList.isEmpty())
    {
      val.objID = -1;
      val.ord = -1;
      return;
    }

    HDT_Record obj = objList.getFirst();
    val.objID = obj.getID();
    val.ord = db.getSubjectList(relType, obj).getOrd(record);

    if (db.relationHasNestedValues(relType) == false) return;

    if (val.tagToNestedItem == null)
      val.tagToNestedItem = new LinkedHashMap<>();

    db.saveNestedValuesToOfflineMap(record, objList.getFirst(), val.tagToNestedItem, val.recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
