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
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.records.HDT_RecordState.writePointerTag;


import java.util.LinkedHashMap;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;

public class HDI_OfflineNestedPointer extends HDI_OfflineBase
{
  int objID = -1;
  private RelationType relType;

  public HDI_OfflineNestedPointer(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
    relType = schema.getRelType();
  }

//---------------------------------------------------------------------------

  public int getObjID()           { return objID; }
  public void setObjID(int objID) { this.objID = objID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems)
  {
    this.objID = objID;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    HDT_RecordType objType = db.getNestedTargetType(relType, mainTag);

    writePointerTag(xml, tag, objID, hdtNone, db.records(objType).getByID(objID).getXMLObjectName());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
