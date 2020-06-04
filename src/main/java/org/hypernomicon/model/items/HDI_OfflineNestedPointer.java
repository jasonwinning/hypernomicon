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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

public class HDI_OfflineNestedPointer extends HDI_OfflineBase
{
  int objID = -1;
  private final HDT_RecordType targetType;

  public HDI_OfflineNestedPointer(HDI_Schema schema, HDT_RecordState recordState)
  {
    super(schema, recordState);
    targetType = schema.getNestedTargetType();
  }

  public int getObjID() { return objID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    this.objID = objID;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    writePointerTag(xml, tag, objID, hdtNone, db.records(targetType).getByID(objID).getXMLObjectName());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
