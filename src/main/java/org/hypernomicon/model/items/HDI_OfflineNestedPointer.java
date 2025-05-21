/*
 * Copyright 2015-2025 Jason Winning
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
import static org.hypernomicon.model.records.RecordType.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HDX_Element;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordState;

//---------------------------------------------------------------------------

public class HDI_OfflineNestedPointer extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int objID = -1;

//---------------------------------------------------------------------------

  public HDI_OfflineNestedPointer(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
  }

//---------------------------------------------------------------------------

  public int getObjID() { return objID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    this.objID = element.getObjID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    writePointerTag(xml, tag, objID, hdtNone, db.records(nestedTargetType()).getByID(objID).getXMLObjectName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
