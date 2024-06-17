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

package org.hypernomicon.model.unities;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HDX_Element;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

public class HDI_OfflineHubSpokes extends HDI_OfflineBase
{

//---------------------------------------------------------------------------

  int noteID = -1, conceptID = -1, debateID = -1, positionID = -1, labelID = -1;

//---------------------------------------------------------------------------

  public HDI_OfflineHubSpokes(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    switch (element.getObjType())
    {
      case hdtDebate    : debateID   = element.getObjID(); break;
      case hdtPosition  : positionID = element.getObjID(); break;
      case hdtNote      : noteID     = element.getObjID(); break;
      case hdtWorkLabel : labelID    = element.getObjID(); break;
      case hdtConcept   : conceptID  = element.getObjID(); break;

      default           :                             break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void writeTag(int id, RecordType type, StringBuilder xml)
  {
    if (id > 0)
      writePointerTag(xml, tagLinkedRecord, id, type, db.records(type).getByID(id).getXMLObjectName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    writeTag(debateID,   hdtDebate   , xml);
    writeTag(conceptID,  hdtConcept  , xml);
    writeTag(labelID,    hdtWorkLabel, xml);
    writeTag(positionID, hdtPosition , xml);
    writeTag(noteID,     hdtNote     , xml);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
