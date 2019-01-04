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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordState.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.LinkedHashMap;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

public class HDI_OfflineHubSpokes extends HDI_OfflineBase
{

//---------------------------------------------------------------------------

  int noteID = -1, conceptID = -1, debateID = -1, positionID = -1, labelID = -1;

//---------------------------------------------------------------------------

  public HDI_OfflineHubSpokes(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems)
  {
    switch (objType)
    {
      case hdtDebate    : debateID   = objID; break;
      case hdtPosition  : positionID = objID; break;
      case hdtNote      : noteID     = objID; break;
      case hdtWorkLabel : labelID    = objID; break;
      case hdtConcept   : conceptID  = objID; break;

      default :           break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeTag(int id, HDT_RecordType type, StringBuilder xml)
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
