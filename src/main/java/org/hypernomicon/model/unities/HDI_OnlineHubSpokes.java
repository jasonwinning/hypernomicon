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

package org.hypernomicon.model.unities;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HDI_OnlineBase;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class HDI_OnlineHubSpokes extends HDI_OnlineBase<HDI_OfflineHubSpokes>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Hub hub;

//---------------------------------------------------------------------------

  public HDI_OnlineHubSpokes(HDI_Schema schema, HDT_Hub hub)
  {
    super(schema, hub);

    this.hub = hub;
  }

//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords) { }
  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items)              { return null; }
  @Override public int getResultCount(Tag tag)                                              { return hub.spokes.size(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineHubSpokes val, Tag tag)
  {
    setOnlineSpoke(hdtDebate   , val.debateID  );
    setOnlineSpoke(hdtPosition , val.positionID);
    setOnlineSpoke(hdtNote     , val.noteID    );
    setOnlineSpoke(hdtWorkLabel, val.labelID   );
    setOnlineSpoke(hdtConcept  , val.conceptID );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setOnlineSpoke(RecordType type, int id)
  {
    if (id > 0)
      hub.spokes.put(type, (HDT_RecordWithMainText) db.records(type).getByID(id));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineHubSpokes val, Tag tag)
  {
    val.debateID   = HDT_Record.getIDSafe(hub.getDebate  ());
    val.positionID = HDT_Record.getIDSafe(hub.getPosition());
    val.noteID     = HDT_Record.getIDSafe(hub.getNote    ());
    val.labelID    = HDT_Record.getIDSafe(hub.getLabel   ());
    val.conceptID  = HDT_Record.getIDSafe(hub.getConcept ());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    // All of the logic in this function is for cases where
    // a hub has less than 2 spokes when the database is first
    // loaded from XML. Under normal circumstances, the hub gets
    // deleted when HDI_OnlineMainTextAndHub.expire calls
    // HDT_Hub.disuniteRecord.

    int spokeCount = 0;

    Iterator<Entry<RecordType, HDT_RecordWithMainText>> iterator = hub.spokes.entrySet().iterator();

    while (iterator.hasNext())
    {
      Entry<RecordType, HDT_RecordWithMainText> entry = iterator.next();

      if (HDT_Record.isEmptyThrowsException(entry.getValue(), false))
        iterator.remove();
      else
        spokeCount++;
    }

    if (spokeCount == 1)  // If less than 2 spokes left, no reason for hub to exist...
    {
      HDT_RecordWithMainText spoke = hub.spokes.values().iterator().next();
      hub.disuniteRecord(spoke.getType());  // This will delete the hub
      spoke.modifyNow();
    }
    else if (spokeCount == 0)
      db.deleteRecord(hub);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
