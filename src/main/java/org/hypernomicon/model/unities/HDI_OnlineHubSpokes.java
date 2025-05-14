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
import static org.hypernomicon.util.Util.*;

import java.util.List;

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

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords) { return; }
  @Override public String getResultTextForTag(Tag tag)                                      { return null; }
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
    val.debateID   = nullSwitch(hub.getDebate  (), -1, HDT_Record::getID);
    val.positionID = nullSwitch(hub.getPosition(), -1, HDT_Record::getID);
    val.noteID     = nullSwitch(hub.getNote    (), -1, HDT_Record::getID);
    val.labelID    = nullSwitch(hub.getLabel   (), -1, HDT_Record::getID);
    val.conceptID  = nullSwitch(hub.getConcept (), -1, HDT_Record::getID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    int spokeCount = 0;

    for (HDT_RecordWithMainText spoke : List.copyOf(hub.spokes.values()))
    {
      if (HDT_Record.isEmptyThrowsException(spoke, false))
        hub.spokes.remove(spoke.getType());
      else
        spokeCount++;
    }

    if (spokeCount == 1)  // If only one spoke left, no reason for hub to exist...
    {
      HDT_RecordWithMainText spoke = List.copyOf(hub.spokes.values()).get(0);
      hub.disuniteRecord(spoke.getType(), false);
      spoke.modifyNow();

      spokeCount = 0;
    }
                                       // hub.expire should be called here, not db.deleteRecord, because pointer resolution is already in progress.
    if (spokeCount == 0) hub.expire(); // HDI_OnlineHubSpokes.resolvePointers (this function) is ultimately called by HyperCore.resolvePointers.
                                       // After the pointers for the hub record items are resolved, HyperCore.resolvePointers checks to see if the
  }                                    // record became expired during the process. If so, the hub is removed from the HyperCore (i.e., it is deleted).

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
