/*
 * Copyright 2015-2022 Jason Winning
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

import java.util.List;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.items.HDI_OnlineBase;
import org.hypernomicon.model.records.HDT_Record;

public class HDI_OnlineHubSpokes extends HDI_OnlineBase<HDI_OfflineHubSpokes>
{
  private final HDT_Hub hub;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_OnlineHubSpokes(HDI_Schema schema, HDT_Hub hub)
  {
    super(schema, hub);

    this.hub = hub;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords) { return; }
  @Override public String getResultTextForTag(Tag tag)                                      { return null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineHubSpokes val, Tag tag)
  {
    if (val.debateID   > 0) hub.debateSpoke   = db.debates   .getByID(val.debateID  );
    if (val.positionID > 0) hub.positionSpoke = db.positions .getByID(val.positionID);
    if (val.noteID     > 0) hub.noteSpoke     = db.notes     .getByID(val.noteID    );
    if (val.labelID    > 0) hub.labelSpoke    = db.workLabels.getByID(val.labelID   );
    if (val.conceptID  > 0) hub.conceptSpoke  = db.concepts  .getByID(val.conceptID );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineHubSpokes val, Tag tag)
  {
    HDT_Record record;

    record = hub.getDebate  (); if (record != null) val.debateID   = record.getID();
    record = hub.getPosition(); if (record != null) val.positionID = record.getID();
    record = hub.getNote    (); if (record != null) val.noteID     = record.getID();
    record = hub.getLabel   (); if (record != null) val.labelID    = record.getID();
    record = hub.getConcept (); if (record != null) val.conceptID  = record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    int spokeCount = 0;

    if (HDT_Record.isEmptyThrowsException(hub.noteSpoke    )) hub.noteSpoke     = null;  else  spokeCount++;
    if (HDT_Record.isEmptyThrowsException(hub.conceptSpoke )) hub.conceptSpoke  = null;  else  spokeCount++;
    if (HDT_Record.isEmptyThrowsException(hub.labelSpoke   )) hub.labelSpoke    = null;  else  spokeCount++;
    if (HDT_Record.isEmptyThrowsException(hub.debateSpoke  )) hub.debateSpoke   = null;  else  spokeCount++;
    if (HDT_Record.isEmptyThrowsException(hub.positionSpoke)) hub.positionSpoke = null;  else  spokeCount++;

    if (spokeCount == 1)  // If only one spoke left, no reason for hub to exist...
    {
      if      (hub.noteSpoke     != null) hub.disuniteRecord(hdtNote,      false);
      else if (hub.debateSpoke   != null) hub.disuniteRecord(hdtDebate,    false);
      else if (hub.positionSpoke != null) hub.disuniteRecord(hdtPosition,  false);
      else if (hub.conceptSpoke  != null) hub.disuniteRecord(hdtConcept,   false);
      else if (hub.labelSpoke    != null) hub.disuniteRecord(hdtWorkLabel, false);

      spokeCount = 0;
    }
                                       // hub.expire should be called here, not db.deleteRecord, because pointer resolution is already in progress.
    if (spokeCount == 0) hub.expire(); // HDI_OnlineHubSpokes.resolvePointers (this function) is ultimately called by HyperCore.resolvePointers.
                                       // After the pointers for the hub record items are resolved, HyperCore.resolvePointers checks to see if the
  }                                    // record became expired during the process. If so, the hub is removed from the HyperCore (i.e., it is deleted).

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
