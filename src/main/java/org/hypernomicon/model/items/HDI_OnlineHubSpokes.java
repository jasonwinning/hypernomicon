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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Hub;

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
    StrongLink link = hub.getLink();

    if (val.debateID   > 0) link.debateSpoke   = db.debates   .getByID(val.debateID  ).getConnector();
    if (val.positionID > 0) link.positionSpoke = db.positions .getByID(val.positionID).getConnector();
    if (val.noteID     > 0) link.noteSpoke     = db.notes     .getByID(val.noteID    ).getConnector();
    if (val.labelID    > 0) link.labelSpoke    = db.workLabels.getByID(val.labelID   ).getConnector();
    if (val.conceptID  > 0) link.conceptSpoke  = db.concepts  .getByID(val.conceptID ).getConnector();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineHubSpokes val, Tag tag)
  {
    StrongLink link = hub.getLink();
    HDT_Record record;

    record = link.getDebate  (); if (record != null) val.debateID   = record.getID();
    record = link.getPosition(); if (record != null) val.positionID = record.getID();
    record = link.getNote    (); if (record != null) val.noteID     = record.getID();
    record = link.getLabel   (); if (record != null) val.labelID    = record.getID();
    record = link.getConcept (); if (record != null) val.conceptID  = record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers()
  {
    int spokeCount = 0;
    StrongLink link = hub.getLink();

    if (Connector.isEmpty(link.noteSpoke    )) link.noteSpoke     = null;  else  spokeCount++;
    if (Connector.isEmpty(link.conceptSpoke )) link.conceptSpoke  = null;  else  spokeCount++;
    if (Connector.isEmpty(link.labelSpoke   )) link.labelSpoke    = null;  else  spokeCount++;
    if (Connector.isEmpty(link.debateSpoke  )) link.debateSpoke   = null;  else  spokeCount++;
    if (Connector.isEmpty(link.positionSpoke)) link.positionSpoke = null;  else  spokeCount++;

    if (spokeCount == 1)  // If only one connector, no reason for hub to exist...
    {
      if      (link.noteSpoke     != null) link.disconnectRecord(hdtNote,      false);
      else if (link.debateSpoke   != null) link.disconnectRecord(hdtDebate,    false);
      else if (link.positionSpoke != null) link.disconnectRecord(hdtPosition,  false);
      else if (link.conceptSpoke  != null) link.disconnectRecord(hdtConcept,   false);
      else if (link.labelSpoke    != null) link.disconnectRecord(hdtWorkLabel, false);

      spokeCount = 0;
    }
                                       // hub.expire should be called here, not db.deleteRecord, because pointer resolution is already in progress.
    if (spokeCount == 0) hub.expire(); // HDI_OnlineHubSpokes.resolvePointers (this function) is ultimately called by HyperCore.resolvePointers.
                                       // After the pointers for the hub record items are resolved, HyperCore.resolvePointers checks to see if the
  }                                    // record became expired during the process. If so, the hub is removed from the HyperCore (i.e., it is deleted).

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
