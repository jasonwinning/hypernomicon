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

import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.hdtHub;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HubChangedException;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.RestoreException;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.unities.MainText.DisplayItem;

/**
 * Every record that has a main HTML description field is an instance of
 * this class. Some of those record types, but not all, also can be "united"
 * to other records so that they have the same {@link MainText MainText} object.
 * They will also each have a reference to the same {@link HDT_Hub HDT_Hub}
 * object (record of type Hub).
 *
 * @author  Jason Winning
 * @since   1.0
 */
public abstract class HDT_RecordWithConnector extends HDT_RecordBase implements HDT_RecordWithDescription
{
  HDT_Hub hub;
  MainText mainText;
  private boolean alreadyModifying;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithConnector(RecordState xmlState, HyperDataset<? extends HDT_RecordWithConnector> dataset, Tag nameTag)
  {
    super(xmlState, dataset, nameTag);

    mainText = new MainText(this);

    if (getType() == hdtHub)
      hub = (HDT_Hub) this;

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------

  @Override public final MainText getDesc()  { return mainText; }
  public MainText getMainText()              { return mainText; }
  public HDT_Hub getHub()                    { return hub; }
  public boolean hasHub()                    { return hub != null; }
  public HDT_RecordWithConnector mainSpoke() { return hub == null ? this : hub.mainSpoke(false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    super.resolvePointers();

    if (HDT_Record.isEmptyThrowsException(hub))
      hub = null;

    mainText.resolvePointers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    boolean expiringHub = false;

    if (hasHub())
    {
      int cnt = 0;
      if (hub.getDebate  () != null) cnt++;
      if (hub.getLabel   () != null) cnt++;
      if (hub.getNote    () != null) cnt++;
      if (hub.getPosition() != null) cnt++;
      if (hub.getConcept () != null) cnt++;

      if (cnt == 2) expiringHub = true;
    }

    for (KeyWork keyWork : mainText.getKeyWorksUnmod())
    {
      if (expiringHub) db.handleKeyWork(hub, keyWork.getRecord(), false); // hub is also getting deleted after this; go ahead and remove it from index
      db.handleKeyWork(this, keyWork.getRecord(), false);
    }

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void modifyMainText()
  {
    modifyNow();

    if (db.runningConversion || alreadyModifying || (hasHub() == false)) return;

    alreadyModifying = true;

    hub.modifyNow();

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addParentDisplayRecord()
  {
    if (mainText.getPlain().trim().length() > 0) return;

    HDT_RecordWithConnector parent = null;
    List<DisplayItem> displayItems = mainText.getDisplayItemsCopy();
    RecordType type = getType();

    if (displayItems.stream().anyMatch(displayItem -> displayItem.type == diRecord)) return;

    switch (type)
    {
      case hdtPosition:

        HDT_Position position = (HDT_Position) this;

        if (position.largerDebates.isEmpty() == false)
          parent = position.largerDebates.get(0);
        else if (position.largerPositions.isEmpty() == false)
          parent = position.largerPositions.get(0);

        break;

      case hdtArgument:

        HDT_Argument argument = (HDT_Argument) this;

        if (argument.positions.isEmpty() == false)
          parent = argument.positions.get(0);
        else if (argument.counteredArgs.isEmpty() == false)
          parent = argument.counteredArgs.get(0);

        break;

      case hdtDebate:

        HDT_Debate debate = (HDT_Debate) this;

        if (debate.largerDebates.isEmpty() == false)
          parent = debate.largerDebates.get(0);

        break;

      default: break;
    }

    if ((parent == null) || HyperDB.isUnstoredRecord(parent.getID(), parent.getType())) return;

    boolean rc = db.runningConversion;
    db.runningConversion = true;
    displayItems.add(new DisplayItem(parent));
    mainText.setDisplayItemsFromList(displayItems);
    db.runningConversion = rc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException
  {
    int backupHubID = nullSwitch((HDI_OfflineConnector)backupState.items.get(tagHub), -1, HDI_OfflineConnector::getHubID);

    if (isOnline() && isUnitable())
    {
      int curHubID = nullSwitch(getHub(), -1, HDT_Hub::getID);

      if (curHubID != backupHubID)
        throw new HubChangedException(curHubID >= 1);
    }

    super.restoreTo(backupState, rebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
