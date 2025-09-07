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

import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.MainText.DisplayItem;

//---------------------------------------------------------------------------

/**
 * Every record that has a main HTML description field is an instance of
 * this class. This description field corresponds to a {@link MainText MainText} object, a reference
 * to which is held in member variable {@code mainText} of this class.<br>
 * <br>
 * Some of those record types instantiating this class, but not all, also can be "united"
 * to other records so that they have the same {@link MainText MainText} object.
 * They will also each have a reference to the same {@link HDT_Hub HDT_Hub}
 * object (record of type Hub).<br>
 * <br>
 * This class differs from {@link org.hypernomicon.model.unities.HDT_RecordWithDescription HDT_RecordWithDescription}
 * because this class will always have a description field that is considered
 * to be the "main" one, whereas implementing {@link org.hypernomicon.model.unities.HDT_RecordWithDescription HDT_RecordWithDescription}
 * only implies that there is at least one description field but none that is
 * considered to be the "main" one. For example, {@link org.hypernomicon.model.records.HDT_Term HDT_Term}
 * instantiates {@code HDT_RecordWithDescription} but not {@code HDT_RecordWithMainText} because there can be multiple
 * definitions (multiple {@link org.hypernomicon.model.records.HDT_Concept HDT_Concept} records). Hence,
 * {@code HDT_Term} records are united one definition at a time.
 *
 * @author  Jason Winning
 * @since   1.0
 */
public abstract class HDT_RecordWithMainText extends HDT_RecordBase implements HDT_RecordWithDescription
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HDT_Hub hub;
  MainText mainText;
  private boolean alreadyModifying;

//---------------------------------------------------------------------------

  public HDT_RecordWithMainText(RecordState xmlState, DatasetAccessor<? extends HDT_RecordWithMainText> dataset)
  {
    super(xmlState, dataset);

    mainText = new MainText(this);

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------

  @Override public final boolean hasMainText() { return true; }
  @Override public final boolean hasDesc()     { return true; }
  @Override public final MainText getDesc()    { return mainText; }

  public MainText getMainText()                { return mainText; }
  public HDT_Hub getHub()                      { return hub; }
  public boolean hasHub()                      { return hub != null; }
  public HDT_RecordWithMainText mainSpoke()    { return hub == null ? this : hub.mainSpoke(false); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void modifyMainText()
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
    if (mainText.getPlain().strip().length() > 0) return;

    HDT_RecordWithMainText parent = null;
    List<DisplayItem> displayItems = mainText.getDisplayItemsCopy();
    RecordType type = getType();

    if (displayItems.stream().anyMatch(displayItem -> displayItem.type == diRecord)) return;

    switch (type)
    {
      case hdtPosition:

        HDT_Position position = (HDT_Position) this;

        if (position.largerDebates.isEmpty() == false)
          parent = position.largerDebates.getFirst();
        else if (position.largerPositions.isEmpty() == false)
          parent = position.largerPositions.getFirst();

        break;

      case hdtArgument:

        HDT_Argument argument = (HDT_Argument) this;

        if (argument.positions.isEmpty() == false)
          parent = argument.positions.getFirst();
        else if (argument.targetArgs.isEmpty() == false)
          parent = argument.targetArgs.getFirst();

        break;

      case hdtDebate:

        HDT_Debate debate = (HDT_Debate) this;

        if (debate.largerDebates.isEmpty() == false)
          parent = debate.largerDebates.getFirst();

        break;

      default: break;
    }

    if ((parent == null) || isUnstoredRecord(parent)) return;

    boolean wasRunningConversion = db.runningConversion;
    db.runningConversion = true;

    displayItems.add(new DisplayItem(parent));
    mainText.setDisplayItemsFromList(displayItems);

    db.runningConversion = wasRunningConversion;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError
  {
    int backupHubID = nullSwitch((HDI_OfflineMainTextAndHub)backupState.items.get(tagHub), -1, HDI_OfflineMainTextAndHub::getHubID);

    if (isOnline() && isUnitable())
    {
      int curHubID = HDT_Record.getIDSafe(getHub());

      if (curHubID != backupHubID)
        throw new HubChangedException(curHubID >= 1);
    }

    super.restoreTo(backupState, rebuildMentions);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
