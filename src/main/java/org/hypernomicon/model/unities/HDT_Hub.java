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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Stream;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;

public class HDT_Hub extends HDT_RecordWithMainText
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean alreadyModifying = false;

  final Map<RecordType, HDT_RecordWithMainText> spokes = new HashMap<>();

  public HDT_Hub(RecordState xmlState, HyperDataset<HDT_Hub> dataset)
  {
    super(xmlState, dataset, tagName);

    hub = this;
  }

  public HDT_Note      getNote    () { return (HDT_Note     ) spokes.get(hdtNote     ); }
  public HDT_Concept   getConcept () { return (HDT_Concept  ) spokes.get(hdtConcept  ); }
  public HDT_Debate    getDebate  () { return (HDT_Debate   ) spokes.get(hdtDebate   ); }
  public HDT_Position  getPosition() { return (HDT_Position ) spokes.get(hdtPosition ); }
  public HDT_WorkLabel getLabel   () { return (HDT_WorkLabel) spokes.get(hdtWorkLabel); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<HDT_RecordWithMainText> getSpokes()
  {
    return Stream.of(hdtDebate, hdtPosition, hdtConcept, hdtNote, hdtWorkLabel).map(spokes::get).filter(Objects::nonNull);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithMainText getSpoke(RecordType spokeType)
  {
    return spokes.get(spokeType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_RecordWithMainText mainSpoke()
  {
    return mainSpoke(false);
  }

  public HDT_RecordWithMainText mainSpoke(boolean prioritizeNoteOverConcept)
  {
    HDT_RecordWithMainText spoke = getDebate();
    if (spoke != null) return spoke;

    spoke = getPosition();
    if (spoke != null) return spoke;

    if (prioritizeNoteOverConcept)
    {
      spoke = getNote();
      if (spoke != null) return spoke;

      spoke = getConcept();
    }
    else
    {
      spoke = getConcept();
      if (spoke != null) return spoke;

      spoke = getNote();
    }

    return spoke != null ? spoke : getLabel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName() { return name(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void setSearchKey(String newKey, boolean noMod, boolean rebuildMentions)
  {
    if (newKey.length() > 0)
      messageDialog("Internal error #72950", mtError);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    mainText.getKeyWorksUnmod().forEach(keyWork -> db.handleKeyWork(this, keyWork.getRecord(), false));

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void modifyNow()
  {
    if (db.runningConversion || alreadyModifying) return;

    super.modifyNow();

    alreadyModifying = true;

    spokes.values().forEach(HDT_RecordWithMainText::modifyMainText);

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean uniteRecords(HDT_RecordWithMainText spoke1, HDT_RecordWithMainText spoke2, String newDesc)
  {
    if ((spoke1.getType() == hdtPosition) && (spoke2.getType() == hdtDebate))  // Sanity checks
      return falseWithErrorMessage("A position record and a problem/debate record cannot be united.");
    if ((spoke2.getType() == hdtPosition) && (spoke1.getType() == hdtDebate))
      return falseWithErrorMessage("A position record and a problem/debate record cannot be united.");
    if (spoke1.getType() == spoke2.getType())
      return falseWithErrorMessage("Two records of the same type cannot be united.");
    if ((spoke1.isUnitable() == false) || (spoke2.isUnitable() == false))
      return falseWithErrorMessage("One or more of the records are not of a unitable type.");
    if (HyperDB.isUnstoredRecord(spoke1.getID(), spoke1.getType()))
      return falseWithErrorMessage("That " + db.getTypeName(spoke1.getType()) + " record cannot be united to another record.");
    if (HyperDB.isUnstoredRecord(spoke2.getID(), spoke2.getType()))
      return falseWithErrorMessage("That " + db.getTypeName(spoke2.getType()) + " record cannot be united to another record.");

    HDT_Hub hub;

    if (spoke1.hasHub())
    {
      if (spoke2.hasHub())
        return falseWithErrorMessage("Both records are already united to another record.");

      hub = spoke1.getHub();

      if ((hub.getPosition() != null) || (hub.getDebate() != null))
        if ((spoke2.getType() == hdtPosition) || (spoke2.getType() == hdtDebate))
        {
          return falseWithErrorMessage(hub.getSpoke(spoke2.getType()) == null ?
            "A position record and a problem/debate record cannot be united."
          :
            "Two records of the same type cannot be united.");
        }
    }

    else if (spoke2.hasHub())
    {
      hub = spoke2.getHub();

      if ((hub.getPosition() != null) || (hub.getDebate() != null))
        if ((spoke1.getType() == hdtPosition) || (spoke1.getType() == hdtDebate))
        {
          return falseWithErrorMessage(hub.getSpoke(spoke1.getType()) == null ?
            "A position record and a problem/debate record cannot be united."
          :
            "Two records of the same type cannot be united.");
        }
    }

    else
    {
      hub = db.createNewBlankRecord(hdtHub);
    }

    MainText mainText = new MainText(spoke1.getMainText(), spoke2.getMainText(), hub, newDesc);

    hub.spokes.put(spoke1.getType(), spoke1);
    hub.spokes.put(spoke2.getType(), spoke2);

    db.replaceMainText(hub.getMainText(), mainText);
    hub.mainText = mainText;

    spoke1.hub = hub;
    spoke2.hub = hub;

    if      (spoke1.name().isEmpty()) spoke1.setName(spoke2.name());
    else if (spoke2.name().isEmpty()) spoke2.setName(spoke1.name());

    hub.spokes.values().forEach(spoke ->
    {
      db.replaceMainText(spoke.mainText, mainText);
      spoke.mainText = mainText;
    });

    nullSwitch(hub.getLabel(), HDT_WorkLabel::refreshSubjects);
    hub.modifyNow();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean disuniteRecord(RecordType spokeType, boolean deleteHub)
  {
    // Disconnect the spoke

    HDT_RecordWithMainText spokeBeingDisconnected = spokes.remove(spokeType);

    if (spokeBeingDisconnected == null) return false;

    modifyNow(); // Okay to set hub as modified here but spoke being disconnected should only be
                 // set as modified by caller, because it may be in the process of being expired

    spokeBeingDisconnected.hub = null;
    spokeBeingDisconnected.mainText = new MainText(mainText, spokeBeingDisconnected);

    // Done disconnecting, now need to disconnect other spoke if only one left

    if (spokes.size() == 1)
      return disuniteRecord(List.copyOf(spokes.keySet()).get(0), deleteHub);

    if (deleteHub && spokes.isEmpty())
      db.deleteRecord(this);  // Hub now has no more spokes, must be sacrificed

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
