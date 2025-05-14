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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.*;

//---------------------------------------------------------------------------

public class HDT_Hub extends HDT_RecordWithMainText
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean alreadyModifying = false;

  final Map<RecordType, HDT_RecordWithMainText> spokes = new EnumMap<>(RecordType.class);

//---------------------------------------------------------------------------

  public HDT_Hub(RecordState xmlState, DatasetAccessor<HDT_Hub> dataset)
  {
    super(xmlState, dataset);

    hub = this;
  }

//---------------------------------------------------------------------------

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
      internalErrorPopup(72950);
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

  public static boolean canUnite(HDT_RecordWithMainText record1, HDT_RecordWithMainText record2, StringBuilder errorMsg)
  {
    if (record2.getType() == record1.getType())
      return assignSBandReturnFalse(errorMsg, "You cannot unite records of the same type.");

    if ((record1.isUnitable() == false) || (record2.isUnitable() == false))
      return assignSBandReturnFalse(errorMsg, "One or more of the records are not of a unitable type.");

    if (((record1.getType() == hdtPosition) && (record2.getType() == hdtDebate)) ||
        ((record2.getType() == hdtPosition) && (record1.getType() == hdtDebate)))
      return assignSBandReturnFalse(errorMsg, "A position record and a problem/debate record cannot be united.");

    if (isUnstoredRecord(record1))
      return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record1.getType()) + " record cannot be united with another record.");

    if (isUnstoredRecord(record2))
      return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record2.getType()) + " record cannot be united with another record.");

    if (record2.hasHub())
    {
      if (record2.getHub().spokes.get(record1.getType()) != null)
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record2.getType()) + " record is already united with a " + getTypeName(record1.getType()) + " record.");

      if ((record1.getType() == hdtDebate) && (record2.getHub().spokes.get(hdtPosition) != null))
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record2.getType()) + " record is already united with a " + getTypeName(hdtPosition) + " record.");

      if ((record1.getType() == hdtPosition) && (record2.getHub().spokes.get(hdtDebate) != null))
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record2.getType()) + " record is already united with a " + getTypeName(hdtDebate) + " record.");

      if (record1.hasHub())
        return assignSBandReturnFalse(errorMsg, "Both records are already united with other records.");
    }

    if (record1.hasHub())
    {
      if (record1.getHub().spokes.get(record2.getType()) != null)
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record1.getType()) + " record is already united with a " + getTypeName(record2.getType()) + " record.");

      if ((record2.getType() == hdtDebate) && (record1.getHub().spokes.get(hdtPosition) != null))
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record1.getType()) + " record is already united with a " + getTypeName(hdtPosition) + " record.");

      if ((record2.getType() == hdtPosition) && (record1.getHub().spokes.get(hdtDebate) != null))
        return assignSBandReturnFalse(errorMsg, "The selected " + getTypeName(record1.getType()) + " record is already united with a " + getTypeName(hdtDebate) + " record.");
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean assignSBandReturnFalse(StringBuilder sb, String str)
  {
    assignSB(sb, str);
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Unites spoke1 and spoke2
   * @param spoke1 First record to unite
   * @param spoke2 Second record to unite
   * @param newDesc What to use for the description for both records
   * @throws HyperDataException if spoke1 and spoke2 cannot be united for some reason
   */
  public static void uniteRecords(HDT_RecordWithMainText spoke1, HDT_RecordWithMainText spoke2, String newDesc) throws HyperDataException
  {
    StringBuilder sb = new StringBuilder();

    if (canUnite(spoke1, spoke2, sb) == false)
      throw new HyperDataException(sb.toString());

    HDT_Hub hub;

    if      (spoke1.hasHub()) hub = spoke1.getHub();
    else if (spoke2.hasHub()) hub = spoke2.getHub();
    else                      hub = db.createNewBlankRecord(hdtHub);

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

    hub.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Disconnects a spoke from the hub and deletes the hub if it has no more spokes.
   *
   * @param spokeType the type of the spoke to disconnect.
   * @return true if the spoke was successfully disconnected, false if the spoke was not found.
   */
  public boolean disuniteRecord(RecordType spokeType)
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
      return disuniteRecord(List.copyOf(spokes.keySet()).get(0));

    if (spokes.isEmpty())
      db.deleteRecord(this);  // Hub now has no more spokes, must be sacrificed

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
