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
import static org.hypernomicon.util.Util.*;

import java.util.Arrays;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.*;

import com.google.common.collect.ImmutableSet;

public class StrongLink
{
  private boolean alreadyModifying = false;

  Connector noteSpoke, conceptSpoke, debateSpoke, positionSpoke, labelSpoke;
  final HDT_Hub hub;

//---------------------------------------------------------------------------

  StrongLink(HDT_Hub hub)
  {
    this.hub = hub;
  }

//---------------------------------------------------------------------------

  public HDT_Hub       getHub     () { return hub; }
  public HDT_Note      getNote    () { return (HDT_Note     ) nullSwitch(noteSpoke    , null, Connector::getSpoke); }
  public HDT_Concept   getConcept () { return (HDT_Concept  ) nullSwitch(conceptSpoke , null, Connector::getSpoke); }
  public HDT_Debate    getDebate  () { return (HDT_Debate   ) nullSwitch(debateSpoke  , null, Connector::getSpoke); }
  public HDT_Position  getPosition() { return (HDT_Position ) nullSwitch(positionSpoke, null, Connector::getSpoke); }
  public HDT_WorkLabel getLabel   () { return (HDT_WorkLabel) nullSwitch(labelSpoke   , null, Connector::getSpoke); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void modifyNow()
  {
    if (db.runningConversion || alreadyModifying) return;

    alreadyModifying = true;

    if (hub           != null) hub          .modifyNow();
    if (noteSpoke     != null) noteSpoke    .modifyNow();
    if (conceptSpoke  != null) conceptSpoke .modifyNow();
    if (debateSpoke   != null) debateSpoke  .modifyNow();
    if (positionSpoke != null) positionSpoke.modifyNow();
    if (labelSpoke    != null) labelSpoke   .modifyNow();

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Connector> getSpokes()
  {
    return EnumSet.of(hdtDebate, hdtPosition, hdtConcept, hdtNote, hdtWorkLabel).stream().map(this::getSpoke)
                                                                                         .filter(Objects::nonNull)
                                                                                         .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Connector getSpoke(RecordType cType)
  {
    switch (cType)
    {
      case hdtNote      : return noteSpoke;
      case hdtPosition  : return positionSpoke;
      case hdtDebate    : return debateSpoke;
      case hdtConcept   : return conceptSpoke;
      case hdtWorkLabel : return labelSpoke;

      default           : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean connectRecords(Connector spoke1, Connector spoke2, String newDesc)
  {
    if ((spoke1.getType() == hdtPosition) && (spoke2.getType() == hdtDebate))  // Sanity checks
      return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
    if ((spoke2.getType() == hdtPosition) && (spoke1.getType() == hdtDebate))
      return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
    if (spoke1.getType() == spoke2.getType())
      return falseWithErrorMessage("Two records of the same type cannot be linked together.");
    if ((spoke1.getSpoke().isUnitable() == false) || (spoke2.getSpoke().isUnitable() == false))
      return falseWithErrorMessage("One or more of the records are not of a linkable type.");
    if (HyperDB.isUnstoredRecord(spoke1.getSpoke().getID(), spoke1.getType()))
      return falseWithErrorMessage("That " + db.getTypeName(spoke1.getType()) + " record cannot be linked to another record.");
    if (HyperDB.isUnstoredRecord(spoke2.getSpoke().getID(), spoke2.getType()))
      return falseWithErrorMessage("That " + db.getTypeName(spoke2.getType()) + " record cannot be linked to another record.");

    HDT_Hub hub;
    StrongLink link;

    if (spoke1.isLinked())
    {
      if (spoke2.isLinked())
        return falseWithErrorMessage("Both records are already linked to another record.");

      link = spoke1.getLink();
      hub = link.hub;

      if ((link.getPosition() != null) || (link.getDebate() != null))
        if ((spoke2.getType() == hdtPosition) || (spoke2.getType() == hdtDebate))
        {
          return falseWithErrorMessage(link.getSpoke(spoke2.getType()) == null ?
            "A position record and a problem/debate record cannot be linked together."
          :
            "Two records of the same type cannot be linked together.");
        }
    }

    else if (spoke2.isLinked())
    {
      link = spoke2.getLink();
      hub = link.hub;

      if ((link.getPosition() != null) || (link.getDebate() != null))
        if ((spoke1.getType() == hdtPosition) || (spoke1.getType() == hdtDebate))
        {
          return falseWithErrorMessage(link.getSpoke(spoke1.getType()) == null ?
            "A position record and a problem/debate record cannot be linked together."
          :
            "Two records of the same type cannot be linked together.");
        }
    }

    else
    {
      hub = db.createNewBlankRecord(hdtHub);
      link = hub.getLink();
    }

    List<Connector> spokes = Arrays.asList(spoke1, spoke2);

    spokes.forEach(spoke ->
    {
      switch (spoke.getType())
      {
        case hdtNote      : link.noteSpoke     = spoke; break;
        case hdtPosition  : link.positionSpoke = spoke; break;
        case hdtDebate    : link.debateSpoke   = spoke; break;
        case hdtConcept   : link.conceptSpoke  = spoke; break;
        case hdtWorkLabel : link.labelSpoke    = spoke; break;

        default           :                             break;
      }
    });

    MainText mainText = new MainText(spoke1.getMainText(), spoke2.getMainText(), hub.getConnector(), newDesc);

    db.replaceMainText(hub.getMainText(), mainText);
    hub.getConnector().mainText = mainText;

    spoke1.link = link;
    spoke2.link = link;

    if      (spoke1.getSpoke().name().isEmpty()) spoke1.getSpoke().setName(spoke2.getSpoke().name());
    else if (spoke2.getSpoke().name().isEmpty()) spoke2.getSpoke().setName(spoke1.getSpoke().name());

    spokes.forEach(spoke ->
    {
      db.replaceMainText(spoke.mainText, mainText);
      spoke.mainText = mainText;

      if (spoke.getType() == hdtWorkLabel)
        ((HDT_WorkLabel) spoke.getSpoke()).refreshSubjects();
    });

    link.modifyNow();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean disconnectRecord(RecordType spokeType, boolean deleteHub)
  {
    if (hub == null) return false;

    Connector firstSpoke = null, otherSpoke = null;
    int numSpokes = 0;

    // check number of spokes

    if (conceptSpoke  != null) { numSpokes++; if (spokeType != hdtConcept  ) otherSpoke = getSpoke(hdtConcept  ); }
    if (positionSpoke != null) { numSpokes++; if (spokeType != hdtPosition ) otherSpoke = getSpoke(hdtPosition ); }
    if (debateSpoke   != null) { numSpokes++; if (spokeType != hdtDebate   ) otherSpoke = getSpoke(hdtDebate   ); }
    if (noteSpoke     != null) { numSpokes++; if (spokeType != hdtNote     ) otherSpoke = getSpoke(hdtNote     ); }
    if (labelSpoke    != null) { numSpokes++; if (spokeType != hdtWorkLabel) otherSpoke = getSpoke(hdtWorkLabel); }

    if (numSpokes == 0) return false;

    // Disconnect the spoke

    switch (spokeType)
    {
      case hdtNote      : firstSpoke = getSpoke(hdtNote     ); noteSpoke     = null; break;
      case hdtDebate    : firstSpoke = getSpoke(hdtDebate   ); debateSpoke   = null; break;
      case hdtPosition  : firstSpoke = getSpoke(hdtPosition ); positionSpoke = null; break;
      case hdtConcept   : firstSpoke = getSpoke(hdtConcept  ); conceptSpoke  = null; break;
      case hdtWorkLabel : firstSpoke = getSpoke(hdtWorkLabel); labelSpoke    = null; break;

      default           : return false;
    }

    hub.modifyNow();

    firstSpoke.link = null;
    firstSpoke.mainText = new MainText(hub.getMainText(), firstSpoke);

    // Done disconnecting, now need to disconnect other connector if only one left

    if (numSpokes > 2)  return true;
    if (numSpokes == 2) return disconnectRecord(otherSpoke.getType(), deleteHub);

    // Hub now has no more spokes, must be sacrificed

    if (deleteHub) db.deleteRecord(hub);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
