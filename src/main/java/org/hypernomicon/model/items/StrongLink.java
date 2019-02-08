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
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import static java.util.Objects.*;

import java.util.LinkedHashSet;
import java.util.Set;

import org.hypernomicon.model.records.*;

public class StrongLink
{
  private boolean alreadyModifying = false;

  Connector noteSpoke, conceptSpoke, debateSpoke, positionSpoke, labelSpoke;
  HDT_Hub hub = null;

//---------------------------------------------------------------------------

  StrongLink(HDT_Hub hub)
  {
    this.hub = hub;
  }

//---------------------------------------------------------------------------

  public HDT_Hub       getHub()      { return hub; }
  public HDT_Note      getNote()     { return (HDT_Note     ) nullSwitch(noteSpoke    , null, sp -> sp.getSpoke()); }
  public HDT_Concept   getConcept()  { return (HDT_Concept  ) nullSwitch(conceptSpoke , null, sp -> sp.getSpoke()); }
  public HDT_Debate    getDebate()   { return (HDT_Debate   ) nullSwitch(debateSpoke  , null, sp -> sp.getSpoke()); }
  public HDT_Position  getPosition() { return (HDT_Position ) nullSwitch(positionSpoke, null, sp -> sp.getSpoke()); }
  public HDT_WorkLabel getLabel()    { return (HDT_WorkLabel) nullSwitch(labelSpoke   , null, sp -> sp.getSpoke()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void modifyNow()
  {
    if (db.runningConversion || alreadyModifying) return;

    alreadyModifying = true;

    if (nonNull(hub          )) hub          .modifyNow();
    if (nonNull(noteSpoke    )) noteSpoke    .modifyNow();
    if (nonNull(conceptSpoke )) conceptSpoke .modifyNow();
    if (nonNull(debateSpoke  )) debateSpoke  .modifyNow();
    if (nonNull(positionSpoke)) positionSpoke.modifyNow();
    if (nonNull(labelSpoke   )) labelSpoke   .modifyNow();

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<Connector> getSpokes()
  {
    Set<Connector> set = new LinkedHashSet<>();

    for (HDT_RecordType cType : new HDT_RecordType[] { hdtDebate, hdtPosition, hdtConcept, hdtNote, hdtWorkLabel })
      nullSwitch(getSpoke(cType), spoke -> set.add(spoke));

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Connector getSpoke(HDT_RecordType cType)
  {
    switch (cType)
    {
      case hdtNote      : return noteSpoke;
      case hdtPosition  : return positionSpoke;
      case hdtDebate    : return debateSpoke;
      case hdtConcept   : return conceptSpoke;
      case hdtWorkLabel : return labelSpoke;

      default : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean connectRecords(Connector spoke1, Connector spoke2, String newDesc)
  {
    HDT_Hub hub = null;
    StrongLink link = null;

    if ((spoke1.getType() == hdtPosition) && (spoke2.getType() == hdtDebate))  // Sanity checks
      return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
    if ((spoke2.getType() == hdtPosition) && (spoke1.getType() == hdtDebate))
      return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
    if (spoke1.getType() == spoke2.getType())
      return falseWithErrorMessage("Two records of the same type cannot be linked together.");
    if ((spoke1.getSpoke().isUnitable() == false) || (spoke2.getSpoke().isUnitable() == false))
      return falseWithErrorMessage("One or more of the records are not of a linkable type.");

    Connector spokes[] = new Connector[2];

    if (spoke1.isLinked())
    {
      if (spoke2.isLinked())
        return falseWithErrorMessage("Both records are already linked to another record.");

      link = spoke1.getLink();
      hub = link.hub;

      if ((link.getPosition() != null) || (link.getDebate() != null))
        if ((spoke2.getType() == hdtPosition) || (spoke2.getType() == hdtDebate))
        {
          if (link.getSpoke(spoke2.getType()) == null)
            return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
          else
            return falseWithErrorMessage("Two records of the same type cannot be linked together.");
        }
    }

    else if (spoke2.isLinked())
    {
      link = spoke2.getLink();
      hub = link.hub;

      if ((link.getPosition() != null) || (link.getDebate() != null))
        if ((spoke1.getType() == hdtPosition) || (spoke1.getType() == hdtDebate))
        {
          if (link.getSpoke(spoke1.getType()) == null)
            return falseWithErrorMessage("A position record and a problem/debate record cannot be linked together.");
          else
            return falseWithErrorMessage("Two records of the same type cannot be linked together.");
        }
    }

    else
    {
      hub = db.createNewBlankRecord(hdtHub);
      link = hub.getLink();
    }

    spokes[0] = spoke1;
    spokes[1] = spoke2;

    for (int ndx = 0; ndx < 2; ndx++)
    {
      switch (spokes[ndx].getType())
      {
        case hdtNote      : link.noteSpoke     = spokes[ndx]; break;
        case hdtPosition  : link.positionSpoke = spokes[ndx]; break;
        case hdtDebate    : link.debateSpoke   = spokes[ndx]; break;
        case hdtConcept   : link.conceptSpoke  = spokes[ndx]; break;
        case hdtWorkLabel : link.labelSpoke    = spokes[ndx]; break;

        default : break;
      }
    }

    MainText mainText = new MainText(spoke1.getMainText(), spoke2.getMainText(), hub.getConnector(), newDesc);

    db.replaceMainText(hub.getMainText(), mainText);
    hub.getConnector().mainText = mainText;

    spoke1.link = link;
    spoke2.link = link;

    if      (spoke1.getSpoke().name().length() == 0) spoke1.getSpoke().setName(spoke2.getSpoke().name());
    else if (spoke2.getSpoke().name().length() == 0) spoke2.getSpoke().setName(spoke1.getSpoke().name());

    for (Connector spoke : spokes)
    {
      db.replaceMainText(spoke.mainText, mainText);
      spoke.mainText = mainText;

      if (spoke.getType() == hdtWorkLabel)
        ((HDT_WorkLabel) spoke.getSpoke()).refreshSubjects();
    }

    link.modifyNow();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean disconnectRecord(HDT_RecordType spokeType, boolean deleteHub)
  {
    if (hub == null) return false;

    Connector firstSpoke = null, otherSpoke = null;
    int numSpokes = 0;

    // check number of spokes

    if (nonNull(conceptSpoke )) { numSpokes++; if (spokeType != hdtConcept  ) otherSpoke = getSpoke(hdtConcept  ); }
    if (nonNull(positionSpoke)) { numSpokes++; if (spokeType != hdtPosition ) otherSpoke = getSpoke(hdtPosition ); }
    if (nonNull(debateSpoke  )) { numSpokes++; if (spokeType != hdtDebate   ) otherSpoke = getSpoke(hdtDebate   ); }
    if (nonNull(noteSpoke    )) { numSpokes++; if (spokeType != hdtNote     ) otherSpoke = getSpoke(hdtNote     ); }
    if (nonNull(labelSpoke   )) { numSpokes++; if (spokeType != hdtWorkLabel) otherSpoke = getSpoke(hdtWorkLabel); }

    if (numSpokes == 0) return false;

    // Disconnect the spoke

    switch (spokeType)
    {
      case hdtNote      : firstSpoke = getSpoke(hdtNote     ); noteSpoke     = null; break;
      case hdtDebate    : firstSpoke = getSpoke(hdtDebate   ); debateSpoke   = null; break;
      case hdtPosition  : firstSpoke = getSpoke(hdtPosition ); positionSpoke = null; break;
      case hdtConcept   : firstSpoke = getSpoke(hdtConcept  ); conceptSpoke  = null; break;
      case hdtWorkLabel : firstSpoke = getSpoke(hdtWorkLabel); labelSpoke    = null; break;

      default :           return false;
    }

    hub.modifyNow();

    firstSpoke.link = null;
    firstSpoke.mainText = new MainText(hub.getMainText(), firstSpoke);

    // Done disconnecting, now need to disconnect other connector if only one left

    if (numSpokes > 2)  return true;
    if (numSpokes == 2) return disconnectRecord(otherSpoke.getType(), deleteHub);

    // Hub now has no more spokes, must be sacrificed

    if (deleteHub) db.deleteRecord(hdtHub, hub.getID());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
