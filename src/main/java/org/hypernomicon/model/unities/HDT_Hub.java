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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.util.Arrays;
import java.util.List;
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

public class HDT_Hub extends HDT_RecordWithConnector
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean alreadyModifying = false;
  HDT_Note noteSpoke;
  HDT_Concept conceptSpoke;
  HDT_Debate debateSpoke;
  HDT_Position positionSpoke;
  HDT_WorkLabel labelSpoke;

  public HDT_Hub(RecordState xmlState, HyperDataset<HDT_Hub> dataset)
  {
    super(xmlState, dataset, tagName);
  }

  public HDT_Note      getNote    () { return noteSpoke;     }
  public HDT_Concept   getConcept () { return conceptSpoke;  }
  public HDT_Debate    getDebate  () { return debateSpoke;   }
  public HDT_Position  getPosition() { return positionSpoke; }
  public HDT_WorkLabel getLabel   () { return labelSpoke;    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<HDT_RecordWithConnector> getSpokes()
  {
    return Stream.of(hdtDebate, hdtPosition, hdtConcept, hdtNote, hdtWorkLabel).map(this::getSpoke)
                                                                               .filter(Objects::nonNull);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithConnector getSpoke(RecordType spokeType)
  {
    switch (spokeType)
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

  @Override public HDT_RecordWithConnector mainSpoke()
  {
    return mainSpoke(false);
  }

  public HDT_RecordWithConnector mainSpoke(boolean prioritizeNoteOverConcept)
  {
    HDT_RecordWithConnector spoke = getDebate();
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

    if (noteSpoke     != null) noteSpoke    .modifyMainText();
    if (conceptSpoke  != null) conceptSpoke .modifyMainText();
    if (debateSpoke   != null) debateSpoke  .modifyMainText();
    if (positionSpoke != null) positionSpoke.modifyMainText();
    if (labelSpoke    != null) labelSpoke   .modifyMainText();

    alreadyModifying = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean uniteRecords(HDT_RecordWithConnector spoke1, HDT_RecordWithConnector spoke2, String newDesc)
  {
    if ((spoke1.getType() == hdtPosition) && (spoke2.getType() == hdtDebate))  // Sanity checks
      return falseWithErrorMessage("A position record and a problem/debate record cannot be united.");
    if ((spoke2.getType() == hdtPosition) && (spoke1.getType() == hdtDebate))
      return falseWithErrorMessage("A position record and a problem/debate record cannot be united.");
    if (spoke1.getType() == spoke2.getType())
      return falseWithErrorMessage("Two records of the same type cannot be united.");
    if ((spoke1.isUnitable() == false) || (spoke2.isUnitable() == false))
      return falseWithErrorMessage("One or more of the records are not of a linkable type.");
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

    List<HDT_RecordWithConnector> spokes = Arrays.asList(spoke1, spoke2);

    spokes.forEach(spoke ->
    {
      switch (spoke.getType())
      {
        case hdtNote      : hub.noteSpoke     = (HDT_Note     ) spoke; break;
        case hdtPosition  : hub.positionSpoke = (HDT_Position ) spoke; break;
        case hdtDebate    : hub.debateSpoke   = (HDT_Debate   ) spoke; break;
        case hdtConcept   : hub.conceptSpoke  = (HDT_Concept  ) spoke; break;
        case hdtWorkLabel : hub.labelSpoke    = (HDT_WorkLabel) spoke; break;

        default           : break;
      }
    });

    MainText mainText = new MainText(spoke1.getMainText(), spoke2.getMainText(), hub, newDesc);

    db.replaceMainText(hub.getMainText(), mainText);
    hub.mainText = mainText;

    spoke1.hub = hub;
    spoke2.hub = hub;

    if      (spoke1.name().isEmpty()) spoke1.setName(spoke2.name());
    else if (spoke2.name().isEmpty()) spoke2.setName(spoke1.name());

    spokes.forEach(spoke ->
    {
      db.replaceMainText(spoke.mainText, mainText);
      spoke.mainText = mainText;

      if (spoke.getType() == hdtWorkLabel)
        ((HDT_WorkLabel) spoke).refreshSubjects();
    });

    hub.modifyNow();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean disuniteRecord(RecordType spokeType, boolean deleteHub)
  {
    HDT_RecordWithConnector firstSpoke = null, otherSpoke = null;
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

    modifyNow();

    firstSpoke.hub = null;
    firstSpoke.mainText = new MainText(mainText, firstSpoke);
    numSpokes--;

    // Done disconnecting, now need to disconnect other spoke if only one left

    if (numSpokes == 1) return disuniteRecord(otherSpoke.getType(), deleteHub);

    if (deleteHub && (numSpokes == 0))
      db.deleteRecord(this);  // Hub now has no more spokes, must be sacrificed

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
