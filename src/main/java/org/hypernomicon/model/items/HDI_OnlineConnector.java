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

import java.util.ArrayList;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.Tag.*;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.items.HDI_OfflineConnector;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordWithConnector;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.items.HDI_OfflineConnector.DisplayItem;

//---------------------------------------------------------------------------

public class HDI_OnlineConnector extends HDI_OnlineBase<HDI_OfflineConnector>
{
  private Connector connector = null;

  //---------------------------------------------------------------------------

  public HDI_OnlineConnector(HDI_Schema newSchema, HDT_RecordWithConnector newRecord)
  {
    super(newSchema, newRecord);

    newRecord.initConnector();
    connector = newRecord.getConnector();

    if (record.getType() != hdtHub)                 // MainText reference should be reset when creating a new Online Item, in case it points to
      connector.mainText = new MainText(connector); // an existing MainText of a linked Hub. If that is the case, it will be pointed back to the
  }                                                 // Hub MainText later in HDT_Record.restoreTo, after the main loop of that procedure

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MainText getMainText()          { return connector.getMainText(); }
  public Connector getConnector()         { return connector; }
  @Override public void expire()          { connector.expire(); }

  @Override public void resolvePointers() throws HDB_InternalError { connector.resolvePointers(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineConnector val, Tag tag) throws RelationCycleException
  {
    MainText mainText = getMainText();

    switch (tag)
    {
      case tagDisplayRecord :

        mainText.displayItems.clear();

        if (val.displayItems.size() > 0)
        {
          for (HDI_OfflineConnector.DisplayItem displayItem : val.displayItems)
          {
            if (displayItem.type == diRecord)
            {
              HDT_RecordWithConnector displayed = (HDT_RecordWithConnector) db.records(displayItem.recordType).getByID(displayItem.recordID);

              mainText.displayItems.add(new MainText.DisplayItem(displayed));

              db.handleDisplayRecord(mainText, displayed.getMainText(), true);
            }
            else if (displayItem.type == diKeyWorks)
            {
              if (MainText.typeHasKeyWorks(record.getType()))
                mainText.displayItems.add(new MainText.DisplayItem(diKeyWorks));
            }
            else
              mainText.displayItems.add(new MainText.DisplayItem(displayItem.type));
          }
        }
        else
          mainText.addDefaultItems();

        break;

      case tagKeyWork :

        mainText.keyWorks.clear();

        for (KeyWork keyWork : val.keyWorks)
        {
          if ((keyWork.getRecordType() != hdtWork) && (keyWork.getRecordType() != hdtMiscFile))
          {
            messageDialog("Internal error #49283", mtError);
            return;
          }

          HDT_RecordWithPath keyWorkRecord = keyWork.getRecord();
          mainText.keyWorks.add(keyWork.getOnlineCopy());

          HDT_RecordState recordState = val.recordState;

          if (recordState.type == hdtHub)
          {
            HDI_OfflineHubSpokes spokes = (HDI_OfflineHubSpokes) recordState.items.get(tagLinkedRecord);

            if (spokes.debateID   > 0) db.handleKeyWork(db.debates  .getByID(spokes.debateID  ), keyWorkRecord, true);
            if (spokes.positionID > 0) db.handleKeyWork(db.positions.getByID(spokes.positionID), keyWorkRecord, true);
            if (spokes.noteID     > 0) db.handleKeyWork(db.notes    .getByID(spokes.noteID    ), keyWorkRecord, true);
            if (spokes.conceptID  > 0) db.handleKeyWork(db.concepts .getByID(spokes.conceptID ), keyWorkRecord, true);
          }
          else
            db.handleKeyWork(connector.getSpoke(), keyWorkRecord, true);
        }

        break;

      case tagHub : return; // this gets taken care of in HDT_Record.restoreTo

      default :

        if (val.htmlText.length() == 0)
          mainText.setInternal("", "");
        else
          mainText.setInternal(val.htmlText, extractTextFromHTML(val.htmlText).trim());
        return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineConnector val, Tag tag)
  {
    MainText mainText = getMainText();

    switch (tag)
    {
      case tagHub :

        if (connector.isLinked())
          val.hubID = connector.getHub().getID();
        else
          val.hubID = -1;
        break;

      case tagDisplayRecord :

        val.displayItems.clear();

        for (MainText.DisplayItem displayItem : mainText.displayItems)
        {
          if (displayItem.type == diRecord)
            val.displayItems.add(new DisplayItem(displayItem.record.getID(), displayItem.record.getType()));
          else
            val.displayItems.add(new DisplayItem(displayItem.type));
        }

        break;

      case tagKeyWork :

        val.keyWorks.clear();

        for (KeyWork keyWork : mainText.keyWorks)
        {
          HDT_Base record = keyWork.getRecord();

          if ((record.getType() != hdtWork) && (record.getType() != hdtMiscFile))
          {
            messageDialog("Internal error #59047", mtError);
            return;
          }

          val.keyWorks.add(keyWork.getOfflineCopy());
        }

        break;

      default :

        if (mainText.getPlain().matches(".*\\p{Alnum}.*") == false)
          val.htmlText = "";
        else
          val.htmlText = mainText.getHtml();

        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords)
  {
    list.add(getMainText().getPlainForDisplay());  // Important: this needs to call the function, not access the member directly
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    switch (tag)
    {
      case tagDisplayRecord :
        return getMainText().getDisplayItemsString();

      case tagKeyWork :
        return getMainText().getKeyWorksString();

      case tagHub :
        return "";

      default :
        return getMainText().getPlain();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
