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

import java.util.List;
import java.util.function.Predicate;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.Tag.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.unities.HDI_OfflineMainTextAndHub.DisplayItem;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.model.items.HDI_OnlineBase;

//---------------------------------------------------------------------------

public class HDI_OnlineMainTextAndHub extends HDI_OnlineBase<HDI_OfflineMainTextAndHub>
{
  private final HDT_RecordWithMainText recordWMT;

  private HDT_Hub getHub()       { return recordWMT.getHub(); }
  private boolean hasHub()       { return recordWMT.hasHub(); }
  private MainText getMainText() { return recordWMT.getMainText(); }

  //---------------------------------------------------------------------------

  public HDI_OnlineMainTextAndHub(HDI_Schema schema, HDT_RecordWithMainText record)
  {
    super(schema, record);

    recordWMT = record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    if (recordWMT.getType() == hdtHub) return;

    if (hasHub())
      getHub().disuniteRecord(recordWMT.getType(), false);

    getMainText().expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineMainTextAndHub val, Tag tag)
  {
    final MainText mainText = getMainText();

    switch (tag)
    {
      case tagDisplayRecord :

        mainText.displayItems.clear();

        if (val.displayItems.size() > 0)
        {
          val.displayItems.forEach(displayItem ->
          {
            if (displayItem.type == diRecord)
            {
              HDT_RecordWithMainText displayed = (HDT_RecordWithMainText) db.records(displayItem.recordType).getByID(displayItem.recordID);

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
          });
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

          RecordState recordState = val.getRecordState();

          if (recordState.type == hdtHub)
          {
            HDI_OfflineHubSpokes spokes = (HDI_OfflineHubSpokes) recordState.items.get(tagLinkedRecord);

            if (spokes.debateID   > 0) db.handleKeyWork(db.debates  .getByID(spokes.debateID  ), keyWorkRecord, true);
            if (spokes.positionID > 0) db.handleKeyWork(db.positions.getByID(spokes.positionID), keyWorkRecord, true);
            if (spokes.noteID     > 0) db.handleKeyWork(db.notes    .getByID(spokes.noteID    ), keyWorkRecord, true);
            if (spokes.conceptID  > 0) db.handleKeyWork(db.concepts .getByID(spokes.conceptID ), keyWorkRecord, true);
          }
          else
            db.handleKeyWork(recordWMT, keyWorkRecord, true);
        }

        break;

      case tagHub :

        if (val.hubID < 1)
          return;

        HDT_Hub hub = db.hubs.getByID(val.hubID);
        recordWMT.hub = hub;
        db.replaceMainText(mainText, hub.getMainText());
        recordWMT.mainText = hub.getMainText();
        return;

      default :

        if (val.htmlText.isEmpty())
          mainText.setInternal("", "");
        else
          mainText.setInternal(val.htmlText, MainTextUtil.extractTextFromHTML(val.htmlText).trim());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineMainTextAndHub val, Tag tag)
  {
    MainText mainText = getMainText();

    switch (tag)
    {
      case tagHub :

        val.hubID = nullSwitch(getHub(), -1, HDT_Record::getID);
        break;

      case tagDisplayRecord :

        val.displayItems.clear();

        mainText.displayItems.forEach(displayItem ->
          val.displayItems.add(displayItem.type == diRecord ?
            new DisplayItem(displayItem.record.getID(), displayItem.record.getType())
          :
            new DisplayItem(displayItem.type)));

        break;

      case tagKeyWork :

        val.keyWorks.clear();

        for (KeyWork keyWork : mainText.keyWorks)
        {
          HDT_Record kwRecord = keyWork.getRecord();

          if ((kwRecord.getType() != hdtWork) && (kwRecord.getType() != hdtMiscFile))
          {
            messageDialog("Internal error #59047", mtError);
            return;
          }

          val.keyWorks.add(keyWork.getOfflineCopy());
        }

        break;

      case tagMainText : // This tag is always redundant because there is always another tag that is the actual main text
                         // tag for the record type. It exists only for the Main Text query result column to work.
        break;

      default :

        val.htmlText = mainText.getPlain().matches(".*\\p{Alnum}.*") ? mainText.getHtml() : "";
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    list.add(getMainText().getPlainForDisplay());  // Important: this needs to call the function, not access the member directly
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    switch (tag)
    {
      case tagDisplayRecord : return getMainText().getDisplayItemsString();
      case tagKeyWork       :

        return getMainText().keyWorks.stream().map(keyWork -> keyWork.getRecord().getCBText())
                                              .filter(Predicate.not(String::isBlank))
                                              .limit(20)
                                              .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
      case tagHub           : return "";
      default               : return getMainText().getPlain();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
