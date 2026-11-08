/*
 * Copyright 2015-2026 Jason Winning
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
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDI_OfflineMainTextAndHub.DisplayItem;
import org.hypernomicon.model.items.HDI_OnlineBase;

//---------------------------------------------------------------------------

public class HDI_OnlineMainTextAndHub extends HDI_OnlineBase<HDI_OfflineMainTextAndHub>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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
    // Spoke and hub records share a single MainText object

    if (recordWMT.getType() == hdtHub)
    {
      // For each key work, remove it from the index and run handler (remove from TreeView)

      recordWMT.keyWorksUnmod().forEach(keyWork -> db.handleKeyWork(recordWMT, keyWork.getRecord(), false));
    }
    else
    {
      if (hasHub())
      {
        // A record whose hub does not list it (only hand-edited files produce one) shares the hub's text
        // with the hub's real spokes and has nothing of its own to expire; disuniting by type would
        // disconnect whichever record of this type the hub does list

        if (getHub().spokes.get(recordWMT.getType()) != recordWMT)
          return;

        getHub().disuniteRecord(recordWMT.getType());
      }

      getMainText().expire();
    }

    // By now the text is this record's alone: a hub is deleted only once it has no spokes, and the
    // disunite above gave a spoke a text of its own. A deleted record's text displays nothing.

    db.removeDisplayer(getMainText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    HDT_Hub hub = recordWMT.hub;  // A hub's own hub is itself

    if ((recordWMT.getType() != hdtHub) && (hub != null) && (HDT_Record.isEmptyThrowsException(hub, false) || (hub.spokes.get(recordWMT.getType()) != recordWMT)))
    {
      // Only an out-of-sync set of files produces a record that names a hub which does not list it, or
      // which no longer exists. Such a record was sharing the hub's text without being a member of the
      // unity, so it was not registered for the key works (only listed members are; see
      // setFromOfflineValue). Give it a text of its own, as a disunite does, and register it

      recordWMT.hub = null;
      recordWMT.mainText = new MainText(recordWMT.mainText, recordWMT);
      recordWMT.mainText.resolvePointers();

      recordWMT.keyWorksStream().forEach(keyWork -> db.handleKeyWork(recordWMT, keyWork.getRecord(), true));
      recordWMT.modifyNow();
      return;
    }

    getMainText().resolvePointers();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineMainTextAndHub val, Tag tag)
  {
    final MainText mainText = getMainText();

    // A united record's description, display items and key works are its hub's. The XML files store them
    // only with the hub (see HDI_OfflineMainTextAndHub.writeToXml), so a saved copy read from them has
    // none, and the hub's own restore puts them back. Restoring them here as well would empty the shared
    // text; at load that was hidden because hubs are restored after their united records.

    if ((val.hubID > 0) && (tag != tagHub))
      return;

    switch (tag)
    {
      case tagDisplayRecord :

        // Unregister the record display items being replaced; the ones the backup names are registered again below

        mainText.getRecordDisplayItems().forEach(displayed -> db.handleDisplayRecord(mainText, displayed, false));
        mainText.displayItems.clear();

        if (val.displayItems.size() > 0)
        {
          val.displayItems.forEach(displayItem ->
          {
            if (displayItem.type == diRecord)
            {
              HDT_RecordWithMainText displayed = (HDT_RecordWithMainText) db.records(displayItem.recordType).getByID(displayItem.recordID);

              if (HDT_Record.isEmpty(displayed, false) == false)
              {
                mainText.displayItems.add(new MainText.DisplayItem(displayed));

                db.handleDisplayRecord(mainText, displayed, true);
              }
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

        // Unregister the key works the backup does not have, for every record they were registered for
        // (each of a hub's united records), as MainText.setKeyWorksFromList does

        Set<HDT_Record> backupKeyWorkRecords = val.keyWorks.stream().map(KeyWork::getRecord).collect(Collectors.toSet());

        mainText.keyWorksStream().map(KeyWork::getRecord).filter(Predicate.not(backupKeyWorkRecords::contains))
                .forEach(keyWorkRecord -> mainText.runKeyWorkHandler(keyWorkRecord, false));

        mainText.keyWorks.clear();

        for (KeyWork keyWork : val.keyWorks)
        {
          if ((keyWork.getRecordType() != hdtWork) && (keyWork.getRecordType() != hdtMiscFile))
          {
            internalErrorPopup(49283);
            return;
          }

          HDT_RecordWithAuthors<? extends RecordAuthors> keyWorkRecord = keyWork.getRecord();

          // Only an out-of-sync set of files, or a saved copy from before a deletion, names a key work that
          // no record has. Registering it would file the record under a null key work record, and
          // resolvePointers would drop it anyway

          if (keyWorkRecord == null) continue;

          mainText.keyWorks.add(keyWork.getOnlineCopy());

          RecordState recordState = val.getRecordState();

          if (recordState.type == hdtHub)
          {
            HDI_OfflineHubSpokes spokes = (HDI_OfflineHubSpokes) recordState.items.get(tagSpokeRecord);

            if (spokes.debateID   > 0) nullSwitch(db.debates   .getByID(spokes.debateID  ), debate   -> db.handleKeyWork(debate  , keyWorkRecord, true));
            if (spokes.positionID > 0) nullSwitch(db.positions .getByID(spokes.positionID), position -> db.handleKeyWork(position, keyWorkRecord, true));
            if (spokes.noteID     > 0) nullSwitch(db.notes     .getByID(spokes.noteID    ), note     -> db.handleKeyWork(note    , keyWorkRecord, true));
            if (spokes.conceptID  > 0) nullSwitch(db.concepts  .getByID(spokes.conceptID ), concept  -> db.handleKeyWork(concept , keyWorkRecord, true));
            if (spokes.labelID    > 0) nullSwitch(db.workLabels.getByID(spokes.labelID   ), label    -> db.handleKeyWork(label   , keyWorkRecord, true));
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

        mainText.setInternal(val.htmlText);
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

        val.hubID = HDT_Record.getIDSafe(getHub());
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
            internalErrorPopup(59047);
            return;
          }

          val.keyWorks.add(keyWork.getOfflineCopy());
        }

        break;

      case tagMainText : // This tag is always redundant because there is always another tag that is the actual main text
                         // tag for the record type. It exists only for the Main Text query result column to work.
        break;

      default :

        val.htmlText = mainText.getPlain().isBlank() ? "" : mainText.getHtml();  // Avoid saving empty HTML boilerplate
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * {@inheritDoc}
   */
  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords, boolean engChar)
  {
    switch (tag)
    {
      case tagDisplayRecord : case tagKeyWork : case tagHub :
        return;
      default:
        break;
    }

    String str = getMainText().getPlainForDisplay();  // Important: this needs to call the function, not access the member directly

    list.add(engChar ? convertToEnglishChars(str) : str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items) { return switch (tag)
  {
    case tagDisplayRecord -> getMainText().getDisplayItemsString();
    case tagKeyWork       ->
    {
      Stream<String> stream = getMainText().keyWorks.stream().map(keyWork -> keyWork.getRecord().defaultChoiceText())
                                                             .filter(Predicate.not(String::isBlank));

      yield limitTo20Items ?
        stream.limit(20).collect(Collectors.joining("; "))
      :
        stream.collect(Collectors.joining("; "));
    }
    case tagHub           -> "";
    default               -> getMainText().getPlain();
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getResultCount(Tag tag) { return switch (tag)
  {
    case tagDisplayRecord -> (int) getMainText().displayItems.stream().filter(item -> item.type == diRecord).count();
    case tagKeyWork       -> getMainText().keyWorks.size();
    case tagHub           -> recordWMT.getHub() == null ? 0 : 1;

    default               -> getMainText().getPlain().isBlank() ? 0 : 1;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
