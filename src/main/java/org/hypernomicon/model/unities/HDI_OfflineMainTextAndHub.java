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

import org.hypernomicon.model.*;
import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.MainText.DisplayItemType;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.unities.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;

import java.util.*;

//---------------------------------------------------------------------------

public class HDI_OfflineMainTextAndHub extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class DisplayItem
  {
    DisplayItem(int recordID, RecordType recordType) { this(recordID, recordType, diRecord); }
    DisplayItem(DisplayItemType type)                { this(-1, hdtNone, type);              }

    private DisplayItem(int recordID, RecordType recordType, DisplayItemType type)
    {
      this.recordID = recordID;
      this.recordType = recordType;
      this.type = type;
    }

    final DisplayItemType type;
    final int recordID;
    final RecordType recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int hubID;
  String htmlText;
  final List<DisplayItem> displayItems;
  final List<KeyWork> keyWorks;
  private final Map<RecordType, Set<Integer>> usedKeyWorks;

//---------------------------------------------------------------------------

  public HDI_OfflineMainTextAndHub(HDI_Schema schema, RecordState recordState)
  {
    super(schema, recordState);
    htmlText = "";

    hubID = -1;
    keyWorks = new ArrayList<>();
    displayItems = new ArrayList<>();
    usedKeyWorks = new EnumMap<>(RecordType.class);
  }

//---------------------------------------------------------------------------

  int getHubID()               { return hubID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(HDX_Element element, String nodeText, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    switch (element.getTag())
    {
      case tagHub :

        hubID = element.getObjID();
        break;

      case tagDisplayRecord :

        switch (nodeText)
        {
          case DI_TYPE_DESC      : displayItems.add(new DisplayItem(diDescription                           )); break;
          case DI_TYPE_KEY_WORKS : displayItems.add(new DisplayItem(diKeyWorks                              )); break;
          case DI_TYPE_RECORD    : displayItems.add(new DisplayItem(element.getObjID(), element.getObjType())); break;
        }

        break;

      case tagKeyWork :

        if ((element.getObjType() == hdtWork) || (element.getObjType() == hdtMiscFile))
        {
          Set<Integer> idSet = usedKeyWorks.computeIfAbsent(element.getObjType(), _ -> new HashSet<>());

          if (idSet.contains(element.getObjID()) == false)
          {
            keyWorks.add(new KeyWork(element.getObjType(), element.getObjID(), nodeText, false));
            idSet.add(element.getObjID());
          }
        }

        break;

      default :

        htmlText = nodeText;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DI_TYPE_DESC      = "description",
                              DI_TYPE_RECORD    = "record",
                              DI_TYPE_KEY_WORKS = "key_works";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    if (tag == tagHub)
    {
      if (hubID > 0)
        writePointerTag(xml, tag, hubID, hdtNone, db.hubs.getByID(hubID).getXMLObjectName());

      return;
    }

    if (hubID > 0) return;

    switch (tag)
    {
      case tagDisplayRecord :

        displayItems.forEach(displayItem ->
        {
          switch (displayItem.type)
          {
            case diDescription:

              writeStringTag(xml, tag, DI_TYPE_DESC);
              break;

            case diKeyWorks:

              writeStringTag(xml, tag, DI_TYPE_KEY_WORKS);
              break;

            case diRecord:

              writePointerTag(xml, tag, displayItem.recordID, displayItem.recordType, DI_TYPE_RECORD);
              break;

            default:
              break;
          }
        });

        break;

      case tagKeyWork :

        keyWorks.forEach(keyWork ->
        {
          String text = recordState.type == hdtInvestigation ? keyWork.getRecord().getCBText() : keyWork.getSearchKey(false);
          writePointerTag(xml, tag, keyWork.getRecordID(), keyWork.getRecordType(), text);
        });

        break;

      case tagMainText : // This tag is always redundant because there is always another tag that is the actual main text
                         // tag for the record type. It exists only for the Main Text query result column to work.
        break;

      default :

        writeStringTag(xml, tag, htmlText);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
