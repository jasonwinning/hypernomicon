/*
 * Copyright 2015-2020 Jason Winning
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

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.items.MainText.DisplayItemType;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;
import static org.hypernomicon.model.records.HDT_RecordState.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.items.MainText.DisplayItemType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class HDI_OfflineConnector extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class DisplayItem
  {
    DisplayItem(int recordID, HDT_RecordType recordType) { this(recordID, recordType, diRecord); }
    DisplayItem(DisplayItemType type)                    { this(-1, hdtNone, type);              }

    private DisplayItem(int recordID, HDT_RecordType recordType, DisplayItemType type)
    {
      this.recordID = recordID;
      this.recordType = recordType;
      this.type = type;
    }

    final DisplayItemType type;
    final int recordID;
    final HDT_RecordType recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int hubID;
  String htmlText;
  final List<DisplayItem> displayItems;
  final List<KeyWork> keyWorks;
  final Map<HDT_RecordType, Set<Integer>> usedKeyWorks;

  private static Map<String, DisplayItemType> strToItemType = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_OfflineConnector(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
    htmlText = "";

    hubID = -1;
    keyWorks = new ArrayList<>();
    displayItems = new ArrayList<>();
    usedKeyWorks = new HashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getHubID() { return hubID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static private void initMap()
  {
    strToItemType = new HashMap<>();

    strToItemType.put(DI_TYPE_DESC, diDescription);
    strToItemType.put(DI_TYPE_RECORD, diRecord);
    strToItemType.put(DI_TYPE_KEY_WORKS, diKeyWorks);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, Map<Tag, HDI_OfflineBase> nestedItems)
  {
    switch (tag)
    {
      case tagHub :

        hubID = objID;
        break;

      case tagDisplayRecord :

        if (strToItemType == null) initMap();
        DisplayItemType itemType = strToItemType.get(nodeText);

        switch (itemType)
        {
          case diRecord: displayItems.add(new DisplayItem(objID, objType)); break;
          default:       displayItems.add(new DisplayItem(itemType));       break;
        }
        return;

      case tagKeyWork :

        if ((objType == hdtWork) || (objType == hdtMiscFile))
        {
          Set<Integer> idSet = usedKeyWorks.get(objType);
          if (idSet == null)
          {
            idSet = new HashSet<>();
            usedKeyWorks.put(objType, idSet);
          }

          if (idSet.contains(objID) == false)
          {
            keyWorks.add(new KeyWork(objType, objID, nodeText, false));
            idSet.add(objID);
          }
        }
        return;

      default :

        htmlText = nodeText;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DI_TYPE_DESC = "description",
                              DI_TYPE_RECORD = "record",
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
          writePointerTag(xml, tag, keyWork.getRecordID(), keyWork.getRecordType(), keyWork.getSearchKey(false)));

        break;

      default :

        writeStringTag(xml, tag, htmlText);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
