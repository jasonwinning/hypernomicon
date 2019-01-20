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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

public class HDI_OfflineConnector extends HDI_OfflineBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class DisplayItem
  {
    public DisplayItem(int recordID, HDT_RecordType recordType)
    {
      this.recordID = recordID;
      this.recordType = recordType;
    }

    public DisplayItem(DisplayItemType type)
    {
      this.type = type;
    }

    public DisplayItemType type = diRecord;
    public int recordID = -1;
    public HDT_RecordType recordType = hdtNone;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int hubID;
  String htmlText;
  ArrayList<DisplayItem> displayItems;
  ArrayList<KeyWork> keyWorks;
  HashMap<HDT_RecordType, Set<Integer>> usedKeyWorks;

  private static HashMap<String, DisplayItemType> strToItemType = null;

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

  public List<KeyWork> getKeyWorks() { return Collections.unmodifiableList(keyWorks); }
  public int getHubID()              { return hubID; }

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

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems)
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
        return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DI_TYPE_DESC = "description";
  private static final String DI_TYPE_RECORD = "record";
  private static final String DI_TYPE_KEY_WORKS = "key_works";

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

        for (DisplayItem displayItem : displayItems)
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
        }

        break;

      case tagKeyWork :

        for (KeyWork keyWork : keyWorks)
          writePointerTag(xml, tag, keyWork.getRecordID(), keyWork.getRecordType(), keyWork.getSearchKey(false));

        break;

      default :

        writeStringTag(xml, tag, htmlText);
        return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
