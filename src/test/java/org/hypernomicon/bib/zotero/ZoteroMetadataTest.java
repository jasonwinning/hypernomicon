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

package org.hypernomicon;

import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.bib.zotero.ZoteroAuthors.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.util.EnumSet;

import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.json.simple.parser.ParseException;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class ZoteroMetadataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void templatesTest()
  {
    JsonArray jServerTemplatesArr;
    ZoteroWrapper zoteroWrapper = assertDoesNotThrow(ZoteroWrapper::createForTesting);

    try
    {
      jServerTemplatesArr = zoteroWrapper.getTemplates(true);
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      fail("Error occurred while retrieving Zotero templates from server: " + getThrowableMessage(e));
      return;
    }

    EnumSet<EntryType> unusedTypes = EnumSet.copyOf(zoteroWrapper.getEntryTypeMap().keySet());

    for (JsonObj jServerTemplateObj : jServerTemplatesArr.getObjs())
    {
      String entryTypeStr = jServerTemplateObj.getStrSafe("itemType");

      EntryType entryType = switch (entryTypeStr)
      {
        case "webpage"  -> etWebPage;  // is supposed to be webPage, but server is inconsistent
        case "Preprint" -> etPreprint; // is supposed to be preprint, but server is inconsistent

        default         -> zoteroWrapper.getEntryTypeMap().inverse().getOrDefault(entryTypeStr, etOther);
      };

      assertNotEquals(etOther, entryType, "Unrecognized Zotero item type found in templates JSON from server: " + jServerTemplateObj.getStrSafe("itemType"));

      unusedTypes.remove(entryType);

      try
      {
        JsonObj jLocalObj = ZoteroWrapper.getTemplateInitIfNecessary(entryType);

        String serverStr = jServerTemplateObj.toString().replace("\"webPage\"", "\"webpage\"")
                                                        .replace("\"Preprint\"", "\"preprint\"");

        assertEquals(jLocalObj.toString(), serverStr, "Zotero entry templates should be the same as the ones from the server");
      }
      catch (IOException | ParseException | HDB_InternalError e)
      {
        fail("Error occurred while loading Zotero templates resource file: " + getThrowableMessage(e));
      }
    }

    assertTrue(unusedTypes.isEmpty(), "Not all local Zotero item types were found in templates JSON from server");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void creatorTypesTest()
  {
    JsonObj jServerCreatorTypesObj = null;
    ZoteroWrapper zoteroWrapper = assertDoesNotThrow(ZoteroWrapper::createForTesting);

    try
    {
      jServerCreatorTypesObj = zoteroWrapper.getCreatorTypes();
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      fail("Error occurred while retrieving Zotero creator types from server: " + getThrowableMessage(e));
    }

    // Make sure the creator types map and entry types map have the same entry types

    for (EntryType entryType : zoteroWrapper.getEntryTypeMap().keySet())
      if (creatorTypes.rowKeySet().contains(entryType) == false)
        fail("Entry types map contains entry type that is missing from creator types map: " + entryType.getUserFriendlyName());

    for (EntryType entryType : creatorTypes.rowKeySet())
      if (zoteroWrapper.getEntryTypeMap().containsKey(entryType) == false)
        fail("Creator types map contains entry type that is missing from entry types map: " + entryType.getUserFriendlyName());

    for (String serverItemTypeStr : jServerCreatorTypesObj.keySet())
    {
      // Make sure all entry types in the creator types listing from server are in local entry type map

      EntryType entryType = zoteroWrapper.getEntryTypeMap().inverse().getOrDefault(serverItemTypeStr, etOther);
      assertNotEquals(etOther, entryType, "Unrecognized Zotero item type found in creator types JSON from server: " + serverItemTypeStr);

      // Make sure each creator type in the local creator map for this entry type is
      // one of the creator types for this entry type in the list from the server

      JsonArray jServerCreatorTypesArr = jServerCreatorTypesObj.getArray(serverItemTypeStr);

      for (String creatorTypeStr : creatorTypes.row(entryType).keySet())
        if (jServerCreatorTypesArr.objStream().noneMatch(creatorsObj -> creatorsObj.getStr("creatorType").equals(creatorTypeStr)))
          fail("Hypernomicon lists creator type not found in creator types JSON from server: " + creatorTypeStr);
    }

    // Make sure all the entry types in the local entry type map are in the creator types listing from server

    for (String localItemTypeStr : zoteroWrapper.getEntryTypeMap().values())
      assertTrue(jServerCreatorTypesObj.containsKey(localItemTypeStr), "Creator types JSON from server does not contain item type: " + localItemTypeStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
