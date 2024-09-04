/*
 * Copyright 2015-2024 Jason Winning
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

class ZoteroMetadataTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void templatesTest()
  {
    JsonArray jServerTemplatesArr;
    ZoteroWrapper zoteroWrapper = new ZoteroWrapper("", "");

    try
    {
      jServerTemplatesArr = zoteroWrapper.getTemplates();
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      fail("Error occurred while retrieving Zotero templates from server: " + getThrowableMessage(e));
      return;
    }

    EnumSet<EntryType> unusedTypes = EnumSet.copyOf(zoteroWrapper.getEntryTypeMap().keySet());

    for (JsonObj jServerTemplateObj : jServerTemplatesArr.getObjs())
    {
      EntryType entryType = zoteroWrapper.getEntryTypeMap().inverse().getOrDefault(jServerTemplateObj.getStrSafe("itemType"), etOther);
      assertNotEquals(etOther, entryType, "Unrecognized Zotero item type found in templates JSON from server: " + jServerTemplateObj.getStrSafe("itemType"));

      unusedTypes.remove(entryType);

      try
      {
        JsonObj jLocalObj = ZoteroWrapper.getTemplateInitIfNecessary(entryType);
        assertEquals(jServerTemplateObj.toString(), jLocalObj.toString(), "Zotero entry templates should be the same as the ones from the server");
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
    ZoteroWrapper zoteroWrapper = new ZoteroWrapper("", "");

    try
    {
      jServerCreatorTypesObj = zoteroWrapper.getCreatorTypes();
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      fail("Error occurred while retrieving Zotero creator types from server: " + getThrowableMessage(e));
    }

    for (String serverItemTypeStr : jServerCreatorTypesObj.keySet())
    {
      EntryType entryType = zoteroWrapper.getEntryTypeMap().inverse().getOrDefault(serverItemTypeStr, etOther);
      assertNotEquals(etOther, entryType, "Unrecognized Zotero item type found in creator types JSON from server: " + serverItemTypeStr);

      JsonArray jServerCreatorTypesArr = jServerCreatorTypesObj.getArray(serverItemTypeStr);

      for (String creatorTypeStr : creatorTypes.row(entryType).keySet())
      {
        if (jServerCreatorTypesArr.objStream().noneMatch(creatorsObj -> creatorsObj.getStr("creatorType").equals(creatorTypeStr)))
          fail("Hypernomicon lists creator type not found in creator types JSON from server: " + creatorTypeStr);
      }
    }

    for (String localItemTypeStr : zoteroWrapper.getEntryTypeMap().values())
    {
      assertTrue(jServerCreatorTypesObj.containsKey(localItemTypeStr), "Creator types JSON from server does not contain item type: " + localItemTypeStr);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
