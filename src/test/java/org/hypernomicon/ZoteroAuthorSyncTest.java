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

package org.hypernomicon;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.zotero.ZoteroItem;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.json.simple.parser.ParseException;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class ZoteroAuthorSyncTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static ZoteroWrapper zWrapper;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
    zWrapper = assertDoesNotThrow(() -> db.linkBibLibrary(LibraryType.ltZotero, ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static JsonObj getTemplate(EntryType entryType)
  {
    try
    {
      return ZoteroWrapper.getTemplateInitIfNecessary(entryType);
    }
    catch (IOException | ParseException | HDB_InternalError e)
    {
      fail("Error occurred while loading Zotero templates resource file: " + getThrowableMessage(e));
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreator(JsonArray creatorsArr, String creatorTypeStr, int nameNumber)
  {
    JsonObj creatorObj = new JsonObj();
    creatorObj.put("creatorType", creatorTypeStr);

    creatorObj.put("firstName", "firstName" + nameNumber);
    creatorObj.put("lastName" , "lastName"  + nameNumber);

    creatorsArr.add(creatorObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Work createWorkForEntry(BibEntry<?, ?> entry)
  {
    return createWorkForEntry(db, entry);
  }

  static HDT_Work createWorkForEntry(TestHyperDB db, BibEntry<?, ?> entry)
  {
    assertThatThisIsUnitTestThread();

    HDT_Work work = db.createNewBlankRecord(RecordType.hdtWork);

    work.getBibData().copyAllFieldsFrom(entry, false, false);

    entry.getAuthors().normalizedList(false).forEach(bibAuthor ->
      work.getAuthors().add(new Author(work, bibAuthor.getName(), bibAuthor.getIsEditor(), bibAuthor.getIsTrans(), Ternary.Unset)));

    work.setBibEntryKey(entry.getKey());

    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static JsonArray exportCreatorsArray(ZoteroItem entry)
  {
    return assertDoesNotThrow(() -> JsonObj.parseJsonObj(entry.exportStandaloneJsonObj(false).toString()).getObj("data").getArray("creators"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorToWork(HDT_Work work, int nameNumber, boolean isEd, boolean isTrans)
  {
    work.getAuthors().add(new Author(work, new PersonName("firstName" + nameNumber, "lastName" + nameNumber), isEd, isTrans, Ternary.Unset));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Test whether creator types unrecognized by Hypernomicon are filtered out, and whether editors are
   * properly ignored when ignoreEditor is true.
   */
  @Test
  void test1()
  {
    JsonObj jData = getTemplate(EntryType.etBookChapter);

    assertNotNull(jData);
    JsonArray origCreatorsArr = jData.getOrAddArray("creators");

    origCreatorsArr.clear();

    addCreator(origCreatorsArr, "bookAuthor", 1);
    addCreator(origCreatorsArr, "author"    , 2);
    addCreator(origCreatorsArr, "editor"    , 3);

    ZoteroItem chapterEntry = ZoteroItem.createForUnitTest(zWrapper, jData);

    HDT_Work work = createWorkForEntry(chapterEntry);

    assertEquals(1, work.getAuthors().size());
    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 4, true, false);

    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 5, true, true);

    assertFalse(chapterEntry.isSynced());

    JsonArray newCreatorsArr = exportCreatorsArray(chapterEntry);

    assertEquals(4, newCreatorsArr.size());

    assertEquals("firstName1", newCreatorsArr.getObj(0).getStr("firstName"));
    assertEquals("bookAuthor", newCreatorsArr.getObj(0).getStr("creatorType"));

    assertEquals("firstName2", newCreatorsArr.getObj(1).getStr("firstName"));
    assertEquals("author"    , newCreatorsArr.getObj(1).getStr("creatorType"));

    assertEquals("firstName3", newCreatorsArr.getObj(2).getStr("firstName"));
    assertEquals("editor"    , newCreatorsArr.getObj(2).getStr("creatorType"));

    assertEquals("firstName5", newCreatorsArr.getObj(3).getStr("firstName"));
    assertEquals("translator", newCreatorsArr.getObj(3).getStr("creatorType"));
  }

//---------------------------------------------------------------------------

  /**
   * Test whether an author type unrecognized by Zotero is filtered out
   */
  @Test
  void test2()
  {
    JsonObj jData = getTemplate(EntryType.etMagazineArticle);

    assertNotNull(jData);
    JsonArray origCreatorsArr = jData.getOrAddArray("creators");

    origCreatorsArr.clear();

    addCreator(origCreatorsArr, "contributor"   , 1);
    addCreator(origCreatorsArr, "translator"    , 2);
    addCreator(origCreatorsArr, "author"        , 3);
    addCreator(origCreatorsArr, "reviewedAuthor", 4);

    ZoteroItem magArtEntry = ZoteroItem.createForUnitTest(zWrapper, jData);

    HDT_Work work = createWorkForEntry(magArtEntry);

    assertEquals(2, work.getAuthors().size());
    assertTrue(magArtEntry.isSynced());
    assertEquals("firstName2", work.getAuthors().get(0).firstName());
    assertTrue(work.getAuthors().get(0).getIsTrans());
    assertFalse(work.getAuthors().get(0).getIsEditor());
    assertEquals("firstName3", work.getAuthors().get(1).firstName());
    assertFalse(work.getAuthors().get(1).getIsTrans());
    assertFalse(work.getAuthors().get(1).getIsEditor());

    addCreatorToWork(work, 5, true, false);

    assertTrue(magArtEntry.isSynced());

    work.getAuthors().setAll(new BibAuthorsStandalone());  // Clear it out

    addCreatorToWork(work, 6, true, false);
    addCreatorToWork(work, 7, true, false);
    addCreatorToWork(work, 8, true, false);

    JsonArray newCreatorsArr = exportCreatorsArray(magArtEntry);

    assertEquals(2, newCreatorsArr.size());

    addCreatorToWork(work, 9, false, false);
    addCreatorToWork(work, 10, true, false);

    newCreatorsArr = exportCreatorsArray(magArtEntry);

    assertEquals(3, newCreatorsArr.size());

    assertEquals("firstName1" , newCreatorsArr.getObj(0).getStr("firstName"));
    assertEquals("contributor", newCreatorsArr.getObj(0).getStr("creatorType"));

    assertEquals("firstName9", newCreatorsArr.getObj(1).getStr("firstName"));
    assertEquals("author"    , newCreatorsArr.getObj(1).getStr("creatorType"));

    assertEquals("firstName4"    , newCreatorsArr.getObj(2).getStr("firstName"));
    assertEquals("reviewedAuthor", newCreatorsArr.getObj(2).getStr("creatorType"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
