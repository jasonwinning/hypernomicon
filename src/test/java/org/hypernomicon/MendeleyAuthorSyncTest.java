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

import static org.junit.jupiter.api.Assertions.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.mendeley.MendeleyDocument;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class MendeleyAuthorSyncTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static MendeleyWrapper mWrapper;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
    mWrapper = assertDoesNotThrow(() -> db.linkBibLibrary(LibraryType.ltMendeley, ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreator(JsonObj jObj, AuthorType authorType, int nameNumber)
  {
    String authorTypeStr = switch (authorType)
    {
      case author     -> "authors";
      case editor     -> "editors";
      case translator -> "translators";
    };

    JsonObj creatorObj = new JsonObj();

    creatorObj.put("first_name", "firstName" + nameNumber);
    creatorObj.put("last_name" , "lastName"  + nameNumber);

    jObj.getOrAddArray(authorTypeStr).add(creatorObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Work createWorkForEntry(BibEntry<?, ?> entry)
  {
    return ZoteroAuthorSyncTest.createWorkForEntry(db, entry);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static JsonObj exportJsonObj(MendeleyDocument entry)
  {
    return assertDoesNotThrow(() -> JsonObj.parseJsonObj(entry.exportStandaloneJsonObj(false).toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorToWork(HDT_Work work, int nameNumber, boolean isEd, boolean isTrans)
  {
    work.getAuthors().add(new RecordAuthor(work, new PersonName("firstName" + nameNumber, "lastName" + nameNumber), isEd, isTrans, Ternary.Unset));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Test whether editors are properly ignored when ignoreEditor is true.
   */
  @Test
  void test1()
  {
    JsonObj originalEntryJson = new JsonObj();

    addCreator(originalEntryJson, author    , 1);
    addCreator(originalEntryJson, editor    , 2);
    addCreator(originalEntryJson, translator, 3);

    MendeleyDocument chapterEntry = MendeleyDocument.createForUnitTest(mWrapper, originalEntryJson, EntryType.etBookChapter);

    HDT_Work work = createWorkForEntry(chapterEntry);

    assertEquals(2, work.getAuthors().size());
    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 4, true, false);

    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 5, true, true);

    assertFalse(chapterEntry.isSynced());

    JsonObj newEntryJson = exportJsonObj(chapterEntry);

    JsonArray authorArr = newEntryJson.getArray("authors"),
              editorArr = newEntryJson.getArray("editors"),
              transArr  = newEntryJson.getArray("translators");

    assertEquals(2, transArr .size());
    assertEquals(1, editorArr.size());
    assertEquals(1, authorArr.size());

    assertEquals("firstName3", transArr .getObj(0).getStr("first_name"));
    assertEquals("firstName5", transArr .getObj(1).getStr("first_name"));
    assertEquals("firstName2", editorArr.getObj(0).getStr("first_name"));
    assertEquals("firstName1", authorArr.getObj(0).getStr("first_name"));
  }

//---------------------------------------------------------------------------

  /**
   * Test whether persons with the same name or person record are treated the same.
   */
  @Test
  void test2()
  {
    JsonObj originalEntryJson = new JsonObj();

    addCreator(originalEntryJson, author    , 1);
    addCreator(originalEntryJson, editor    , 2);
    addCreator(originalEntryJson, translator, 2);

    MendeleyDocument jrnArtEntry = MendeleyDocument.createForUnitTest(mWrapper, originalEntryJson, EntryType.etJournalArticle);

    HDT_Work work = createWorkForEntry(jrnArtEntry);

    assertEquals(2, work.getAuthors().size());
    assertTrue(jrnArtEntry.isSynced());

    work.getAuthors().setAll(new BibAuthorsStandalone());  // Clear it out

    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    person.setName(new PersonName("firstName1", "lastName1"));

    RecordAuthor author1 = new RecordAuthor(work, new PersonName("firstName2", "lastName2"), true, true, Ternary.Unset),
                 author2 = new RecordAuthor(work, person);

    work.getAuthors().add(author1);
    work.getAuthors().add(author2);

    assertEquals(2, work.getAuthors().size());
    assertTrue(jrnArtEntry.isSynced());

    person.setName(new PersonName("firstName3", "lastName3"));

    assertFalse(jrnArtEntry.isSynced());

    JsonObj newEntryJson = exportJsonObj(jrnArtEntry);

    JsonArray authorArr = newEntryJson.getArray("authors"),
              editorArr = newEntryJson.getArray("editors"),
              transArr  = newEntryJson.getArray("translators");

    assertEquals(1, transArr .size());
    assertEquals(1, editorArr.size());
    assertEquals(1, authorArr.size());

    assertEquals("firstName2", transArr .getObj(0).getStr("first_name"));
    assertEquals("firstName2", editorArr.getObj(0).getStr("first_name"));
    assertEquals("firstName3", authorArr.getObj(0).getStr("first_name"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
