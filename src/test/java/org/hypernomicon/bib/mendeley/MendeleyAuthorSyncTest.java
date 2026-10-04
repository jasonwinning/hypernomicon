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

package org.hypernomicon.bib.mendeley;

import static org.junit.jupiter.api.Assertions.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.records.RecordType.*;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.authors.RecordAuthor;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.junit.jupiter.api.*;

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

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither the linked library nor the works created here for the next test class
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

  private static JsonObj exportJsonObj(MendeleyDocument entry)
  {
    return assertDoesNotThrow(() -> JsonObj.parseJsonObj(entry.exportStandaloneJsonObj(false).toString()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorToWork(HDT_Work work, int nameNumber, Roles roles)
  {
    work.getAuthors().add(new RecordAuthor(work, new PersonName("firstName" + nameNumber, "lastName" + nameNumber), roles, Ternary.Unset));
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

    HDT_Work work = db.createWorkForEntry(chapterEntry);

    assertEquals(2, work.getAuthors().size());
    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 4, Roles.EDITOR);

    assertTrue(chapterEntry.isSynced());

    addCreatorToWork(work, 5, new Roles(true, true));

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

    HDT_Work work = db.createWorkForEntry(jrnArtEntry);

    assertEquals(2, work.getAuthors().size());
    assertTrue(jrnArtEntry.isSynced());

    work.getAuthors().setAll(new BibAuthorsStandalone());  // Clear it out

    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    person.setName(new PersonName("firstName1", "lastName1"));

    RecordAuthor author1 = new RecordAuthor(work, new PersonName("firstName2", "lastName2"), new Roles(true, true), Ternary.Unset),
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

  /**
   * A letter the server spells decomposed (a base letter followed by a combining mark) is not a change: the
   * document's text is composed as Hypernomicon reads it, as the work record's is on assignment, so the two
   * compare equal and nothing is pushed back. The JSON itself keeps the server's spelling, so what goes back
   * to the server is unchanged until Hypernomicon has a different text to write.
   */
  @Test
  void decomposedTextFromServerIsNotAChange()
  {
    String titleNFD = "Problemy poėtiki Dostoevskogo",  // e followed by a combining dot above (U+0307)
           titleNFC = "Problemy poėtiki Dostoevskogo";   // the precomposed letter (U+0117)

    JsonObj originalEntryJson = new JsonObj();

    originalEntryJson.put("title", titleNFD);
    originalEntryJson.put("abstract", titleNFD);  // A field Hypernomicon does not manage

    JsonObj authorObj = new JsonObj();

    authorObj.put("first_name", "Mikhaĭl");  // i followed by a combining breve (U+0306)
    authorObj.put("last_name" , "Bakhtin");

    originalEntryJson.getOrAddArray("authors").add(authorObj);

    MendeleyDocument bookEntry = MendeleyDocument.createForUnitTest(mWrapper, originalEntryJson, EntryType.etBook);

    assertEquals(titleNFC, bookEntry.getStr(bfTitle));
    assertEquals("Mikhaĭl", bookEntry.getAuthors().iterator().next().firstName());  // The precomposed letter (U+012D)
    assertEquals(titleNFD, originalEntryJson.getStr("title"));  // Read composed, kept as sent

    HDT_Work work = db.createWorkForEntry(bookEntry);

    assertEquals(titleNFC, work.name());
    assertTrue(bookEntry.isSynced());

    // Assigning the decomposed spelling to the work changes nothing either

    work.setName(titleNFD);

    assertTrue(bookEntry.isSynced());

    // Nothing Hypernomicon holds differs from the server's text, so an export keeps the server's spelling
    // of the title and of the field Hypernomicon does not manage; a real change goes out composed

    JsonObj exported = exportJsonObj(bookEntry);

    assertEquals(titleNFD, exported.getStr("title"));
    assertEquals(titleNFD, exported.getStr("abstract"));

    work.setName(titleNFD + " II");

    assertFalse(bookEntry.isSynced());

    exported = exportJsonObj(bookEntry);

    assertEquals(titleNFC + " II", exported.getStr("title"));
    assertEquals(titleNFD, exported.getStr("abstract"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
