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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.stream.Stream;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.util.json.JsonObj;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * How bibliographic entries sort: by the names that stand for each entry (its plain authors, else
 * its editors, else its translators), then by date, then by title. Mendeley entries stand in for any
 * library's, their unit-test factory taking plain JSON.
 */
class BibEntryComparatorTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;
  private static MendeleyWrapper mWrapper;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
    mWrapper = assertDoesNotThrow(() -> db.linkBibLibrary(LibraryType.ltMendeley, ""));
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither the linked library nor the entries created here for the next test class
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static JsonObj entryJson(String title, int year)
  {
    JsonObj jObj = new JsonObj();

    jObj.put("title", title);
    jObj.put("year" , Long.valueOf(year));

    return jObj;
  }

//---------------------------------------------------------------------------

  private static void addCreator(JsonObj jObj, AuthorType authorType, String lastName)
  {
    String arrayName = switch (authorType)
    {
      case author     -> "authors";
      case editor     -> "editors";
      case translator -> "translators";
    };

    JsonObj creatorObj = new JsonObj();

    creatorObj.put("first_name", "First");
    creatorObj.put("last_name" , lastName);

    jObj.getOrAddArray(arrayName).add(creatorObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void plainAuthorsThenEditorsThenTranslatorsNameAnEntry()
  {
    JsonObj writtenByZed       = entryJson("Written by Zed"       , 2001),   // Adams edited it, but Zed wrote it: Zed is the name that counts
            writtenByBaker     = entryJson("Written by Baker"     , 2001),
            editedByAdams      = entryJson("Edited by Adams"      , 2001),   // Young translated it; Adams, the editor, stands for it
            translatedByAbbott = entryJson("Translated by Abbott" , 2010),   // Only a translator each: the translator's name counts, so these
            translatedByCarter = entryJson("Translated by Carter" , 2000);   // two take their alphabetical places whatever their dates say

    addCreator(writtenByZed      , author    , "Zed"   );
    addCreator(writtenByZed      , editor    , "Adams" );
    addCreator(writtenByBaker    , author    , "Baker" );
    addCreator(editedByAdams     , editor    , "Adams" );
    addCreator(editedByAdams     , translator, "Young" );
    addCreator(translatedByAbbott, translator, "Abbott");
    addCreator(translatedByCarter, translator, "Carter");

    Stream<MendeleyDocument> entries = Stream.of(writtenByZed, writtenByBaker, editedByAdams, translatedByAbbott, translatedByCarter)
                                             .map(jObj -> MendeleyDocument.createForUnitTest(mWrapper, jObj, EntryType.etBook));

    assertEquals(List.of("Translated by Abbott", "Edited by Adams", "Written by Baker", "Translated by Carter", "Written by Zed"),
                 entries.sorted(BibEntry.comparator()).map(entry -> entry.getStr(bfTitle)).toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
