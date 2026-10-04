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

package org.hypernomicon.bib.data;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.BibAuthorsStandalone;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.PersonName;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * The author names a bibliographic lookup sends are the names that stand for the work: its plain
 * authors, else its editors, else its translators. A translation with no recorded author is found by
 * its translator's name, where its title alone can match hundreds of other translations.
 */
class LookupQueryAuthorsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Author ADAMS  = new AuthorStandalone(Roles.AUTHOR    , new PersonName("Alice", "Adams" )),
                              BAKER  = new AuthorStandalone(Roles.EDITOR    , new PersonName("Bob"  , "Baker" )),
                              CARTER = new AuthorStandalone(Roles.TRANSLATOR, new PersonName("Carol", "Carter"));

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    GoogleBibData.setApiKeyForTesting(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static BibAuthors bibAuthors(Author... authors)
  {
    BibAuthorsStandalone bibAuthors = new BibAuthorsStandalone();

    for (Author author : authors)
      bibAuthors.add(author);

    return bibAuthors;
  }

//---------------------------------------------------------------------------

  private static String crossrefAuthorParam(Author... authors)
  {
    return StringUtils.substringBetween(CrossrefBibData.getQueryUrl("Some Title", null, List.of(authors), true, null), "query.author=", "&");
  }

//---------------------------------------------------------------------------

  private static List<String> googleKeywords(Author... authors)
  {
    List<String> authKeywords = new ArrayList<>();

    GoogleBibData.getQueryUrl("Some Title", bibAuthors(authors), authKeywords, null);

    return authKeywords;
  }

//---------------------------------------------------------------------------

  private static List<String> locKeywords(Author... authors)
  {
    List<String> authKeywords = new ArrayList<>();

    LibraryOfCongressBibData.getQueryUrl("Some Title", bibAuthors(authors), authKeywords, "");

    return authKeywords;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void crossrefAuthorQuery()
  {
    assertEquals("Adams" , crossrefAuthorParam(BAKER, ADAMS, CARTER), "Plain authors only, when there are any");
    assertEquals("Baker" , crossrefAuthorParam(CARTER, BAKER       ), "Editors when there are no plain authors");
    assertEquals("Carter", crossrefAuthorParam(CARTER              ), "Translators when there is nobody else");
  }

//---------------------------------------------------------------------------

  @Test
  void googleAuthorKeywords()
  {
    GoogleBibData.setApiKeyForTesting("");  // Keyless, so no preference is read

    assertEquals(List.of("Adams" ), googleKeywords(BAKER, ADAMS, CARTER), "Plain authors only, when there are any");
    assertEquals(List.of("Baker" ), googleKeywords(CARTER, BAKER       ), "Editors when there are no plain authors");
    assertEquals(List.of("Carter"), googleKeywords(CARTER              ), "Translators when there is nobody else");

    String url = GoogleBibData.getQueryUrl("Some Title", bibAuthors(CARTER), new ArrayList<>(), null);

    assertTrue(url.contains("%22Carter%22"), url);
  }

//---------------------------------------------------------------------------

  @Test
  void libraryOfCongressAuthorClause()
  {
    assertEquals(List.of("Adams" ), locKeywords(BAKER, ADAMS, CARTER), "Plain authors only, when there are any");
    assertEquals(List.of("Baker" ), locKeywords(CARTER, BAKER       ), "Editors when there are no plain authors");
    assertEquals(List.of("Carter"), locKeywords(CARTER              ), "Translators when there is nobody else");

    String url = LibraryOfCongressBibData.getQueryUrl("Some Title", bibAuthors(CARTER), new ArrayList<>(), "");

    assertTrue(url.contains("+and+dc.author%3D%22Carter%22"), url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
