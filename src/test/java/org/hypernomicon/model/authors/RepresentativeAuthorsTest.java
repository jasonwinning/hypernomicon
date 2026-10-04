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

package org.hypernomicon.model.authors;

import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.model.records.HDT_Argument.ArgumentAuthor;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.settings.WorkSearchKeySettings;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Whose names stand for a work: its plain authors, else its editors, else everyone it has. Pins the
 * choice itself and two of the places that make it, a work's search key and the people behind an
 * argument. The New Argument dialog's name suggestion is covered by its in-app test, and the sorting
 * of bibliographic entries by BibEntryComparatorTest.
 */
class RepresentativeAuthorsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  private static final AuthorStandalone ADAMS  = new AuthorStandalone(author               , new PersonName("Alice", "Adams" )),
                                        BAKER  = new AuthorStandalone(editor               , new PersonName("Bob"  , "Baker" )),
                                        CARTER = new AuthorStandalone(translator           , new PersonName("Carol", "Carter")),
                                        DAVIS  = new AuthorStandalone(new Roles(true, true), new PersonName("Dan"  , "Davis" )),
                                        EVANS  = new AuthorStandalone(author               , new PersonName("Eve"  , "Evans" ));

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no records behind for other test classes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A work dated 2001 whose contributors are copies of the given authors, in the given order */
  private static HDT_Work workWith(Author... authors)
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    work.setBibDate(BibliographicDate.fromYearStr("2001", false));

    for (Author author : authors)
      work.getAuthors().add(new RecordAuthor(work, author, Ternary.Unset));

    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void plainAuthorsStandForTheWork()
  {
    List<AuthorStandalone> all = List.of(BAKER, ADAMS, CARTER, EVANS);

    assertEquals(List.of(ADAMS, EVANS), Author.representativeAuthors(all), "The plain authors only, in their original order");
  }

//---------------------------------------------------------------------------

  @Test
  void editorsStandForAnEditedWork()
  {
    List<AuthorStandalone> all = List.of(CARTER, BAKER, DAVIS);

    assertEquals(List.of(BAKER, DAVIS), Author.representativeAuthors(all), "An editor who also translated counts as an editor");
  }

//---------------------------------------------------------------------------

  @Test
  void translatorsStandForAWorkWithNobodyElse()
  {
    assertEquals(List.of(CARTER), Author.representativeAuthors(List.of(CARTER)));
  }

//---------------------------------------------------------------------------

  @Test
  void nobodyStandsForNobody()
  {
    assertEquals(List.of(), Author.representativeAuthors(List.<Author>of()));
  }

//---------------------------------------------------------------------------

  @Test
  void searchKeyNamesTheRepresentativeAuthor()
  {
    WorkSearchKeySettings settings = WorkSearchKeySettings.loadFromPrefNode();

    assertEquals(settings.format(List.of("Adams" ), "2001"), workWith(BAKER , ADAMS, CARTER).makeWorkSearchKey(false, false), "The plain author, not the editor listed first");
    assertEquals(settings.format(List.of("Baker" ), "2001"), workWith(CARTER, BAKER        ).makeWorkSearchKey(false, false), "The editor when there is no plain author");
    assertEquals(settings.format(List.of("Carter"), "2001"), workWith(CARTER               ).makeWorkSearchKey(false, false), "The translator when there is nobody else");
  }

//---------------------------------------------------------------------------

  @Test
  void argumentPeopleAreTheRepresentativeAuthorsOfItsWorks()
  {
    HDT_Argument argument = db.createNewBlankRecord(hdtArgument);

    argument.works.add(workWith(BAKER , ADAMS, CARTER));
    argument.works.add(workWith(CARTER, BAKER));
    argument.works.add(workWith(CARTER));

    assertEquals(List.of("Adams", "Baker", "Carter"), argument.getPeople().map(ArgumentAuthor::getAuthObj).map(Author::singleName).toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
