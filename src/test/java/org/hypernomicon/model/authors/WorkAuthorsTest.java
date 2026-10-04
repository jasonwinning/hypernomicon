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

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class WorkAuthorsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

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

  /**
   * Adding a person who is already one of a work's authors changes nothing: the person is listed once
   * and keeps the roles they have. The author without a person record matters, because it makes the
   * work keep its own list of authors, which is where a second entry for the person would appear.
   */
  @Test
  void addingAnExistingAuthorChangesNothing()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    person.setName(new PersonName("First", "Last"));

    WorkAuthors authors = work.getAuthors();

    authors.add(new RecordAuthor(work, new PersonName("Other", "Author"), Roles.AUTHOR, Ternary.Unset));
    authors.add(person);
    work.setPersonRoles(person, Roles.EDITOR);

    assertEquals(2, authors.size());

    authors.add(person);

    assertEquals(2, authors.size(), "The person should still be listed once");
    assertEquals(Roles.EDITOR, work.personRoles(person), "Adding the person again should not change their roles");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
