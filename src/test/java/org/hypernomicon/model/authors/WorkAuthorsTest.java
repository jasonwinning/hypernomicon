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

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;

import org.junit.jupiter.api.*;

import java.util.List;

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

  /**
   * The group an author table row gives for an author without a person record, as a plain author
   * with "in file name" unset
   */
  private static ObjectGroup rowGroup(String text)
  {
    ObjectGroup group = new ObjectGroup(text);

    group.addNestedEntry(tagEditor    , new NestedValue(hdcBoolean));
    group.addNestedEntry(tagTranslator, new NestedValue(hdcBoolean));
    group.addNestedEntry(tagInFileName, new NestedValue(hdcTernary));

    return group;
  }

//---------------------------------------------------------------------------

  private static String onlyAuthorName(HDT_Work work)
  {
    assertEquals(1, work.getAuthors().size());

    return work.getAuthors().iterator().next().nameLastFirst();
  }

//---------------------------------------------------------------------------

  /**
   * An edit to an author without a person record is saved even when it changes only the
   * capitalization or a parenthetical in the first name.
   */
  @Test
  void editChangingOnlyCaseOrParentheticalIsSaved()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    work.getAuthors().add(new RecordAuthor(work, new PersonName("John (Jack)", "Smith"), Roles.AUTHOR, Ternary.Unset));

    for (String text : List.of("Smith, John", "Smith, John (Johnny)", "smith, John (Johnny)"))
    {
      work.setAuthors(List.of(rowGroup(text)));

      assertEquals(text, onlyAuthorName(work));
    }
  }

//---------------------------------------------------------------------------

  /**
   * Saving a row nobody edited leaves the author's name as it is, although parsing the row's text
   * would give a different name (in title case, for a name written in capitals).
   */
  @Test
  void unchangedRowLeavesNameAsItIs()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    work.getAuthors().add(new RecordAuthor(work, new PersonName("JOHN", "SMITH"), Roles.AUTHOR, Ternary.Unset));

    assertEquals("Smith, John", new PersonName("SMITH, JOHN").getLastFirst());

    work.setAuthors(List.of(rowGroup("SMITH, JOHN")));

    assertEquals("SMITH, JOHN", onlyAuthorName(work));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
