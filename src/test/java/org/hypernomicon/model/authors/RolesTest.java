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
import static org.hypernomicon.model.authors.Author.AuthorType.*;
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

//---------------------------------------------------------------------------

/**
 * The roles a contributor plays in a work are one value, not an editor flag and a translator flag
 * carried side by side. This pins the value's own behavior and the two places it is kept: on a
 * standalone author, and for a person record, on the work.
 */
class RolesTest
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

  @Test
  void headingsAndRolesCorrespond()
  {
    assertEquals(Roles.AUTHOR    , Roles.of(author    ));
    assertEquals(Roles.EDITOR    , Roles.of(editor    ));
    assertEquals(Roles.TRANSLATOR, Roles.of(translator));

    assertTrue (Roles.AUTHOR.isAuthor());
    assertFalse(Roles.EDITOR.isAuthor());
    assertFalse(Roles.TRANSLATOR.isAuthor());
    assertFalse(new Roles(true, true).isAuthor());
  }

//---------------------------------------------------------------------------

  @Test
  void matchesTest()
  {
    assertTrue (Roles.AUTHOR.matches(author));
    assertFalse(Roles.AUTHOR.matches(editor));
    assertFalse(Roles.AUTHOR.matches(translator));

    assertFalse(Roles.EDITOR.matches(author));
    assertTrue (Roles.EDITOR.matches(editor));
    assertFalse(Roles.EDITOR.matches(translator));

    assertFalse(Roles.TRANSLATOR.matches(author));
    assertFalse(Roles.TRANSLATOR.matches(editor));
    assertTrue (Roles.TRANSLATOR.matches(translator));

    Roles both = new Roles(true, true);

    assertFalse(both.matches(author), "An editor who also translated is not a plain author");
    assertTrue (both.matches(editor));
    assertTrue (both.matches(translator));
  }

//---------------------------------------------------------------------------

  @Test
  void combinedWithTest()
  {
    assertEquals(new Roles(true, true), Roles.EDITOR.combinedWith(Roles.TRANSLATOR));
    assertEquals(Roles.EDITOR         , Roles.EDITOR.combinedWith(Roles.AUTHOR), "A plain author contributes no role");
    assertEquals(Roles.EDITOR         , Roles.AUTHOR.combinedWith(Roles.EDITOR));
    assertEquals(Roles.TRANSLATOR     , Roles.TRANSLATOR.combinedWith(null), "Nothing to combine with leaves the roles as they are");
  }

//---------------------------------------------------------------------------

  @Test
  void rolesOfAnAuthorsTableRow()
  {
    ObjectGroup row = new ObjectGroup("Last, First");

    NestedValue editorVal = new NestedValue(hdcBoolean),
                transVal  = new NestedValue(hdcBoolean);

    editorVal.bool = true;

    row.addNestedEntry(tagEditor    , editorVal);
    row.addNestedEntry(tagTranslator, transVal );

    assertEquals(Roles.EDITOR, Roles.of(row));
  }

//---------------------------------------------------------------------------

  @Test
  void standaloneAuthorKeepsItsRoles()
  {
    PersonName name = new PersonName("First", "Last");

    AuthorStandalone editorAuthor = new AuthorStandalone(editor, name),
                     plainAuthor  = new AuthorStandalone(Roles.AUTHOR, name);

    assertEquals(Roles.EDITOR, editorAuthor.getRoles());
    assertTrue (editorAuthor.getIsEditor());
    assertFalse(editorAuthor.getIsTrans ());
    assertFalse(editorAuthor.getIsAuthor());

    assertTrue(plainAuthor.getIsAuthor());

    assertNotEquals(editorAuthor, plainAuthor, "Same name, different roles");
    assertEquals(editorAuthor, new AuthorStandalone(editorAuthor), "The copy constructor carries the roles");
  }

//---------------------------------------------------------------------------

  @Test
  void personRolesLiveOnTheWork()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    person.setName(new PersonName("First", "Last"));
    work.getAuthors().add(person);

    RecordAuthor author = new RecordAuthor(work, person);

    assertEquals(Roles.AUTHOR, author.getRoles());

    work.setPersonRoles(person, new Roles(true, true));

    assertEquals(new Roles(true, true), author.getRoles(), "A record author reads the person's current roles from the work");
    assertEquals(new Roles(true, true), work.personRoles(person));
    assertTrue(work.getAuthors().get(0).getIsEditor());
    assertTrue(work.getAuthors().get(0).getIsTrans ());

    work.setPersonRoles(person, Roles.TRANSLATOR);

    assertFalse(author.getIsEditor());
    assertTrue (author.getIsTrans ());
  }

//---------------------------------------------------------------------------

  @Test
  void nameOnlyRecordAuthorKeepsItsRoles()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);

    RecordAuthor author = new RecordAuthor(work, new PersonName("First", "Last"), Roles.TRANSLATOR, Ternary.Unset);

    work.getAuthors().add(author);

    assertEquals(Roles.TRANSLATOR, author.getRoles());
    assertEquals(Roles.TRANSLATOR, work.getAuthors().get(0).getRoles());

    assertEquals(Roles.AUTHOR, new RecordAuthor(new PersonName("First", "Last")).getRoles(), "A bare name is a plain author");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
