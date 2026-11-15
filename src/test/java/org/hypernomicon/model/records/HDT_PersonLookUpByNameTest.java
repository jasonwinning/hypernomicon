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

package org.hypernomicon.model.records;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.items.PersonName;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class HDT_PersonLookUpByNameTest
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

  @BeforeEach
  void resetDB()
  {
    db.closeAndOpen();  // lookUpByName searches every person, so each test starts with none
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no records behind for other test classes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Person createPerson(PersonName name)
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    person.setName(name);
    return person;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Gives the person the search key the app generates for a new person with that name
   */
  private static void giveGeneratedSearchKey(HDT_Person person)
  {
    String searchKey = HDT_Person.generateSearchKeyCheckingIfUsed(person.getName(), person).searchKey();

    assertFalse(searchKey.isEmpty());
    assertDoesNotThrow(() -> person.setSearchKey(searchKey, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void fullNameIsFoundThroughSearchKey()
  {
    HDT_Person person = createPerson(new PersonName("John", "Smith"));
    giveGeneratedSearchKey(person);

    assertSame(person, HDT_Person.lookUpByName(new PersonName("John Smith")));
  }

//---------------------------------------------------------------------------

  /**
   * Every search key the name would get already belongs to the person, but none of them has a full
   * first name, so the search keys do not identify the person by themselves. The name does.
   */
  @Test
  void nameWithInitialIsFoundWhenThePersonHasAllOfItsSearchKeys()
  {
    HDT_Person person = createPerson(new PersonName("J.", "Smith"));
    giveGeneratedSearchKey(person);

    assertSame(person, HDT_Person.lookUpByName(new PersonName("J. Smith")));
  }

//---------------------------------------------------------------------------

  /**
   * A person record with only a last name has a sort key ending in the separator that a name with
   * both parts would have before the first name. The name has to be compared, not the sort key.
   */
  @Test
  void singleNameIsFoundWithoutSearchKey()
  {
    HDT_Person person = createPerson(new PersonName("Plato"));

    assertSame(person, HDT_Person.lookUpByName(new PersonName("Plato")));
  }

//---------------------------------------------------------------------------

  /**
   * A sort_key attribute in the XML replaces the sort key made from the name, so it must not
   * keep the person from being found by name.
   */
  @Test
  void sortKeyAttributeDoesNotHideThePerson()
  {
    HDT_Person person = createPerson(new PersonName("John", "Smith"));
    person.setSortKeyAttr("zzz");

    assertSame(person, HDT_Person.lookUpByName(new PersonName("John Smith")));
  }

//---------------------------------------------------------------------------

  @Test
  void caseDiacriticsAndNicknameAreIgnored()
  {
    HDT_Person person = createPerson(new PersonName("José (Pepe)", "Núñez"));

    assertSame(person, HDT_Person.lookUpByName(new PersonName("jose nunez")));
  }

//---------------------------------------------------------------------------

  /**
   * The other name's search key "^Smith" belongs to the person, but that does not make it the
   * person's name.
   */
  @Test
  void differentFirstNameIsNotAMatch()
  {
    giveGeneratedSearchKey(createPerson(new PersonName("John", "Smith")));

    assertNull(HDT_Person.lookUpByName(new PersonName("Jane Smith")));
  }

//---------------------------------------------------------------------------

  @Test
  void initialDoesNotMatchFullFirstName()
  {
    giveGeneratedSearchKey(createPerson(new PersonName("John", "Smith")));

    assertNull(HDT_Person.lookUpByName(new PersonName("J. Smith")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
