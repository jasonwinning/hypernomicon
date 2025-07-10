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

import java.util.List;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

class BibAuthorsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------

  @Test
  void normalizeTest()
  {
    List<Author> inputList, outputList, expectedList;

  //---------------------------------------------------------------------------

    inputList = null;

    assertEquals(0, BibAuthors.normalizeAuthors(inputList, false).size());

  //---------------------------------------------------------------------------

    inputList = List.of();

    assertEquals(0, BibAuthors.normalizeAuthors(inputList, false).size());

  //---------------------------------------------------------------------------

    inputList = List.of(new AuthorStandalone(new PersonName("First1","Last1"), null, false, true));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertFalse(outputList.get(0).getIsEditor());
    assertTrue (outputList.get(0).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, true ));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsEditor());
    assertTrue(outputList.get(0).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, true ));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsAuthor());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsAuthor());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, true, true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true, false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsEditor());
    assertTrue(outputList.get(0).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, true ));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(2, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsEditor());
    assertTrue(outputList.get(0).getIsTrans ());
    assertEquals("First2", outputList.get(1).firstName());
    assertFalse(outputList.get(1).getIsEditor());
    assertFalse(outputList.get(1).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, false),
        new AuthorStandalone(new PersonName("First3","Last3"), null, true , false),
        new AuthorStandalone(new PersonName("First4","Last4"), null, false, true ),
        new AuthorStandalone(new PersonName("First5","Last5"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, false),
        new AuthorStandalone(new PersonName("First3","Last3"), null, true , false),
        new AuthorStandalone(new PersonName("First4","Last4"), null, false, true ),
        new AuthorStandalone(new PersonName("First5","Last5"), null, true , false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First3","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First4","Last1"), null, false, true ),
        new AuthorStandalone(new PersonName("First5","Last1"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First3","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First4","Last1"), null, false, true ),
        new AuthorStandalone(new PersonName("First5","Last1"), null, true , false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, true ),
        new AuthorStandalone(new PersonName("First3","Last3"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals("First1", outputList.get(0).firstName());
    assertTrue(outputList.get(0).getIsAuthor());

    assertEquals("First2", outputList.get(1).firstName());
    assertTrue(outputList.get(1).getIsAuthor());

    assertEquals("First3", outputList.get(2).firstName());
    assertTrue (outputList.get(2).getIsEditor());
    assertFalse(outputList.get(2).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First3","Last3"), null, false, true ),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , true ),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First3","Last3"), null, false, false),
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, false, true ),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null, true , false),
        new AuthorStandalone(new PersonName("First2","Last2"), null, true , true ),
        new AuthorStandalone(new PersonName("First3","Last3"), null, false, false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    HDT_Person person1 = db.createNewBlankRecord(hdtPerson),
               person2 = db.createNewBlankRecord(hdtPerson),
               person3 = db.createNewBlankRecord(hdtPerson);

    person1.setName(new PersonName("First1", "Last1"));
    person2.setName(new PersonName("First2", "Last2"));
    person3.setName(new PersonName("First3", "Last3"));

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null   , true , false),
        new AuthorStandalone(null                            , person1, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertSame(outputList.get(0).getPerson(), person1);
    assertTrue (outputList.get(0).getIsEditor());
    assertFalse(outputList.get(0).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null   , true , false),
        new AuthorStandalone(new PersonName("First1","Last1"), person2, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(2, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertNull(outputList.get(0).getPerson());
    assertTrue (outputList.get(0).getIsEditor());
    assertFalse(outputList.get(0).getIsTrans ());
    assertEquals("First2", outputList.get(1).firstName());
    assertSame(outputList.get(1).getPerson(), person2);
    assertTrue (outputList.get(1).getIsEditor());
    assertFalse(outputList.get(1).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null   , true , false),
        new AuthorStandalone(new PersonName("First1","Last1"), person2, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(2, outputList.size());
    assertEquals("First1", outputList.get(0).firstName());
    assertNull(outputList.get(0).getPerson());
    assertTrue (outputList.get(0).getIsEditor());
    assertFalse(outputList.get(0).getIsTrans ());
    assertEquals("First2", outputList.get(1).firstName());
    assertSame(outputList.get(1).getPerson(), person2);
    assertTrue (outputList.get(1).getIsEditor());
    assertFalse(outputList.get(1).getIsTrans ());

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First2","Last2"), null   , false, true ),
        new AuthorStandalone(new PersonName("First1","Last1"), person2, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    assertEquals(1, outputList.size());
    assertEquals("First2", outputList.get(0).firstName());
    assertSame(outputList.get(0).getPerson(), person2);
    assertTrue(outputList.get(0).getIsEditor());
    assertTrue(outputList.get(0).getIsTrans ());

  //---------------------------------------------------------------------------

    HDT_Person otherPerson1 = db.createNewBlankRecord(hdtPerson);

    person1.setName(new PersonName("First1", "Last1"));

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1"), null        , false, true ),
        new AuthorStandalone(new PersonName("First2","Last2"), person1     , false, false),
        new AuthorStandalone(new PersonName("First1","Last1"), otherPerson1, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(null, person1     , false, false),
        new AuthorStandalone(null, otherPerson1, true,  false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1")      , null, false, true ),
        new AuthorStandalone(new PersonName("Фіŕşт2","Лàşт2")      , null, true , false),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("Фіrşт2","Лaşт2")      , null, false, true ),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("First3 (Bob)","Last3"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, true);

    expectedList = List.of(
        new AuthorStandalone(null, person1, false, true ),
        new AuthorStandalone(null, person2, true , true ),
        new AuthorStandalone(null, person3, true , false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1")      , null, false, true ),
        new AuthorStandalone(new PersonName("Фіŕşт2","Лàşт2")      , null, true , false),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("Фіrşт2","Лaşт2")      , null, false, true ),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("First3 (Bob)","Last3"), null, true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1")      , null, false, true ),
        new AuthorStandalone(new PersonName("Фіŕşт2","Лàşт2")      , null, true , true ),
        new AuthorStandalone(new PersonName("First3 (Bob)","Last3"), null, true , false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

    inputList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1")      , null   , false, true ),
        new AuthorStandalone(new PersonName("Фіŕşт2","Лàşт2")      , null   , true , false),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("Фіrşт2","Лaşт2")      , person2, false, true ),         // English characters replaced with non-English homoglyphs
        new AuthorStandalone(new PersonName("First3 (Bob)","Last3"), null   , true , false));

    outputList = BibAuthors.normalizeAuthors(inputList, false);

    expectedList = List.of(
        new AuthorStandalone(new PersonName("First1","Last1")      , null   , false, true ),
        new AuthorStandalone(null                                  , person2, true , true ),
        new AuthorStandalone(new PersonName("First3 (Bob)","Last3"), null   , true , false));

    assertEquals(expectedList, outputList);

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
