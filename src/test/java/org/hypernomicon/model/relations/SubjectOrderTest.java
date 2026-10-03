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

package org.hypernomicon.model.relations;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.PopupRobot;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Pins how the order of a relation's subjects is persisted. Once the user has arranged an object's
 * subject list (the sub-works of a work, the investigations of a person, the divisions of an
 * institution), every subject of that list is saved with an {@code ord} attribute on its pointer
 * tag, which the loader uses to put the subjects back in that order; the subjects of a list never
 * arranged are saved without one and come back in the order they were brought online. The reload
 * tests feed records written by the application's own XML writer back through the loader, including
 * a file in the form earlier versions wrote, which carried the attribute only for subjects that had
 * been reordered.
 */
class SubjectOrderTest
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
    PopupRobot.clear();

    db.closeAndOpen();  // Each test starts from the template, with every record ID free
  }

//---------------------------------------------------------------------------

  @AfterEach
  void clearLoadFilters()
  {
    db.setRecordsLoadFilter(null, null);
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no injected records behind for other test classes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Work work(String title)
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName(title);
    return work;
  }

//---------------------------------------------------------------------------

  private static HDT_Work workWithID(int id, String title) throws Exception
  {
    HDT_Work work = db.createNewRecordFromState(new RecordState(hdtWork, id), true);
    work.setName(title);
    return work;
  }

//---------------------------------------------------------------------------

  private static <HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> List<Integer> ords(HyperSubjList<HDT_SubjType, HDT_ObjType> subjects)
  {
    return subjects.stream().map(subjects::getOrd).toList();
  }

//---------------------------------------------------------------------------

  private static List<Integer> ids(List<? extends HDT_Record> records)
  {
    return records.stream().map(HDT_Record::getID).toList();
  }

//---------------------------------------------------------------------------

  private static String pointerTag(String tagName, HDT_Record obj, int ord)
  {
    return "<" + tagName + " id=\"" + obj.getID() + "\" ord=\"" + ord + "\">";
  }

  private static String pointerTag(String tagName, HDT_Record obj)
  {
    return "<" + tagName + " id=\"" + obj.getID() + "\">";
  }

//---------------------------------------------------------------------------

  /**
   * The records as a save writes them, through the same writer the save uses.
   */
  private static String xmlOf(HDT_RecordBase... records) throws Exception
  {
    StringBuilder xml = new StringBuilder();

    for (HDT_RecordBase record : records)
    {
      record.saveToStoredState();
      record.writeStoredStateToXML(xml);
    }

    return xml.toString();
  }

//---------------------------------------------------------------------------

  /**
   * Reopens the database with the given records added to the template's works file, so that
   * they go through the real loader.
   */
  private static void reloadWithWorks(String recordsXml)
  {
    db.setRecordsLoadFilter("Works.xml", xml -> xml.replace("</records>", recordsXml + "</records>"));

    PopupRobot.setDefaultResponse(mrYes);  // Continue past the integrity-checksum prompt, since the works file no longer matches the manifest

    db.closeAndOpen();

    assertTrue(db.isOnline(), () -> "Reload failed: " + PopupRobot.getLastMessage());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void subWorksOfABookNeverArrangedAreSavedWithoutAPosition() throws Exception
  {
    HDT_Work book = work("Book"), chapterA = work("Chapter A"), chapterB = work("Chapter B");

    chapterA.largerWork.set(book);
    chapterB.largerWork.set(book);

    assertEquals(List.of(-1, -1), ords(book.subWorks));

    String xml = xmlOf(chapterA, chapterB);

    assertTrue(xml.contains(pointerTag("larger_work", book)));
    assertFalse(xml.contains(" ord="));
  }

//---------------------------------------------------------------------------

  @Test
  void reorderingSubWorksSavesEveryOneWithItsPosition() throws Exception
  {
    HDT_Work book = work("Book"), chapterA = work("Chapter A"), chapterB = work("Chapter B"), chapterC = work("Chapter C");

    chapterA.largerWork.set(book);
    chapterB.largerWork.set(book);
    chapterC.largerWork.set(book);

    book.subWorks.reorder(List.of(chapterC, chapterA, chapterB), false);

    assertEquals(List.of(chapterC, chapterA, chapterB), book.subWorks);
    assertEquals(List.of(1, 2, 3), ords(book.subWorks));
    assertTrue(xmlOf(chapterC).contains(pointerTag("larger_work", book, 1)));
    assertTrue(xmlOf(chapterA).contains(pointerTag("larger_work", book, 2)));
    assertTrue(xmlOf(chapterB).contains(pointerTag("larger_work", book, 3)));
  }

//---------------------------------------------------------------------------

  @Test
  void reorderingOneBookKeepsThePositionsSavedForAnother() throws Exception
  {
    HDT_Work book1 = work("Book 1"), chapterA1 = work("Chapter A1"), chapterB1 = work("Chapter B1"),
             book2 = work("Book 2"), chapterA2 = work("Chapter A2"), chapterB2 = work("Chapter B2");

    chapterA1.largerWork.set(book1);
    chapterB1.largerWork.set(book1);
    chapterA2.largerWork.set(book2);
    chapterB2.largerWork.set(book2);

    book1.subWorks.reorder(List.of(chapterB1, chapterA1), false);
    book2.subWorks.reorder(List.of(chapterB2, chapterA2), false);

    assertEquals(List.of(1, 2), ords(book1.subWorks));
    assertEquals(List.of(1, 2), ords(book2.subWorks));

    String xml = xmlOf(chapterB1, chapterA1);

    assertTrue(xml.contains(pointerTag("larger_work", book1, 1)));
    assertTrue(xml.contains(pointerTag("larger_work", book1, 2)));
  }

//---------------------------------------------------------------------------

  @Test
  void aSubWorkAddedAfterAReorderIsSavedInItsPosition() throws Exception
  {
    HDT_Work book = work("Book"), chapterA = work("Chapter A"), chapterB = work("Chapter B");

    chapterA.largerWork.set(book);
    chapterB.largerWork.set(book);
    book.subWorks.reorder(List.of(chapterB, chapterA), false);

    HDT_Work chapterC = work("Chapter C");
    chapterC.largerWork.set(book);

    assertEquals(List.of(chapterB, chapterA, chapterC), book.subWorks);
    assertEquals(List.of(1, 2, 3), ords(book.subWorks));
    assertTrue(xmlOf(chapterC).contains(pointerTag("larger_work", book, 3)));
  }

//---------------------------------------------------------------------------

  @Test
  void reorderedSubWorksOfTwoBooksSurviveAReloadAndAThirdBookStaysUnarranged() throws Exception
  {
    HDT_Work book1 = workWithID(1, "Book 1"), chapterA1 = workWithID(2, "Chapter A1"), chapterB1 = workWithID(3, "Chapter B1"),
             book2 = workWithID(4, "Book 2"), chapterA2 = workWithID(5, "Chapter A2"), chapterB2 = workWithID(6, "Chapter B2"),
             book3 = workWithID(7, "Book 3"), chapterA3 = workWithID(8, "Chapter A3"), chapterB3 = workWithID(9, "Chapter B3");

    chapterA1.largerWork.set(book1);
    chapterB1.largerWork.set(book1);
    chapterA2.largerWork.set(book2);
    chapterB2.largerWork.set(book2);
    chapterA3.largerWork.set(book3);
    chapterB3.largerWork.set(book3);

    book1.subWorks.reorder(List.of(chapterB1, chapterA1), false);
    book2.subWorks.reorder(List.of(chapterB2, chapterA2), false);

    reloadWithWorks(xmlOf(book1, chapterA1, chapterB1, book2, chapterA2, chapterB2, book3, chapterA3, chapterB3));

    assertEquals(List.of(3, 2), ids(db.works.getByID(1).subWorks));
    assertEquals(List.of(6, 5), ids(db.works.getByID(4).subWorks));
    assertEquals(List.of(8, 9), ids(db.works.getByID(7).subWorks));

    assertEquals(List.of(1, 2)  , ords(db.works.getByID(1).subWorks));
    assertEquals(List.of(-1, -1), ords(db.works.getByID(7).subWorks));
  }

//---------------------------------------------------------------------------

  /**
   * Files written by earlier versions carried the attribute only for sub-works that had been
   * reordered, so a sub-work added afterwards was saved without it. If its ID was lower than its
   * siblings' (a deleted record's ID is reused), it was brought online first, and the loader then
   * failed to place the siblings around it.
   */
  @Test
  void aSubWorkSavedWithoutAPositionLoadsLastAndGetsOneAtTheNextSave() throws Exception
  {
    HDT_Work book = workWithID(1, "Book"), chapterA = workWithID(3, "Chapter A"), chapterB = workWithID(4, "Chapter B");

    chapterA.largerWork.set(book);
    chapterB.largerWork.set(book);
    book.subWorks.reorder(List.of(chapterB, chapterA), false);

    HDT_Work chapterC = workWithID(2, "Chapter C");
    chapterC.largerWork.set(book);

    String xml = xmlOf(book, chapterC, chapterA, chapterB).replace(pointerTag("larger_work", book, 3), pointerTag("larger_work", book));

    assertTrue(xml.contains(pointerTag("larger_work", book)));  // Chapter C's tag now has no position, as in the earlier form

    reloadWithWorks(xml);

    assertEquals(List.of(4, 3, 2), ids (db.works.getByID(1).subWorks));
    assertEquals(List.of(1, 2, 3), ords(db.works.getByID(1).subWorks));
  }

//---------------------------------------------------------------------------

  @Test
  void investigationsKeepTheOrderTheyAreArrangedIn() throws Exception
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Investigation inv1 = db.createNewBlankRecord(hdtInvestigation), inv2 = db.createNewBlankRecord(hdtInvestigation);

    inv1.person.set(person);
    inv2.person.set(person);
    person.investigations.reorder(List.of(inv2, inv1), false);

    assertEquals(List.of(inv2, inv1), person.investigations);
    assertEquals(List.of(1, 2), ords(person.investigations));
    assertTrue(xmlOf(inv2).contains(pointerTag("person", person, 1)));
    assertTrue(xmlOf(inv1).contains(pointerTag("person", person, 2)));
  }

//---------------------------------------------------------------------------

  @Test
  void divisionsKeepTheOrderTheyAreArrangedIn() throws Exception
  {
    HDT_Institution university = db.createNewBlankRecord(hdtInstitution),
                    division1  = db.createNewBlankRecord(hdtInstitution),
                    division2  = db.createNewBlankRecord(hdtInstitution);

    division1.parentInst.set(university);
    division2.parentInst.set(university);
    university.subInstitutions.reorder(List.of(division2, division1), false);

    assertEquals(List.of(division2, division1), university.subInstitutions);
    assertEquals(List.of(1, 2), ords(university.subInstitutions));
    assertTrue(xmlOf(division2).contains(pointerTag("parent_institution", university, 1)));
    assertTrue(xmlOf(division1).contains(pointerTag("parent_institution", university, 2)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
