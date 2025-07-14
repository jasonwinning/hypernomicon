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

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.*;
import org.hypernomicon.model.unities.MainText.DisplayItem;
import org.hypernomicon.model.unities.MainText.DisplayItemType;
import org.junit.jupiter.api.*;

import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.mutable.MutableBoolean;

//---------------------------------------------------------------------------

class MentionsAndDisplayIndexTest
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
  void testFirstMentionsSecond()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    person.getMainText().setHtml("abcdefg");

    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    assertDoesNotThrow(() -> term.setSearchKey("abcde"));

    MutableBoolean choseNotToWait = new MutableBoolean();
    assertTrue(db.firstMentionsSecond(person, term, false, choseNotToWait));
    assertFalse(choseNotToWait.booleanValue());

    assertTrue(db.getMentionerSet(term, false).contains(person));

    person.getMainText().setHtml("abcd");
    assertFalse(db.firstMentionsSecond(person, term, false, choseNotToWait));
    assertFalse(choseNotToWait.booleanValue());

    assertFalse(db.getMentionerSet(term, false).contains(person));

    person.getMainText().setHtml("abcde");
    assertTrue(db.firstMentionsSecond(person, term, false, choseNotToWait));
    assertFalse(choseNotToWait.booleanValue());

    assertTrue(db.getMentionerSet(term, false).contains(person));

    assertDoesNotThrow(() -> term.setSearchKey("abcdef"));
    assertFalse(db.firstMentionsSecond(person, term, false, choseNotToWait));
    assertFalse(choseNotToWait.booleanValue());

    assertFalse(db.getMentionerSet(term, false).contains(person));

    assertDoesNotThrow(() -> term.setSearchKey(""));
  }

//---------------------------------------------------------------------------

  @Test
  void testDisplayedAt()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.get(0);

    assertNotNull(concept);
    assertFalse(firstDisplaysSecond(concept, person));

    person.getMainText().setDisplayItemsFromList(List.of(new DisplayItem(concept)));

    assertTrue(firstDisplaysSecond(person, concept));

    Set<HDT_Record> set = db.getMentionerSet(concept, false);
    assertTrue(set.contains(person));
  }

//---------------------------------------------------------------------------

  @Test
  void testKeyWorkIndex()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);

    assertTrue(db.keyWorkMentionerStream(work, true ).findAny().isEmpty());
    assertTrue(db.keyWorkMentionerStream(work, false).findAny().isEmpty());

    debate.getMainText().setKeyWorksFromList(List.of(new KeyWork(work)));
    label .getMainText().setKeyWorksFromList(List.of(new KeyWork(work)));

    assertTrue(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == debate));
    assertTrue(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == debate));
    assertTrue(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == label ));
    assertTrue(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == label ));

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, label, ""));

    assertTrue (db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == debate));
    assertTrue (db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == debate));
    assertFalse(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == label ));  // Label is not the main spoke
    assertTrue (db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == label ));

    assertTrue (db.keyWorkMentionerStream(work, HDT_Debate.class).anyMatch(rec -> rec == debate));
    assertTrue (db.keyWorkMentionerStream(work, hdtDebate).anyMatch(rec -> rec == debate));
    assertFalse(db.keyWorkMentionerStream(work, hdtDebate).anyMatch(rec -> rec == label ));

    assertTrue (db.keyWorkMentionerStream(work, HDT_WorkLabel.class).anyMatch(rec -> rec == label));
    assertFalse(db.keyWorkMentionerStream(work, hdtWorkLabel).anyMatch(rec -> rec == debate));
    assertTrue (db.keyWorkMentionerStream(work, hdtWorkLabel).anyMatch(rec -> rec == label ));

    debate.getHub().disuniteRecord(hdtWorkLabel);

    assertTrue(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == debate));
    assertTrue(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == debate));
    assertTrue(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == label ));
    assertTrue(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == label ));

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, label, ""));

    db.deleteRecord(debate);

    assertFalse(db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == debate));
    assertFalse(db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == debate));
    assertTrue (db.keyWorkMentionerStream(work, true ).anyMatch(rec -> rec == label ));
    assertTrue (db.keyWorkMentionerStream(work, false).anyMatch(rec -> rec == label ));
  }

//---------------------------------------------------------------------------

  @Test
  void testDeleteMentioner()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    person.getMainText().setHtml("abcdefg");

    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    assertDoesNotThrow(() -> term.setSearchKey("abcde"));

    MutableBoolean choseNotToWait = new MutableBoolean();
    assertTrue(db.firstMentionsSecond(person, term, false, choseNotToWait));

    db.deleteRecord(person);

    Set<HDT_Record> set = db.getMentionerSet(term, false);
    assertTrue(set.isEmpty());

    assertDoesNotThrow(() -> term.setSearchKey(""));
  }

//---------------------------------------------------------------------------

  @Test
  void testDeleteDisplayer()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.get(0);

    person.getMainText().setDisplayItemsFromList(List.of(new DisplayItem(concept)));

    assertTrue(firstDisplaysSecond(person, concept));

    db.deleteRecord(person);

    assertTrue(displaysNoRecords(concept));
  }

//---------------------------------------------------------------------------

  @Test
  void testDeleteDisplayed()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.get(0);

    person.getMainText().setDisplayItemsFromList(List.of(new DisplayItem(concept)));

    assertTrue(firstDisplaysSecond(person, concept));

    db.deleteRecord(term);

    assertTrue(displaysNoRecords(person));
  }

//---------------------------------------------------------------------------

  @Test
  void testUniteAndDisuniteMentioner()
  {
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    note.getMainText().setHtml("abcdefg");

    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    assertDoesNotThrow(() -> term.setSearchKey("abcde"));

    assertTrue(db.firstMentionsSecond(note, term, false, new MutableBoolean(false)));

    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, note, note.getMainText().getHtml()));

    assertTrue(db.firstMentionsSecond(note  , term, false, new MutableBoolean(false)));
    assertTrue(db.firstMentionsSecond(debate, term, false, new MutableBoolean(false)));

    assertTrue(debate.getHub().disuniteRecord(hdtDebate));

    assertTrue(db.firstMentionsSecond(note  , term, false, new MutableBoolean(false)));
    assertTrue(db.firstMentionsSecond(debate, term, false, new MutableBoolean(false)));

    db.deleteRecord(note);
    db.deleteRecord(debate);
    assertDoesNotThrow(() -> term.setSearchKey(""));
  }

//---------------------------------------------------------------------------

  @Test
  void testUniteAndDisuniteDisplayer()
  {
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.get(0);

    note.getMainText().setDisplayItemsFromList(List.of(new DisplayItem(concept)));

    assertTrue(firstDisplaysSecond(note, concept));

    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, note, ""));

    assertTrue(firstDisplaysSecond(debate, concept));
    assertTrue(firstDisplaysSecond(note  , concept));

    assertTrue(debate.getHub().disuniteRecord(hdtDebate));

    assertTrue(firstDisplaysSecond(debate, concept));
    assertTrue(firstDisplaysSecond(note  , concept));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean firstDisplaysSecond(HDT_RecordWithMainText displayer, HDT_RecordWithMainText displayed)
  {
    boolean rv1 = displayer.getMainText().getDisplayItemsUnmod().stream().filter(item -> item.type == DisplayItemType.diRecord)
                                                                         .anyMatch(item -> item.record == displayed);

    HDT_RecordWithMainText mainSpoke = displayer.mainSpoke();

    boolean rv2 = db.displayerStream(displayed).anyMatch(mainSpoke::equals);

    assertEquals(rv1, rv2);

    return rv1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean displaysNoRecords(HDT_RecordWithMainText displayer)
  {
    return db.displayerStream(displayer).findAny().isEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
