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

package org.hypernomicon.model;

import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.*;
import org.hypernomicon.model.unities.MainText.DisplayItem;
import org.hypernomicon.model.unities.MainText.DisplayItemType;

import org.junit.jupiter.api.*;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
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
//---------------------------------------------------------------------------

  private static final MutableBoolean choseNotToWait = new MutableBoolean();

  private static boolean firstMentionsSecond(HDT_Record mentioner, HDT_Record target, boolean descOnly)
  {
    boolean rv = db.firstMentionsSecond(mentioner, target, descOnly, choseNotToWait);
    assertFalse(choseNotToWait.booleanValue());

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean firstDisplaysSecond(HDT_RecordWithMainText displayer, HDT_RecordWithMainText displayed)
  {
    boolean rv1 = displayer.displayItemsStream().filter(item -> item.type == DisplayItemType.diRecord)
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

  @Test
  void testFirstMentionsSecond()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term[] termHolder = { null };

    try
    {
      assertDoesNotThrow(() -> person.setSearchKey("abcde", true));
      person.setName(new PersonName("abcde"));

      assertSame(person, HDT_Person.lookUpByName(new PersonName("abcde")), "lookUpByName should match the person record");

      assertFalse(firstMentionsSecond(person, person, false), "A record should not automatically be considered as \"mentioning\" itself.");
      assertFalse(firstMentionsSecond(person, person, true ), "A record should not automatically be considered as \"mentioning\" itself.");

      assertDoesNotThrow(() -> person.setSearchKey("", true));

      HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
      termHolder[0] = term;

      assertDoesNotThrow(() -> term.setSearchKey("abcde", true));

      assertTrue(firstMentionsSecond(person, term, false), "Person should mention the term because of the person name");
      person.setName(new PersonName("", ""));
      assertFalse(firstMentionsSecond(person, term, false), "Clearing the person name should update the mentions index");

      person.getMainText().setHtml("abcdefg");

      HDT_Concept concept = term.concepts.getFirst();

      assertTrue(firstMentionsSecond(person, term, false));

      assertFalse(firstMentionsSecond(term, concept, false), "A term should not be considered as \"mentioning\" its concepts or vice versa");
      assertFalse(firstMentionsSecond(term, concept, true ), "A term should not be considered as \"mentioning\" its concepts or vice versa");
      assertFalse(firstMentionsSecond(concept, term, false), "A term should not be considered as \"mentioning\" its concepts or vice versa");
      assertFalse(firstMentionsSecond(concept, term, true ), "A term should not be considered as \"mentioning\" its concepts or vice versa");

      assertTrue(db.getMentionerSet(term, false).contains(person));

      person.getMainText().setHtml("abcd");
      assertFalse(firstMentionsSecond(person, term, false));

      assertFalse(db.getMentionerSet(term, false).contains(person));

      person.getMainText().setHtml("abcde");
      assertTrue(firstMentionsSecond(person, term, false));

      assertTrue(db.getMentionerSet(term, false).contains(person));

      assertDoesNotThrow(() -> term.setSearchKey("abcdef", true));
      assertFalse(firstMentionsSecond(person, term, false));

      assertFalse(db.getMentionerSet(term, false).contains(person));
    }
    finally
    {
      HDT_Term term = termHolder[0];
      if (term != null)
        assertDoesNotThrow(() -> term.setSearchKey("", true));

      assertDoesNotThrow(() -> person.setSearchKey("", true));
      person.getMainText().setHtml("");
    }
  }

//---------------------------------------------------------------------------

  @Test
  void testDisplayedAtAndTermMentionLogic()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.getFirst();

    try
    {
      assertNotNull(concept);
      assertFalse(firstDisplaysSecond(concept, person));

      person.getMainText().setDisplayItemsFromList(List.of(new DisplayItem(concept)));

      assertTrue(firstDisplaysSecond(person, concept));

      Set<HDT_Record> set = db.getMentionerSet(concept, false);
      assertTrue(set.contains(person));

      set = db.getMentionerSet(term, false);
      assertTrue(set.contains(person), "A term's mentioners should include mentioners of its concepts.");

      person.getMainText().setDisplayItemsFromList(List.of());

      assertFalse(firstDisplaysSecond(person, concept));
      set = db.getMentionerSet(concept, false);
      assertFalse(set.contains(person));

      person.getMainText().setHtml("abcdefg");

      assertDoesNotThrow(() -> term.setSearchKey("abcde", true));

      assertTrue(firstMentionsSecond(person, term, false));

      assertFalse(firstMentionsSecond(person, concept, false), "A concept's mentioners should not automatically include mentioners of its term");
    }
    finally
    {
      assertDoesNotThrow(() -> term.setSearchKey("", true));
      person.getMainText().setHtml("");
    }
  }

//---------------------------------------------------------------------------

  @Test
  void testEmbeddedMiscFiles()
  {
    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    String imageTag = '<' + EMBEDDED_FILE_TAG + " id=\"" + miscFile.getID() + "\" width=\"300px\"/>";

    assertFalse(firstMentionsSecond(note, miscFile, false));
    assertFalse(firstMentionsSecond(note, miscFile, true ));

    note.getMainText().setHtml("<html><body><div> " + htmlEscaper.escape(imageTag) + "<br></div></body></html>");

    assertTrue(firstMentionsSecond(note, miscFile, false));
    assertTrue(firstMentionsSecond(note, miscFile, true ));

    // Cleanup

    note.getMainText().setHtml("");

    assertFalse(firstMentionsSecond(note, miscFile, false));
    assertFalse(firstMentionsSecond(note, miscFile, true ));
  }

//---------------------------------------------------------------------------

  @Test
  void testKeyWorkIndexUniting()
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
  void testInvestigationMentionLogic()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);
    HDT_Person person = db.createNewBlankRecord(hdtPerson);

    HDT_Investigation inv = db.createNewBlankRecord(hdtInvestigation);
    inv.person.set(person);
    HDT_Note note = null;

    try
    {
      inv.getMainText().setKeyWorksFromList(List.of(new KeyWork(work)));

      assertEquals(1, work.investigationSet().size());
      assertSame(inv, work.investigationSet().iterator().next());

      assertTrue(firstMentionsSecond(inv, work, false));
      assertTrue(firstMentionsSecond(inv, work, true ));

      List<HDT_RecordWithAuthors<? extends RecordAuthors>> list = inv.worksAndMiscFilesStream().toList();

      assertEquals(1, list.size());
      assertSame(work, list.getFirst());

      inv.getMainText().setKeyWorksFromList(List.of(new KeyWork(miscFile)));

      assertTrue (firstMentionsSecond(inv, miscFile, false));
      assertFalse(firstMentionsSecond(inv, work    , false));

      assertTrue (firstMentionsSecond(inv, miscFile, true));
      assertFalse(firstMentionsSecond(inv, work    , true));

      list = inv.worksAndMiscFilesStream().toList();
      assertEquals(1, list.size());
      assertSame(miscFile, list.getFirst());

      assertEquals(0, work.investigationSet().size());
      assertEquals(1, miscFile.investigationSet().size());
      assertSame(inv, miscFile.investigationSet().iterator().next());

      assertDoesNotThrow(() -> person.setSearchKey("abcde", true));

      note = db.createNewBlankRecord(hdtNote);

      note.getMainText().setHtml("abcdefg");

      assertFalse(firstMentionsSecond(note, inv, true ), "An investigation's mentioners should not include mentioners of its person.");
      assertFalse(firstMentionsSecond(note, inv, false), "An investigation's mentioners should not include mentioners of its person.");

      assertDoesNotThrow(() -> person.setSearchKey("", true));
      assertDoesNotThrow(() -> inv.setSearchKey("abcde", true));

      assertTrue(firstMentionsSecond(note, person, true ), "An persons's mentioners should include mentioners of its investigations.");
      assertTrue(firstMentionsSecond(note, person, false), "An persons's mentioners should include mentioners of its investigations.");
    }
    finally
    {
      if (note != null)
        note.getMainText().setHtml("");

      assertDoesNotThrow(() -> person.setSearchKey("", true));
      assertDoesNotThrow(() -> inv.setSearchKey("", true));
    }
  }

//---------------------------------------------------------------------------

  @Test
  void testLabelMentionLogic()
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);

    HDT_WorkLabel label  = db.createNewBlankRecord(hdtWorkLabel);

    label.getMainText().setKeyWorksFromList(List.of(new KeyWork(work)));

    assertEquals(1, work.labelStream().count());
    assertSame(label, work.labelStream().findFirst().orElse(null));

    assertTrue(firstMentionsSecond(label, work, false));

    assertTrue(firstMentionsSecond(label, work, true));

    List<HDT_RecordWithAuthors<? extends RecordAuthors>> list = label.worksAndMiscFilesStream().toList();

    assertEquals(1, list.size());
    assertSame(work, list.getFirst());

    label.getMainText().setKeyWorksFromList(List.of(new KeyWork(miscFile)));

    assertTrue (firstMentionsSecond(label, miscFile, false));
    assertFalse(firstMentionsSecond(label, work    , false));

    assertTrue (firstMentionsSecond(label, miscFile, true));
    assertFalse(firstMentionsSecond(label, work    , true));

    list = label.worksAndMiscFilesStream().toList();
    assertEquals(1, list.size());
    assertSame(miscFile, list.getFirst());

    assertEquals(0, work.labelStream().count());
    assertEquals(1, miscFile.labelStream().count());
    assertSame(label, miscFile.labelStream().findFirst().orElse(null));
  }

//---------------------------------------------------------------------------

  @Test
  void testDeleteMentioner()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));

    try
    {
      person.getMainText().setHtml("abcdefg");
      assertDoesNotThrow(() -> term.setSearchKey("abcde", true));

      assertTrue(firstMentionsSecond(person, term, false));

      db.deleteRecord(person);

      Set<HDT_Record> set = db.getMentionerSet(term, false);
      assertTrue(set.isEmpty());
    }
    finally
    {
      person.getMainText().setHtml("");
      assertDoesNotThrow(() -> term.setSearchKey("", true));
    }
  }

//---------------------------------------------------------------------------

  @Test
  void testDeleteDisplayer()
  {
    HDT_Person person = db.createNewBlankRecord(hdtPerson);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.getFirst();

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
    HDT_Concept concept = term.concepts.getFirst();

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
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));

    try
    {
      note.getMainText().setHtml("abcdefg");
      assertDoesNotThrow(() -> term.setSearchKey("abcde", true));

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
    }
    finally
    {
      assertDoesNotThrow(() -> term.setSearchKey("", true));
    }
  }

//---------------------------------------------------------------------------

  @Test
  void testUniteAndDisuniteDisplayer()
  {
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_Term term = HDT_Term.create(db.glossaries.getByID(1));
    HDT_Concept concept = term.concepts.getFirst();

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

}
