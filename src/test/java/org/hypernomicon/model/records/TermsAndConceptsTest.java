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
import static org.hypernomicon.util.PopupDialog.DialogResult.*;

import static org.junit.jupiter.api.Assertions.*;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_ConceptSense;
import org.hypernomicon.model.searchKeys.KeywordLink;
import org.hypernomicon.model.searchKeys.KeywordLinkScanner;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.model.unities.MainText;
import org.hypernomicon.util.PopupRobot;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Tests for the model of terms, concepts, glossaries and senses: a term is a word, each of
 * its concepts is what the word means within one glossary and sense, and it is the concept
 * that carries the text and can be united with other records. The Term tab's editing rules
 * are pinned separately, by {@code TermTabCtrlr.runTests}.
 */
class TermsAndConceptsTest
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
  void setUp()
  {
    db.closeAndOpen();  // The template has no terms, concepts or senses, and only the General glossary
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    PopupRobot.setDefaultResponse(mrOk);  // The robot's own default; one test answers No to a confirmation
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    db.closeAndOpen();  // Leave no records behind for other test classes
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Glossary general() { return db.glossaries.getByID(1); }

  private static HDT_Glossary newGlossary(String name)
  {
    HDT_Glossary glossary = db.createNewBlankRecord(hdtGlossary);
    glossary.setName(name);
    return glossary;
  }

  private static HDT_ConceptSense newSense(String name)
  {
    HDT_ConceptSense sense = db.createNewBlankRecord(hdtConceptSense);
    sense.setName(name);
    return sense;
  }

  private static HDT_Term newTerm(String name, HDT_Glossary glossary)
  {
    HDT_Term term = HDT_Term.create(glossary);
    term.setName(name);
    return term;
  }

  /** Gives the term a further concept, in the glossary under the sense, as the Term tab does when a row is filled in. */
  private static HDT_Concept addConcept(HDT_Term term, HDT_Glossary glossary, HDT_ConceptSense sense)
  {
    HDT_Concept concept = db.createNewBlankRecord(hdtConcept);
    term.concepts.add(concept);
    concept.glossary.set(glossary);
    concept.sense.set(sense);
    return concept;
  }

  private static void setSearchKey(HDT_Record record, String key)
  {
    assertDoesNotThrow(() -> record.setSearchKey(key, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void createGivesTheTermOneConceptInTheGlossaryAndTheConceptTheTermsName()
  {
    HDT_Term term = newTerm("Materialism", general());
    HDT_Concept concept = term.concepts.getFirst();

    assertEquals(1, term.concepts.size());
    assertSame(term, concept.term.get());
    assertSame(general(), concept.glossary.get());
    assertTrue(concept.sense.isNull());
    assertSame(concept, term.getConcept(general(), null));
    assertEquals(List.of(general()), term.getGlossaries());

    assertEquals("Materialism", concept.name(), "A concept is named by its term");

    concept.setName("Physicalism");
    assertEquals("Physicalism", term.name(), "Naming a concept names its term");
  }

//---------------------------------------------------------------------------

  @Test
  void extendedNameAddsTheGlossaryAndSenseOnlyWhenTheyTellConceptsApart()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept inGeneral = term.concepts.getFirst();

    assertEquals("Cause", inGeneral.extendedName(true), "The General glossary is not named while it is the term's only glossary");

    HDT_Glossary physics = newGlossary("Physics");
    HDT_Concept inPhysics = addConcept(term, physics, newSense("Strict"));

    assertEquals("Cause (General)"        , inGeneral.extendedName(true ), "Once the term spans two glossaries, General is named too");
    assertEquals("Cause (Physics, Strict)", inPhysics.extendedName(true ));
    assertEquals("Cause"                  , inGeneral.extendedName(false), "Without the glossary, a blank sense adds nothing");
    assertEquals("Cause (Strict)"         , inPhysics.extendedName(false));
  }

//---------------------------------------------------------------------------

  @Test
  void theFirstConceptIsTheTermsDescriptionAndTheUserCanReorderThem()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept first  = term.concepts.getFirst(),
                second = addConcept(term, general(), newSense("Final"));

    first .getMainText().setHtml("<p>first text</p>");
    second.getMainText().setHtml("<p>second text</p>");

    assertSame(first.getMainText(), term.getDesc());

    term.concepts.reorder(List.of(second, first));

    assertSame(second, term.concepts.getFirst());
    assertSame(second.getMainText(), term.getDesc(), "After reordering, the new first concept supplies the description");
  }

//---------------------------------------------------------------------------

  @Test
  void aTermIsModifiedWheneverOneOfItsConceptsIs() throws InterruptedException
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept first  = term.concepts.getFirst(),
                second = addConcept(term, general(), newSense("Final"));

    Instant before = term.getModifiedDate();

    Thread.sleep(5);  // So that the edit's time stamp is later than every earlier one

    second.getMainText().setHtml("<p>edited</p>");

    assertEquals(second.getModifiedDate(), term.getModifiedDate(), "The term's modified date is its latest concept's");
    assertTrue(term.getModifiedDate().isAfter(before));
    assertTrue(term.getModifiedDate().isAfter(first.getModifiedDate()));
  }

//---------------------------------------------------------------------------

  @Test
  void parentConceptsMustShareTheGlossaryAndNotTheTermAndMayNotFormACycle()
  {
    HDT_Term materialism = newTerm("Materialism", general()),
             nonreductive = newTerm("Nonreductive materialism", general()),
             force = newTerm("Force", newGlossary("Physics"));

    HDT_Concept wider = materialism.concepts.getFirst(),
                narrower = nonreductive.concepts.getFirst(),
                otherGlossary = force.concepts.getFirst(),
                sibling = addConcept(nonreductive, general(), newSense("Strict"));

    assertTrue(assertDoesNotThrow(() -> narrower.setParentConcepts(List.of(wider))));
    assertEquals(List.of(wider), List.copyOf(narrower.parentConcepts));
    assertEquals(List.of(narrower), List.copyOf(wider.subConcepts));

    assertFalse(assertDoesNotThrow(() -> narrower.setParentConcepts(List.of(otherGlossary))), "A parent must be in the same glossary");
    assertFalse(assertDoesNotThrow(() -> narrower.setParentConcepts(List.of(sibling))), "A parent may not be another concept of the same term");
    assertEquals(List.of(wider), List.copyOf(narrower.parentConcepts), "A refused list leaves the parents as they were");

    assertThrows(RelationCycleException.class, () -> narrower.setParentConcepts(List.of(narrower)));
    assertThrows(RelationCycleException.class, () -> wider.setParentConcepts(List.of(narrower)), "The wider concept cannot become a child of its own child");
  }

//---------------------------------------------------------------------------

  @Test
  void addNewSubConceptCreatesATermWhoseConceptIsAChildInTheSameGlossary()
  {
    HDT_Glossary physics = newGlossary("Physics");
    HDT_Concept parent = newTerm("Force", physics).concepts.getFirst();

    int termCount = db.terms.size();

    HDT_Concept child = parent.addNewSubConcept();

    assertEquals(termCount + 1, db.terms.size());
    assertNotSame(parent.term.get(), child.term.get());
    assertSame(physics, child.glossary.get());
    assertEquals(List.of(parent), List.copyOf(child.parentConcepts));
    assertEquals(List.of(child), List.copyOf(parent.subConcepts));
  }

//---------------------------------------------------------------------------

  @Test
  void movingAConceptToAnotherTermCanChangeItsGlossaryAndNameANewSense()
  {
    HDT_Glossary physics = newGlossary("Physics");

    HDT_Term source = newTerm("Force", general()),
             target = newTerm("Power", general()),
             parentTerm = newTerm("Quantity", general());

    HDT_Concept moving = source.concepts.getFirst(),
                parent = parentTerm.concepts.getFirst();

    addConcept(source, general(), newSense("Other"));  // So that the source term keeps a concept
    assertDoesNotThrow(() -> moving.setParentConcepts(List.of(parent)));

    moving.moveToDifferentTerm(target, false, physics, null, "Strict");

    assertSame(target, moving.term.get());
    assertFalse(source.concepts.contains(moving));
    assertTrue(target.concepts.contains(moving));
    assertSame(physics, moving.glossary.get());
    assertEquals("Strict", moving.sense.get().name(), "A sense named in the move is created");
    assertTrue(moving.parentConcepts.isEmpty(), "Moving to another glossary drops the parents");
    assertTrue(parent.subConcepts.isEmpty());

    HDT_Term created = db.createNewBlankRecord(hdtTerm);
    HDT_Concept kept = target.concepts.getFirst();

    kept.moveToDifferentTerm(created, true, null, null, null);

    assertSame(created, kept.term.get());
    assertSame(general(), kept.glossary.get(), "A move to a newly created term keeps the glossary and sense");
  }

//---------------------------------------------------------------------------

  @Test
  void aSenseIsDeletedWithTheLastConceptThatHasIt()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept first  = term.concepts.getFirst(),
                second = addConcept(term, general(), newSense("Final"));

    HDT_ConceptSense shared = newSense("Efficient");

    first .sense.set(shared);
    second.sense.set(shared);

    first.sense.set(null);
    assertFalse(HDT_Record.isEmpty(shared, false), "A sense another concept still has stays");

    db.deleteRecord(second);
    assertTrue(HDT_Record.isEmpty(shared, false), "A sense no concept has is deleted with its last concept");
    assertTrue(db.conceptSenses.isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void unitingAConceptSharesOneTextWithTheRecordAndSoWithTheTerm()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept concept = term.concepts.getFirst();
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, concept, "<p>shared text</p>"));

    MainText shared = debate.getMainText();

    assertSame(shared, concept.getMainText());
    assertSame(shared, term.getDesc(), "The term's description is the text its concept shares");
    assertSame(concept, debate.getHub().getConcept());
    assertTrue(shared.getPlain().contains("shared text"));

    StringBuilder reason = new StringBuilder();

    assertFalse(HDT_Hub.canUnite(concept, newTerm("Reason", general()).concepts.getFirst(), reason), "Two concepts cannot be united");
    assertFalse(reason.isEmpty());

    assertFalse(HDT_Hub.canUnite(db.createNewBlankRecord(hdtDebate), concept, reason), "The unity already has a debate");
  }

//---------------------------------------------------------------------------

  @Test
  void deletingATermDeletesItsConceptsAndLeavesAUnitedRecordWithTheTextAsItsOwn()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept concept = term.concepts.getFirst();
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, concept, "<p>shared text</p>"));

    HDT_Hub hub = debate.getHub();

    db.deleteRecord(term);

    assertTrue(HDT_Record.isEmpty(term   , false));
    assertTrue(HDT_Record.isEmpty(concept, false), "A term's concepts go with it");
    assertTrue(HDT_Record.isEmpty(hub    , false), "A hub left with one member dissolves");

    assertFalse(debate.hasHub());
    assertTrue(debate.getMainText().getPlain().contains("shared text"), "The remaining member keeps the shared text as its own");
  }

//---------------------------------------------------------------------------

  @Test
  void conceptsAreReachableThroughTheirOwnKeysOrAUnitedRecordsKey()
  {
    HDT_Term term = newTerm("Cause", general());
    HDT_Concept concept = term.concepts.getFirst();

    assertTrue(HDT_Concept.conceptsDontAllHaveSearchKey(term.concepts.stream()), "A keyless concept is unreachable");

    setSearchKey(concept, "efficient cause");
    assertFalse(HDT_Concept.conceptsDontAllHaveSearchKey(term.concepts.stream()), "A concept's own key reaches it");

    HDT_Concept second = addConcept(term, general(), newSense("Final"));
    assertTrue(HDT_Concept.conceptsDontAllHaveSearchKey(term.concepts.stream()), "Every concept must be reachable, not just one");

    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    setSearchKey(debate, "final causation");
    assertDoesNotThrow(() -> HDT_Hub.uniteRecords(debate, second, ""));

    assertFalse(HDT_Concept.conceptsDontAllHaveSearchKey(term.concepts.stream()), "A united record's key reaches the concept");
  }

//---------------------------------------------------------------------------

  @Test
  void twoRecordsMayShareAKeyOnceConfirmedAndALinkThenNamesBoth()
  {
    HDT_Term bank = newTerm("Bank", general());
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);

    setSearchKey(bank, "riverbank");

    PopupRobot.setDefaultResponse(mrNo);

    assertThrows(DuplicateSearchKeyException.class, () -> debate.setSearchKey("riverbank", true), "Declining the confirmation refuses the shared key");
    assertEquals("", debate.getSearchKey());

    PopupRobot.setDefaultResponse(mrYes);

    setSearchKey(debate, "riverbank");

    List<KeywordLink> links = KeywordLinkScanner.scan("The riverbank was steep.");

    assertEquals(1, links.size());
    assertFalse(links.getFirst().isSingle(), "A key two records share yields one link that names both");
    assertEquals(Set.of(bank, debate), links.getFirst().recordStream().collect(Collectors.toSet()));
  }

//---------------------------------------------------------------------------

  @Test
  void mergeMovesTheConceptsIntoTheFirstTermAndDeletesTheSecond()
  {
    HDT_Term keep = newTerm("Cause", general()),
             absorbed = newTerm("Causation", newGlossary("Physics"));

    setSearchKey(keep    , "cause");
    setSearchKey(absorbed, "causation");

    HDT_Concept kept = keep.concepts.getFirst(),
                moved = absorbed.concepts.getFirst();

    assertDoesNotThrow(() -> HDT_Term.merge(keep, absorbed, "Cause (merged)", "cause; causation"));

    assertEquals(List.of(kept, moved), List.copyOf(keep.concepts), "The second term's concepts follow its own");
    assertSame(keep, moved.term.get());
    assertEquals("Cause (merged)", keep.name());
    assertTrue(keep.getSearchKey().contains("cause") && keep.getSearchKey().contains("causation"), keep.getSearchKey());
    assertTrue(HDT_Record.isEmpty(absorbed, false));
  }

//---------------------------------------------------------------------------

  @Test
  void mergeRestoresBothKeysWhenTheNewKeyIsRefused()
  {
    HDT_Term keep = newTerm("Cause", general()),
             absorbed = newTerm("Causation", newGlossary("Physics")),
             other = newTerm("Reason", general());

    setSearchKey(keep    , "cause");
    setSearchKey(absorbed, "causation");
    setSearchKey(other   , "reason");

    PopupRobot.setDefaultResponse(mrNo);  // Decline to share "reason" with the other term

    assertThrows(DuplicateSearchKeyException.class, () -> HDT_Term.merge(keep, absorbed, "Cause", "reason"));

    assertEquals("cause"    , keep    .getSearchKey(), "A refused merge puts the first term's key back");
    assertEquals("causation", absorbed.getSearchKey(), "and the second term's");
    assertFalse(HDT_Record.isEmpty(absorbed, false));
    assertEquals(1, keep.concepts.size());
  }

//---------------------------------------------------------------------------

  @Test
  void mergeRequiresAKeyUnlessTheDatabaseSettingWaivesIt()
  {
    HDT_Term keep = newTerm("Cause", general()),
             absorbed = newTerm("Causation", newGlossary("Physics"));

    assertThrows(SearchKeyTooShortException.class, () -> HDT_Term.merge(keep, absorbed, "Cause", ""));
    assertFalse(HDT_Record.isEmpty(absorbed, false), "Nothing happens when the key is refused");

    db.prefs.putBoolean(PrefKey.TERM_REQUIRE_SEARCH_KEY, false);

    assertDoesNotThrow(() -> HDT_Term.merge(keep, absorbed, "Cause", ""));
    assertEquals(2, keep.concepts.size());
    assertTrue(HDT_Record.isEmpty(absorbed, false));
  }

//---------------------------------------------------------------------------

  @Test
  void restoreRefusesATermWhoseConceptsChangedSinceTheBackup()
  {
    HDT_Term term = newTerm("Cause", general());

    RecordState backup = term.getRecordStateBackup();

    addConcept(term, general(), newSense("Final"));

    assertThrows(ConceptChangedException.class, () -> term.restoreTo(backup, false));
    assertEquals(2, term.concepts.size(), "A refused restore changes nothing");

    RecordState backupOfBoth = term.getRecordStateBackup();

    term.setName("Renamed");

    assertDoesNotThrow(() -> term.restoreTo(backupOfBoth, false));
    assertEquals("Cause", term.name(), "With the concepts unchanged, the restore goes ahead");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
