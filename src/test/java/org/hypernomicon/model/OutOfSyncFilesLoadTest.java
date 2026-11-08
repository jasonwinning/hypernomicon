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

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.model.authors.RecordAuthors;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.*;
import org.hypernomicon.model.unities.MainText.DisplayItem;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * A set of record files that are out of sync with one another, as a sync that copied only some of them
 * or one file restored from a backup leaves them, can disagree about which records a hub unites, or
 * name a key work that no record has. These tests pin how a load repairs each such state: a record is
 * a member of a unity only when its own file and the hub's file agree, a record that is not a member
 * gets a text of its own, and the key work index follows.
 */
class OutOfSyncFilesLoadTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  private static final String NOTES_FILE  = "Notes.xml",
                              HUBS_FILE   = "Hubs.xml",
                              SHARED_HTML = "<html><head></head><body>Shared</body></html>";

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
    db.closeAndOpen();
  }

//---------------------------------------------------------------------------

  private static HDT_Work work(String title)
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName(title);
    return work;
  }

//---------------------------------------------------------------------------

  private static void addKeyWork(HDT_RecordWithMainText record, HDT_RecordWithAuthors<? extends RecordAuthors> work)
  {
    List<KeyWork> keyWorks = record.getMainText().getKeyWorksCopy();
    keyWorks.add(new KeyWork(work));
    record.getMainText().setKeyWorksFromList(keyWorks);
  }

//---------------------------------------------------------------------------

  private static boolean hasKeyWork(HDT_RecordWithMainText record, HDT_Record work)
  {
    return record.keyWorksStream().anyMatch(keyWork -> keyWork.getRecord() == work);
  }

//---------------------------------------------------------------------------

  private static boolean indexed(HDT_RecordWithAuthors<? extends RecordAuthors> work, HDT_RecordWithMainText record)
  {
    return db.keyWorkMentionerStream(work, false).anyMatch(mentioner -> mentioner == record);
  }

//---------------------------------------------------------------------------

  /**
   * The element in Hubs.xml that lists a record as a member of its hub
   */
  private static String spokeElement(HDT_RecordWithMainText record)
  {
    return "<spoke type=\"" + Tag.getTypeTagStr(record.getType()) + "\" id=\"" + record.getID() + "\">";
  }

//---------------------------------------------------------------------------

  /**
   * The element in a member's own file that names its hub
   */
  private static String hubElement(HDT_Hub hub)
  {
    return "<hub id=\"" + hub.getID() + "\">";
  }

//---------------------------------------------------------------------------

  private static String withoutLinesContaining(String xml, String needle)
  {
    return Arrays.stream(xml.split("\\R")).filter(line -> line.contains(needle) == false).collect(Collectors.joining(System.lineSeparator()));
  }

//---------------------------------------------------------------------------

  /**
   * The text of one record file as a save would write it now
   */
  private static String currentXml(String fileName) throws Exception
  {
    AbstractHyperDB.RecordsXML xml = db.buildRecordsXML(null);
    return xml.contents().get(xml.fileNames().indexOf(fileName)).toString();
  }

//---------------------------------------------------------------------------

  /**
   * The note's file names the hub, but the hub's file, an older copy, does not list the note. The note
   * was sharing the hub's text without being a member, and so without being registered for its key works.
   */
  @Test
  void aRecordNamingAHubThatDoesNotListItGetsATextOfItsOwn() throws Exception
  {
    HDT_Debate    debate    = db.createNewBlankRecord(hdtDebate);
    HDT_Note      note      = db.createNewBlankRecord(hdtNote);
    HDT_WorkLabel label     = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Position  displayed = db.createNewBlankRecord(hdtPosition);
    HDT_Work      work = work("Key work");

    HDT_Hub.uniteRecords(debate, note , SHARED_HTML);
    HDT_Hub.uniteRecords(debate, label, SHARED_HTML);
    addKeyWork(debate, work);

    List<DisplayItem> displayItems = debate.getMainText().getDisplayItemsCopy();
    displayItems.add(new DisplayItem(displayed));
    debate.getMainText().setDisplayItemsFromList(displayItems);

    int debateID = debate.getID(), noteID = note.getID(), labelID = label.getID(), displayedID = displayed.getID(), workID = work.getID();
    String noteSpoke = spokeElement(note);

    db.saveAndReopen((fileName, xml) -> fileName.equals(HUBS_FILE) ? withoutLinesContaining(xml, noteSpoke) : xml);

    HDT_Debate    loadedDebate    = db.debates   .getByID(debateID);
    HDT_Note      loadedNote      = db.notes     .getByID(noteID);
    HDT_WorkLabel loadedLabel     = db.workLabels.getByID(labelID);
    HDT_Position  loadedDisplayed = db.positions .getByID(displayedID);
    HDT_Work      loadedWork      = db.works     .getByID(workID);

    assertEquals(2, loadedDebate.getHub().getSpokes().count(), "The hub keeps the members it lists");
    assertSame(loadedLabel, loadedDebate.getHub().getLabel());

    assertFalse(loadedNote.hasHub(), "The note is not a member");
    assertNotSame(loadedDebate.getMainText(), loadedNote.getMainText(), "and has a text of its own");
    assertSame(loadedNote, loadedNote.getMainText().getRecord());
    assertEquals("Shared", loadedNote.getMainText().getPlain().strip(), "copied from the one it was sharing");
    assertTrue(hasKeyWork(loadedNote, loadedWork));

    assertTrue(indexed(loadedWork, loadedNote), "The note is registered for the key work it shows");
    assertTrue(indexed(loadedWork, loadedDebate) && indexed(loadedWork, loadedLabel), "as the members still are");
    assertTrue(db.displayerStream(loadedDisplayed).anyMatch(displayer -> displayer == loadedNote), "and for the display item");

    HDT_Work other = work("Other");
    addKeyWork(loadedNote, other);

    assertTrue(indexed(other, loadedNote), "A key work added to the note afterwards is the note's alone");
    assertFalse(hasKeyWork(loadedDebate, other) || indexed(other, loadedDebate));
  }

//---------------------------------------------------------------------------

  /**
   * The same, where the hub's file lists only one other record: the hub dissolves at load, and the note
   * must not be left holding the dissolved hub's text.
   */
  @Test
  void aRecordNamingAHubLeftWithOneListedMemberGetsATextOfItsOwnBeforeTheHubDissolves() throws Exception
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    HDT_Note   note   = db.createNewBlankRecord(hdtNote);
    HDT_Work   work   = work("Key work");

    HDT_Hub.uniteRecords(debate, note, SHARED_HTML);
    addKeyWork(debate, work);

    int debateID = debate.getID(), noteID = note.getID(), workID = work.getID();
    String noteSpoke = spokeElement(note);

    db.saveAndReopen((fileName, xml) -> fileName.equals(HUBS_FILE) ? withoutLinesContaining(xml, noteSpoke) : xml);

    HDT_Debate loadedDebate = db.debates.getByID(debateID);
    HDT_Note   loadedNote   = db.notes  .getByID(noteID);
    HDT_Work   loadedWork   = db.works  .getByID(workID);

    assertTrue(db.hubs.isEmpty(), "The hub dissolved");
    assertFalse(loadedDebate.hasHub() || loadedNote.hasHub());
    assertNotSame(loadedDebate.getMainText(), loadedNote.getMainText());
    assertSame(loadedDebate, loadedDebate.getMainText().getRecord(), "Each record owns its text");
    assertSame(loadedNote, loadedNote.getMainText().getRecord());
    assertEquals("Shared", loadedNote.getMainText().getPlain().strip());
    assertTrue(hasKeyWork(loadedDebate, loadedWork) && hasKeyWork(loadedNote, loadedWork));
    assertTrue(indexed(loadedWork, loadedDebate) && indexed(loadedWork, loadedNote));

    HDT_Work other = work("Other");
    addKeyWork(loadedNote, other);

    assertEquals(List.of(loadedNote), db.keyWorkMentionerStream(other, true).toList(), "A key work added to the note afterwards is registered for the note, not for the dissolved hub");
  }

//---------------------------------------------------------------------------

  /**
   * The hub's file lists the note, but the note's file, an older copy, does not name the hub and holds a
   * text of the note's own with its own key work. The hub registered the note for the shared key works
   * when it came online. The note is not a member, and keeps only the registration for its own key work.
   */
  @Test
  void aHubListingARecordWhoseOwnFileDoesNotNameItDropsTheRecord() throws Exception
  {
    HDT_Debate    debate  = db.createNewBlankRecord(hdtDebate);
    HDT_Note      note    = db.createNewBlankRecord(hdtNote);
    HDT_WorkLabel label   = db.createNewBlankRecord(hdtWorkLabel);
    HDT_Work      ownWork = work("The note's own key work"), sharedWork = work("Shared key work");

    note.getMainText().setHtml("<html><head></head><body>Own</body></html>");
    addKeyWork(note, ownWork);

    String olderNotesXml = currentXml(NOTES_FILE);

    HDT_Hub.uniteRecords(debate, note , SHARED_HTML);
    HDT_Hub.uniteRecords(debate, label, SHARED_HTML);
    addKeyWork(debate, sharedWork);

    int debateID = debate.getID(), noteID = note.getID(), labelID = label.getID(), ownWorkID = ownWork.getID(), sharedWorkID = sharedWork.getID();

    db.saveAndReopen((fileName, xml) -> fileName.equals(NOTES_FILE) ? olderNotesXml : xml);

    HDT_Debate    loadedDebate     = db.debates   .getByID(debateID);
    HDT_Note      loadedNote       = db.notes     .getByID(noteID);
    HDT_WorkLabel loadedLabel      = db.workLabels.getByID(labelID);
    HDT_Work      loadedOwnWork    = db.works     .getByID(ownWorkID),
                  loadedSharedWork = db.works     .getByID(sharedWorkID);

    assertEquals(2, loadedDebate.getHub().getSpokes().count(), "The hub keeps the members whose files name it");
    assertNull(loadedDebate.getHub().getNote());
    assertFalse(loadedNote.hasHub());

    assertEquals("Own", loadedNote.getMainText().getPlain().strip(), "The note keeps the text of its own file");
    assertTrue  (hasKeyWork(loadedNote, loadedOwnWork) && (hasKeyWork(loadedNote, loadedSharedWork) == false));
    assertTrue  (indexed(loadedOwnWork, loadedNote), "and the registration for its own key work");
    assertFalse (indexed(loadedSharedWork, loadedNote), "but not the one for the shared key work");
    assertTrue  (indexed(loadedSharedWork, loadedDebate) && indexed(loadedSharedWork, loadedLabel));

    HDT_Work other = work("Other");
    addKeyWork(loadedDebate, other);

    assertTrue(indexed(other, loadedLabel) && (indexed(other, loadedNote) == false), "A key work added to the unity afterwards does not reach the note");
  }

//---------------------------------------------------------------------------

  /**
   * The same, where the hub's file lists only one other record: the hub dissolves, that record gets the
   * shared text as its own, and the note is as its own file has it.
   */
  @Test
  void aHubListingARecordWhoseOwnFileDoesNotNameItDissolvesWhenOneMemberIsLeft() throws Exception
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate);
    HDT_Note   note   = db.createNewBlankRecord(hdtNote);
    HDT_Work   work   = work("Key work");

    HDT_Hub.uniteRecords(debate, note, SHARED_HTML);
    addKeyWork(debate, work);

    int debateID = debate.getID(), noteID = note.getID(), workID = work.getID();
    String noteHub = hubElement(debate.getHub());

    db.saveAndReopen((fileName, xml) -> fileName.equals(NOTES_FILE) ? withoutLinesContaining(xml, noteHub) : xml);

    HDT_Debate loadedDebate = db.debates.getByID(debateID);
    HDT_Note   loadedNote   = db.notes  .getByID(noteID);
    HDT_Work   loadedWork   = db.works  .getByID(workID);

    assertTrue  (db.hubs.isEmpty(), "The hub dissolved");
    assertFalse (loadedDebate.hasHub() || loadedNote.hasHub());
    assertEquals("Shared", loadedDebate.getMainText().getPlain().strip(), "The listed member keeps the shared text as its own");
    assertTrue  (hasKeyWork(loadedDebate, loadedWork) && indexed(loadedWork, loadedDebate));
    assertTrue  (loadedNote.getMainText().getPlain().isBlank() && loadedNote.keyWorksUnmod().isEmpty(), "The note has what its own file holds, which is nothing");
    assertFalse (indexed(loadedWork, loadedNote));
  }

//---------------------------------------------------------------------------

  /**
   * The note's file names a key work by an ID that no record has, as a Works.xml from before the work
   * was added leaves it, or a saved copy from before the work was deleted. The key work is left out
   * instead of being registered under a missing record.
   */
  @Test
  void aKeyWorkNamingNoRecordIsLeftOut() throws Exception
  {
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_Work work = work("Key work"), kept = work("Kept key work");

    addKeyWork(note, work);
    addKeyWork(note, kept);

    int noteID = note.getID(), keptID = kept.getID();
    String keyWorkElement = "<key_work type=\"" + Tag.getTypeTagStr(hdtWork) + "\" id=\"" + work.getID() + "\">",
           danglingElement = "<key_work type=\"" + Tag.getTypeTagStr(hdtWork) + "\" id=\"99999\">";

    db.saveAndReopen((fileName, xml) -> fileName.equals(NOTES_FILE) ? xml.replace(keyWorkElement, danglingElement) : xml);

    HDT_Note loadedNote = db.notes.getByID(noteID);
    HDT_Work loadedKept = db.works.getByID(keptID);

    assertEquals(List.of(loadedKept), loadedNote.keyWorksStream().map(KeyWork::getRecord).toList(), "Only the key work whose record exists is loaded");
    assertTrue(indexed(loadedKept, loadedNote));
    assertTrue(db.keyWorkMentionerStream(null, false).findAny().isEmpty(), "Nothing is registered under a missing key work record");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
