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

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * "Revert to XML version" restores a record from its saved copy, which names the records it
 * points to by ID. These tests pin what keeps those IDs meaning the same records (an ID freed by a
 * deletion or an ID change is not reused until the next save, and a save replaces the saved copies
 * only once it can no longer be cancelled) and the refusal to revert a united record whose unity
 * has changed since the save.
 */
class RevertToXmlVersionTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

  private static final String HTML_AS_SAVED = "<html><head></head><body>As saved</body></html>",
                              HTML_EDITED   = "<html><head></head><body>Edited</body></html>";

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
    db.closeAndOpen();  // So that each test's IDs are counted from a fresh session, with nothing freed yet
  }

//---------------------------------------------------------------------------

  /**
   * Does to the records what a save does, without writing any file: builds the records XML, then
   * commits, as a save does once it can no longer be cancelled.
   */
  private static void save() throws Exception
  {
    db.commitSavedCopies(db.buildRecordsXML(null));
  }

//---------------------------------------------------------------------------

  private static HDT_Work work(String title)
  {
    HDT_Work work = db.createNewBlankRecord(hdtWork);
    work.setName(title);
    return work;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aRevertDoesNotPointAtARecordCreatedAfterTheOneItPointedToWasDeleted() throws Exception
  {
    HDT_Work book = work("Book"), chapter = work("Chapter");

    chapter.largerWork.set(book);

    save();

    int bookID = book.getID();

    db.deleteRecord(book);

    HDT_Work newcomer = work("Newcomer");

    assertNotEquals(bookID, newcomer.getID(), "The deleted book's ID is held until the next save");

    chapter.revertToXmlVersion();

    assertNull(chapter.largerWork.get(), "The larger work in the saved copy was deleted, so the revert drops it");
    assertTrue(newcomer.subWorks.isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void aDeletedRecordsIDIsAvailableAgainAfterTheNextSave() throws Exception
  {
    HDT_Work first = work("First");
    int firstID = first.getID();

    save();

    db.deleteRecord(first);

    assertTrue (db.idFreedSinceSave(hdtWork, firstID));
    assertFalse(db.idAvailable     (hdtWork, firstID));
    assertNotEquals(firstID, work("Second").getID());

    save();

    assertFalse(db.idFreedSinceSave(hdtWork, firstID));
    assertTrue (db.idAvailable     (hdtWork, firstID));
    assertEquals(firstID, work("Third").getID(), "Once no saved copy can name it, the lowest free ID is reused as before");
  }

//---------------------------------------------------------------------------

  @Test
  void anIDChangeHoldsTheOldIDUntilTheNextSave() throws Exception
  {
    HDT_Debate debate = db.createNewBlankRecord(hdtDebate), other = db.createNewBlankRecord(hdtDebate);
    HDT_Position position = db.createNewBlankRecord(hdtPosition);

    position.largerDebates.add(debate);

    save();

    int oldID = debate.getID();

    db.changeRecordID(debate, db.getNextID(hdtDebate));

    assertTrue(db.idFreedSinceSave(hdtDebate, oldID));
    assertNotEquals(oldID, db.createNewBlankRecord(hdtDebate).getID());
    assertThrows(HDB_InternalError.class, () -> db.changeRecordID(other, oldID), "Nor can another record be given the old ID");

    position.revertToXmlVersion();

    assertTrue(position.largerDebates.isEmpty(), "Until a save, the saved copy names the debate by its old ID, which no record has; hence Change ID saves");

    position.largerDebates.add(debate);

    save();

    position.revertToXmlVersion();

    assertTrue(position.largerDebates.contains(debate), "After the save, the saved copy names the debate by its new ID");
  }

//---------------------------------------------------------------------------

  @Test
  void aSaveStoppedBeforeItsPointOfNoReturnChangesNoSavedCopy() throws Exception
  {
    HDT_Work work = work("As saved"), deleted = work("Deleted");

    save();

    int deletedID = deleted.getID();

    db.deleteRecord(deleted);
    work.setName("Edited");

    HDT_Work created = work("Created");

    db.buildRecordsXML(null);  // What a save has done when a cancel can still stop it

    assertFalse(created.hasStoredState(), "A record created since the last save still has no saved copy");
    assertTrue(db.idFreedSinceSave(hdtWork, deletedID), "The deleted record's ID is still held");

    work.revertToXmlVersion();

    assertEquals("As saved", work.name(), "The saved copy is still the one from the last save");

    save();

    assertTrue(created.hasStoredState());
    assertFalse(db.idFreedSinceSave(hdtWork, deletedID));
  }

//---------------------------------------------------------------------------

  @Test
  void revertingAUnitedRecordAlsoRevertsTheTextItShares() throws Exception
  {
    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    HDT_Note note = db.createNewBlankRecord(hdtNote);

    HDT_Hub.uniteRecords(position, note, HTML_AS_SAVED);

    save();

    note.getMainText().setHtml(HTML_EDITED);

    position.revertToXmlVersion();

    assertEquals(HTML_AS_SAVED, position.getMainText().getHtml());
    assertEquals(HTML_AS_SAVED, note    .getMainText().getHtml(), "The text is the unity's, so the note's edit is reverted too");
  }

//---------------------------------------------------------------------------

  @Test
  void aRevertIsRefusedWhenARecordHasJoinedTheUnitySinceTheSave() throws Exception
  {
    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);

    HDT_Hub.uniteRecords(position, note, HTML_AS_SAVED);

    save();

    HDT_Hub.uniteRecords(label, note, HTML_EDITED);

    assertThrows(HubSpokesChangedException.class, position::revertToXmlVersion);

    HDT_Hub hub = position.getHub();

    assertEquals(3, hub.getSpokes().count(), "A refused revert changes nothing");
    assertSame(hub, label.getHub());
    assertEquals(HTML_EDITED, position.getMainText().getHtml());
  }

//---------------------------------------------------------------------------

  @Test
  void aRevertIsRefusedWhenARecordHasLeftTheUnitySinceTheSave() throws Exception
  {
    HDT_Position position = db.createNewBlankRecord(hdtPosition);
    HDT_Note note = db.createNewBlankRecord(hdtNote);
    HDT_WorkLabel label = db.createNewBlankRecord(hdtWorkLabel);

    HDT_Hub.uniteRecords(position, note, HTML_AS_SAVED);
    HDT_Hub.uniteRecords(label, note, HTML_AS_SAVED);

    save();

    HDT_Hub hub = position.getHub();

    assertTrue(hub.disuniteRecord(hdtWorkLabel));

    assertThrows(HubSpokesChangedException.class, position::revertToXmlVersion);

    assertNull(hub.getLabel(), "A refused revert does not put the label back in the unity");
    assertFalse(label.hasHub());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
