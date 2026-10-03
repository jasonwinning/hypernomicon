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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.PopupRobot;

import org.junit.jupiter.api.*;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * A work linked to a reference manager entry that the library file does not contain is a sign
 * of an inconsistent set of files. Loading asks before removing such links, because restoring
 * a copy of the library file that has the entries keeps them.
 */
class MissingBibEntryLoadTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String MISSING_ENTRY_KEY = "ABCD1234",
                              WORK_TITLE        = "Work linked to a missing entry";

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

    if (db.isOffline())
      db = TestHyperDB.instance();  // The previous test aborted a load
    else
      db.closeAndOpen();            // Each test starts from the template, with every record ID free
  }

//---------------------------------------------------------------------------

  @AfterEach
  void clearLoadFilters()
  {
    db.setRecordsLoadFilter(null, null);
    db.setSettingsLoadFilter(null);
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither the injected work nor the linked library for the next test class
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reopens the database through the real loader with its settings naming a Zotero library,
   * which the test database links offline, and optionally with a work linked to an entry
   * that no library file has.
   */
  private static void reloadLinked(boolean withDanglingWork) throws Exception
  {
    db.setSettingsLoadFilter(settingsXml -> settingsXml.replace("<entry key=\"" + PrefKey.SETTINGS_VERSION + '"',
      "<entry key=\"" + PrefKey.BIB_LIBRARY_TYPE + "\" value=\"" + LibraryType.ltZotero.descriptor + "\"/>" +
      "<entry key=\"" + PrefKey.BIB_USER_ID      + "\" value=\"1\"/>" +
      "<entry key=\"" + PrefKey.SETTINGS_VERSION + '"'));

    if (withDanglingWork)
    {
      HDT_Work work = db.createNewBlankRecord(hdtWork);
      work.setName(WORK_TITLE);
      work.setBibEntryKey(MISSING_ENTRY_KEY);

      StringBuilder xml = new StringBuilder();
      work.saveToStoredState();
      work.writeStoredStateToXML(xml);

      String recordXml = xml.toString();
      assertTrue(recordXml.contains(MISSING_ENTRY_KEY), "The key must be in the record as the save writes it");

      db.setRecordsLoadFilter("Works.xml", worksXml -> worksXml.replace("</records>", recordXml + "</records>"));
    }

    db.closeAndOpen();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void worksLinkedToMissingEntriesAreUnlinkedOnlyAfterConfirmation() throws Exception
  {
    PopupRobot.setDefaultResponse(mrYes);  // Continue past the integrity-checksum prompt (the works file changed) and then the unlink prompt

    reloadLinked(true);

    assertTrue(db.isOnline(), () -> "Reload failed: " + PopupRobot.getLastMessage());
    assertTrue(db.bibLibraryIsLinked());

    assertEquals(2, PopupRobot.getInvocationCount(), "the integrity-checksum prompt, then the unlink prompt");
    assertEquals(AlertType.CONFIRMATION, PopupRobot.getLastType());

    String msg = PopupRobot.getLastMessage();

    assertTrue(msg.contains("A work record is linked to a Zotero entry that is not present in " + BIB_FILE_NAME), msg);
    assertTrue(msg.contains("1: " + WORK_TITLE), msg);
    assertTrue(msg.contains("Continue loading?"), msg);

    HDT_Work work = db.works.getByID(1);

    assertEquals(WORK_TITLE, work.name());
    assertEquals("", work.getBibEntryKey(), "The link must be removed once the user continues");
    assertNull(db.getWorkByBibEntryKey(MISSING_ENTRY_KEY));
  }

//---------------------------------------------------------------------------

  @Test
  void abortingAtThePromptLeavesTheDatabaseClosed() throws Exception
  {
    PopupRobot.enqueueResponses(mrYes, mrNo);  // Continue past the integrity-checksum prompt, abort at the unlink prompt

    reloadLinked(true);

    assertTrue(db.isOffline(), "Aborting must leave the database closed");
    assertEquals(2, PopupRobot.getInvocationCount(), "the integrity-checksum prompt, then the unlink prompt");
    assertTrue(PopupRobot.getLastMessage().contains(WORK_TITLE), PopupRobot.getLastMessage());
  }

//---------------------------------------------------------------------------

  @Test
  void aLinkedLibraryWithNothingToUnlinkLoadsWithoutAPrompt() throws Exception
  {
    reloadLinked(false);

    assertTrue(db.isOnline(), () -> "Reload failed: " + PopupRobot.getLastMessage());
    assertTrue(db.bibLibraryIsLinked());
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
