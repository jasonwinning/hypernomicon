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
import static org.hypernomicon.util.PopupDialog.DialogResult.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.regex.*;

import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.util.VersionNumber;

import org.junit.jupiter.api.*;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * A database whose record files or settings file are in an older format than this build saves gets
 * one warning that saving will upgrade them: after the load rather than from inside it, naming each
 * kind of data, and giving the newest version of Hypernomicon that the saved database will no longer
 * open in.
 */
class UpgradeWarningLoadTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A record file with no records in it, so that dating it exercises nothing but the version check */
  private static final String DATED_RECORDS_FILE = "Notes.xml",
                              OLD_RECORDS_VERSION = "1.10", OLD_SETTINGS_VERSION = "1.5",
                              RECORDS_DATA_NAME = "this XML record data", SETTINGS_DATA_NAME = "the Settings XML file";

  private static final Pattern WARNING_PATTERN = Pattern.compile("When you save changes, (.+) will be upgraded and will no longer be compatible with Hypernomicon v([0-9.]+) or older\\.");

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
    PopupRobot.setDefaultResponse(mrYes);  // Continue past the integrity-checksum prompt that the dated record file raises

    if (db.isOffline())
      db = TestHyperDB.instance();
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
    TestHyperDB.closeIfOpen();  // Leave no session loaded as an existing database for the next test class
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reopens the template as an existing database, with the version of a record file and/or the
   * settings file set to one this build no longer saves.
   */
  private static void reloadAsExisting(boolean oldRecords, boolean oldSettings)
  {
    PopupRobot.clear();

    db.setRecordsLoadFilter(null, null);
    db.setSettingsLoadFilter(null);

    if (oldRecords)
      db.setRecordsLoadFilter(DATED_RECORDS_FILE, xml -> xml.replaceFirst("<records version=\"[^\"]*\"", "<records version=\"" + OLD_RECORDS_VERSION + '"'));

    if (oldSettings)
      db.setSettingsLoadFilter(xml -> xml.replaceFirst("<entry key=\"" + PrefKey.SETTINGS_VERSION + "\" value=\"[^\"]*\"",
                                                       "<entry key=\"" + PrefKey.SETTINGS_VERSION + "\" value=\"" + OLD_SETTINGS_VERSION + '"'));

    db.closeAndOpenAsExisting();

    assertTrue(db.isOnline(), () -> "Reload failed: " + PopupRobot.getLastMessage());
  }

//---------------------------------------------------------------------------

  /**
   * The upgrade warning, which must be the last popup of the load: group 1 names the data, group 2 is
   * the version cutoff.
   */
  private static Matcher upgradeWarning()
  {
    assertEquals(AlertType.WARNING, PopupRobot.getLastType(), PopupRobot.getLastMessage());

    Matcher matcher = WARNING_PATTERN.matcher(PopupRobot.getLastMessage());
    assertTrue(matcher.matches(), PopupRobot.getLastMessage());

    return matcher;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void oldRecordDataIsReportedOnceAfterTheLoad()
  {
    reloadAsExisting(true, false);

    assertEquals(2, PopupRobot.getInvocationCount(), "the integrity-checksum prompt for the changed record file, then the warning");
    assertEquals(RECORDS_DATA_NAME, upgradeWarning().group(1));
  }

//---------------------------------------------------------------------------

  @Test
  void anOldSettingsFileIsReportedAfterTheLoad()
  {
    reloadAsExisting(false, true);

    assertEquals(1, PopupRobot.getInvocationCount(), "only the warning: the settings file is not in the integrity manifest");
    assertEquals(SETTINGS_DATA_NAME, upgradeWarning().group(1));
  }

//---------------------------------------------------------------------------

  @Test
  void bothKindsOfDataShareOneWarningWithTheNewerCutoff()
  {
    reloadAsExisting(true, false);
    VersionNumber recordsCutoff = new VersionNumber(upgradeWarning().group(2));

    reloadAsExisting(false, true);
    VersionNumber settingsCutoff = new VersionNumber(upgradeWarning().group(2));

    reloadAsExisting(true, true);

    assertEquals(2, PopupRobot.getInvocationCount(), "the integrity-checksum prompt, then one warning for both");

    Matcher matcher = upgradeWarning();

    assertEquals(RECORDS_DATA_NAME + " and " + SETTINGS_DATA_NAME, matcher.group(1));
    assertEquals(Collections.max(List.of(recordsCutoff, settingsCutoff)), new VersionNumber(matcher.group(2)),
                 "The database as a whole stops opening in the newer of the two cutoffs");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
