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

package org.hypernomicon.bib;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * The reference manager library file is written first in a save, and a failure to write it
 * must fail the whole save: the records files written after it would otherwise link works
 * to entries that were never saved, and the integrity manifest would certify that set.
 */
class LibraryWrapperSaveTest
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

  @AfterAll
  static void tearDownOnce()
  {
    TestHyperDB.closeIfOpen();  // Leave neither the linked library nor the temporary root for the next test class
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reopens the database with the given folder as its root and links an offline Zotero library,
   * which is how a newly linked database looks before its first save: no library file and no
   * library preferences.
   */
  private static ZoteroWrapper reopenAndLink(Path root) throws HyperDataException
  {
    db.closeAndOpen(FilePath.of(root));

    assertTrue(db.isOnline(), "Reopen failed");
    assertEquals("", db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""), "The template must not name a library");

    return db.linkBibLibrary(LibraryType.ltZotero, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void aWriteFailureFailsTheSaveAndRecordsNothing(@TempDir Path root) throws Exception
  {
    ZoteroWrapper library = reopenAndLink(root);  // The XML subfolder does not exist, so the file cannot be written

    HyperDataException e = assertThrows(HyperDataException.class, () -> library.saveAllToPersistentStorage(null));

    assertTrue(e.getMessage().contains("saving bibliographic data"), e.getMessage());
    assertNotNull(e.getCause(), "The failure must carry the I/O exception");

    assertFalse(db.xmlPath().exists(), "A failed save must create nothing");
    assertNull(db.getBibChecksum(), "No checksum may be recorded for a file that was not written");
    assertEquals("", db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""), "No library preferences may be recorded for a file that was not written");
  }

//---------------------------------------------------------------------------

  @Test
  void aSuccessfulWriteRecordsTheChecksumAndThePreferences(@TempDir Path root) throws Exception
  {
    ZoteroWrapper library = reopenAndLink(root);

    db.xmlPath().createDirectories();

    assertDoesNotThrow(() -> library.saveAllToPersistentStorage(null));

    FilePath bibFilePath = db.xmlPath(BIB_FILE_NAME);

    assertTrue(bibFilePath.exists());
    assertFalse(db.xmlPath(BIB_FILE_NAME + ".tmp").exists(), "The temporary file must have been renamed to the final name");

    try (InputStream is = Files.newInputStream(bibFilePath.toPath()))
    {
      assertEquals(md5Hex(is), db.getBibChecksum(), "The recorded checksum must be the written file's");
    }

    assertEquals(LibraryType.ltZotero.descriptor, db.prefs.get(PrefKey.BIB_LIBRARY_TYPE, ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
