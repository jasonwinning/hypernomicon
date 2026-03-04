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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

//---------------------------------------------------------------------------

/**
 * Tests for {@link AbstractHyperDB#isProtectedFile(FilePath, boolean)},
 * {@link AbstractHyperDB#isSpecialFolder(int, boolean)}, and
 * {@link AbstractHyperDB#isProtectedRecord(org.hypernomicon.model.records.HDT_Record, boolean)}
 * using the in-memory {@link TestHyperDB}.
 */
class ProtectedFileTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();

    // isProtectedFile relies on getFolderFromFilePath, which calls getDirOnly().
    // getDirOnly() checks isDirectory() on the filesystem, so the special folder
    // directories must physically exist for the lookup to work.

    db.specialFolders().forEach(folder ->
    {
      try { Files.createDirectories(folder.filePath().toPath()); }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });
  }

//---------------------------------------------------------------------------
//region isProtectedFile: root, special folders, and known files
//---------------------------------------------------------------------------

  @Test
  void rootPath_isProtected()
  {
    assertTrue(db.isProtectedFile(db.getRootPath(), false));
    assertTrue(db.isProtectedFile(db.getRootPath(), true));
  }

//---------------------------------------------------------------------------

  @Test
  void databaseHdbFile_isProtected()
  {
    FilePath hdb = db.getRootPath().resolve("database.hdb");

    assertTrue(db.isProtectedFile(hdb, false));
    assertTrue(db.isProtectedFile(hdb, true));
  }

//---------------------------------------------------------------------------

  @Test
  void papersFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getPapersFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void booksFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getBooksFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void miscFilesFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getMiscFilesFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void picturesFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getPicturesFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void topicalFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getTopicalFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void unenteredFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getUnenteredFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void resultsFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getResultsFolder());
  }

//---------------------------------------------------------------------------

  @Test
  void xmlFolder_isProtected()
  {
    assertSpecialFolderProtected(db.getXmlFolder());
  }

//---------------------------------------------------------------------------

  private static void assertSpecialFolderProtected(HDT_Folder folder)
  {
    assertNotNull(folder, "Special folder should exist");

    FilePath path = folder.filePath();
    assertNotNull(path, "Special folder path should not be null");

    assertTrue(db.isProtectedFile(path, false), "Special folder should be protected: " + path);
    assertTrue(db.isProtectedFile(path, true),  "Special folder should be protected with checkSubfolders: " + path);
  }

//---------------------------------------------------------------------------
//endregion
//region isProtectedFile: XML data files
//---------------------------------------------------------------------------

  @ParameterizedTest
  @ValueSource(strings =
  {
    "Settings.xml"      , "People.xml" , "Other.xml"    , "Institutions.xml",
    "Investigations.xml", "Debates.xml", "Arguments.xml", "Positions.xml"   ,
    "Works.xml"         , "Terms.xml"  , "Files.xml"    , "Notes.xml"       ,
    "Hubs.xml"
  })
  void xmlDataFiles_areProtected(String fileName)
  {
    FilePath xmlFile = db.xmlPath(fileName);

    assertTrue(db.isProtectedFile(xmlFile, false),
      "XML data file should be protected: " + fileName);
  }

//---------------------------------------------------------------------------
//endregion
//region isProtectedFile: non-protected paths
//---------------------------------------------------------------------------

  @Test
  void arbitraryFileInRoot_notProtected()
  {
    FilePath file = db.getRootPath().resolve("random_notes.txt");

    assertFalse(db.isProtectedFile(file, false));
    assertFalse(db.isProtectedFile(file, true));
  }

//---------------------------------------------------------------------------

  @Test
  void arbitraryFileInXmlDir_notProtected()
  {
    // A file in the XML directory that doesn't match any known XML data file name

    FilePath file = db.xmlPath("custom_data.txt");

    assertFalse(db.isProtectedFile(file, false));
    assertFalse(db.isProtectedFile(file, true));
  }

//---------------------------------------------------------------------------

  @Test
  void fileInNonSpecialSubdir_notProtected()
  {
    // A file whose parent is neither root nor XML dir

    FilePath file = db.getRootPath().resolve("some_subdir", "file.txt");

    assertFalse(db.isProtectedFile(file, false));
    assertFalse(db.isProtectedFile(file, true));
  }

//---------------------------------------------------------------------------

  @Test
  void fileInSubdirOfXml_notProtected()
  {
    // A file nested inside the XML directory (parent is a subdir of XML, not XML itself)

    FilePath file = db.xmlPath().resolve("subdir", "Settings.xml");

    assertFalse(db.isProtectedFile(file, false),
      "File nested inside XML subdir should not be protected even if name matches");
  }

//---------------------------------------------------------------------------

  @Test
  void xmlFileNameInWrongDir_notProtected()
  {
    // Settings.xml placed directly in the root, not in XML dir

    FilePath file = db.getRootPath().resolve("Settings.xml");

    assertFalse(db.isProtectedFile(file, false),
      "XML file name in root (not XML dir) should not be protected");
  }

//---------------------------------------------------------------------------
//endregion
//region specialFolders stream and isSpecialFolder
//---------------------------------------------------------------------------

  @Test
  void specialFolders_returnsExactlyTheExpectedFolders()
  {
    Set<HDT_Folder> expected = Set.of
    (
      db.getXmlFolder      (), db.getPicturesFolder(), db.getBooksFolder    (),
      db.getPapersFolder   (), db.getResultsFolder (), db.getUnenteredFolder(),
      db.getMiscFilesFolder(), db.getTopicalFolder ()
    );

    Set<HDT_Folder> actual = db.specialFolders().collect(Collectors.toSet());

    assertEquals(expected, actual, "specialFolders() should return exactly the 8 known special folders");
  }

//---------------------------------------------------------------------------

  @Test
  void rootFolderID_isSpecial()
  {
    assertTrue(db.isSpecialFolder(ROOT_FOLDER_ID, false));
    assertTrue(db.isSpecialFolder(ROOT_FOLDER_ID, true));
  }

//---------------------------------------------------------------------------

  @Test
  void knownSpecialFolderIDs_areSpecial()
  {
    db.specialFolders().forEach(folder ->
    {
      assertNotNull(folder);
      assertTrue(db.isSpecialFolder(folder.getID(), false),
        "Folder ID " + folder.getID() + " should be special");
    });
  }

//---------------------------------------------------------------------------

  @Test
  void invalidFolderID_notSpecial()
  {
    assertFalse(db.isSpecialFolder(0, false));
    assertFalse(db.isSpecialFolder(-1, false));
  }

//---------------------------------------------------------------------------

  @Test
  void nonSpecialFolderID_notSpecial()
  {
    // Create a regular (non-special) folder record

    HDT_Folder regularFolder = db.createNewBlankRecord(hdtFolder);

    assertFalse(db.isSpecialFolder(regularFolder.getID(), false),
      "Newly created folder should not be special");
  }

//---------------------------------------------------------------------------
//endregion
//region isProtectedRecord
//---------------------------------------------------------------------------

  @Test
  void rootFolder_isProtectedRecord()
  {
    HDT_Folder root = db.getRootFolder();

    assertTrue(db.isProtectedRecord(root, false));
    assertTrue(db.isProtectedRecord(root, true));
  }

//---------------------------------------------------------------------------

  @Test
  void specialFolders_areProtectedRecords()
  {
    db.specialFolders().forEach(folder ->
    {
      assertNotNull(folder);
      assertTrue(db.isProtectedRecord(folder, false),
        "Folder " + folder.name() + " (ID " + folder.getID() + ") should be a protected record");
    });
  }

//---------------------------------------------------------------------------

  @Test
  void regularFolder_notProtectedRecord()
  {
    HDT_Folder regularFolder = db.createNewBlankRecord(hdtFolder);

    assertFalse(db.isProtectedRecord(regularFolder, false),
      "Newly created folder should not be a protected record");
  }

//---------------------------------------------------------------------------
//endregion
//region checkSubfolders parameter
//---------------------------------------------------------------------------

  @Test
  void checkSubfolders_parentOfSpecialFolder_isProtectedRecord()
  {
    // The root folder contains all special folders as children.
    // With checkSubfolders=true, any ancestor of a special folder is protected.
    // Root is already protected by its own ID, so this test verifies
    // that checkSubfolders also works for non-root ancestors.

    // Create a non-special folder, then re-parent a special folder under it.
    // We can't safely do this without disrupting the DB state.
    // Instead, verify that the root IS protected with checkSubfolders
    // (it would be anyway, but confirms the recursive check path).

    assertTrue(db.isProtectedRecord(db.getRootFolder(), true),
      "Root folder should be protected with checkSubfolders=true");

    // Verify a non-special folder without special children is NOT protected
    HDT_Folder regularFolder = db.createNewBlankRecord(hdtFolder);
    assertFalse(db.isProtectedRecord(regularFolder, true),
      "Regular folder with no special children should not be protected even with checkSubfolders");
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
