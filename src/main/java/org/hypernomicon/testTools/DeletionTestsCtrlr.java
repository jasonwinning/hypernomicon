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

package org.hypernomicon.testTools;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;

import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.model.records.*;
import org.hypernomicon.testTools.TestConsoleDlgCtrlr.TestConsoleTab;
import org.hypernomicon.util.file.FilePath;

import javafx.fxml.FXML;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

/**
 * The Test Console's Deletion tab: mass record deletion, the folder-deletion
 * bypass comparison, and the FileDeletion API suite, all run against the
 * transient test database (see {@link TestConsoleDlgCtrlr#requireTransientDBLoaded}).
 */
public class DeletionTestsCtrlr implements TestConsoleTab
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnNukeTest, btnFolderBypassTest;
  @FXML private CheckBox chkFolderBypass, chkWatcherEvents;

  private TestConsoleDlgCtrlr console;

//---------------------------------------------------------------------------

  @Override public void init(TestConsoleDlgCtrlr console)
  {
    this.console = console;

    enableAllIff(db.isOnline(), btnNukeTest, btnFolderBypassTest);

    chkFolderBypass.setSelected(db.folderDeletionBypassEnabled);
    chkFolderBypass.selectedProperty().addListener((ob, oldVal, newVal) -> db.folderDeletionBypassEnabled = newVal);

    chkWatcherEvents.setSelected(FolderTreeWatcher.consoleLogging);
    chkWatcherEvents.selectedProperty().addListener((ob, oldVal, newVal) -> FolderTreeWatcher.consoleLogging = newVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void nukeTest()
  {
    if (console.requireTransientDBLoaded() == false) return;

    if (confirmDialog("This will delete most of the records in the entire database. Proceed?", false) == false)
      return;

    db.recordDeletionTestInProgress = true;

    Random random = new Random();

    EnumSet<RecordType> types = EnumSet.allOf(RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub, hdtFolder));  // Folders deleted last
    List<RecordType> typeList = List.copyOf(types);

    int deleteCtr = 0;

    while (types.stream().anyMatch(recordType -> (nextRecordToDelete(recordType) > 0)))
    {
      RecordType randomType;
      int randomID;

      do
      {
        randomType = typeList.get(random.nextInt(typeList.size()));

        randomID = db.records(randomType).getRandomUsedID(random);
      }
      while (randomID < 1);

      HDT_Record record = db.records(randomType).getByID(randomID);

      boolean doDelete = (HDT_Record.isEmpty(record, false) == false) && (db.isProtectedRecord(record, true) == false);

      // Glossary should only be deleted if it has no concepts
      if (doDelete && (randomType == hdtGlossary))
      {
        HDT_Glossary glossary = (HDT_Glossary) record;

        if (glossary.concepts.isEmpty() == false)
          doDelete = false;
      }

      if (doDelete)
      {
        db.deleteRecord(record);
        deleteCtr++;

        if ((deleteCtr % 100) == 0)
          System.out.println("Records deleted: " + deleteCtr);
      }
    }

    System.out.println("Non-folder records deleted: " + deleteCtr);

    // Delete folders last so the bypass preconditions hold (no non-folder records pointing to folders).

    deleteNonProtectedFolders(deleteCtr, "Records");

    System.out.println("Record deletion complete.");

    db.recordDeletionTestInProgress = false;
    db.rebuildMentions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int nextRecordToDelete(RecordType recordType)
  {
    return db.records(recordType).stream().filter(record -> (HDT_Record.isEmpty(record, false) == false))
                                          .filter(record -> (db.isProtectedRecord(record, true) == false))
                                          .map(HDT_Record::getID)
                                          .findFirst()
                                          .orElse(-1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Delete all non-protected folders, sorted deepest first so children are deleted before parents.
   * @param deleteCount number of records already deleted, used for progress logging
   * @param recordLabel label to use in progress messages (e.g. "Records" or "Folders")
   * @return the updated total delete count (deleteCount + number of folders deleted)
   */
  private static int deleteNonProtectedFolders(int deleteCount, String recordLabel)
  {
    List<HDT_Folder> foldersToDelete = db.folders.stream()
      .filter(folder -> HDT_Record.isEmpty(folder, false) == false)
      .filter(folder -> db.isProtectedRecord(folder, true) == false)
      .sorted(Comparator.comparingInt(DeletionTestsCtrlr::folderDepth).reversed())
      .toList();

    for (HDT_Folder folder : foldersToDelete)
    {
      if (folder.isExpired()) continue;

      db.deleteRecord(folder);
      deleteCount++;

      if ((deleteCount % 100) == 0)
        System.out.println(recordLabel + " deleted: " + deleteCount);
    }

    return deleteCount;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int folderDepth(HDT_Folder folder)
  {
    int depth = 0;

    while ((folder = folder.parentFolder()) != null)
      depth++;

    return depth;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Test to verify that folder deletion bypass produces identical results to the non-bypass path.
   * <p>
   * This test should be run on a copy of a real database. It:
   * <ol>
   *   <li>Loops through all HDT_Folder records</li>
   *   <li>For non-protected folders, severs links to HDT_WorkFile, HDT_MiscFile, HDT_Note, and HDT_Person records</li>
   *   <li>Loops again and deletes all non-protected folders</li>
   *   <li>Saves the database to XML</li>
   * </ol>
   * To verify correctness, run this test twice: once with bypass enabled and once disabled,
   * then diff the resulting XML files.
   */
  @FXML private void folderBypassTest()
  {
    if (console.requireTransientDBLoaded() == false) return;

    if (confirmDialog("This will sever folder links and delete non-protected folders. Proceed?", false) == false)
      return;

    db.recordDeletionTestInProgress = true;
    db.runningConversion = true;

    System.out.println("=== Folder Bypass Test: Severing non-folder links ===");

    // First pass: sever links from non-folder records to non-protected folders

    int severedCount = 0;

    for (HDT_Folder folder : List.copyOf(db.folders))
    {
      if (db.isProtectedRecord(folder, true))
        continue;

      // Sever links from HDT_WorkFile records

      for (HDT_WorkFile workFile : List.copyOf(db.<HDT_Folder, HDT_WorkFile>getSubjectList(rtFolderOfWorkFile, folder)))
      {
        workFile.getPath().clear(false);
        severedCount++;
      }

      // Sever links from HDT_MiscFile records

      for (HDT_MiscFile miscFile : List.copyOf(db.<HDT_Folder, HDT_MiscFile>getSubjectList(rtFolderOfMiscFile, folder)))
      {
        miscFile.getPath().clear(false);
        severedCount++;
      }

      // Sever links from HDT_Note records

      for (HDT_Note note : List.copyOf(db.<HDT_Folder, HDT_Note>getSubjectList(rtFolderOfNote, folder)))
      {
        note.folder.setID(-1);
        severedCount++;
      }

      // Sever links from HDT_Person picture folder

      for (HDT_Person person : List.copyOf(db.<HDT_Folder, HDT_Person>getSubjectList(rtPictureFolderOfPerson, folder)))
      {
        person.getPath().clear(false);
        severedCount++;
      }
    }

    System.out.println("Severed " + severedCount + " links.");
    System.out.println("=== Folder Bypass Test: Deleting non-protected folders ===");

    // Second pass: delete non-protected folders (deepest first so children are deleted before parents)

    int deleteCount = deleteNonProtectedFolders(0, "Folders");

    System.out.println("Deleted " + deleteCount + " folders total.");

    db.recordDeletionTestInProgress = false;
    db.runningConversion = false;
    db.rebuildMentions();

    System.out.println("=== Folder Bypass Test: Complete. ===");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void fileDeletionTest()
  {
    if (console.requireTransientDBLoaded() == false) return;

    FileDeletionTestRunner.runTests(db.getRootPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void copyForNukeTest()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    FilePath transientDBFilePath = console.getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath))
    {
      errorPopup("Transient DB folder path needs to be entered.");
      return;
    }

    if (db.getRootPath().equals(transientDBFilePath))
    {
      errorPopup("Transient DB is currently loaded.");
      return;
    }

    if (console.clearTransientDB() == false)
      return;

    try
    {
      db.getHdbPath().copyTo(transientDBFilePath.resolve(db.getHdbPath().getNameOnly()) , false);

      FileUtils.copyDirectory(db.xmlPath().toFile(), transientDBFilePath.resolve(DEFAULT_XML_PATH).toFile());

      Path srcRoot = db.getRootPath().toPath(),
           dstRoot = transientDBFilePath.toPath();

      try (Stream<Path> dirs = Files.walk(srcRoot))
      {
        dirs.filter(Files::isDirectory).forEach(srcDir ->
        {
          try { Files.createDirectories(dstRoot.resolve(srcRoot.relativize(srcDir))); }
          catch (IOException e) { throw new UncheckedIOException(e); }
        });
      }

      infoPopup("Database copied successfully.");
    }
    catch (IOException | UncheckedIOException e)
    {
      errorPopup("Error while copying: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
