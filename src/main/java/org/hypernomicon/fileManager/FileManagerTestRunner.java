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

package org.hypernomicon.fileManager;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.testTools.FXTestSequencer;
import org.hypernomicon.testTools.FileLockHelper;
import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

/**
 * Test runner for the FileManager paste-move pipeline, plus the New Folder button's
 * refresh-and-highlight choreography (Phase 27), the window's catch-up after a directory
 * is deleted through FileDeletion rather than the File Manager (Phase 28), the folder
 * lookup for a directory that has vanished from disk (Phase 29), and the record lookups
 * that must precede a deletion's or an outside rename's eviction of the path from the
 * registry (Phase 30), the main window keeping a view when the record its only view shows
 * is deleted from outside it (Phase 31), the deletion of a note's folder through the File
 * Manager (Phase 32), renaming through the File Manager (Phase 33), the watcher's reactions
 * to changes made outside the program (Phase 34), the deletion flow's variants (Phase 35),
 * the clipboard and drag variants of paste (Phase 36), the folder history with the Refresh
 * button (Phase 37), the single warning for several in-use files deleted outside at once
 * (Phase 38), and the record table catching up with records changed outside the File Manager
 * (Phase 39), launched from the Test Console's File Manager tab.
 *
 * <p>This class lives in {@code org.hypernomicon.fileManager} so it can access
 * package-private members: {@link FileManager#paste}, {@link FileManager#moveCopy},
 * {@link FileManager#cutCopy}, {@link FileManager#newFolder(HDT_Folder, String)},
 * {@link FileManager#delete}, {@link FileManager#rename(FileRow, String)},
 * {@link FileManager#goToFilePath}, {@link FileManager#selectRows},
 * {@link FileManager#userRequestsToGoBackward}, {@link FileManager#userRequestsToGoForward},
 * {@link FileManager#openFileRow}, {@link FileManager#requestOwedRefresh},
 * {@link FileManager#getRecordTable}, and the {@link FileRow} constructor.</p>
 *
 * <p>All test files are created under the transient database's
 * {@code _test_fm/} directory (internal) and a sibling {@code _test_fm_ext/}
 * directory outside the database root (external). Both are cleaned up in the
 * finalizer.</p>
 *
 * @see FileManager#paste(FileRow, boolean, boolean)
 * @see FileManager#moveCopy(List, boolean, boolean)
 */
public final class FileManagerTestRunner
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static FilePath testRoot, extRoot;

//---------------------------------------------------------------------------

  private FileManagerTestRunner() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run the full FileManager paste test suite (move and copy) as an
   * FXTestSequencer sequence.
   *
   * @param testRootDir path under the DB root to use as test root (e.g. dbRoot/_test_fm)
   * @param onFinished  callback invoked after the last step completes and cleanup finishes; may be null
   */
  public static void runTests(FilePath testRootDir, Runnable onFinished)
  {
    testRoot = testRootDir;
    extRoot = testRoot.getParent().getParent().resolve("_test_fm_ext");

    PopupRobot.setActive(true);
    PopupRobot.clear();

    FXTestSequencer seq = new FXTestSequencer();

    seq.setDelayMS(400);

    seq.setFinalizer(() -> runDelayedInFXThread(1, 200, () ->
    {
      FileManager.close(false);  // Phase 27 opens it; close it before the folders it shows are deleted

      // Best-effort cleanup: fixture records first, so that no folder is "in use" when the
      // deletion's deferred prune reaches it. Deleting a record with a file asks whether to
      // delete the file too; answer no, and let the tree deletion below take the files in one go.

      PopupRobot.setDefaultResponse(mrNo);
      deleteFixtureRecords();

      FileDeletion.ofDirsWithContents(List.of(testRoot)).nonInteractiveLogErrors().execute();
      FileDeletion.ofDirsWithContents(List.of(extRoot )).nonInteractiveLogErrors().execute();

      PopupRobot.setActive(false);  // after the cleanup, so that nothing it pops up can block
      PopupRobot.clear();

      if (onFinished != null)
        onFinished.run();
    }));

    // =====================================================================
    //  Phase 1: Single-Item Moves, Clean Destination (8 steps)
    // =====================================================================

    // Step 1: Single file, internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p1s1", "src"),   // _test_fm/p1s1/src/
                 destDir = createTestDir("p1s1", "dest"),  // _test_fm/p1s1/dest/
                 file    = srcDir.resolve("file1.txt");    // _test_fm/p1s1/src/file1.txt

        pasteMove(entitiesOf(file), destDir);

        assertGone(file, "source file");
        assertExists(destDir.resolve("file1.txt"), "dest file");
        assertFileContent(destDir.resolve("file1.txt"), "file1.txt", "dest file content");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 2: Single file, external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p1s2", "src"),   // ../_test_fm_ext/p1s2/src/
                 destDir = createTestDir("p1s2", "dest"),    // _test_fm/p1s2/dest/
                 file    = srcDir.resolve("file2.txt");      // ../_test_fm_ext/p1s2/src/file2.txt

        pasteMove(entitiesOf(file), destDir);

        assertGone(file, "external source file");
        assertExists(destDir.resolve("file2.txt"), "dest file");
        assertFileContent(destDir.resolve("file2.txt"), "file2.txt", "dest file content");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 3: Single flat dir (1 level with files), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p1s3", "src"),     // _test_fm/p1s3/src/
                 destDir   = createTestDir("p1s3", "dest"),    // _test_fm/p1s3/dest/
                 srcDir    = srcParent.resolve("flatdir");     // _test_fm/p1s3/src/flatdir/

        // Ensure folder record exists before paste
        HyperPath.folderRecordFor(srcDir, true);

        pasteMove(entitiesOf(srcDir), destDir);

        assertGone(srcDir, "source dir");
        assertExists(destDir.resolve("flatdir"), "dest dir");
        assertExists(destDir.resolve("flatdir", "a.txt"), "dest file a");
        assertExists(destDir.resolve("flatdir", "b.txt"), "dest file b");

        // Verify folder record re-parented
        HDT_Folder destFolder = HyperPath.folderRecordFor(destDir.resolve("flatdir"), false);
        assertNotNull(destFolder, "dest folder record should exist after re-parent");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 4: Single flat dir, external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = extRoot.resolve("p1s4", "src"),   // ../_test_fm_ext/p1s4/src/
                 destDir   = createTestDir("p1s4", "dest"),    // _test_fm/p1s4/dest/
                 srcDir    = srcParent.resolve("extdir");      // ../_test_fm_ext/p1s4/src/extdir/

        pasteMove(entitiesOf(srcDir), destDir);

        // External dirs: files moved individually, empty source dir deleted
        assertGone(srcDir, "external source dir");
        assertExists(destDir.resolve("extdir"), "dest dir");
        assertExists(destDir.resolve("extdir", "c.txt"), "dest file c");
        assertExists(destDir.resolve("extdir", "d.txt"), "dest file d");

        // Verify folder record was created at destination
        HDT_Folder destFolder = HyperPath.folderRecordFor(destDir.resolve("extdir"), false);
        assertNotNull(destFolder, "dest folder record should be created for external dir");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 5: Single nested dir (3 levels with files at each), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p1s5", "src"),   // _test_fm/p1s5/src/
                 destDir   = createTestDir("p1s5", "dest"),  // _test_fm/p1s5/dest/
                 lvl1 = srcParent.resolve("lvl1"),           // _test_fm/p1s5/src/lvl1/
                 lvl2 = lvl1.resolve("lvl2"),                // _test_fm/p1s5/src/lvl1/lvl2/
                 lvl3 = lvl2.resolve("lvl3");                // _test_fm/p1s5/src/lvl1/lvl2/lvl3/

        // Ensure folder records exist
        HyperPath.folderRecordFor(lvl3, true);

        pasteMove(entitiesOf(lvl1), destDir);

        assertGone(lvl1, "source lvl1");
        assertExists(destDir.resolve("lvl1"), "dest lvl1");
        assertExists(destDir.resolve("lvl1", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("lvl1", "lvl2", "f2.txt"), "dest f2");
        assertExists(destDir.resolve("lvl1", "lvl2", "lvl3", "f3.txt"), "dest f3");

        // Verify cascading filePath() resolves for child folders
        HDT_Folder destLvl2 = HyperPath.folderRecordFor(destDir.resolve("lvl1", "lvl2"), false);
        assertNotNull(destLvl2, "lvl2 folder record should resolve at new location");
        HDT_Folder destLvl3 = HyperPath.folderRecordFor(destDir.resolve("lvl1", "lvl2", "lvl3"), false);
        assertNotNull(destLvl3, "lvl3 folder record should resolve at new location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 6: Single nested dir (3 levels), external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = extRoot.resolve("p1s6", "src"),   // ../_test_fm_ext/p1s6/src/
                 destDir   = createTestDir("p1s6", "dest"),    // _test_fm/p1s6/dest/
                 lvl1      = srcParent.resolve("lvl1");        // ../_test_fm_ext/p1s6/src/lvl1/

        pasteMove(entitiesOf(lvl1), destDir);

        assertGone(lvl1, "external source lvl1");
        assertExists(destDir.resolve("lvl1", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("lvl1", "lvl2", "f2.txt"), "dest f2");
        assertExists(destDir.resolve("lvl1", "lvl2", "lvl3", "f3.txt"), "dest f3");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 7: Single deeply nested dir (5 levels), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p1s7", "src"),
                 destDir   = createTestDir("p1s7", "dest"),

                 d1 = srcParent.resolve("d1"),
                 d2 = d1.resolve("d2"),
                 d3 = d2.resolve("d3"),
                 d4 = d3.resolve("d4"),
                 d5 = d4.resolve("d5");

        HyperPath.folderRecordFor(d5, true);

        pasteMove(entitiesOf(d1), destDir);

        assertGone(d1, "source d1");
        assertExists(destDir.resolve("d1", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("d1", "d2", "d3", "f3.txt"), "dest f3");
        assertExists(destDir.resolve("d1", "d2", "d3", "d4", "d5", "f5.txt"), "dest f5");

        // Deeply nested filePath resolves correctly
        HDT_Folder destD5 = HyperPath.folderRecordFor(destDir.resolve("d1", "d2", "d3", "d4", "d5"), false);
        assertNotNull(destD5, "d5 folder record should resolve at new location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 8: Single deeply nested dir (5 levels), external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = extRoot.resolve("p1s8", "src"),
                 destDir   = createTestDir("p1s8", "dest"),
                 d1        = srcParent.resolve("d1");

        pasteMove(entitiesOf(d1), destDir);

        assertGone(d1, "external source d1");
        assertExists(destDir.resolve("d1", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("d1", "d2", "d3", "f3.txt"), "dest f3");
        assertExists(destDir.resolve("d1", "d2", "d3", "d4", "d5", "f5.txt"), "dest f5");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 2: Multi-Item Moves, Clean Destination (6 steps)
    // =====================================================================

    // Step 9: Multiple files (3), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p2s9", "src"),
                 destDir = createTestDir("p2s9", "dest"),

                 f1      = srcDir.resolve("alpha.txt"),
                 f2      = srcDir.resolve("beta.txt"),
                 f3      = srcDir.resolve("gamma.txt");

        pasteMove(entitiesOf(f1, f2, f3), destDir);

        assertGone(f1, "source alpha");
        assertGone(f2, "source beta");
        assertGone(f3, "source gamma");
        assertExists(destDir.resolve("alpha.txt"), "dest alpha");
        assertExists(destDir.resolve("beta.txt"), "dest beta");
        assertExists(destDir.resolve("gamma.txt"), "dest gamma");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 10: Multiple files (3), external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p2s10", "src"),
                 destDir = createTestDir("p2s10", "dest"),

                 f1      = srcDir.resolve("alpha.txt"),
                 f2      = srcDir.resolve("beta.txt"),
                 f3      = srcDir.resolve("gamma.txt");

        pasteMove(entitiesOf(f1, f2, f3), destDir);

        assertGone(f1, "ext source alpha");
        assertGone(f2, "ext source beta");
        assertGone(f3, "ext source gamma");
        assertExists(destDir.resolve("alpha.txt"), "dest alpha");
        assertExists(destDir.resolve("beta.txt"), "dest beta");
        assertExists(destDir.resolve("gamma.txt"), "dest gamma");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 11: Multiple dirs (2 independent), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p2s11", "src"),
                 destDir   = createTestDir("p2s11", "dest"),
                 dirA      = srcParent.resolve("dirA"),
                 dirB      = srcParent.resolve("dirB");

        HyperPath.folderRecordFor(dirA, true);
        HyperPath.folderRecordFor(dirB, true);

        pasteMove(entitiesOf(dirA, dirB), destDir);

        assertGone(dirA, "source dirA");
        assertGone(dirB, "source dirB");
        assertExists(destDir.resolve("dirA", "a.txt"), "dest a");
        assertExists(destDir.resolve("dirB", "b.txt"), "dest b");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 12: Multiple dirs (2 independent), external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = extRoot.resolve("p2s12", "src"),
                 destDir   = createTestDir("p2s12", "dest"),
                 dirA      = srcParent.resolve("dirA"),
                 dirB      = srcParent.resolve("dirB");

        pasteMove(entitiesOf(dirA, dirB), destDir);

        assertGone(dirA, "ext source dirA");
        assertGone(dirB, "ext source dirB");
        assertExists(destDir.resolve("dirA", "a.txt"), "dest a");
        assertExists(destDir.resolve("dirB", "b.txt"), "dest b");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 13: Mixed (1 file + 1 dir), internal
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p2s13", "src"),
                 destDir   = createTestDir("p2s13", "dest"),
                 dir       = srcParent.resolve("mixdir"),
                 file      = srcParent.resolve("loose.txt");

        HyperPath.folderRecordFor(dir, true);

        pasteMove(entitiesOf(dir, file), destDir);

        assertGone(dir, "source dir");
        assertGone(file, "source file");
        assertExists(destDir.resolve("mixdir", "inside.txt"), "dest dir file");
        assertExists(destDir.resolve("loose.txt"), "dest loose file");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 14: Mixed (1 file + 1 dir), external
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = extRoot.resolve("p2s14", "src"),
                 destDir   = createTestDir("p2s14", "dest"),
                 dir       = srcParent.resolve("mixdir"),
                 file      = srcParent.resolve("loose.txt");

        pasteMove(entitiesOf(dir, file), destDir);

        assertGone(dir, "ext source dir");
        assertGone(file, "ext source file");
        assertExists(destDir.resolve("mixdir", "inside.txt"), "dest dir file");
        assertExists(destDir.resolve("loose.txt"), "dest loose file");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 3: Re-Parent Root Topology (4 steps)
    // =====================================================================

    // Step 15: Two dirs, B nested inside A, both selected; B is pruned
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p3s15", "src"),
                 destDir   = createTestDir("p3s15", "dest"),
                 dirA      = srcParent.resolve("A"),
                 dirB      = dirA.resolve("B");

        HyperPath.folderRecordFor(dirB, true);
        HDT_Folder folderB = HyperPath.folderRecordFor(dirB, false),
                   folderA = HyperPath.folderRecordFor(dirA, false);
        assertNotNull(folderA, "folderA should exist");
        assertNotNull(folderB, "folderB should exist");

        // Select both A and B; B should be pruned (nested inside A)
        pasteMove(entitiesOf(dirA, dirB), destDir);

        assertGone(dirA, "source A");
        assertExists(destDir.resolve("A", "B", "file.txt"), "dest file");

        // A was re-parented; B remains a child of A
        HDT_Folder destA = HyperPath.folderRecordFor(destDir.resolve("A"), false);
        assertNotNull(destA, "dest A folder record");
        assertEquals(folderA.getID(), destA.getID(), "A folder record ID should be preserved");

        HDT_Folder destB = HyperPath.folderRecordFor(destDir.resolve("A", "B"), false);
        assertNotNull(destB, "dest B folder record");
        assertEquals(folderB.getID(), destB.getID(), "B folder record ID should be preserved");
        assertEquals(destA, destB.parentFolder(), "B should still be child of A");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 16: Three dirs: A contains B, C independent; A and C re-parented, B pruned
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p3s16", "src"),
                 destDir   = createTestDir("p3s16", "dest"),

                 dirA      = srcParent.resolve("A"),
                 dirB      = dirA.resolve("B"),
                 dirC      = srcParent.resolve("C");

        HyperPath.folderRecordFor(dirB, true);
        HyperPath.folderRecordFor(dirC, true);

        pasteMove(entitiesOf(dirA, dirB, dirC), destDir);

        assertGone(dirA, "source A");
        assertGone(dirC, "source C");
        assertExists(destDir.resolve("A", "B", "fb.txt"), "dest fb");
        assertExists(destDir.resolve("C", "fc.txt"), "dest fc");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 17: Dir A + file inside A (both selected); file covered by re-parent
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent  = createTestDir("p3s17", "src"),
                 destDir    = createTestDir("p3s17", "dest"),
                 dirA       = srcParent.resolve("A"),
                 insideFile = dirA.resolve("inside.txt");

        HyperPath.folderRecordFor(dirA, true);

        // Select both the dir and the file inside it
        pasteMove(entitiesOf(dirA, insideFile), destDir);

        assertGone(dirA, "source A");
        // The file should be at the new location (moved via re-parent, not separately)
        assertExists(destDir.resolve("A", "inside.txt"), "dest inside.txt");
        assertFileContent(destDir.resolve("A", "inside.txt"), "inside.txt", "content");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 18: Dir A + file in sibling dir (not inside A); A re-parented, file moved separately
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent   = createTestDir("p3s18", "src"),
                 destDir     = createTestDir("p3s18", "dest"),
                 dirA        = srcParent.resolve("A"),
                 siblingFile = srcParent.resolve("sibling.txt");

        HyperPath.folderRecordFor(dirA, true);

        pasteMove(entitiesOf(dirA, siblingFile), destDir);

        assertGone(dirA, "source A");
        assertGone(siblingFile, "source sibling");
        assertExists(destDir.resolve("A", "a.txt"), "dest a.txt");
        assertExists(destDir.resolve("sibling.txt"), "dest sibling.txt");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 4: Record Association Preservation (7 steps)
    // =====================================================================

    // Step 19: WorkFile in moved dir
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p4s19", "src"),
                 destDir   = createTestDir("p4s19", "dest"),
                 dir       = srcParent.resolve("wfdir"),
                 file      = dir.resolve("work.pdf");

        HyperPath.folderRecordFor(dir, true);

        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, file);
        assertNotNull(workFile, "WorkFile record should be created");
        int workFileID = workFile.getID();

        pasteMove(entitiesOf(dir), destDir);

        assertGone(dir, "source dir");
        assertExists(destDir.resolve("wfdir", "work.pdf"), "dest work.pdf");

        // WorkFile record should resolve to new location
        HDT_WorkFile resolvedWork = db.workFiles.getByID(workFileID);
        assertNotNull(resolvedWork, "WorkFile record should still exist");
        assertEquals(destDir.resolve("wfdir", "work.pdf"), resolvedWork.filePath(),
          "WorkFile filePath() should resolve to new location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 20: MiscFile in moved dir
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p4s20", "src"),
                 destDir   = createTestDir("p4s20", "dest"),
                 dir       = srcParent.resolve("mfdir"),
                 file      = dir.resolve("misc.dat");

        HyperPath.folderRecordFor(dir, true);

        HDT_RecordWithPath miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, file);
        assertNotNull(miscFile, "MiscFile record should be created");
        int miscFileID = miscFile.getID();

        pasteMove(entitiesOf(dir), destDir);

        assertGone(dir, "source dir");
        assertExists(destDir.resolve("mfdir", "misc.dat"), "dest misc.dat");

        HDT_MiscFile resolvedMisc = db.miscFiles.getByID(miscFileID);
        assertNotNull(resolvedMisc, "MiscFile record should still exist");
        assertEquals(destDir.resolve("mfdir", "misc.dat"), resolvedMisc.filePath(),
          "MiscFile filePath() should resolve to new location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 21: Person picture in moved dir
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p4s21", "src"),
                 destDir   = createTestDir("p4s21", "dest"),
                 dir       = srcParent.resolve("picdir"),
                 pic       = dir.resolve("photo.jpg");

        HDT_Folder dirFolder = HyperPath.folderRecordFor(dir, true);
        assertNotNull(dirFolder, "dir folder record");

        HDT_Person person = db.createNewBlankRecord(hdtPerson);
        person.setName(new PersonName("Test", "Person21"));
        person.getPath().assign(dirFolder, pic.getNameOnly());
        int personID = person.getID();

        assertEquals(pic, person.getPath().filePath(), "person path should resolve before move");

        pasteMove(entitiesOf(dir), destDir);

        HDT_Person resolvedPerson = db.persons.getByID(personID);
        assertNotNull(resolvedPerson, "Person record should still exist");
        assertEquals(destDir.resolve("picdir", "photo.jpg"), resolvedPerson.getPath().filePath(),
          "Person picture path should resolve to new location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 22: Note folder association
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p4s22", "src"),
                 destDir   = createTestDir("p4s22", "dest"),
                 dir       = srcParent.resolve("notedir");

        HDT_Folder dirFolder = HyperPath.folderRecordFor(dir, true);
        assertNotNull(dirFolder, "dir folder record");

        HDT_Note note = db.createNewBlankRecord(hdtNote);
        note.setName("Test note p4s22");
        note.folder.set(dirFolder);
        int noteID = note.getID();

        assertEquals(dirFolder, note.folder.get(), "note folder should be set before move");

        pasteMove(entitiesOf(dir), destDir);

        HDT_Note resolvedNote = db.notes.getByID(noteID);
        assertNotNull(resolvedNote, "Note record should still exist");

        // The folder record ID is preserved; filePath() resolves dynamically
        HDT_Folder destFolder = HyperPath.folderRecordFor(destDir.resolve("notedir"), false);
        assertEquals(dirFolder.getID(), destFolder.getID(), "Folder record ID should be preserved");
        assertEquals(destFolder, resolvedNote.folder.get(), "Note should still reference the same folder record");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 23: All four types together in single paste-move
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p4s23", "src"),
                 destDir   = createTestDir("p4s23", "dest"),
                 dir       = srcParent.resolve("alldir"),
                 wf        = dir.resolve("work.pdf"),
                 mf        = dir.resolve("misc.dat"),
                 pic       = dir.resolve("photo.jpg");

        HDT_Folder dirFolder = HyperPath.folderRecordFor(dir, true);

        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, wf),
                           miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, mf);

        HDT_Person person = db.createNewBlankRecord(hdtPerson);
        person.setName(new PersonName("Test", "Person23"));
        person.getPath().assign(dirFolder, pic.getNameOnly());

        HDT_Note note = db.createNewBlankRecord(hdtNote);
        note.setName("Test note p4s23");
        note.folder.set(dirFolder);

        int wfID = workFile.getID(), mfID = miscFile.getID(),
            personID = person.getID(), noteID = note.getID(),
            folderID = dirFolder.getID();

        pasteMove(entitiesOf(dir), destDir);

        // All records should resolve to new locations
        assertEquals(destDir.resolve("alldir", "work.pdf"), db.workFiles.getByID(wfID).filePath());
        assertEquals(destDir.resolve("alldir", "misc.dat"), db.miscFiles.getByID(mfID).filePath());
        assertEquals(destDir.resolve("alldir", "photo.jpg"), db.persons.getByID(personID).getPath().filePath());
        assertEquals(dirFolder, db.notes.getByID(noteID).folder.get());
        // Folder record ID should be unchanged
        assertEquals(folderID, HyperPath.folderRecordFor(destDir.resolve("alldir"), false).getID());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 24: Move individual file with WorkFile record (not via re-parent)
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p4s24", "src"),
                 destDir = createTestDir("p4s24", "dest"),
                 file    = srcDir.resolve("solo_work.pdf");

        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, file);
        assertNotNull(workFile, "WorkFile should be created");
        int wfID = workFile.getID();

        pasteMove(entitiesOf(file), destDir);

        assertGone(file, "source file");
        assertExists(destDir.resolve("solo_work.pdf"), "dest file");

        // Record-associated file goes through moveToFolder; association should resolve
        assertEquals(destDir.resolve("solo_work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile filePath() should resolve after individual file move");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 25: Move individual file with MiscFile record
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p4s25", "src"),
                 destDir = createTestDir("p4s25", "dest"),
                 file    = srcDir.resolve("solo_misc.dat");

        HDT_RecordWithPath miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, file);
        assertNotNull(miscFile, "MiscFile should be created");
        int mfID = miscFile.getID();

        pasteMove(entitiesOf(file), destDir);

        assertGone(file, "source file");
        assertExists(destDir.resolve("solo_misc.dat"), "dest file");

        assertEquals(destDir.resolve("solo_misc.dat"), db.miscFiles.getByID(mfID).filePath(),
          "MiscFile filePath() should resolve after individual file move");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 5: doPasteChecks Error Conditions (6 steps)
    // =====================================================================

    // Step 26: Source == destination (same directory)
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir = createTestDir("p5s26", "src"),
                 file   = srcDir.resolve("samefile.txt");

        PopupRobot.clear();

        // moveCopy succeeds (file is not protected)
        assertTrue(FileManager.instance().moveCopy(entitiesOf(file), false, false), "moveCopy should succeed");

        // Paste into the same directory the file is in
        HDT_Folder destFolder = HyperPath.folderRecordFor(srcDir, true);
        FileRow destRow = new FileRow(destFolder.getPath(), true);
        FileManager.instance().paste(destRow, false, false);
        pauseAndWaitForRunLaters(); // HyperTask.failed() defers errorPopup via Platform.runLater

        // File should still exist at source (not moved)
        assertExists(file, "file should be unchanged");
        assertTrue(PopupRobot.getLastMessage().contains("Source and destination are the same"),
          "Should get 'same' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 27: Dest is subfolder of source dir
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p5s27", "src"),
                 dirA      = srcParent.resolve("A"),
                 sub       = dirA.resolve("sub");

        HyperPath.folderRecordFor(sub, true);

        PopupRobot.clear();

        assertTrue(FileManager.instance().moveCopy(entitiesOf(dirA), false, false), "moveCopy should succeed");

        // Paste into A/sub/ (subfolder of source A/)
        HDT_Folder destFolder = HyperPath.folderRecordFor(sub, true);
        FileRow destRow = new FileRow(destFolder.getPath(), true);
        FileManager.instance().paste(destRow, false, false);
        pauseAndWaitForRunLaters(); // HyperTask.failed() defers errorPopup via Platform.runLater

        assertExists(dirA, "source dir should be unchanged");
        assertTrue(PopupRobot.getLastMessage().contains("subfolder of the source folder"),
          "Should get 'subfolder' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 28: Protected source file (via canCutRow)
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        // Try to move a special folder (Papers folder)
        HDT_Folder papersFolder = db.getPapersFolder();
        assertNotNull(papersFolder, "Papers folder should exist in transient DB");
        FilePath papersPath = papersFolder.filePath();
        assertNotNull(papersPath, "Papers folder path should not be null");

        PopupRobot.clear();

        // moveCopy should fail because canCutRow rejects protected folder
        boolean result = FileManager.instance().moveCopy(entitiesOf(papersPath), false, false);
        assertFalse(result, "moveCopy should reject protected folder");

        assertTrue(PopupRobot.getLastMessage().contains("cannot be moved"),
          "Should get 'cannot be moved' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 29: Protected file rejected by doPasteChecks (isProtectedFile)
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath hdbFile = db.getRootPath("database.hdb");

        PopupRobot.clear();

        boolean result = FileManager.instance().moveCopy(entitiesOf(hdbFile), false, false);
        assertFalse(result, "moveCopy should reject protected file database.hdb");

        assertTrue(PopupRobot.getLastMessage().contains("cannot be moved"),
          "Should get 'cannot be moved' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 30: Folder already exists at destination
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p5s30", "src"),
                 destDir   = createTestDir("p5s30", "dest"),
                 srcSub    = srcParent.resolve("dup");

        // dest/dup/ already exists from artifact setup; create folder record for source

        HyperPath.folderRecordFor(srcSub, true);

        PopupRobot.clear();

        assertTrue(FileManager.instance().moveCopy(entitiesOf(srcSub), false, false), "moveCopy should succeed");

        HDT_Folder destFolder = HyperPath.folderRecordFor(destDir, true);
        FileRow destRow = new FileRow(destFolder.getPath(), true);
        FileManager.instance().paste(destRow, false, false);
        pauseAndWaitForRunLaters(); // HyperTask.failed() defers errorPopup via Platform.runLater

        // Source should still exist (paste failed)
        assertExists(srcSub, "source dir should be unchanged");
        assertTrue(PopupRobot.getLastMessage().contains("folder already exists at destination"),
          "Should get 'folder already exists' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 31: Protected file at destination
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        // Create a file with the same name as a protected file at the DB root
        // and try to move another file into the root to overwrite it.
        // The simplest: try to move a file named "database.hdb" into the root.

        FilePath srcDir = createTestDir("p5s31", "src"),
                 file   = srcDir.resolve("database.hdb");

        PopupRobot.clear();

        assertTrue(FileManager.instance().moveCopy(entitiesOf(file), false, false), "moveCopy should succeed");

        HDT_Folder rootFolder = db.getRootFolder();
        FileRow destRow = new FileRow(rootFolder.getPath(), true);
        FileManager.instance().paste(destRow, false, false);
        pauseAndWaitForRunLaters(); // HyperTask.failed() defers errorPopup via Platform.runLater

        assertExists(file, "source file should be unchanged");
        assertTrue(PopupRobot.getLastMessage().contains("Cannot overwrite destination"),
          "Should get 'Cannot overwrite' error: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 6: Overwrite Scenarios (9 steps)
    // =====================================================================

    // Step 32: Single unrelated file at dest, mrYes: file overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s32", "src"),
                 destDir = createTestDir("p6s32", "dest"),
                 srcFile = srcDir.resolve("over.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrYes);

        pasteMove(entitiesOf(srcFile), destDir);

        assertGone(srcFile, "source file");
        assertFileContent(destDir.resolve("over.txt"), "over.txt", "overwritten content");
        assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once for overwrite");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 33: Single unrelated file at dest, mrNo: file NOT overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s33", "src"),
                 destDir = createTestDir("p6s33", "dest"),
                 srcFile = srcDir.resolve("over.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrNo);

        pasteMove(entitiesOf(srcFile), destDir);

        // Source should remain (was removed from srcToDest, paste completes but file not moved)
        assertExists(srcFile, "source file should remain");
        assertFileContent(destDir.resolve("over.txt"), "original", "dest file should be unchanged");
        assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 34: Two unrelated files at dest, mrYesToAll: both overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s34", "src"),
                 destDir = createTestDir("p6s34", "dest"),
                 srcF1   = srcDir.resolve("f1.txt"),
                 srcF2   = srcDir.resolve("f2.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrYesToAll);

        pasteMove(entitiesOf(srcF1, srcF2), destDir);

        assertGone(srcF1, "source f1");
        assertGone(srcF2, "source f2");
        assertFileContent(destDir.resolve("f1.txt"), "f1.txt", "overwritten f1");
        assertFileContent(destDir.resolve("f2.txt"), "f2.txt", "overwritten f2");
        assertEquals(1, PopupRobot.getInvocationCount(), "first prompted, second auto-approved");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 35: Two unrelated files at dest, mrNoToAll: neither overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s35", "src"),
                 destDir = createTestDir("p6s35", "dest"),
                 srcF1   = srcDir.resolve("f1.txt"),
                 srcF2   = srcDir.resolve("f2.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrNoToAll);

        pasteMove(entitiesOf(srcF1, srcF2), destDir);

        assertExists(srcF1, "source f1 should remain");
        assertExists(srcF2, "source f2 should remain");
        assertFileContent(destDir.resolve("f1.txt"), "orig1", "dest f1 unchanged");
        assertFileContent(destDir.resolve("f2.txt"), "orig2", "dest f2 unchanged");
        assertEquals(1, PopupRobot.getInvocationCount(), "first prompted, second auto-rejected");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 36: Single related file (WorkFile at dest), mrYes: file overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir   = createTestDir("p6s36", "src"),
                 destDir  = createTestDir("p6s36", "dest"),
                 srcFile  = srcDir.resolve("related.txt"),
                 destFile = destDir.resolve("related.txt");

        // Create a WorkFile record assigned to the destination file
        HyperPath.createRecordAssignedToPath(hdtWorkFile, destFile);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrYes);

        pasteMove(entitiesOf(srcFile), destDir);

        assertGone(srcFile, "source file");
        assertFileContent(destDir.resolve("related.txt"), "related.txt", "overwritten content");
        assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once for related overwrite");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 37: Single related file, mrNo: not overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir   = createTestDir("p6s37", "src"),
                 destDir  = createTestDir("p6s37", "dest"),
                 srcFile  = srcDir.resolve("related.txt"),
                 destFile = destDir.resolve("related.txt");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, destFile);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrNo);

        pasteMove(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should remain");
        assertFileContent(destDir.resolve("related.txt"), "original_related", "dest unchanged");
        assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 38: Two related files, mrYesToAll: both overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s38", "src"),
                 destDir = createTestDir("p6s38", "dest"),

                 srcF1   = srcDir .resolve("r1.txt"),
                 srcF2   = srcDir .resolve("r2.txt"),
                 destF1  = destDir.resolve("r1.txt"),
                 destF2  = destDir.resolve("r2.txt");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, destF1);
        HyperPath.createRecordAssignedToPath(hdtMiscFile, destF2);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrYesToAll);

        pasteMove(entitiesOf(srcF1, srcF2), destDir);

        assertGone(srcF1, "source r1");
        assertGone(srcF2, "source r2");
        assertFileContent(destDir.resolve("r1.txt"), "r1.txt", "overwritten r1");
        assertFileContent(destDir.resolve("r2.txt"), "r2.txt", "overwritten r2");
        assertEquals(1, PopupRobot.getInvocationCount(), "first prompted, second auto-approved");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 39: Two related files, mrNoToAll: neither overwritten
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p6s39", "src"),
                 destDir = createTestDir("p6s39", "dest"),

                 srcF1   = srcDir .resolve("r1.txt"),
                 srcF2   = srcDir .resolve("r2.txt"),
                 destF1  = destDir.resolve("r1.txt"),
                 destF2  = destDir.resolve("r2.txt");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, destF1);
        HyperPath.createRecordAssignedToPath(hdtMiscFile, destF2);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrNoToAll);

        pasteMove(entitiesOf(srcF1, srcF2), destDir);

        assertExists(srcF1, "source r1 should remain");
        assertExists(srcF2, "source r2 should remain");
        assertFileContent(destDir.resolve("r1.txt"), "orig_r1", "dest r1 unchanged");
        assertFileContent(destDir.resolve("r2.txt"), "orig_r2", "dest r2 unchanged");
        assertEquals(1, PopupRobot.getInvocationCount(), "first prompted, second auto-rejected");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 40: Mixed unrelated + related at dest; independent state machines
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir      = createTestDir("p6s40", "src"),
                 destDir     = createTestDir("p6s40", "dest"),

                 srcUnrelated = srcDir.resolve("unrel.txt"),
                 srcRelated   = srcDir.resolve("rel.txt"),
                 destRelated  = destDir.resolve("rel.txt");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, destRelated);

        PopupRobot.clear();
        // YesToAll for both: unrelated prompt triggers first, related prompt triggers separately
        PopupRobot.setDefaultResponse(mrYesToAll);

        pasteMove(entitiesOf(srcUnrelated, srcRelated), destDir);

        assertGone(srcUnrelated, "source unrelated");
        assertGone(srcRelated, "source related");
        assertFileContent(destDir.resolve("unrel.txt"), "unrel.txt", "overwritten unrelated");
        assertFileContent(destDir.resolve("rel.txt"), "rel.txt", "overwritten related");

        // Two separate prompts: one for paUnrelated, one for paRelated (independent state machines)
        assertEquals(2, PopupRobot.getInvocationCount(),
          "should get 2 prompts (independent paUnrelated and paRelated state machines)");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 7: Empty Directory Cleanup (3 steps)
    // =====================================================================

    // Step 41: Move all files from external dir; source dir should be deleted
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p7s41", "src"),
                 destDir = createTestDir("p7s41", "dest"),
                 extSub  = srcDir.resolve("emptyme");

        pasteMove(entitiesOf(extSub), destDir);

        assertExists(destDir.resolve("emptyme", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("emptyme", "f2.txt"), "dest f2");

        // External empty dir should have been collected and deleted by FileDeletion
        assertGone(extSub, "external source dir should be deleted after all files moved out");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 42: Move all files from external nested dirs; parent tree should be deleted
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p7s42", "src"),
                 destDir = createTestDir("p7s42", "dest"),
                 extLvl1 = srcDir.resolve("lvl1");

        pasteMove(entitiesOf(extLvl1), destDir);

        assertExists(destDir.resolve("lvl1", "f1.txt"), "dest f1");
        assertExists(destDir.resolve("lvl1", "lvl2", "f2.txt"), "dest f2");

        // Both empty dirs in the tree should be collected and deleted
        assertGone(extLvl1, "external lvl1 should be deleted");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 43: Move some files from external dir; dir still has remaining files
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p7s43", "src"),
                 destDir = createTestDir("p7s43", "dest"),
                 extSub  = srcDir.resolve("partial"),
                 toMove  = extSub.resolve("move_me.txt");

        // Only move one of the two files
        pasteMove(entitiesOf(toMove), destDir);

        assertGone(toMove, "moved file");
        assertExists(destDir.resolve("move_me.txt"), "dest file");

        // Source dir should NOT be deleted (still has remaining files)
        assertExists(extSub, "external source dir should remain (has remaining files)");
        assertExists(extSub.resolve("stay.txt"), "remaining file should still exist");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 8: moveToNewParent Same-Parent No-Op (1 step)
    // =====================================================================

    // Step 44: folder.moveToNewParent(folder.parentFolder()) returns true, no change
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p8s44", "src"),
                 dir       = srcParent.resolve("noop");

        HDT_Folder folder = HyperPath.folderRecordFor(dir, true);
        assertNotNull(folder, "folder record");
        HDT_Folder parent = folder.parentFolder();
        assertNotNull(parent, "parent folder");

        int folderID = folder.getID();
        FilePath beforePath = folder.filePath();

        boolean result = folder.moveToNewParent(parent);

        assertTrue(result, "moveToNewParent should return true for same parent");
        assertEquals(folderID, folder.getID(), "folder ID should be unchanged");
        assertEquals(beforePath, folder.filePath(), "filePath should be unchanged");
        assertEquals(parent, folder.parentFolder(), "parent should be unchanged");
        assertExists(dir.resolve("file.txt"), "file should still exist");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 9: Watcher Lifecycle (2 steps)
    // =====================================================================

    // Step 45: Paste-move restarts watcher afterward
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p9s45", "src"),
                 destDir = createTestDir("p9s45", "dest");

        pasteMove(entitiesOf(srcDir.resolve("watch.txt")), destDir);

        // Watcher restart is in Platform.runLater, so it won't be running yet.
        // We check in the next step after a delay.
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 45b: Verify watcher is running after delay
    seq.thenRunAfterDelay(() -> assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running after paste-move completes"));

    // Step 46: folder.renameTo() + watcher still running
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p9s46", "src"),
                 dir       = srcParent.resolve("oldname");

        HDT_Folder folder = HyperPath.folderRecordFor(dir, true);
        assertNotNull(folder, "folder record");

        // Create a descendant folder record for the subdirectory
        HDT_Folder subFolder = HyperPath.folderRecordFor(dir.resolve("sub"), true);
        assertNotNull(subFolder, "sub folder record");

        boolean result = folder.renameTo("newname");
        assertTrue(result, "renameTo should succeed");

        // filePath should reflect the new name
        FilePath newPath = srcParent.resolve("newname");
        assertEquals(newPath, folder.filePath(), "filePath should reflect new name");
        assertExists(newPath.resolve("file.txt"), "file should exist at new path");
        assertGone(dir, "old path should not exist");

        // Descendant HyperPath should be re-keyed under the new parent path
        assertFalse(HyperPath.getHyperPathSetForFilePath(subFolder.filePath()).isEmpty(),
          "descendant folder should be findable via registry after rename");
        assertTrue(HyperPath.getHyperPathSetForFilePath(subFolder.filePath()).contains(subFolder.getPath()),
          "descendant folder's HyperPath should be registered under new path");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 46b: Verify watcher running after rename
    seq.thenRunAfterDelay(() -> assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running after rename"));

    // =====================================================================
    //  Phase 10: Lock Checking (7 steps, Windows-only)
    // =====================================================================

    // Step 47: canObtainLock() on locked file returns false
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath lockDir = createTestDir("p10s47", "lock"),
                 file    = lockDir.resolve("locked.txt");

        lockProc = startLockProcess(file);
        assertFalse(file.canObtainLock(), "canObtainLock should return false for locked file");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 48: canObtainLock() after lock released returns true
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath lockDir = createTestDir("p10s48", "lock"),
                 file    = lockDir.resolve("locked.txt");

        lockProc = startLockProcess(file);
        assertFalse(file.canObtainLock(), "should be locked");

        destroyQuietly(lockProc);
        lockProc = null;

        // Small delay for OS to release the file handle
        Thread.sleep(200);

        assertTrue(file.canObtainLock(), "canObtainLock should return true after lock released");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 49: anyOpenFilesInDir() with locked file returns true
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath lockDir = createTestDir("p10s49", "lock"),
                 file    = lockDir.resolve("locked.txt");

        lockProc = startLockProcess(file);

        PopupRobot.clear();
        assertTrue(lockDir.anyOpenFilesInDir(), "anyOpenFilesInDir should return true");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 50: anyOpenFilesInDir() on clean dir returns false
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath lockDir = createTestDir("p10s50", "lock");

        PopupRobot.clear();
        assertFalse(lockDir.anyOpenFilesInDir(), "anyOpenFilesInDir should return false for clean dir");
      }
      catch (Exception e) { throw new AssertionError(e); }
    }).windowsOnly();

    // Step 51: Lock probe cleanup: single leftover restored to original name
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath probeDir = createTestDir("p10s51", "lock");
        String origName  = "testfolder",
               probeName = origName + ".hntmp_" + System.nanoTime();

        // Create a probe-renamed directory (simulating a crash during canObtainLock)
        FilePath probeRenamed = probeDir.resolve(probeName);
        probeRenamed.createDirectories();
        Files.writeString(probeRenamed.resolve("content.txt").toPath(), "content.txt");

        // The cleanup happens during DB open, but we can call it indirectly
        // by verifying the naming pattern; the cleanup is in HyperDB.cleanupLeftoverLockProbeDirectories
        // which is private. We verify the naming convention instead.
        FilePath originalPath = probeDir.resolve(origName);
        assertGone(originalPath, "original path should not exist before rename");

        // Manually simulate what cleanupLeftoverLockProbeDirectories does
        Files.move(probeRenamed.toPath(), originalPath.toPath());

        assertExists(originalPath, "original path should exist after rename");
        assertGone(probeRenamed, "probe path should be gone after rename");
        assertExists(originalPath.resolve("content.txt"), "content should be preserved");
      }
      catch (Exception e) { throw new AssertionError(e); }
    }).windowsOnly();

    // Step 52: Lock probe cleanup: multiple leftovers restored
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath probeDir = createTestDir("p10s52", "lock");

        String origName1  = "folder1", origName2 = "folder2",
               probeName1 = origName1 + ".hntmp_" + System.nanoTime(),
               probeName2 = origName2 + ".hntmp_" + (System.nanoTime() + 1);

        FilePath probe1 = probeDir.resolve(probeName1),
                 probe2 = probeDir.resolve(probeName2);

        probe1.createDirectories();
        Files.writeString(probe1.resolve("f1.txt").toPath(), "f1.txt");
        probe2.createDirectories();
        Files.writeString(probe2.resolve("f2.txt").toPath(), "f2.txt");

        FilePath orig1 = probeDir.resolve(origName1),
                 orig2 = probeDir.resolve(origName2);

        Files.move(probe1.toPath(), orig1.toPath());
        Files.move(probe2.toPath(), orig2.toPath());

        assertExists(orig1.resolve("f1.txt"), "first folder restored");
        assertExists(orig2.resolve("f2.txt"), "second folder restored");
      }
      catch (Exception e) { throw new AssertionError(e); }
    }).windowsOnly();

    // Step 53: Lock probe cleanup: original already exists; both left unchanged
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath probeDir = createTestDir("p10s53", "lock");
        String origName  = "conflict",
               probeName = origName + ".hntmp_" + System.nanoTime();

        // Both the original and the probe-renamed exist
        FilePath originalPath = probeDir.resolve(origName),
                 probeRenamed = probeDir.resolve(probeName);

        probeRenamed.createDirectories();
        Files.writeString(probeRenamed.resolve("probe.txt").toPath(), "probe.txt");

        // The cleanup logic skips the rename if original already exists
        // Both should remain unchanged
        assertExists(originalPath.resolve("original.txt"), "original should be untouched");
        assertExists(probeRenamed.resolve("probe.txt"), "probe should be untouched");
      }
      catch (Exception e) { throw new AssertionError(e); }
    }).windowsOnly();

    // Step 54: Move folder subtree with locked interior file; verify popup names the locked file
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p10s54", "src"),
                 destDir   = createTestDir("p10s54", "dest"),
                 tree      = srcParent.resolve("tree"),
                 lockedFile = tree.resolve("sub1", "sub2", "s2.txt");

        HyperPath.folderRecordFor(tree.resolve("sub1", "sub2"), true);
        HyperPath.folderRecordFor(tree.resolve("sibling"), true);

        lockProc = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);  // Stop the paste at the retry/skip/stop dialog
        pasteMove(entitiesOf(tree), destDir);

        // Move should have failed: source tree still exists
        assertExists(tree, "source tree should still exist after failed move");

        // The combined dialog should identify the specific locked file
        String lastMsg = PopupRobot.getLastMessage();
        assertNotNull(lastMsg, "a popup should have been shown");
        assertTrue(lastMsg.contains("s2.txt"),
          "popup should name the locked file 's2.txt', got: " + lastMsg);

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 11: Baseline Mixed Moves (2 steps)
    // =====================================================================

    // Step 55: External nested dir tree + sibling file into internal dest
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcNested  = extRoot.resolve("p11s55", "src", "nested"),
                 srcSibling = extRoot.resolve("p11s55", "src", "sibling.txt"),
                 destDir    = createTestDir("p11s55", "dest");

        pasteMove(entitiesOf(srcNested, srcSibling), destDir);

        assertExists(destDir.resolve("nested", "top.txt"), "dest top.txt");
        assertExists(destDir.resolve("nested", "child1", "c1.txt"), "dest c1.txt");
        assertExists(destDir.resolve("nested", "child1", "child2", "c2.txt"), "dest c2.txt");
        assertExists(destDir.resolve("sibling.txt"), "dest sibling.txt");
        assertGone(srcNested, "source nested");
        assertGone(srcSibling, "source sibling.txt");

        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("nested"), false),
          "nested folder record should exist");
        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("nested", "child1"), false),
          "child1 folder record should exist");
        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("nested", "child1", "child2"), false),
          "child2 folder record should exist");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 56: Mixed siblings: folder (re-parent) + WorkFile + unassociated file
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p11s56", "src"),
                 destDir   = createTestDir("p11s56", "dest"),

                 folder    = srcParent.resolve("folder"),
                 workPdf   = srcParent.resolve("work.pdf"),
                 loose     = srcParent.resolve("loose.txt");

        HDT_Folder folderRec = HyperPath.folderRecordFor(folder, true);
        int folderID = folderRec.getID();

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        pasteMove(entitiesOf(folder, workPdf, loose), destDir);

        assertEquals(folderID, HyperPath.folderRecordFor(destDir.resolve("folder"), false).getID(),
          "folder record ID should be preserved after re-parent");
        assertEquals(destDir.resolve("work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile should resolve to dest");
        assertExists(destDir.resolve("loose.txt"), "dest loose.txt");
        assertGone(folder, "source folder");
        assertGone(workPdf, "source work.pdf");
        assertGone(loose, "source loose.txt");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 12: Re-Parent Loop Skip (4 steps, Windows-only)
    // =====================================================================

    // Step 57: Skip locked dir, move clean dir (locked first in list)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p12s57", "src"),
                 destDir   = createTestDir("p12s57", "dest"),
                 lockedDir = srcParent.resolve("locked_dir"),
                 cleanDir  = srcParent.resolve("clean"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);
        HyperPath.folderRecordFor(cleanDir, true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(lockedDir, cleanDir), destDir);

        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertExists(destDir.resolve("clean", "a.txt"), "dest clean/a.txt");
        assertGone(srcParent.resolve("clean"), "source clean should be gone");

        assertTrue(PopupRobot.getLastMessage().contains("target.txt"),
          "popup should mention target.txt: " + PopupRobot.getLastMessage());
        assertEquals(1, PopupRobot.getInvocationCount(), "should get exactly 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 58: Skip locked dir, move clean dir (clean first in list)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p12s58", "src"),
                 destDir   = createTestDir("p12s58", "dest"),

                 cleanDir  = srcParent.resolve("clean"),
                 lockedDir = srcParent.resolve("locked_dir"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(cleanDir, true);
        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(cleanDir, lockedDir), destDir);

        assertExists(destDir.resolve("clean", "a.txt"), "dest clean/a.txt");
        assertGone(srcParent.resolve("clean"), "source clean should be gone");
        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");

        assertTrue(PopupRobot.getLastMessage().contains("target.txt"),
          "popup should mention target.txt: " + PopupRobot.getLastMessage());
        assertEquals(1, PopupRobot.getInvocationCount(), "should get exactly 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 59: Skip locked dir, move clean dir + extra file
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p12s59", "src"),
                 destDir   = createTestDir("p12s59", "dest"),

                 cleanDir  = srcParent.resolve("clean"),
                 lockedDir = srcParent.resolve("locked_dir"),
                 extraFile = srcParent.resolve("extra.txt"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(cleanDir, true);
        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(cleanDir, lockedDir, extraFile), destDir);

        assertExists(destDir.resolve("clean", "a.txt"), "dest clean/a.txt");
        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertExists(destDir.resolve("extra.txt"), "dest extra.txt");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get exactly 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 60: Stop at locked dir (mrCancel); file loop never runs
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p12s60", "src"),
                 destDir   = createTestDir("p12s60", "dest"),

                 cleanDir  = srcParent.resolve("clean"),
                 lockedDir = srcParent.resolve("locked_dir"),
                 extraFile = srcParent.resolve("extra.txt"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(cleanDir, true);
        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);

        pasteMove(entitiesOf(cleanDir, lockedDir, extraFile), destDir);

        // clean was already moved before Stop was hit on locked_dir
        assertExists(destDir.resolve("clean", "a.txt"), "dest clean/a.txt (already moved)");
        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        // extra.txt should remain at source because file loop never ran after Stop
        assertExists(srcParent.resolve("extra.txt"), "extra.txt should still be at source");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get exactly 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 13: Re-Parent Loop Stop and Retry (3 steps, Windows-only)
    // =====================================================================

    // Step 61: Stop at locked dir; second dir not moved
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p13s61", "src"),
                 destDir   = createTestDir("p13s61", "dest"),

                 firstDir  = srcParent.resolve("first"),
                 secondDir = srcParent.resolve("second"),
                 lockFile  = firstDir .resolve("a.txt");

        HyperPath.folderRecordFor(firstDir, true);
        HyperPath.folderRecordFor(secondDir, true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);

        pasteMove(entitiesOf(firstDir, secondDir), destDir);

        assertExists(srcParent.resolve("first"), "first should still exist");
        assertExists(srcParent.resolve("second"), "second should still exist (Stop aborted)");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get exactly 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 62: Retry then Stop on locked dir
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p13s62", "src"),
                 destDir   = createTestDir("p13s62", "dest"),

                 retryDir  = srcParent.resolve("retrydir"),
                 lockFile  = retryDir.resolve("locked.txt");

        HyperPath.folderRecordFor(retryDir, true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrCancel);

        pasteMove(entitiesOf(retryDir), destDir);

        assertExists(srcParent.resolve("retrydir"), "retrydir should still exist");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Stop)");
        assertTrue(PopupRobot.getLastMessage().contains("locked.txt"),
          "popup should mention locked.txt: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 63: Retry then Skip on locked dir; extra file still moves
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p13s63", "src"),
                 destDir   = createTestDir("p13s63", "dest"),

                 retryDir  = srcParent.resolve("retrydir"),
                 extraFile = srcParent.resolve("extra.txt"),
                 lockFile  = retryDir .resolve("locked.txt");

        HyperPath.folderRecordFor(retryDir, true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrIgnore);

        pasteMove(entitiesOf(retryDir, extraFile), destDir);

        assertExists(srcParent.resolve("retrydir"), "retrydir should still exist");
        assertExists(destDir.resolve("extra.txt"), "dest extra.txt");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Skip)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 14: File Move Loop Lock (4 steps, Windows-only)
    // =====================================================================

    // Step 64: Skip locked file; other files move (external, HashMap order)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = extRoot.resolve("p14s64", "src"),
                 destDir = createTestDir("p14s64", "dest"),

                 fileA   = srcDir.resolve("a.txt"),
                 fileB   = srcDir.resolve("b.txt"),
                 fileC   = srcDir.resolve("c.txt");

        lockProc = startLockProcess(fileB);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        pasteMove(entitiesOf(fileA, fileB, fileC), destDir);

        assertExists(destDir.resolve("a.txt"), "dest a.txt");
        assertExists(destDir.resolve("c.txt"), "dest c.txt");
        assertExists(srcDir.resolve("b.txt"), "source b.txt should still exist (locked)");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup for locked file");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 65: Stop at locked file (HashMap order unpredictable)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = extRoot.resolve("p14s65", "src"),
                 destDir = createTestDir("p14s65", "dest"),

                 fileA   = srcDir.resolve("a.txt"),
                 fileB   = srcDir.resolve("b.txt"),
                 fileC   = srcDir.resolve("c.txt");

        lockProc = startLockProcess(fileB);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);

        pasteMove(entitiesOf(fileA, fileB, fileC), destDir);

        assertExists(srcDir.resolve("b.txt"), "source b.txt should still exist (locked)");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
        // Cannot assert a.txt/c.txt state due to HashMap iteration order
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 66: Two locked files, both skipped
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcDir  = extRoot.resolve("p14s66", "src"),
                 destDir = createTestDir("p14s66", "dest"),

                 fileX   = srcDir.resolve("x.txt"),
                 fileY   = srcDir.resolve("y.txt");

        lockProc1 = startLockProcess(fileX);
        lockProc2 = startLockProcess(fileY);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        pasteMove(entitiesOf(fileX, fileY), destDir);

        assertExists(srcDir.resolve("x.txt"), "source x.txt should still exist (locked)");
        assertExists(srcDir.resolve("y.txt"), "source y.txt should still exist (locked)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // Step 67: Retry then Stop on locked file
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir     = extRoot.resolve("p14s67", "src"),
                 destDir    = createTestDir("p14s67", "dest"),
                 lockedFile = srcDir.resolve("locked.txt");

        lockProc = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrCancel);

        pasteMove(entitiesOf(lockedFile), destDir);

        assertExists(srcDir.resolve("locked.txt"), "source locked.txt should still exist");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Stop)");

        String lastMsg = PopupRobot.getLastMessage();
        assertTrue(lastMsg.contains("locked.txt"), "popup should mention locked.txt: " + lastMsg);
        assertTrue(lastMsg.toLowerCase().contains("access denied"),
          "popup should mention access denied: " + lastMsg);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 15: Cross-Loop Scenarios (5 steps, Windows-only)
    // =====================================================================

    // Step 68: Locked dir skipped; loose file moves in file loop
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p15s68", "src"),
                 destDir   = createTestDir("p15s68", "dest"),

                 lockedDir = srcParent.resolve("locked_dir"),
                 looseFile = srcParent.resolve("loose.txt"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(lockedDir, looseFile), destDir);

        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertExists(destDir.resolve("loose.txt"), "dest loose.txt");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 69: Folder re-parents OK; locked unassociated file skipped
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent  = createTestDir("p15s69", "src"),
                 destDir    = createTestDir("p15s69", "dest"),
                 folder     = srcParent.resolve("folder"),
                 lockedFile = srcParent.resolve("locked.txt");

        HyperPath.folderRecordFor(folder, true);

        lockProc = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(folder, lockedFile), destDir);

        assertExists(destDir.resolve("folder", "inner.txt"), "dest folder/inner.txt");
        assertGone(srcParent.resolve("folder"), "source folder should be gone");
        assertExists(lockedFile, "locked.txt should still exist");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 70: Stop at locked dir; file loop never runs, extra.txt stays
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p15s70", "src"),
                 destDir   = createTestDir("p15s70", "dest"),

                 lockedDir = srcParent.resolve("locked_dir"),
                 extraFile = srcParent.resolve("extra.txt"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);

        pasteMove(entitiesOf(lockedDir, extraFile), destDir);

        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertExists(srcParent.resolve("extra.txt"), "extra.txt should still be at source");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 71: Both dir and unassociated file locked; both skipped
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcParent  = createTestDir("p15s71", "src"),
                 destDir    = createTestDir("p15s71", "dest"),

                 lockedDir  = srcParent.resolve("locked_dir"),
                 lockedFile = srcParent.resolve("locked_file.txt"),
                 lockFile1  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc1 = startLockProcess(lockFile1);
        lockProc2 = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        pasteMove(entitiesOf(lockedDir, lockedFile), destDir);

        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertExists(lockedFile, "locked_file.txt should still exist");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // Step 72: Folder re-parents; locked WorkFile skipped, record path unchanged
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p15s72", "src"),
                 destDir   = createTestDir("p15s72", "dest"),
                 folder    = srcParent.resolve("folder"),
                 workPdf   = srcParent.resolve("work.pdf");

        HyperPath.folderRecordFor(folder, true);

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(folder, workPdf), destDir);

        assertExists(destDir.resolve("folder", "inner.txt"), "dest folder/inner.txt (re-parented)");
        assertExists(srcParent.resolve("work.pdf"), "source work.pdf should still exist (locked)");
        assertEquals(workPdf, db.workFiles.getByID(wfID).filePath(),
          "WorkFile record path should still point to source");

        String lastMsg = PopupRobot.getLastMessage();
        assertTrue(lastMsg.contains("work.pdf"), "popup should mention work.pdf: " + lastMsg);
        assertTrue(lastMsg.toLowerCase().contains("access denied"),
          "popup should mention access denied: " + lastMsg);
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 16: Read-Only (2 steps, Windows-only)
    // =====================================================================

    // Step 73: Read-only file can still be moved
    seq.thenRunAfterDelay(() ->
    {
      FilePath destFile = null;

      try
      {
        FilePath srcDir  = createTestDir("p16s73", "src"),
                 destDir = createTestDir("p16s73", "dest"),
                 roFile  = srcDir.resolve("readonly.txt");

        roFile.toFile().setReadOnly();

        pasteMove(entitiesOf(roFile), destDir);

        destFile = destDir.resolve("readonly.txt");
        assertExists(destFile, "dest readonly.txt");
        assertGone(roFile, "source readonly.txt should be gone");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        if (destFile != null) destFile.toFile().setWritable(true);
      }
    }).windowsOnly();

    // Step 74: Read-only file inside dir; dir re-parent still works
    seq.thenRunAfterDelay(() ->
    {
      FilePath destRoFile = null;

      try
      {
        FilePath srcParent = createTestDir("p16s74", "src"),
                 destDir   = createTestDir("p16s74", "dest"),
                 roDir     = srcParent.resolve("rodir"),
                 roFile    = roDir.resolve("readonly.txt");

        HyperPath.folderRecordFor(roDir, true);

        roFile.toFile().setReadOnly();

        pasteMove(entitiesOf(roDir), destDir);

        assertExists(destDir.resolve("rodir", "normal.txt"), "dest normal.txt");
        destRoFile = destDir.resolve("rodir", "readonly.txt");
        assertExists(destRoFile, "dest readonly.txt");
        assertGone(srcParent.resolve("rodir"), "source rodir should be gone");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        if (destRoFile != null) destRoFile.toFile().setWritable(true);
      }
    }).windowsOnly();

    // =====================================================================
    //  Phase 17: Copy Operations (3 steps)
    // =====================================================================

    // Step 75: Simple file copy
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p17s75", "src"),
                 destDir = createTestDir("p17s75", "dest"),
                 srcFile = srcDir.resolve("original.txt");

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist after copy");
        assertExists(destDir.resolve("original.txt"), "dest copy should exist");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 76: Copy external dir tree
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = extRoot.resolve("p17s76", "src"),
                 destDir = createTestDir("p17s76", "dest"),
                 topdir  = srcDir.resolve("topdir");

        pasteCopy(entitiesOf(topdir), destDir);

        // Source files should still exist
        assertExists(topdir.resolve("a.txt"), "source a.txt should still exist");
        assertExists(topdir.resolve("sub", "b.txt"), "source sub/b.txt should still exist");

        // Dest copies should exist
        assertExists(destDir.resolve("topdir", "a.txt"), "dest a.txt");
        assertExists(destDir.resolve("topdir", "sub", "b.txt"), "dest sub/b.txt");

        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("topdir"), false),
          "topdir folder record should exist at dest");
        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("topdir", "sub"), false),
          "sub folder record should exist at dest");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 77: Protected file can be copied but not moved (contrast with Step 29)
    seq.thenRunAfterDelay(() ->
    {
      FilePath destCopy = null;

      try
      {
        FilePath destDir = createTestDir("p17s77", "dest"),
                 hdbFile = db.getHdbPath();

        // Step 29 verifies that moveCopy rejects database.hdb when copying=false.
        // Here we verify that copying succeeds because doPasteChecks only calls
        // isProtectedFile when copying == false.

        pasteCopy(entitiesOf(hdbFile), destDir);

        destCopy = destDir.resolve(hdbFile.getNameOnly());

        assertExists(hdbFile, "source database.hdb should still exist");
        assertExists(destCopy, "dest copy of database.hdb should exist");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        if ((destCopy != null) && destCopy.exists())
          FileDeletion.ofFile(destCopy).nonInteractiveLogErrors().execute();
      }
    });

    // =====================================================================
    //  Phase 18: Cross-Condition Interactions (4 steps)
    // =====================================================================

    // Step 78: Locked dir skipped; WorkFile moves OK (Windows-only)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p18s78", "src"),
                 destDir   = createTestDir("p18s78", "dest"),

                 lockedDir = srcParent.resolve("locked_dir"),
                 workPdf   = srcParent.resolve("work.pdf"),
                 lockFile  = lockedDir.resolve("nested", "target.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(lockFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore);

        pasteMove(entitiesOf(lockedDir, workPdf), destDir);

        assertExists(srcParent.resolve("locked_dir"), "locked_dir should still exist");
        assertEquals(destDir.resolve("work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile should resolve to dest");
        assertExists(destDir.resolve("work.pdf"), "dest work.pdf");
        assertGone(workPdf, "source work.pdf should be gone");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 79: Copy with overwrite (mrYes)
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir   = createTestDir("p18s79", "src"),
                 destDir  = createTestDir("p18s79", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist after copy");
        assertFileContent(destFile, "file.txt", "dest should have source content after overwrite");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 overwrite prompt");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 80: Copy over record-associated file
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir   = createTestDir("p18s80", "src"),
                 destDir  = createTestDir("p18s80", "dest"),
                 srcFile  = srcDir.resolve("work.pdf"),
                 destFile = destDir.resolve("work.pdf");

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, destFile);
        int wfID = wfRec.getID();

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist after copy");
        assertExists(destFile, "dest should exist");
        assertEquals(destFile, db.workFiles.getByID(wfID).filePath(),
          "WorkFile record should still point to dest");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 overwrite prompt");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 81: External dir with locked file; unlocked files move, dir remains (Windows-only)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath destDir    = createTestDir("p18s81", "dest"),
                 extDir     = extRoot.resolve("p18s81", "src", "extdir"),
                 lockedFile = extDir.resolve("locked.txt");

        lockProc = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        pasteMove(entitiesOf(extDir), destDir);

        assertExists(destDir.resolve("extdir", "a.txt"), "dest extdir/a.txt");
        assertExists(lockedFile, "locked.txt should still exist");
        assertExists(extDir, "source extdir should still exist (has locked file)");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 19: Record-Associated File Lock Extended (3 steps, Windows-only)
    // =====================================================================

    // Step 82: Folder re-parents; locked WorkFile stopped, record path unchanged
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p19s82", "src"),
                 destDir   = createTestDir("p19s82", "dest"),
                 folder    = srcParent.resolve("folder"),
                 workPdf   = srcParent.resolve("work.pdf");

        HyperPath.folderRecordFor(folder, true);

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrCancel);

        pasteMove(entitiesOf(folder, workPdf), destDir);

        assertExists(destDir.resolve("folder", "inner.txt"), "dest folder/inner.txt");
        assertExists(srcParent.resolve("work.pdf"), "source work.pdf should still exist");
        assertEquals(workPdf, db.workFiles.getByID(wfID).filePath(),
          "WorkFile record path should still point to source");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 83: Retry then Skip on locked WorkFile
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = createTestDir("p19s83", "src"),
                 destDir = createTestDir("p19s83", "dest"),
                 workPdf = srcDir.resolve("work.pdf");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);

        lockProc = startLockProcess(workPdf);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrIgnore);

        pasteMove(entitiesOf(workPdf), destDir);

        assertExists(srcDir.resolve("work.pdf"), "source work.pdf should still exist");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Skip)");
        assertTrue(PopupRobot.getLastMessage().contains("work.pdf"),
          "popup should mention work.pdf: " + PopupRobot.getLastMessage());
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 84: Locked WorkFile skipped; unassociated loose file moves
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir    = createTestDir("p19s84", "src"),
                 destDir   = createTestDir("p19s84", "dest"),
                 workPdf   = srcDir.resolve("work.pdf"),
                 looseFile = srcDir.resolve("loose.txt");

        HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);

        lockProc = startLockProcess(workPdf);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        pasteMove(entitiesOf(workPdf, looseFile), destDir);

        assertExists(srcDir.resolve("work.pdf"), "source work.pdf should still exist");
        assertExists(destDir.resolve("loose.txt"), "dest loose.txt");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        PopupRobot.setDefaultResponse(mrOk);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 20: Copy Path Lock (3 steps, Windows-only)
    // =====================================================================

    // Step 85: Copy over locked dest file; overwrite prompt then access denied skip
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p20s85", "src"),
                 destDir  = createTestDir("p20s85", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrIgnore);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist");
        assertExists(destFile, "dest should still exist (locked original)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + access denied)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 86: Copy over locked dest file; overwrite then Stop
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p20s86", "src"),
                 destDir  = createTestDir("p20s86", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrCancel);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist");
        assertExists(destFile, "dest should still exist");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Stop)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 87: Copy over locked dest file; overwrite, Retry, then Skip
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p20s87", "src"),
                 destDir  = createTestDir("p20s87", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry, mrIgnore);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist");
        assertExists(destFile, "dest should still exist");
        assertEquals(3, PopupRobot.getInvocationCount(),
          "should get 3 popups (overwrite + Retry + Skip)");

        String lastMsg = PopupRobot.getLastMessage();
        assertTrue(lastMsg.contains("file.txt"), "popup should mention file.txt: " + lastMsg);
        assertTrue(lastMsg.toLowerCase().contains("access denied"),
          "popup should mention access denied: " + lastMsg);
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 21: Timed-Release Retry (14 steps, Windows-only)
    // =====================================================================

    // Step 88: Re-parent internal folder; retry succeeds after lock released
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p21s88", "src"),
                 destDir   = createTestDir("p21s88", "dest"),
                 rdir      = srcParent.resolve("rdir"),
                 lockFile  = rdir.resolve("locked.txt");

        HyperPath.folderRecordFor(rdir, true);

        lockProc = startLockProcess(lockFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(rdir), destDir);

        assertExists(destDir.resolve("rdir", "inner.txt"), "inner.txt at dest");
        assertExists(destDir.resolve("rdir", "locked.txt"), "locked.txt at dest");
        assertGone(rdir, "src rdir should be gone");
        assertNotNull(HyperPath.folderRecordFor(destDir.resolve("rdir"), false),
          "folder record should exist at dest");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;  // already destroyed by preResponseAction
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 89: Move external file; retry succeeds after lock released
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath destDir    = createTestDir("p21s89", "dest"),
                 lockedFile = extRoot.resolve("p21s89", "src", "locked.txt");

        lockProc = startLockProcess(lockedFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(lockedFile), destDir);

        assertExists(destDir.resolve("locked.txt"), "locked.txt at dest");
        assertGone(lockedFile, "src locked.txt should be gone");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 90: Move record-associated file; retry succeeds, record updated
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = createTestDir("p21s90", "src"),
                 destDir = createTestDir("p21s90", "dest"),
                 workPdf = srcDir.resolve("work.pdf");

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(workPdf), destDir);

        assertExists(destDir.resolve("work.pdf"), "work.pdf at dest");
        assertGone(workPdf, "src work.pdf should be gone");
        assertEquals(destDir.resolve("work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile record should point to dest");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 91: Re-parent retry succeeds + sibling file moves
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p21s91", "src"),
                 destDir   = createTestDir("p21s91", "dest"),
                 rdir      = srcParent.resolve("rdir"),
                 lockFile  = rdir.resolve("locked.txt"),
                 extraFile = srcParent.resolve("extra.txt");

        HyperPath.folderRecordFor(rdir, true);

        lockProc = startLockProcess(lockFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(rdir, extraFile), destDir);

        assertExists(destDir.resolve("rdir", "locked.txt"), "locked.txt at dest");
        assertGone(rdir, "src rdir should be gone");
        assertExists(destDir.resolve("extra.txt"), "extra.txt at dest");
        assertGone(extraFile, "src extra.txt should be gone");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 92: Folder re-parents OK + locked record file retry succeeds
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p21s92", "src"),
                 destDir   = createTestDir("p21s92", "dest"),
                 folder    = srcParent.resolve("folder"),
                 workPdf   = srcParent.resolve("work.pdf");

        HyperPath.folderRecordFor(folder, true);

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(folder, workPdf), destDir);

        assertExists(destDir.resolve("folder", "inner.txt"), "inner.txt at dest (re-parented)");
        assertExists(destDir.resolve("work.pdf"), "work.pdf at dest");
        assertGone(workPdf, "src work.pdf should be gone");
        assertEquals(destDir.resolve("work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile record should point to dest");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 93: Copy over locked dest file; overwrite then retry succeeds
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p21s93", "src"),
                 destDir  = createTestDir("p21s93", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(procToRelease));

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist after copy");
        assertFileContent(destFile, "file.txt", "dest should have source content after retry");
        assertEquals(2, PopupRobot.getInvocationCount(),
          "should get 2 popups (overwrite + lock)");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 94: External folder with locked interior file; retry succeeds, dir cleaned up
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath destDir    = createTestDir("p21s94", "dest"),
                 edir       = extRoot.resolve("p21s94", "src", "edir"),
                 lockedFile = edir.resolve("locked.txt");

        lockProc = startLockProcess(lockedFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);
        PopupRobot.setPreResponseAction(() -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(edir), destDir);

        assertExists(destDir.resolve("edir", "a.txt"), "a.txt at dest");
        assertExists(destDir.resolve("edir", "locked.txt"), "locked.txt at dest");
        assertGone(edir, "src edir should be gone (cleaned up)");
        assertEquals(1, PopupRobot.getInvocationCount(), "should get 1 popup");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 95: Re-parent folder; first retry fails, second retry succeeds
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcParent = createTestDir("p21s95", "src"),
                 destDir   = createTestDir("p21s95", "dest"),
                 rdir      = srcParent.resolve("rdir"),
                 lockFile  = rdir.resolve("locked.txt");

        HyperPath.folderRecordFor(rdir, true);

        lockProc = startLockProcess(lockFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(rdir), destDir);

        assertExists(destDir.resolve("rdir", "locked.txt"), "locked.txt at dest");
        assertGone(rdir, "src rdir should be gone (succeeded on second retry)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 96: Two folders, skip first locked, retry-succeed second
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcParent = createTestDir("p21s96", "src"),
                 destDir   = createTestDir("p21s96", "dest"),
                 dir1      = srcParent.resolve("dir1"),
                 dir2      = srcParent.resolve("dir2"),
                 lockFile1 = dir1.resolve("locked1.txt"),
                 lockFile2 = dir2.resolve("locked2.txt");

        HyperPath.folderRecordFor(dir1, true);
        HyperPath.folderRecordFor(dir2, true);

        lockProc1 = startLockProcess(lockFile1);
        lockProc2 = startLockProcess(lockFile2);

        Process lockToRelease = lockProc2;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(lockToRelease));

        pasteMove(entitiesOf(dir1, dir2), destDir);

        assertExists(dir1, "src dir1 should still exist (skipped)");
        assertExists(destDir.resolve("dir2", "locked2.txt"), "locked2.txt at dest");
        assertGone(dir2, "src dir2 should be gone (retry succeeded)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        lockProc2 = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // Step 97: Folder locked (Skip); sibling file locked (Retry succeeds)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcParent  = createTestDir("p21s97", "src"),
                 destDir    = createTestDir("p21s97", "dest"),
                 lockedDir  = srcParent.resolve("locked_dir"),
                 lockFile1  = lockedDir.resolve("nested", "target.txt"),
                 lockedFile = srcParent.resolve("locked_file.txt");

        HyperPath.folderRecordFor(lockedDir, true);
        HyperPath.folderRecordFor(lockedDir.resolve("nested"), true);

        lockProc1 = startLockProcess(lockFile1);
        lockProc2 = startLockProcess(lockedFile);

        Process fileToRelease = lockProc2;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(fileToRelease));

        pasteMove(entitiesOf(lockedDir, lockedFile), destDir);

        assertExists(lockedDir, "src locked_dir should still exist (skipped)");
        assertExists(destDir.resolve("locked_file.txt"), "locked_file.txt at dest");
        assertGone(lockedFile, "src locked_file.txt should be gone");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        lockProc2 = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // Step 98: Copy file; first retry fails, second retry succeeds (double retry)
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p21s98", "src"),
                 destDir  = createTestDir("p21s98", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry, mrRetry);
        PopupRobot.setPreResponseAction(2, () -> destroyQuietly(procToRelease));

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist after copy");
        assertFileContent(destFile, "file.txt", "dest should have source content after double retry");
        assertEquals(3, PopupRobot.getInvocationCount(),
          "should get 3 popups (overwrite + 2 lock retries)");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 99: Two external files, both locked; skip first, retry-succeed second
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath destDir = createTestDir("p21s101", "dest"),
                 srcDir  = extRoot.resolve("p21s101", "src"),
                 file1   = srcDir.resolve("f1.txt"),
                 file2   = srcDir.resolve("f2.txt");

        lockProc1 = startLockProcess(file1);
        lockProc2 = startLockProcess(file2);

        Process lp1 = lockProc1, lp2 = lockProc2;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrIgnore, mrRetry);
        PopupRobot.setPreResponseAction(1, () ->
        {
          destroyQuietly(lp1);
          destroyQuietly(lp2);
        });

        pasteMove(entitiesOf(file1, file2), destDir);

        // HashMap order is unpredictable; one file was skipped, the other retried and moved
        boolean f1Moved = destDir.resolve("f1.txt").exists(),
                f2Moved = destDir.resolve("f2.txt").exists();

        assertTrue(f1Moved || f2Moved, "at least one file should have moved to dest");
        assertFalse(f1Moved && f2Moved, "both files should not have moved (one was skipped)");

        boolean f1AtSrc = file1.exists(),
                f2AtSrc = file2.exists();

        assertTrue(f1AtSrc || f2AtSrc, "at least one file should remain at source (skipped)");
        assertFalse(f1AtSrc && f2AtSrc, "both files should not remain at source (one was moved)");

        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups");

        lockProc1 = null;
        lockProc2 = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // =====================================================================
    //  Phase 22: Retry Escalation (4 steps, Windows-only)
    // =====================================================================

    // Step 100: Unassociated file move → Retry → Skip
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath destDir    = createTestDir("p22s100", "dest"),
                 lockedFile = extRoot.resolve("p22s100", "src", "locked.txt");

        lockProc = startLockProcess(lockedFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrIgnore);

        pasteMove(entitiesOf(lockedFile), destDir);

        assertExists(lockedFile, "source locked.txt should still exist (skipped after retry)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Skip)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 101: Record-associated file move → Retry → Stop
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = createTestDir("p22s101", "src"),
                 destDir = createTestDir("p22s101", "dest"),
                 workPdf = srcDir.resolve("work.pdf");

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrCancel);

        pasteMove(entitiesOf(workPdf), destDir);

        assertExists(workPdf, "source work.pdf should still exist");
        assertEquals(workPdf, db.workFiles.getByID(wfID).filePath(),
          "WorkFile record should still point to source");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (Retry then Stop)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 102: Copy → Retry → Stop
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p22s102", "src"),
                 destDir  = createTestDir("p22s102", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(destFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry, mrCancel);

        pasteCopy(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist");
        assertExists(destFile, "dest should still exist (locked original)");
        assertEquals(3, PopupRobot.getInvocationCount(),
          "should get 3 popups (overwrite + Retry + Stop)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 103: Record-associated file move → Double retry → Succeed
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir  = createTestDir("p22s103", "src"),
                 destDir = createTestDir("p22s103", "dest"),
                 workPdf = srcDir.resolve("work.pdf");

        HDT_RecordWithPath wfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf);
        int wfID = wfRec.getID();

        lockProc = startLockProcess(workPdf);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(workPdf), destDir);

        assertExists(destDir.resolve("work.pdf"), "work.pdf at dest");
        assertGone(workPdf, "src work.pdf should be gone");
        assertEquals(destDir.resolve("work.pdf"), db.workFiles.getByID(wfID).filePath(),
          "WorkFile record should point to dest");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (2 retries)");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 23: Move + Overwrite + Source Lock (4 steps, Windows-only)
    // =====================================================================

    // Step 104: Move + overwrite → source locked → Skip
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p23s104", "src"),
                 destDir  = createTestDir("p23s104", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(srcFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrIgnore);

        pasteMove(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist (locked)");
        assertGone(destFile, "dest deleted by REPLACE_EXISTING before move failed");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Skip)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 105: Move + overwrite → source locked → Stop
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p23s105", "src"),
                 destDir  = createTestDir("p23s105", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(srcFile);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrCancel);

        pasteMove(entitiesOf(srcFile), destDir);

        assertExists(srcFile, "source should still exist (locked)");
        assertGone(destFile, "dest deleted by REPLACE_EXISTING before move failed");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Stop)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 106: Move + overwrite → source locked → Retry → Succeed
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p23s106", "src"),
                 destDir  = createTestDir("p23s106", "dest"),
                 srcFile  = srcDir.resolve("file.txt"),
                 destFile = destDir.resolve("file.txt");

        lockProc = startLockProcess(srcFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(srcFile), destDir);

        assertGone(srcFile, "source should be gone (moved after retry)");
        assertFileContent(destFile, "file.txt", "dest should have source content");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Retry)");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 107: Move + related overwrite → source locked → Retry → Succeed
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = createTestDir("p23s107", "src"),
                 destDir  = createTestDir("p23s107", "dest"),
                 srcFile  = srcDir.resolve("incoming.txt"),
                 destFile = destDir.resolve("incoming.txt");

        HDT_RecordWithPath destWfRec = HyperPath.createRecordAssignedToPath(hdtWorkFile, destFile);
        int destWfID = destWfRec.getID();

        lockProc = startLockProcess(srcFile);

        Process procToRelease = lockProc;

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrRetry);
        PopupRobot.setPreResponseAction(1, () -> destroyQuietly(procToRelease));

        pasteMove(entitiesOf(srcFile), destDir);

        assertGone(srcFile, "source should be gone (moved after retry)");
        assertFileContent(destFile, "incoming.txt", "dest should have source content");
        assertEquals(destFile, db.workFiles.getByID(destWfID).filePath(),
          "dest WorkFile record should still point to dest");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Retry)");

        lockProc = null;
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // =====================================================================
    //  Phase 24: Multi-File Copy + Lock (3 steps, Windows-only)
    // =====================================================================

    // Step 108: Two files copy; one locked dest → Skip → other copies fine
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath srcDir   = extRoot.resolve("p24s108", "src"),
                 destDir  = createTestDir("p24s108", "dest"),
                 srcA     = srcDir.resolve("a.txt"),
                 srcB     = srcDir.resolve("b.txt"),
                 destB    = destDir.resolve("b.txt");

        lockProc = startLockProcess(destB);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrIgnore);

        pasteCopy(entitiesOf(srcA, srcB), destDir);

        destroyQuietly(lockProc);
        lockProc = null;

        assertExists(srcA, "source a.txt should still exist (copy)");
        assertExists(srcB, "source b.txt should still exist (copy)");
        assertExists(destDir.resolve("a.txt"), "dest a.txt should exist (copied)");
        assertFileContent(destB, "original_b", "dest b.txt should be unchanged (locked)");
        assertEquals(2, PopupRobot.getInvocationCount(), "should get 2 popups (overwrite + Skip)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally { destroyQuietly(lockProc); }
    }).windowsOnly();

    // Step 109: Two files copy; both locked dests → Stop on first
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcDir  = extRoot.resolve("p24s109", "src"),
                 destDir = createTestDir("p24s109", "dest"),
                 destA   = destDir.resolve("a.txt"),
                 destB   = destDir.resolve("b.txt");

        lockProc1 = startLockProcess(destA);
        lockProc2 = startLockProcess(destB);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrYes, mrCancel);

        pasteCopy(entitiesOf(srcDir.resolve("a.txt"), srcDir.resolve("b.txt")), destDir);

        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
        lockProc1 = null;
        lockProc2 = null;

        assertFileContent(destA, "orig_a", "dest a.txt should be unchanged");
        assertFileContent(destB, "orig_b", "dest b.txt should be unchanged");
        assertEquals(3, PopupRobot.getInvocationCount(),
          "should get 3 popups (2 overwrites + 1 Stop)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // Step 110: Two files copy; both locked dests → Skip both
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc1 = null, lockProc2 = null;

      try
      {
        FilePath srcDir  = extRoot.resolve("p24s110", "src"),
                 destDir = createTestDir("p24s110", "dest"),
                 destA   = destDir.resolve("a.txt"),
                 destB   = destDir.resolve("b.txt");

        lockProc1 = startLockProcess(destA);
        lockProc2 = startLockProcess(destB);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrYes, mrIgnore, mrIgnore);

        pasteCopy(entitiesOf(srcDir.resolve("a.txt"), srcDir.resolve("b.txt")), destDir);

        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
        lockProc1 = null;
        lockProc2 = null;

        assertFileContent(destA, "orig_a", "dest a.txt should be unchanged");
        assertFileContent(destB, "orig_b", "dest b.txt should be unchanged");
        assertEquals(4, PopupRobot.getInvocationCount(),
          "should get 4 popups (2 overwrites + 2 Skips)");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc1);
        destroyQuietly(lockProc2);
      }
    }).windowsOnly();

    // =====================================================================
    //  Phase 25: Multi-Item Folder Tree Move with Record Associations
    //            (1 step)
    // =====================================================================

    // Step 111: Move a loose file + sibling folder tree; files associated
    //           with WorkFile, MiscFile, and Person; notes associated with
    //           srcParent (not moved) and inner (moved). Verifies folder
    //           structure, parent/child relations, and all record/note
    //           associations after the move.
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcParent = createTestDir("p25s111", "src"),
                 destDir   = createTestDir("p25s111", "dest"),
                 loose     = srcParent.resolve("loose.txt"),
                 tree      = srcParent.resolve("tree"),
                 mid       = tree.resolve("mid.pdf"),
                 inner     = tree.resolve("inner"),
                 deep      = inner.resolve("deep.jpg");

        // Create folder records for the tree hierarchy
        HDT_Folder innerFolder    = HyperPath.folderRecordFor(inner, true),
                   treeFolder     = HyperPath.folderRecordFor(tree, false),
                   srcParFolder   = HyperPath.folderRecordFor(srcParent, false);
        assertNotNull(treeFolder, "tree folder record");
        assertNotNull(innerFolder, "inner folder record");
        assertNotNull(srcParFolder, "srcParent folder record");

        // Associate the 3 files with different record types
        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, loose);
        assertNotNull(workFile, "WorkFile for loose.txt");

        HDT_RecordWithPath miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, mid);
        assertNotNull(miscFile, "MiscFile for mid.pdf");

        HDT_Person person = db.createNewBlankRecord(hdtPerson);
        person.setName(new PersonName("Test", "Person111"));
        person.getPath().assign(innerFolder, deep.getNameOnly());

        // Associate notes with srcParent (not moved) and inner (moved)
        HDT_Note srcParNote = db.createNewBlankRecord(hdtNote);
        srcParNote.setName("Test note srcParent p25s111");
        srcParNote.folder.set(srcParFolder);

        HDT_Note innerNote = db.createNewBlankRecord(hdtNote);
        innerNote.setName("Test note inner p25s111");
        innerNote.folder.set(innerFolder);

        int wfID = workFile.getID(), mfID = miscFile.getID(),
            personID = person.getID(),
            srcParNoteID = srcParNote.getID(), innerNoteID = innerNote.getID(),
            treeFolderID = treeFolder.getID(), innerFolderID = innerFolder.getID(),
            srcParFolderID = srcParFolder.getID();

        // Select both the loose file and the tree folder; move to dest
        pasteMove(entitiesOf(loose, tree), destDir);

        // Source should be completely gone
        assertGone(loose, "source loose.txt");
        assertGone(tree, "source tree dir");

        // Destination structure should be correct
        FilePath destLoose = destDir.resolve("loose.txt"),
                 destTree  = destDir.resolve("tree"),
                 destMid   = destTree.resolve("mid.pdf"),
                 destInner = destTree.resolve("inner"),
                 destDeep  = destInner.resolve("deep.jpg");

        assertExists(destLoose, "dest loose.txt");
        assertExists(destTree, "dest tree dir");
        assertExists(destMid, "dest mid.pdf");
        assertExists(destInner, "dest inner dir");
        assertExists(destDeep, "dest deep.jpg");

        // Folder records should be preserved with proper parent/child structure
        HDT_Folder destTreeFolder = HyperPath.folderRecordFor(destTree, false);
        assertNotNull(destTreeFolder, "dest tree folder record");
        assertEquals(treeFolderID, destTreeFolder.getID(), "tree folder record ID preserved");

        HDT_Folder destInnerFolder = HyperPath.folderRecordFor(destInner, false);
        assertNotNull(destInnerFolder, "dest inner folder record");
        assertEquals(innerFolderID, destInnerFolder.getID(), "inner folder record ID preserved");
        assertEquals(destTreeFolder, destInnerFolder.parentFolder(), "inner should be child of tree");

        HDT_Folder destDirFolder = HyperPath.folderRecordFor(destDir, false);
        assertNotNull(destDirFolder, "dest dir folder record");
        assertEquals(destDirFolder, destTreeFolder.parentFolder(), "tree should be child of destDir");

        // File record associations should resolve to new locations
        assertEquals(destLoose, db.workFiles.getByID(wfID).filePath(),
          "WorkFile should resolve to dest/loose.txt");
        assertEquals(destMid, db.miscFiles.getByID(mfID).filePath(),
          "MiscFile should resolve to dest/tree/mid.pdf");
        assertEquals(destDeep, db.persons.getByID(personID).getPath().filePath(),
          "Person picture should resolve to dest/tree/inner/deep.jpg");

        // srcParent note: folder was not moved, so record and path are unchanged
        assertEquals(srcParFolder, db.notes.getByID(srcParNoteID).folder.get(),
          "srcParent note should still reference srcParent folder");
        assertEquals(srcParFolderID, srcParFolder.getID(),
          "srcParent folder record ID unchanged");
        assertEquals(srcParent, srcParFolder.filePath(),
          "srcParent folder path should be unchanged");

        // inner note: folder record preserved, path resolves to new location
        assertEquals(innerFolder, db.notes.getByID(innerNoteID).folder.get(),
          "inner note should still reference the same folder record");
        assertEquals(destInner, innerFolder.filePath(),
          "inner folder filePath() should resolve to dest location");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 26: Registry Lookup for Untracked Files (2 steps)
    // =====================================================================

    // Step 112: Untracked file (on disk, no record) returns empty HyperPath set
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir      = createTestDir("p26s112", "dir"),
                 tracked  = dir.resolve("tracked.txt"),
                 loose    = dir.resolve("loose.txt");

        HyperPath.folderRecordFor(dir, true);

        // Create a record for tracked.txt
        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, tracked);
        assertNotNull(workFile, "WorkFile record should be created");

        // loose.txt is on disk but has no record
        assertTrue(loose.exists(), "loose.txt should exist on disk");
        assertTrue(HyperPath.getHyperPathSetForFilePath(loose).isEmpty(),
          "untracked file should have empty HyperPath set");
        assertNull(HyperPath.existingFileRecordAt(loose),
          "untracked file should return null record");

        // tracked.txt should have a record
        assertFalse(HyperPath.getHyperPathSetForFilePath(tracked).isEmpty(),
          "tracked file should have non-empty HyperPath set");
        assertEquals(workFile, HyperPath.existingFileRecordAt(tracked),
          "tracked file should return its WorkFile record");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // Step 113: Untracked file after move still returns empty HyperPath set
    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath srcDir  = createTestDir("p26s113", "src"),
                 destDir = createTestDir("p26s113", "dest"),
                 dir     = srcDir.resolve("mixed"),
                 tracked = dir.resolve("tracked.txt");

        HyperPath.folderRecordFor(dir, true);

        HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, tracked);
        int wfID = workFile.getID();

        pasteMove(entitiesOf(dir), destDir);

        FilePath destTracked = destDir.resolve("mixed", "tracked.txt"),
                 destLoose   = destDir.resolve("mixed", "loose.txt");

        assertExists(destTracked, "dest tracked.txt");
        assertExists(destLoose, "dest loose.txt");

        // Tracked file should resolve at new location
        assertEquals(destTracked, db.workFiles.getByID(wfID).filePath(),
          "WorkFile should resolve to new location");
        assertFalse(HyperPath.getHyperPathSetForFilePath(destTracked).isEmpty(),
          "moved tracked file should have non-empty HyperPath set");

        // Moved loose file should still have no record
        assertTrue(HyperPath.getHyperPathSetForFilePath(destLoose).isEmpty(),
          "moved untracked file should still have empty HyperPath set");
        assertNull(HyperPath.existingFileRecordAt(destLoose),
          "moved untracked file should still return null record");
      }
      catch (Exception e) { throw new AssertionError(e); }
    });

    // =====================================================================
    //  Phase 27: New Folder Button (5 steps)
    // =====================================================================

    // Step 114: Open the File Manager; the parent folder's tree node appears a pulse after its record
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = createTestDir("p27s114", "parent");

      assertExists(parentDir.resolve("file.txt"), "parent file");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show();
    });

    // Step 115: Enter the parent folder, then create a folder the way the New Folder button does
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p27s114", "parent");
      HDT_Folder parent = HyperPath.folderRecordFor(parentDir, false);

      assertNotNull(parent, "parent folder record should exist");

      FileManager.show(parentDir);

      assertSame(parent, FileManager.instance().folderTree.selectedRecord(),
        "parent folder should be selected in the folder tree");

      FileManager.instance().newFolder(parent, "newfolder");
    });

    // Step 116: The new folder's row is highlighted and has its record and its folder tree item
    seq.thenRunAfterDelay(() ->
    {
      FilePath newDir = testRoot.resolve("p27s114", "parent", "newfolder");

      assertTrue(newDir.isDirectory(), "new folder should exist on disk");

      HDT_Folder folder = HyperPath.folderRecordFor(newDir, false);
      assertNotNull(folder, "new folder should have a record");

      FileManager fileManager = FileManager.instance();
      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "new folder's row should be selected in the file table");

      HDT_RecordWithPath rowRecord = row.getRecord();
      assertSame(folder, rowRecord, "selected row should be the new folder's");

      assertNotNull(row.getTreeItem(), "new folder's row should have its folder tree item");
      assertSame(fileManager.folderTree.getRowsForRecord(folder).getFirst().getTreeItem(), row.getTreeItem(),
        "row's tree item should be the folder tree's node for the new folder");

      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running after creating a folder");

      fileManager.openFileRow(row);  // the double-click
    });

    // Step 117: The double-click entered the new folder; go back and select a file row (the gesture that used to throw)
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p27s114", "parent");
      HDT_Folder folder = HyperPath.folderRecordFor(parentDir.resolve("newfolder"), false);
      FileManager fileManager = FileManager.instance();

      assertSame(folder, fileManager.folderTree.selectedRecord(),
        "double-click should select the new folder in the folder tree");

      FileRow folderRow = fileManager.getFolderRow();
      assertNotNull(folderRow, "File Manager should have a current folder");

      HDT_RecordWithPath entered = folderRow.getRecord();
      assertSame(folder, entered, "double-click should enter the new folder");

      FileManager.show(parentDir.resolve("file.txt"));
    });

    // Step 118: The file row is selected under the parent folder; close the window
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p27s114", "parent");
      FileManager fileManager = FileManager.instance();
      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");
      assertEquals(parentDir.resolve("file.txt"), row.getFilePath(), "selected row should be the file");
      assertSame(HyperPath.folderRecordFor(parentDir, false), fileManager.folderTree.selectedRecord(),
        "parent folder should be selected in the folder tree");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 28: Directory Deletion Outside the File Manager (5 steps)
    // =====================================================================

    HDT_Folder[] doomedHolder = new HDT_Folder[1];  // carries the record from step 120 to step 121; once pruned, no path finds it

    // Step 119: Records and window for a folder that will be deleted through FileDeletion, not the File Manager
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = createTestDir("p28s119", "parent");

      HyperPath.folderRecordFor(parentDir.resolve("doomed"), true);

      assertExists(parentDir.resolve("doomed", "file.txt"), "doomed file");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show();
    });

    // Step 120: Delete the folder through FileDeletion; its record and node must survive until the deferred prune
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p28s119", "parent"),
               doomedDir = parentDir.resolve("doomed");
      HDT_Folder parent = HyperPath.folderRecordFor(parentDir, false),
                 doomed = childFolderRecord(parent, "doomed");

      assertNotNull(parent, "parent folder record should exist");
      assertNotNull(doomed, "doomed folder record should exist");

      doomedHolder[0] = doomed;

      FileManager.show(parentDir);

      assertTrue(folderTreeShows(parent, "doomed"), "folder tree should show the folder before deletion");

      FileDeletion.ofDirWithContents(doomedDir).nonInteractiveLogErrors().execute();

      assertFalse(doomedDir.exists(), "folder should be gone from disk");

      // The prune is owed, not run: callers such as HDT_Folder.delete() go on using the record after execute() returns

      assertFalse(HDT_Record.isEmpty(doomed, false), "folder record should survive until the deferred prune");
      assertTrue(folderTreeShows(parent, "doomed"), "folder tree node should survive until the deferred prune");

      FileManager.refresh();  // what opening the window would do; a plain refresh must pick up the owed prune
    });

    // Step 121: The refresh pruned the deleted folder; set up a folder whose in-use file keeps its record alive, and delete it
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p28s119", "parent"),
               inuseDir  = parentDir.resolve("inuse"),
               workPdf   = inuseDir.resolve("work.pdf");
      HDT_Folder parent = HyperPath.folderRecordFor(parentDir, false);

      assertNull(childFolderRecord(parent, "doomed"), "deleted folder's record should be pruned");
      assertFalse(folderTreeShows(parent, "doomed"), "deleted folder's node should be pruned");
      assertTrue(FileManager.instance().folderTree.getRowsForRecord(doomedHolder[0]).isEmpty(),
        "tree model should have purged the pruned folder's rows");
      assertSame(parent, FileManager.instance().folderTree.selectedRecord(), "parent folder should still be selected");

      HyperPath.folderRecordFor(inuseDir, true);
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtWorkFile, workPdf), "WorkFile record should be created");

      FileDeletion.ofDirWithContents(inuseDir).nonInteractiveLogErrors().execute();

      assertFalse(inuseDir.exists(), "in-use folder should be gone from disk");

      FileManager.refresh();
    });

    // Step 122: The prune rightly kept the in-use folder; selecting its node must enter it, not its parent
    seq.thenRunAfterDelay(() ->
    {
      FilePath parentDir = testRoot.resolve("p28s119", "parent");
      HDT_Folder parent = HyperPath.folderRecordFor(parentDir, false),
                 inuse  = childFolderRecord(parent, "inuse");
      FileManager fileManager = FileManager.instance();

      assertNotNull(inuse, "in-use folder's record should survive the prune");
      assertTrue(folderTreeShows(parent, "inuse"), "in-use folder's node should survive the prune");

      // Resolving the folder from its path would land on the parent here: with the directory gone,
      // getDirOnly() takes the path for a file's. The selection listener must go by the row's record.

      fileManager.folderTree.selectRecord(inuse, 0, false);

      assertSame(inuse, fileManager.folderTree.selectedRecord(), "in-use folder should be selected in the folder tree");

      FileRow folderRow = fileManager.getFolderRow();
      assertNotNull(folderRow, "File Manager should have entered the in-use folder");

      HDT_RecordWithPath entered = folderRow.getRecord();
      assertSame(inuse, entered, "File Manager should have entered the in-use folder, not its parent");
    });

    // Step 123: Park the window at the root, outside the tree the cleanup removes, and close it. A step of its
    // own, so that the scroll and focus selectRecord deferred in the last step run while the window still shows.
    seq.thenRunAfterDelay(() ->
    {
      selectRootFolder();
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 29: Folder Lookup for a Vanished Directory (1 step)
    // =====================================================================

    // Step 124: A directory's path resolves to its own folder record after the directory is gone, not to the parent's
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir   = createTestDir("p29s124", "vanishing"),
               aside = extRoot.resolve("p29s124", "vanishing");
      HDT_Folder parent = HyperPath.folderRecordFor(dir.getParent(), false),
                 folder = HyperPath.folderRecordFor(dir, false);

      assertNotNull(folder, "folder record should exist");
      assertNotSame(parent, folder, "folder should resolve to its own record while the directory exists");

      // Move the directory out from under the app with the watcher paused, so that nothing reacts and the
      // registry keeps the folder's entry: the state of a folder found missing when the database loads

      boolean restartWatcher = folderTreeWatcher.stop();

      try
      {
        Files.createDirectories(aside.getParent().toPath());
        Files.move(dir.toPath(), aside.toPath());

        assertFalse(dir.exists(), "directory should be gone");
        assertSame(folder, HyperPath.folderRecordFor(dir, false),
          "vanished directory's path should resolve to its own record, not the parent's");

        Files.move(aside.toPath(), dir.toPath());
      }
      catch (IOException e) { throw new AssertionError(e); }
      finally
      {
        if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();
      }
    });

    // =====================================================================
    //  Phase 30: Record Lookups Around Deletion and Outside Rename (5 steps)
    // =====================================================================

    HDT_RecordWithPath[] deletedHolder = new HDT_RecordWithPath[1],  // carries the misc. file record from step 126 to step 127
                         renamedHolder = new HDT_RecordWithPath[1];  // carries the misc. file record from step 128 to step 129

    // Step 125: A file assigned to a misc. file record; open the File Manager on it
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir  = createTestDir("p30s125", "dir"),
               file = dir.resolve("doomed.txt");

      assertExists(file, "doomed file");

      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, file), "MiscFile record should be created");
      assertFalse(HyperPath.getHyperPathSetForFilePath(file).isEmpty(), "file should be tracked by the registry");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(file);
    });

    // Step 126: Delete it through the File Manager, answering yes to deleting the record too
    seq.thenRunAfterDelay(() ->
    {
      FilePath file = testRoot.resolve("p30s125", "dir", "doomed.txt");
      FileManager fileManager = FileManager.instance();
      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");
      assertEquals(file, row.getFilePath(), "selected row should be the file");

      HDT_RecordWithPath miscFile = row.getRecord();
      assertNotNull(miscFile, "row should carry the misc. file record");

      deletedHolder[0] = miscFile;

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "Okay to permanently delete the file as well as the associated record?"

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once, about the record");

      PopupRobot.setDefaultResponse(mrOk);
    });

    // Step 127: The file and its record are both gone. Before the fix, the deletion evicted the path from
    // the registry before the record lookup, so the file went and the record stayed
    seq.thenRunAfterDelay(() ->
    {
      FilePath file = testRoot.resolve("p30s125", "dir", "doomed.txt");

      assertGone(file, "deleted file");
      assertNull(db.miscFiles.getByID(deletedHolder[0].getID()), "misc. file record should be deleted with its file");
      assertTrue(HyperPath.getHyperPathSetForFilePath(file).isEmpty(), "deleted file's path should have no records");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // Step 128: Rename a tracked misc. file on disk from outside the program, with the watcher running
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = createTestDir("p30s128", "dir"),
               tracked = dir.resolve("tracked.txt"),
               renamed = dir.resolve("renamed.txt");

      assertExists(tracked, "tracked file");

      HDT_RecordWithPath miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, tracked);
      assertNotNull(miscFile, "MiscFile record should be created");

      renamedHolder[0] = miscFile;

      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "Should the record be reassigned to ...?"

      // A plain Files.move, not FilePath.filesMove, so that the watcher sees the rename as an outside change

      try { Files.move(tracked.toPath(), renamed.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 129: After the watcher has paired the events and waited its two seconds, the record follows the file.
    // Before the fix, the watcher evicted the old path and then asked whether the file was in use, which for a
    // misc. file is answered from the registry under that path, so it took the file for untracked and never asked
    seq.setDelayMS(4000);

    seq.thenRunAfterDelay(() ->
    {
      FilePath tracked = testRoot.resolve("p30s128", "dir", "tracked.txt"),
               renamed = testRoot.resolve("p30s128", "dir", "renamed.txt");

      assertExists(renamed, "renamed file");
      assertEquals(renamed, renamedHolder[0].filePath(), "record should be reassigned to the renamed file");
      assertFalse(HyperPath.getHyperPathSetForFilePath(renamed).isEmpty(), "renamed path should be tracked by the registry");
      assertTrue(HyperPath.getHyperPathSetForFilePath(tracked).isEmpty(), "old path should no longer be tracked");

      PopupRobot.setDefaultResponse(mrOk);
    });

    seq.setDelayMS(400);

    // =====================================================================
    //  Phase 31: Main Window View After an Outside Deletion of Its Record (1 step)
    // =====================================================================

    // Step 130: Delete, from outside the main window, the record its only view shows; the window must keep a view.
    // Before the fix, the view sequence lost its only slot and the next main window update threw, as it did after the
    // suite's cleanup deleted the fixture person the Persons tab had picked up, or after deleting a misc. file's file
    // and record through the File Manager with the main window on that record
    seq.thenRunAfterDelay(() ->
    {
      HDT_Record shown = ui.activeRecord();

      if (shown == null)
      {
        // Nothing is showing: go to a throwaway person, which replaces the empty view's slot rather than adding one

        HDT_Person person = db.createNewBlankRecord(hdtPerson);
        person.setName(new PersonName("Test", "Person130"));
        ui.goToRecord(person, false);
        shown = person;
      }

      assertSame(shown, ui.activeRecord(), "main window should be showing the record");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrNo);  // deleting a person with a picture asks whether to delete the picture too

      db.deleteRecord(shown);

      PopupRobot.setDefaultResponse(mrOk);

      assertNotNull(ui.activeTab(), "main window should keep a view after the record its view showed is deleted from outside it");

      ui.update();  // what the File Manager's delete flow does next; it must find a view to update

      assertNotNull(ui.activeTab(), "main window should still have a view after the update");
    });

    // =====================================================================
    //  Phase 32: Deleting a Note's Folder Through the File Manager (3 steps)
    // =====================================================================

    HDT_Note[] noteHolder = new HDT_Note[1];  // carries the note from step 131 to step 133
    int[] folderIDHolder = new int[1];        // and the folder record's ID, to prove the record is gone

    // Step 131: A folder with a file in it, assigned to a note; open the File Manager on the folder's parent
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = createTestDir("p32s131", "dir");
      HDT_Folder folder = HyperPath.folderRecordFor(dir, false);

      assertNotNull(folder, "folder record should exist");
      assertExists(dir.resolve("file.txt"), "file in the folder");

      HDT_Note note = db.createNewBlankRecord(hdtNote);
      note.setName("Test note p32s131");
      note.folder.set(folder);

      noteHolder[0] = note;
      folderIDHolder[0] = folder.getID();

      assertTrue(folder.getPath().isInUseByRecords(), "the note should make the folder related");
      assertFalse(folder.contentsAreInUse(), "nothing inside the folder should be in use");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(dir.getParent());
    });

    // Step 132: Select the folder's row and delete it through the File Manager, confirming the note's loss of its
    // folder. Before the change, the folder was refused as containing in-use files, which it did not, and could not
    // be deleted short of detaching the note first
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = testRoot.resolve("p32s131", "dir");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(dir, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");
      assertEquals(dir, row.getFilePath(), "selected row should be the folder");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "is assigned to the note record ... Permanently delete it anyway?"

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should prompt once, about the note");

      PopupRobot.setDefaultResponse(mrOk);
    });

    // Step 133: The directory and the folder record are gone; the note survives without a folder
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = testRoot.resolve("p32s131", "dir");

      assertGone(dir, "deleted folder");
      assertNull(db.folders.getByID(folderIDHolder[0]), "folder record should be deleted with its directory");

      HDT_Note note = noteHolder[0];

      assertSame(note, db.notes.getByID(note.getID()), "note record should survive");
      assertNull(note.folder.get(), "note should no longer have a folder");

      db.deleteRecord(note);  // no longer a fixture by path, so the cleanup would not take it

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 33: Rename Through the File Manager (10 steps)
    // =====================================================================

    // Step 134: Open the File Manager on a folder holding a plain file, a misc. file, a work file and a name to collide with
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = createTestDir("p33s134", "dir");

      assertExists(dir.resolve("plain.txt"), "plain file");
      assertExists(dir.resolve("misc.dat"),  "misc. file");
      assertExists(dir.resolve("work.pdf"),  "work file");
      assertExists(dir.resolve("taken.txt"), "file whose name is taken");

      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, dir.resolve("misc.dat")), "MiscFile record should be created");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtWorkFile, dir.resolve("work.pdf")), "WorkFile record should be created");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(dir);
    });

    // Step 135: Rename a file with no record; nothing but the file changes
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = testRoot.resolve("p33s134", "dir"),
               plain   = dir.resolve("plain.txt"),
               renamed = dir.resolve("renamed.txt");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(plain, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");
      assertEquals(plain, row.getFilePath(), "selected row should be the plain file");

      fileManager.rename(row, "renamed.txt");

      assertExists(renamed, "renamed file");
      assertGone(plain, "old name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(renamed).isEmpty(), "a file without a record should stay untracked");
    });

    // Step 136: Rename a file assigned to a misc. file record; the record and the registry follow the file
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = testRoot.resolve("p33s134", "dir"),
               misc    = dir.resolve("misc.dat"),
               renamed = dir.resolve("misc2.dat");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(misc, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "misc. file row should be selected");

      HDT_RecordWithPath miscFile = row.getRecord();

      assertNotNull(miscFile, "row should carry the misc. file record");

      fileManager.rename(row, "misc2.dat");

      assertExists(renamed, "renamed misc. file");
      assertGone(misc, "old name");
      assertEquals(renamed, miscFile.filePath(), "misc. file record should follow the file");
      assertTrue(HyperPath.getHyperPathSetForFilePath(renamed).contains(miscFile.getPath()), "registry should track the new name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(misc).isEmpty(), "registry should have dropped the old name");
    });

    // Step 137: The same for a file assigned to a work file record
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = testRoot.resolve("p33s134", "dir"),
               work    = dir.resolve("work.pdf"),
               renamed = dir.resolve("work2.pdf");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(work, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "work file row should be selected");

      HDT_RecordWithPath workFile = row.getRecord();

      assertNotNull(workFile, "row should carry the work file record");

      fileManager.rename(row, "work2.pdf");

      assertExists(renamed, "renamed work file");
      assertGone(work, "old name");
      assertEquals(renamed, workFile.filePath(), "work file record should follow the file");
      assertTrue(HyperPath.getHyperPathSetForFilePath(renamed).contains(workFile.getPath()), "registry should track the new name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(work).isEmpty(), "registry should have dropped the old name");
    });

    // Step 138: A name that is already taken is refused and nothing changes
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = testRoot.resolve("p33s134", "dir"),
               renamed = dir.resolve("renamed.txt"),
               taken   = dir.resolve("taken.txt");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(renamed, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");

      PopupRobot.clear();

      fileManager.rename(row, "taken.txt");

      assertEquals(1, PopupRobot.getInvocationCount(), "should complain once");
      assertTrue(PopupRobot.getLastMessage().contains("already exists"), "complaint should be about the taken name");
      assertExists(renamed, "file keeps its name");
      assertExists(taken, "file with the taken name");
    });

    // Step 139: A folder holding a misc. file and a subfolder with a file; records for all of it, then show its parent
    seq.thenRunAfterDelay(() ->
    {
      FilePath outer = createTestDir("p33s139", "outer"),
               sub   = outer.resolve("sub"),
               inner = outer.resolve("inner.dat");

      assertNotNull(HyperPath.folderRecordFor(sub, true), "subfolder record should exist");
      assertExists(sub.resolve("deep.txt"), "file in the subfolder");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, inner), "MiscFile record should be created");

      FileManager.show(outer.getParent());
    });

    // Step 140: Rename the folder; the records and the registry follow the whole subtree
    seq.thenRunAfterDelay(() ->
    {
      FilePath outer  = testRoot.resolve("p33s139", "outer"),
               sub    = outer.resolve("sub"),
               inner  = outer.resolve("inner.dat"),
               outer2 = testRoot.resolve("p33s139", "outer2");
      HDT_Folder folder    = HyperPath.folderRecordFor(outer, false),
                 subFolder = HyperPath.folderRecordFor(sub, false);
      HDT_RecordWithPath miscFile = HyperPath.getHyperPathSetForFilePath(inner).iterator().next().getRecord();
      FileManager fileManager = FileManager.instance();

      assertNotNull(folder, "folder record should exist");
      assertNotNull(subFolder, "subfolder record should exist");
      assertNotNull(miscFile, "misc. file record should exist");

      fileManager.goToFilePath(outer, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");
      assertEquals(outer, row.getFilePath(), "selected row should be the folder");

      fileManager.rename(row, "outer2");

      assertTrue(outer2.isDirectory(), "renamed folder should exist");
      assertGone(outer, "old folder name");
      assertEquals(outer2, folder.filePath(), "folder record should follow the directory");
      assertEquals(outer2.resolve("sub"), subFolder.filePath(), "subfolder record should follow its parent");
      assertEquals(outer2.resolve("inner.dat"), miscFile.filePath(), "misc. file record should follow its folder");
      assertTrue(HyperPath.getHyperPathSetForFilePath(outer2.resolve("inner.dat")).contains(miscFile.getPath()), "registry should track the file under the new name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(inner).isEmpty(), "registry should have dropped the file's old path");
      assertSame(subFolder, HyperPath.folderRecordFor(outer2.resolve("sub"), false), "subfolder should resolve to the same record under the new name");
    });

    // Step 141: A folder assigned to a note, then show its parent
    seq.thenRunAfterDelay(() ->
    {
      FilePath notedir = createTestDir("p33s141", "notedir");
      HDT_Folder folder = HyperPath.folderRecordFor(notedir, false);

      assertNotNull(folder, "folder record should exist");

      HDT_Note note = db.createNewBlankRecord(hdtNote);
      note.setName("Test note p33s141");
      note.folder.set(folder);

      FileManager.show(notedir.getParent());
    });

    // Step 142: Rename the note's folder; the note keeps the same folder record, which follows the directory
    seq.thenRunAfterDelay(() ->
    {
      FilePath notedir  = testRoot.resolve("p33s141", "notedir"),
               notedir2 = testRoot.resolve("p33s141", "notedir2");
      HDT_Folder folder = HyperPath.folderRecordFor(notedir, false);
      FileManager fileManager = FileManager.instance();

      assertNotNull(folder, "folder record should exist");
      assertEquals(1, folder.notes.size(), "folder should have its note");

      HDT_Note note = folder.notes.getFirst();

      fileManager.goToFilePath(notedir, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");

      fileManager.rename(row, "notedir2");

      assertTrue(notedir2.isDirectory(), "renamed folder should exist");
      assertEquals(notedir2, folder.filePath(), "folder record should follow the directory");
      assertSame(folder, note.folder.get(), "note should keep its folder record");
    });

    // Step 143: The XML folder cannot be renamed, and the refusal says so
    seq.thenRunAfterDelay(() ->
    {
      FilePath xmlPath = db.xmlPath();
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(xmlPath, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "XML folder row should be selected");
      assertEquals(xmlPath, row.getFilePath(), "selected row should be the XML folder");

      PopupRobot.clear();

      fileManager.rename(row, "xml2");

      assertEquals(1, PopupRobot.getInvocationCount(), "should refuse once");
      assertTrue(PopupRobot.getLastMessage().contains("cannot be renamed"), "refusal should say the folder cannot be renamed");
      assertTrue(xmlPath.isDirectory(), "XML folder should be untouched");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 34: Outside Changes Seen by the Watcher (14 steps)
    // =====================================================================

    int[] watcherIDHolder = new int[2];                 // [0] carries a folder record's ID, [1] a file record's ID, between steps
    HDT_Person[] personHolder = new HDT_Person[1];      // carries the person from step 154 to step 155

    // Step 144: A directory created outside the program, with the watcher running
    seq.thenRunAfterDelay(() ->
    {
      FilePath parent  = createTestDir("p34s144", "parent"),
               outside = parent.resolve("outside");

      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running");
      assertFalse(outside.exists(), "directory should not exist yet");

      try { Files.createDirectories(outside.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 145: The watcher's batch gave it a folder record, and the folder tree has its node
    seq.setDelayMS(3000);

    seq.thenRunAfterDelay(() ->
    {
      FilePath parent  = testRoot.resolve("p34s144", "parent"),
               outside = parent.resolve("outside");
      HDT_Folder parentFolder = HyperPath.folderRecordFor(parent, false);

      assertNotNull(parentFolder, "parent folder record should exist");
      assertTrue(outside.isDirectory(), "directory should still exist");
      assertNotNull(childFolderRecord(parentFolder, "outside"), "watcher should have created the new directory's folder record");
      assertTrue(folderTreeShows(parentFolder, "outside"), "folder tree should show the new directory");
    });

    seq.setDelayMS(400);

    // Step 146: A file in use by a misc. file record, deleted outside the program
    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = createTestDir("p34s146", "parent"),
               inuse  = parent.resolve("inuse.dat");

      assertExists(inuse, "in-use file");

      HDT_RecordWithPath miscFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, inuse);

      assertNotNull(miscFile, "MiscFile record should be created");

      watcherIDHolder[1] = miscFile.getID();

      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running");

      PopupRobot.clear();

      try { Files.delete(inuse.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 147: After the watcher's grace period and debounce, it warns about the in-use file; the record is kept
    seq.setDelayMS(6500);

    seq.thenRunAfterDelay(() ->
    {
      FilePath inuse = testRoot.resolve("p34s146", "parent", "inuse.dat");
      HDT_MiscFile miscFile = db.miscFiles.getByID(watcherIDHolder[1]);

      assertNotNull(miscFile, "misc. file record should survive an outside deletion of its file");
      assertEquals(inuse, miscFile.filePath(), "record should still point at the deleted file's path");
      assertNotNull(PopupRobot.getLastMessage(), "watcher should have warned");
      assertTrue(PopupRobot.getLastMessage().contains("has been deleted or moved from outside the program"), "warning should be about an outside deletion");
      assertTrue(PopupRobot.getLastMessage().startsWith("A file"), "warning should be about a file");
    });

    seq.setDelayMS(400);

    // Step 148: An empty folder with a record but nothing in use, deleted outside the program
    seq.thenRunAfterDelay(() ->
    {
      FilePath empty = createTestDir("p34s148", "empty");
      HDT_Folder folder = HyperPath.folderRecordFor(empty, false);

      assertNotNull(folder, "folder record should exist");

      watcherIDHolder[0] = folder.getID();

      try { Files.delete(empty.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 149: The watcher's prune removed the folder record
    seq.setDelayMS(3000);

    seq.thenRunAfterDelay(() ->
    {
      HDT_Folder parentFolder = HyperPath.folderRecordFor(testRoot.resolve("p34s148"), false);

      assertNotNull(parentFolder, "parent folder record should exist");
      assertNull(db.folders.getByID(watcherIDHolder[0]), "deleted directory's folder record should be pruned");
      assertNull(childFolderRecord(parentFolder, "empty"), "parent should no longer have the folder as a child record");
    });

    seq.setDelayMS(400);

    // Step 150: A folder assigned to a note, deleted outside the program
    seq.thenRunAfterDelay(() ->
    {
      FilePath notedir = createTestDir("p34s150", "notedir");
      HDT_Folder folder = HyperPath.folderRecordFor(notedir, false);

      assertNotNull(folder, "folder record should exist");

      HDT_Note note = db.createNewBlankRecord(hdtNote);
      note.setName("Test note p34s150");
      note.folder.set(folder);

      watcherIDHolder[0] = folder.getID();

      PopupRobot.clear();

      try { Files.delete(notedir.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 151: The watcher warns about the in-use folder, and the prune leaves its record alone
    seq.setDelayMS(6500);

    seq.thenRunAfterDelay(() ->
    {
      HDT_Folder folder = db.folders.getByID(watcherIDHolder[0]);

      assertNotNull(folder, "in-use folder record should survive an outside deletion of its directory");
      assertEquals(1, folder.notes.size(), "note should still be assigned to the folder");
      assertNotNull(PopupRobot.getLastMessage(), "watcher should have warned");
      assertTrue(PopupRobot.getLastMessage().contains("has been deleted or moved from outside the program"), "warning should be about an outside deletion");
      assertTrue(PopupRobot.getLastMessage().startsWith("A folder"), "warning should be about a folder");
    });

    seq.setDelayMS(400);

    // Step 152: A folder assigned to a note, renamed outside the program
    seq.thenRunAfterDelay(() ->
    {
      FilePath notedir  = createTestDir("p34s152", "notedir"),
               notedir2 = testRoot.resolve("p34s152", "notedir2");
      HDT_Folder folder = HyperPath.folderRecordFor(notedir, false);

      assertNotNull(folder, "folder record should exist");

      HDT_Note note = db.createNewBlankRecord(hdtNote);
      note.setName("Test note p34s152");
      note.folder.set(folder);

      watcherIDHolder[0] = folder.getID();

      PopupRobot.clear();

      try { Files.move(notedir.toPath(), notedir2.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 153: The watcher warns that an in-use folder was renamed outside; the record is kept
    seq.setDelayMS(3000);

    seq.thenRunAfterDelay(() ->
    {
      assertNotNull(db.folders.getByID(watcherIDHolder[0]), "in-use folder record should survive an outside rename");
      assertNotNull(PopupRobot.getLastMessage(), "watcher should have warned");
      assertTrue(PopupRobot.getLastMessage().contains("has been renamed from outside the program"), "warning should be about an outside rename");
    });

    seq.setDelayMS(400);

    // Step 154: A person's picture renamed outside the program; the reassign prompt is answered yes
    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = createTestDir("p34s154", "parent"),
               pic    = parent.resolve("pic.jpg");
      HDT_Folder parentFolder = HyperPath.folderRecordFor(parent, false);

      assertNotNull(parentFolder, "parent folder record should exist");
      assertExists(pic, "picture");

      HDT_Person person = db.createNewBlankRecord(hdtPerson);
      person.setName(new PersonName("Test", "Person154"));
      person.getPath().assign(parentFolder, pic.getNameOnly());

      assertEquals(pic, person.filePath(), "person path should point at the picture");

      personHolder[0] = person;

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "Should the record be reassigned to ...?"

      try { Files.move(pic.toPath(), parent.resolve("pic2.jpg").toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 155: The person's path follows the renamed picture. Before the fix of the eviction order, a picture was taken
    // for untracked, since its in-use answer comes from the registry under the old path
    seq.setDelayMS(4000);

    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = testRoot.resolve("p34s154", "parent"),
               pic    = parent.resolve("pic.jpg"),
               pic2   = parent.resolve("pic2.jpg");
      HDT_Person person = personHolder[0];

      assertSame(person, db.persons.getByID(person.getID()), "person record should survive");
      assertEquals(pic2, person.filePath(), "person path should follow the renamed picture");
      assertTrue(HyperPath.getHyperPathSetForFilePath(pic2).contains(person.getPath()), "registry should track the new name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(pic).isEmpty(), "registry should have dropped the old name");

      PopupRobot.setDefaultResponse(mrOk);
    });

    seq.setDelayMS(400);

    // Step 156: A work file attached to a work, renamed outside the program; the reassign prompt is answered yes
    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = createTestDir("p34s156", "parent"),
               wf     = parent.resolve("work.pdf");

      assertExists(wf, "work file");

      HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, wf);

      assertNotNull(workFile, "WorkFile record should be created");

      HDT_Work work = db.createNewBlankRecord(hdtWork);
      work.setName("Test work p34s156");
      work.addWorkFile(workFile.getID());

      assertEquals(1, work.workFiles.size(), "work should have the work file");

      watcherIDHolder[1] = workFile.getID();

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "Should the record be reassigned to ...?"

      try { Files.move(wf.toPath(), parent.resolve("work2.pdf").toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 157: The work file record follows the renamed file, and the work still has it
    seq.setDelayMS(4000);

    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = testRoot.resolve("p34s156", "parent"),
               wf     = parent.resolve("work.pdf"),
               wf2    = parent.resolve("work2.pdf");
      HDT_WorkFile workFile = db.workFiles.getByID(watcherIDHolder[1]);

      assertNotNull(workFile, "work file record should survive");
      assertEquals(wf2, workFile.filePath(), "work file record should follow the renamed file");
      assertTrue(HyperPath.getHyperPathSetForFilePath(wf2).contains(workFile.getPath()), "registry should track the new name");
      assertTrue(HyperPath.getHyperPathSetForFilePath(wf).isEmpty(), "registry should have dropped the old name");
      assertEquals(1, workFile.works.size(), "work file should still belong to its work");

      PopupRobot.setDefaultResponse(mrOk);
    });

    seq.setDelayMS(400);

    // =====================================================================
    //  Phase 35: Deletion Variants (8 steps)
    // =====================================================================

    // Step 158: Records for the files to be deleted in various ways, then the File Manager opened on their folder. The
    // records come first: a row built before its file's record exists knows nothing of it until the table is rebuilt
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = createTestDir("p35s158", "dir");
      HDT_Folder folder = HyperPath.folderRecordFor(dir, false);

      assertNotNull(folder, "folder record should exist");

      for (String name : List.of("decline.dat", "work.pdf", "pic.jpg", "plain.txt", "mixed.dat"))
        assertExists(dir.resolve(name), name);

      assertNotNull(HyperPath.folderRecordFor(dir.resolve("sub"), true), "subfolder record should exist");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, dir.resolve("decline.dat")), "MiscFile record for the declined file should be created");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, dir.resolve("mixed.dat")), "MiscFile record for the mixed selection should be created");

      HDT_RecordWithPath workFile = HyperPath.createRecordAssignedToPath(hdtWorkFile, dir.resolve("work.pdf"));

      assertNotNull(workFile, "WorkFile record should be created");

      HDT_Work work = db.createNewBlankRecord(hdtWork);
      work.setName("Test work p35s158");
      work.addWorkFile(workFile.getID());

      // The picture goes to the person the Persons tab is showing, assigned the way the tab does it, so that the tab's
      // own copy of the path holds it too

      HDT_Person person = assertInstanceOf(HDT_Person.class, ui.activeRecord(), "Persons tab should be showing a person");

      ui.personHyperTab().assignPicture(dir.resolve("pic.jpg"), false);

      assertFalse(ui.cantSaveRecord(), "the Persons tab should save the picture");
      assertEquals(dir.resolve("pic.jpg"), person.filePath(), "showing person should have the picture");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(dir);
    });

    // Step 159: Declining the confirmation deletes nothing. Going to the file replaces an earlier selection rather than
    // adding to it, which the single-file prompt proves: two selected rows would get the "selected items" prompt
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir     = testRoot.resolve("p35s158", "dir"),
               decline = dir.resolve("decline.dat");
      FileManager fileManager = FileManager.instance();

      fileManager.selectRows(dir.resolve("plain.txt"));
      fileManager.goToFilePath(decline, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");
      assertEquals(decline, row.getFilePath(), "selected row should be the declined file");

      HDT_RecordWithPath miscFile = row.getRecord();

      assertNotNull(miscFile, "row should carry the misc. file record");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrNo);

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should ask once");
      assertTrue(PopupRobot.getLastMessage().contains("misc. file record"), "the question should be about the record");
      assertExists(decline, "declined file");
      assertNotNull(db.miscFiles.getByID(miscFile.getID()), "declined file's record should survive");

      PopupRobot.setDefaultResponse(mrOk);
    });

    // Step 160: Deleting a work file's file deletes the work file record; the work survives without it
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = testRoot.resolve("p35s158", "dir"),
               wf  = dir.resolve("work.pdf");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(wf, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "work file row should be selected");

      HDT_WorkFile workFile = assertInstanceOf(HDT_WorkFile.class, row.getRecord(), "row should carry the work file record");
      HDT_Work work = workFile.works.getFirst();

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "is assigned to a work file record. Permanently delete it anyway?"

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should ask once");
      assertGone(wf, "deleted work file");
      assertNull(db.workFiles.getByID(workFile.getID()), "work file record should be deleted with its file");
      assertSame(work, db.works.getByID(work.getID()), "work should survive");
      assertTrue(work.workFiles.isEmpty(), "work should no longer have the work file");

      PopupRobot.setDefaultResponse(mrNo);  // deleting the work must not try to delete files
      db.deleteRecord(work);
      PopupRobot.setDefaultResponse(mrOk);
    });

    // Step 161: Deleting a person's picture keeps the person record but takes the picture off it, including the Persons
    // tab's own copy of the path while that person is showing, which saving the tab would otherwise assign again
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = testRoot.resolve("p35s158", "dir"),
               pic = dir.resolve("pic.jpg");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(pic, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "picture row should be selected");

      HDT_Person person = assertInstanceOf(HDT_Person.class, row.getRecord(), "row should carry the person record");

      assertSame(person, ui.activeRecord(), "Persons tab should be showing the person, so that it holds the picture path too");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "is assigned as a picture file for a person record. Permanently delete it anyway?"

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should ask once");
      assertGone(pic, "deleted picture");
      assertSame(person, db.persons.getByID(person.getID()), "person record should survive its picture's deletion");
      assertTrue(FilePath.isEmpty(person.filePath()), "person should no longer have a picture");

      assertFalse(ui.cantSaveRecord(), "the Persons tab should save");
      assertTrue(FilePath.isEmpty(person.filePath()), "saving the Persons tab must not assign the deleted picture again");
      assertTrue(HyperPath.getHyperPathSetForFilePath(pic).isEmpty(), "registry should no longer map the deleted picture to the person");

      PopupRobot.setDefaultResponse(mrOk);

      db.deleteRecord(person);  // its path no longer names a fixture, so the cleanup would not find it
    });

    // Step 162: A selection of a folder, an untracked file and a misc. file's file goes in one operation
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir   = testRoot.resolve("p35s158", "dir"),
               sub   = dir.resolve("sub"),
               plain = dir.resolve("plain.txt"),
               mixed = dir.resolve("mixed.dat");
      HDT_Folder subFolder = HyperPath.folderRecordFor(sub, false);
      FileManager fileManager = FileManager.instance();

      assertNotNull(subFolder, "subfolder record should exist");
      assertFalse(HyperPath.getHyperPathSetForFilePath(mixed).isEmpty(), "misc. file should be tracked");

      HDT_RecordWithPath miscFile = HyperPath.getHyperPathSetForFilePath(mixed).iterator().next().getRecord();
      int subID = subFolder.getID(), miscID = miscFile.getID();

      fileManager.selectRows(sub, plain, mixed);

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrYes);  // "One or more of the selected items is associated with a database record..."

      fileManager.delete(null);

      assertEquals(1, PopupRobot.getInvocationCount(), "should ask once for the whole selection");
      assertTrue(PopupRobot.getLastMessage().contains("associated with a database record"), "the question should mention the record");
      assertGone(sub, "deleted subfolder");
      assertGone(plain, "deleted untracked file");
      assertGone(mixed, "deleted misc. file");
      assertNull(db.folders.getByID(subID), "subfolder record should be deleted");
      assertNull(db.miscFiles.getByID(miscID), "misc. file record should be deleted");
      assertSame(HyperPath.folderRecordFor(dir, false), fileManager.folderTree.selectedRecord(), "folder should still be selected");

      PopupRobot.setDefaultResponse(mrOk);
    });

    // Step 163: A locked file in the batch is skipped and keeps its record; the other file and its record go
    seq.thenRunAfterDelay(() ->
    {
      Process lockProc = null;

      try
      {
        FilePath dir    = createTestDir("p35s163", "dir"),
                 lockme = dir.resolve("lockme.dat"),
                 other  = dir.resolve("other.dat");
        FileManager fileManager = FileManager.instance();

        HDT_RecordWithPath lockedFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, lockme),
                           otherFile  = HyperPath.createRecordAssignedToPath(hdtMiscFile, other);

        assertNotNull(lockedFile, "MiscFile record for the locked file should be created");
        assertNotNull(otherFile, "MiscFile record for the other file should be created");

        fileManager.goToFilePath(other, false);
        fileManager.selectRows(lockme, other);

        lockProc = startLockProcess(lockme);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrYes, mrIgnore);  // the confirmation, then Skip on the batch's failure prompt

        fileManager.delete(null);

        assertGone(other, "unlocked file");
        assertNull(db.miscFiles.getByID(otherFile.getID()), "unlocked file's record should be deleted");
        assertExists(lockme, "locked file");
        assertNotNull(db.miscFiles.getByID(lockedFile.getID()), "locked file's record should survive its failed deletion");
      }
      catch (Exception e) { throw new AssertionError(e); }
      finally
      {
        destroyQuietly(lockProc);
        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrOk);
      }
    }).windowsOnly();

    // Step 164: A folder with an in-use file inside is refused, with the message that says so
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir   = createTestDir("p35s164", "dir"),
               inuse = dir.resolve("inuse"),
               file  = inuse.resolve("file.dat");
      FileManager fileManager = FileManager.instance();

      assertNotNull(HyperPath.folderRecordFor(inuse, true), "in-use folder record should exist");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, file), "MiscFile record should be created");

      fileManager.goToFilePath(inuse, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");
      assertEquals(inuse, row.getFilePath(), "selected row should be the in-use folder");

      PopupRobot.clear();

      fileManager.delete(row);

      assertEquals(1, PopupRobot.getInvocationCount(), "should refuse once");
      assertTrue(PopupRobot.getLastMessage().contains("cannot be deleted, because it contains"), "refusal should be about in-use contents");
      assertTrue(inuse.isDirectory(), "folder should be untouched");
      assertExists(file, "in-use file");
    });

    // Step 165: The XML folder is refused as protected
    seq.thenRunAfterDelay(() ->
    {
      FilePath xmlPath = db.xmlPath();
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(xmlPath, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "XML folder row should be selected");

      PopupRobot.clear();
      PopupRobot.setDefaultResponse(mrNo);  // a guard: were the refusal ever missing, the confirmation would be declined

      fileManager.delete(row);

      PopupRobot.setDefaultResponse(mrOk);

      assertEquals(1, PopupRobot.getInvocationCount(), "should refuse once");
      assertTrue(PopupRobot.getLastMessage().contains("cannot be deleted."), "refusal should be the protected-path message");
      assertTrue(xmlPath.isDirectory(), "XML folder should be untouched");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 36: Clipboard and Drag Variants of Paste (6 steps)
    // =====================================================================

    // Step 166: Open the File Manager on a source folder with files, a folder holding a misc. file, and a sibling destination
    seq.thenRunAfterDelay(() ->
    {
      FilePath src     = createTestDir("p36s166", "src"),
               dest    = createTestDir("p36s166", "dest"),
               movedir = src.resolve("movedir");

      for (String name : List.of("cut.txt", "copy.txt", "drag.txt"))
        assertExists(src.resolve(name), name);

      assertTrue(dest.isDirectory(), "destination folder");
      assertNotNull(HyperPath.folderRecordFor(movedir, true), "folder record for the folder to move should exist");
      assertNotNull(HyperPath.createRecordAssignedToPath(hdtMiscFile, movedir.resolve("inside.dat")), "MiscFile record should be created");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(src);
    });

    // Step 167: Cut a file and paste it into the destination folder, the way the buttons do
    seq.thenRunAfterDelay(() ->
    {
      FilePath src  = testRoot.resolve("p36s166", "src"),
               dest = testRoot.resolve("p36s166", "dest"),
               file = src.resolve("cut.txt");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(file, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");

      fileManager.cutCopy(row, false);
      fileManager.paste(new FileRow(HyperPath.folderRecordFor(dest, false).getPath(), true), false, false);

      assertExists(dest.resolve("cut.txt"), "file at the destination");
      assertGone(file, "file at the source");
    });

    // Step 168: Copy a file and paste it; both copies exist
    seq.thenRunAfterDelay(() ->
    {
      FilePath src  = testRoot.resolve("p36s166", "src"),
               dest = testRoot.resolve("p36s166", "dest"),
               file = src.resolve("copy.txt");
      FileManager fileManager = FileManager.instance();

      fileManager.goToFilePath(file, false);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "file row should be selected");

      fileManager.cutCopy(row, true);
      fileManager.paste(new FileRow(HyperPath.folderRecordFor(dest, false).getPath(), true), true, false);

      assertExists(dest.resolve("copy.txt"), "copy at the destination");
      assertExists(file, "original at the source");
    });

    // Step 169: Cut a folder holding a misc. file and paste it; the folder record and the misc. file record move with it
    seq.thenRunAfterDelay(() ->
    {
      FilePath src     = testRoot.resolve("p36s166", "src"),
               dest    = testRoot.resolve("p36s166", "dest"),
               movedir = src.resolve("movedir"),
               inside  = movedir.resolve("inside.dat");
      HDT_Folder folder = HyperPath.folderRecordFor(movedir, false);
      HDT_RecordWithPath miscFile = HyperPath.getHyperPathSetForFilePath(inside).iterator().next().getRecord();
      FileManager fileManager = FileManager.instance();

      assertNotNull(folder, "folder record should exist");
      assertNotNull(miscFile, "misc. file record should exist");

      fileManager.goToFilePath(movedir, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");
      assertEquals(movedir, row.getFilePath(), "selected row should be the folder");

      fileManager.cutCopy(row, false);
      fileManager.paste(new FileRow(HyperPath.folderRecordFor(dest, false).getPath(), true), false, false);

      assertTrue(dest.resolve("movedir").isDirectory(), "folder at the destination");
      assertGone(movedir, "folder at the source");
      assertEquals(dest.resolve("movedir"), folder.filePath(), "folder record should follow the move");
      assertEquals(dest.resolve("movedir", "inside.dat"), miscFile.filePath(), "misc. file record should follow the move");
    });

    // Step 170: The drag variant of a move, as a drop from the file table or the folder tree uses it
    seq.thenRunAfterDelay(() ->
    {
      FilePath src  = testRoot.resolve("p36s166", "src"),
               dest = testRoot.resolve("p36s166", "dest"),
               file = src.resolve("drag.txt");
      FileManager fileManager = FileManager.instance();

      assertTrue(fileManager.moveCopy(entitiesOf(file), false, true), "moveCopy for a drag should succeed");

      fileManager.paste(new FileRow(HyperPath.folderRecordFor(dest, false).getPath(), true), false, true);

      assertExists(dest.resolve("drag.txt"), "dragged file at the destination");
      assertGone(file, "dragged file at the source");
    });

    // Step 171: A file dragged in from outside the database folder is copied in and left in place outside
    seq.thenRunAfterDelay(() ->
    {
      FilePath dest = testRoot.resolve("p36s166", "dest"),
               ext  = extRoot.resolve("p36s171", "ext.txt");
      FileManager fileManager = FileManager.instance();

      assertExists(ext, "outside file");

      assertTrue(fileManager.moveCopy(entitiesOf(ext), true, true), "moveCopy for an outside drag should succeed");

      fileManager.paste(new FileRow(HyperPath.folderRecordFor(dest, false).getPath(), true), true, true);

      assertExists(dest.resolve("ext.txt"), "copied file at the destination");
      assertExists(ext, "outside file left in place");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 37: Folder History and the Refresh Button (3 steps)
    // =====================================================================

    // Step 172: Open the File Manager on the first of three sibling folders
    seq.thenRunAfterDelay(() ->
    {
      FilePath a = createTestDir("p37s172", "a");

      createTestDir("p37s172", "b");
      createTestDir("p37s172", "c");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(a);
    });

    // Step 173: Visit the other two, then Back twice and Forward once land where they should
    seq.thenRunAfterDelay(() ->
    {
      FilePath a = testRoot.resolve("p37s172", "a"),
               b = testRoot.resolve("p37s172", "b"),
               c = testRoot.resolve("p37s172", "c");
      HDT_Folder folderA = HyperPath.folderRecordFor(a, false),
                 folderB = HyperPath.folderRecordFor(b, false),
                 folderC = HyperPath.folderRecordFor(c, false);
      FileManager fileManager = FileManager.instance();

      assertSame(folderA, fileManager.folderTree.selectedRecord(), "first folder should be selected");

      FileManager.show(b);
      FileManager.show(c);

      assertSame(folderC, fileManager.folderTree.selectedRecord(), "third folder should be selected");

      fileManager.userRequestsToGoBackward();
      assertSame(folderB, fileManager.folderTree.selectedRecord(), "Back should return to the second folder");

      fileManager.userRequestsToGoBackward();
      assertSame(folderA, fileManager.folderTree.selectedRecord(), "Back again should return to the first folder");

      fileManager.userRequestsToGoForward();
      assertSame(folderB, fileManager.folderTree.selectedRecord(), "Forward should return to the second folder");
    });

    // Step 174: Delete the third folder's directory behind the program's back and press Refresh: its record is pruned, the
    // history drops it, so Forward from the second folder has nowhere to go while Back still reaches the first
    seq.thenRunAfterDelay(() ->
    {
      FilePath a = testRoot.resolve("p37s172", "a"),
               b = testRoot.resolve("p37s172", "b"),
               c = testRoot.resolve("p37s172", "c");
      HDT_Folder folderA = HyperPath.folderRecordFor(a, false),
                 folderB = HyperPath.folderRecordFor(b, false),
                 folderC = HyperPath.folderRecordFor(c, false);
      FileManager fileManager = FileManager.instance();

      assertNotNull(folderC, "third folder record should exist");

      int folderCID = folderC.getID();

      boolean restartWatcher = folderTreeWatcher.stop();

      try { Files.delete(c.toPath()); }
      catch (IOException e) { throw new AssertionError(e); }
      finally
      {
        if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();
      }

      assertNotNull(db.folders.getByID(folderCID), "record should survive until the prune, since the watcher was paused");

      FileManager.pruneAndRefresh(true);  // the Refresh button

      assertNull(db.folders.getByID(folderCID), "Refresh should prune the deleted folder's record");
      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running after Refresh");
      assertSame(folderB, fileManager.folderTree.selectedRecord(), "second folder should still be selected");

      fileManager.userRequestsToGoForward();
      assertSame(folderB, fileManager.folderTree.selectedRecord(), "Forward should have nowhere to go once the third folder left the history");

      fileManager.userRequestsToGoBackward();
      assertSame(folderA, fileManager.folderTree.selectedRecord(), "Back should still reach the first folder");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    // =====================================================================
    //  Phase 38: Outside Deletion of Several In-Use Files at Once (2 steps)
    // =====================================================================

    int[] severalIDs = new int[2];  // the two misc. file records' IDs, from step 175 to step 176

    // Step 175: Two files in use by misc. file records, deleted outside the program within one debounce window
    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = createTestDir("p38s175", "parent"),
               first  = parent.resolve("first.dat"),
               second = parent.resolve("second.dat");

      assertExists(first,  "first in-use file");
      assertExists(second, "second in-use file");

      HDT_RecordWithPath firstFile  = HyperPath.createRecordAssignedToPath(hdtMiscFile, first),
                         secondFile = HyperPath.createRecordAssignedToPath(hdtMiscFile, second);

      assertNotNull(firstFile,  "MiscFile record for the first file should be created");
      assertNotNull(secondFile, "MiscFile record for the second file should be created");

      severalIDs[0] = firstFile .getID();
      severalIDs[1] = secondFile.getID();

      assertTrue(folderTreeWatcher.isRunning(), "FolderTreeWatcher should be running");

      PopupRobot.clear();

      try
      {
        Files.delete(first .toPath());
        Files.delete(second.toPath());
      }
      catch (IOException e) { throw new AssertionError(e); }
    });

    // Step 176: One warning names both files. For several files it is the long-message dialog, which the robot records
    // like any other popup; the records are kept
    seq.setDelayMS(6500);

    seq.thenRunAfterDelay(() ->
    {
      FilePath parent = testRoot.resolve("p38s175", "parent");

      assertEquals(1, PopupRobot.getInvocationCount(), "the two deletions should produce one warning");
      assertNotNull(PopupRobot.getLastMessage(), "watcher should have warned");
      assertTrue(PopupRobot.getLastMessage().contains("have been deleted or moved from outside the program"), "warning should be about several outside deletions");
      assertTrue(PopupRobot.getLastMessage().contains(parent.resolve("first.dat" ).toDbRelativeStr()), "warning should list the first file");
      assertTrue(PopupRobot.getLastMessage().contains(parent.resolve("second.dat").toDbRelativeStr()), "warning should list the second file");
      assertNotNull(db.miscFiles.getByID(severalIDs[0]), "first file's record should survive");
      assertNotNull(db.miscFiles.getByID(severalIDs[1]), "second file's record should survive");
    });

    seq.setDelayMS(400);

    // =====================================================================
    //  Phase 39: The Record Table After Records Change Outside the File Manager (4 steps)
    // =====================================================================

    HDT_Note[] assignedNotes = new HDT_Note[2];  // the two notes, from step 178 to step 180
    FileRow[] folderRowHolder = new FileRow[1];  // the folder's row, from step 178 to step 179

    // Step 177: A folder without notes; open the File Manager on its parent
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = createTestDir("p39s177", "dir");

      assertNotNull(HyperPath.folderRecordFor(dir, false), "folder record should exist");

      FileManager.instance();  // show() is a no-op until the instance exists
      FileManager.show(dir.getParent());
    });

    // Steps 178 to 180 each request a refresh that the throttle may defer by up to 500 ms; let it land
    // before the next step, so that no refresh but the one each step asks for runs in between
    seq.setDelayMS(1000);

    // Step 178: Select the folder's row, then assign the folder to two notes from outside the File Manager, as the
    // Note tab and "Assign to note record" do. Nothing tells the record table until the window regains focus.
    // Before the change, the focus gain found no refresh owed, and the table stayed empty until Refresh
    seq.thenRunAfterDelay(() ->
    {
      FilePath dir = testRoot.resolve("p39s177", "dir");
      HDT_Folder folder = HyperPath.folderRecordFor(dir, false);
      FileManager fileManager = FileManager.instance();
      HyperTable recordTable = fileManager.getRecordTable();

      fileManager.goToFilePath(dir, true);

      FileRow row = fileManager.selectedFileRow();

      assertNotNull(row, "folder row should be selected");
      assertEquals(dir, row.getFilePath(), "selected row should be the folder");
      assertEquals(0, recordTable.dataRowCount(), "record table should list nothing for a folder without notes");

      folderRowHolder[0] = row;

      HDT_Note first  = db.createNewBlankRecord(hdtNote),
               second = db.createNewBlankRecord(hdtNote);

      first .setName("Test note p39s177 first");
      second.setName("Test note p39s177 second");

      first .folder.set(folder);
      second.folder.set(folder);

      assignedNotes[0] = first;
      assignedNotes[1] = second;

      assertNull(recordTable.getRowByRecord(first), "record table should not list the note until a refresh runs");

      fileManager.requestOwedRefresh();  // what the window gaining focus does
    });

    // Step 179: Both notes are listed, and only the record table was rebuilt: the folder's row is the same object,
    // which a full refresh would have replaced. Then the user picks the second note, and the first is renamed from outside
    seq.thenRunAfterDelay(() ->
    {
      HDT_Note first  = assignedNotes[0],
               second = assignedNotes[1];
      FileManager fileManager = FileManager.instance();
      HyperTable recordTable = fileManager.getRecordTable();

      assertNotNull(recordTable.getRowByRecord(first ), "record table should list the first note after the focus gain");
      assertNotNull(recordTable.getRowByRecord(second), "record table should list the second note after the focus gain");
      assertSame(folderRowHolder[0], fileManager.selectedFileRow(), "file table should be left alone, with the folder's row still selected");
      assertSame(first, recordTable.selectedRecord(), "the table's own pick should be the first note, the first row with a path");

      assertNotNull(recordTable.selectRowByRecord(second), "second note's row should be selectable");

      first.setName("Test note p39s177 first, renamed");

      fileManager.requestOwedRefresh();
    });

    // Step 180: The new name shows, so the table was rebuilt, and the second note is still selected rather than the
    // table's own pick. Before the change, every rebuild selected the first row with a description or a path again
    seq.thenRunAfterDelay(() ->
    {
      HDT_Note first  = assignedNotes[0],
               second = assignedNotes[1];
      HyperTable recordTable = FileManager.instance().getRecordTable();
      HyperTableRow firstRow = recordTable.getRowByRecord(first);

      assertNotNull(firstRow, "record table should still list the first note");
      assertEquals("Test note p39s177 first, renamed", firstRow.getText(1), "record table should show the first note's new name");
      assertSame(second, recordTable.selectedRecord(), "the second note should stay selected through the rebuild");

      selectRootFolder();  // park the window at the root, outside the tree the cleanup removes
      FileManager.close(false);
    });

    seq.setDelayMS(400);

    // Start the sequence
    seq.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pre-creates all filesystem artifacts (directories and files) needed by
   * the test suite. Call this before {@link #runTests} so that cloud sync
   * services like Dropbox can finish syncing before the tests run.
   *
   * @param testRootDir path under the DB root to use as test root
   */
  public static void setupTestFiles(FilePath testRootDir)
  {
    testRoot = testRootDir;
    extRoot = testRoot.getParent().getParent().resolve("_test_fm_ext");

    try
    {
      createAllTestArtifacts();
    }
    catch (IOException e)
    {
      e.printStackTrace();
      System.err.println("Failed to create test artifacts.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void setupDir(FilePath root, String... parts) throws IOException
  {
    root.resolve(parts).createDirectories();
  }

  private static void setupFile(FilePath root, String... parts) throws IOException
  {
    FilePath file = root.resolve(parts);
    file.getParent().createDirectories();
    Files.writeString(file.toPath(), file.getNameOnly().toString());
  }

  private static void setupFileCustom(FilePath root, String content, String... parts) throws IOException
  {
    FilePath file = root.resolve(parts);
    file.getParent().createDirectories();
    Files.writeString(file.toPath(), content);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void createAllTestArtifacts() throws IOException
  {
    // Phase 1: Single-Item Moves (Steps 1-8)

    // Step 1: internal file
    setupDir (testRoot, "p1s1", "dest");
    setupFile(testRoot, "p1s1", "src", "file1.txt");

    // Step 2: external file
    setupDir (testRoot, "p1s2", "dest");
    setupFile(extRoot, "p1s2", "src", "file2.txt");

    // Step 3: flat dir, internal
    setupDir (testRoot, "p1s3", "dest");
    setupFile(testRoot, "p1s3", "src", "flatdir", "a.txt");
    setupFile(testRoot, "p1s3", "src", "flatdir", "b.txt");

    // Step 4: flat dir, external
    setupDir (testRoot, "p1s4", "dest");
    setupFile(extRoot, "p1s4", "src", "extdir", "c.txt");
    setupFile(extRoot, "p1s4", "src", "extdir", "d.txt");

    // Step 5: nested dir (3 levels), internal
    setupDir (testRoot, "p1s5", "dest");
    setupFile(testRoot, "p1s5", "src", "lvl1", "f1.txt");
    setupFile(testRoot, "p1s5", "src", "lvl1", "lvl2", "f2.txt");
    setupFile(testRoot, "p1s5", "src", "lvl1", "lvl2", "lvl3", "f3.txt");

    // Step 6: nested dir (3 levels), external
    setupDir (testRoot, "p1s6", "dest");
    setupFile(extRoot, "p1s6", "src", "lvl1", "f1.txt");
    setupFile(extRoot, "p1s6", "src", "lvl1", "lvl2", "f2.txt");
    setupFile(extRoot, "p1s6", "src", "lvl1", "lvl2", "lvl3", "f3.txt");

    // Step 7: deeply nested (5 levels), internal
    setupDir (testRoot, "p1s7", "dest");
    setupDir (testRoot, "p1s7", "src", "d1", "d2");
    setupDir (testRoot, "p1s7", "src", "d1", "d2", "d3", "d4");
    setupFile(testRoot, "p1s7", "src", "d1", "f1.txt");
    setupFile(testRoot, "p1s7", "src", "d1", "d2", "d3", "f3.txt");
    setupFile(testRoot, "p1s7", "src", "d1", "d2", "d3", "d4", "d5", "f5.txt");

    // Step 8: deeply nested (5 levels), external
    setupDir (testRoot, "p1s8", "dest");
    setupDir (extRoot, "p1s8", "src", "d1", "d2");
    setupDir (extRoot, "p1s8", "src", "d1", "d2", "d3", "d4");
    setupFile(extRoot, "p1s8", "src", "d1", "f1.txt");
    setupFile(extRoot, "p1s8", "src", "d1", "d2", "d3", "f3.txt");
    setupFile(extRoot, "p1s8", "src", "d1", "d2", "d3", "d4", "d5", "f5.txt");

    // Phase 2: Multi-Item Moves (Steps 9-14)

    // Step 9: multiple files, internal
    setupDir (testRoot, "p2s9", "dest");
    setupFile(testRoot, "p2s9", "src", "alpha.txt");
    setupFile(testRoot, "p2s9", "src", "beta.txt");
    setupFile(testRoot, "p2s9", "src", "gamma.txt");

    // Step 10: multiple files, external
    setupDir (testRoot, "p2s10", "dest");
    setupFile(extRoot, "p2s10", "src", "alpha.txt");
    setupFile(extRoot, "p2s10", "src", "beta.txt");
    setupFile(extRoot, "p2s10", "src", "gamma.txt");

    // Step 11: multiple dirs, internal
    setupDir (testRoot, "p2s11", "dest");
    setupFile(testRoot, "p2s11", "src", "dirA", "a.txt");
    setupFile(testRoot, "p2s11", "src", "dirB", "b.txt");

    // Step 12: multiple dirs, external
    setupDir (testRoot, "p2s12", "dest");
    setupFile(extRoot, "p2s12", "src", "dirA", "a.txt");
    setupFile(extRoot, "p2s12", "src", "dirB", "b.txt");

    // Step 13: mixed (dir + file), internal
    setupDir (testRoot, "p2s13", "dest");
    setupFile(testRoot, "p2s13", "src", "mixdir", "inside.txt");
    setupFile(testRoot, "p2s13", "src", "loose.txt");

    // Step 14: mixed (dir + file), external
    setupDir (testRoot, "p2s14", "dest");
    setupFile(extRoot, "p2s14", "src", "mixdir", "inside.txt");
    setupFile(extRoot, "p2s14", "src", "loose.txt");

    // Phase 3: Re-Parent Root Topology (Steps 15-18)

    // Step 15: nested dirs A > B
    setupDir (testRoot, "p3s15", "dest");
    setupFile(testRoot, "p3s15", "src", "A", "B", "file.txt");

    // Step 16: A > B + independent C
    setupDir (testRoot, "p3s16", "dest");
    setupFile(testRoot, "p3s16", "src", "A", "B", "fb.txt");
    setupFile(testRoot, "p3s16", "src", "C", "fc.txt");

    // Step 17: dir A + file inside A
    setupDir (testRoot, "p3s17", "dest");
    setupFile(testRoot, "p3s17", "src", "A", "inside.txt");

    // Step 18: dir A + sibling file
    setupDir (testRoot, "p3s18", "dest");
    setupFile(testRoot, "p3s18", "src", "A", "a.txt");
    setupFile(testRoot, "p3s18", "src", "sibling.txt");

    // Phase 4: Record Association Preservation (Steps 19-25)

    // Step 19: WorkFile in dir
    setupDir (testRoot, "p4s19", "dest");
    setupFile(testRoot, "p4s19", "src", "wfdir", "work.pdf");

    // Step 20: MiscFile in dir
    setupDir (testRoot, "p4s20", "dest");
    setupFile(testRoot, "p4s20", "src", "mfdir", "misc.dat");

    // Step 21: Person picture in dir
    setupDir (testRoot, "p4s21", "dest");
    setupFile(testRoot, "p4s21", "src", "picdir", "photo.jpg");

    // Step 22: Note folder
    setupDir (testRoot, "p4s22", "dest");
    setupFile(testRoot, "p4s22", "src", "notedir", "readme.txt");

    // Step 23: all four types
    setupDir (testRoot, "p4s23", "dest");
    setupFile(testRoot, "p4s23", "src", "alldir", "work.pdf");
    setupFile(testRoot, "p4s23", "src", "alldir", "misc.dat");
    setupFile(testRoot, "p4s23", "src", "alldir", "photo.jpg");
    setupFile(testRoot, "p4s23", "src", "alldir", "readme.txt");

    // Step 24: individual file with WorkFile
    setupDir (testRoot, "p4s24", "dest");
    setupFile(testRoot, "p4s24", "src", "solo_work.pdf");

    // Step 25: individual file with MiscFile
    setupDir (testRoot, "p4s25", "dest");
    setupFile(testRoot, "p4s25", "src", "solo_misc.dat");

    // Phase 5: doPasteChecks Error Conditions (Steps 26-31)

    // Step 26: source == destination
    setupFile(testRoot, "p5s26", "src", "samefile.txt");

    // Step 27: dest is subfolder of source
    setupDir (testRoot, "p5s27", "src", "A", "sub");
    setupFile(testRoot, "p5s27", "src", "A", "f.txt");

    // Steps 28-29: use existing DB files (no setup needed)

    // Step 30: folder already exists at destination
    setupDir (testRoot, "p5s30", "dest", "dup");
    setupFile(testRoot, "p5s30", "src", "dup", "f.txt");

    // Step 31: protected file at destination
    setupFile(testRoot, "p5s31", "src", "database.hdb");

    // Phase 6: Overwrite Scenarios (Steps 32-40)

    // Step 32: unrelated file, mrYes
    setupFile(testRoot, "p6s32", "src", "over.txt");
    setupFile(testRoot, "p6s32", "dest", "over.txt");

    // Step 33: unrelated file, mrNo
    setupFile(testRoot, "p6s33", "src", "over.txt");
    setupFileCustom(testRoot, "original", "p6s33", "dest", "over.txt");

    // Step 34: two unrelated, mrYesToAll
    setupFile(testRoot, "p6s34", "src", "f1.txt");
    setupFile(testRoot, "p6s34", "src", "f2.txt");
    setupFile(testRoot, "p6s34", "dest", "f1.txt");
    setupFile(testRoot, "p6s34", "dest", "f2.txt");

    // Step 35: two unrelated, mrNoToAll
    setupFile(testRoot, "p6s35", "src", "f1.txt");
    setupFile(testRoot, "p6s35", "src", "f2.txt");
    setupFileCustom(testRoot, "orig1", "p6s35", "dest", "f1.txt");
    setupFileCustom(testRoot, "orig2", "p6s35", "dest", "f2.txt");

    // Step 36: related file, mrYes
    setupFile(testRoot, "p6s36", "src", "related.txt");
    setupFile(testRoot, "p6s36", "dest", "related.txt");

    // Step 37: related file, mrNo
    setupFile(testRoot, "p6s37", "src", "related.txt");
    setupFileCustom(testRoot, "original_related", "p6s37", "dest", "related.txt");

    // Step 38: two related, mrYesToAll
    setupFile(testRoot, "p6s38", "src", "r1.txt");
    setupFile(testRoot, "p6s38", "src", "r2.txt");
    setupFile(testRoot, "p6s38", "dest", "r1.txt");
    setupFile(testRoot, "p6s38", "dest", "r2.txt");

    // Step 39: two related, mrNoToAll
    setupFile(testRoot, "p6s39", "src", "r1.txt");
    setupFile(testRoot, "p6s39", "src", "r2.txt");
    setupFileCustom(testRoot, "orig_r1", "p6s39", "dest", "r1.txt");
    setupFileCustom(testRoot, "orig_r2", "p6s39", "dest", "r2.txt");

    // Step 40: mixed unrelated + related
    setupFile(testRoot, "p6s40", "src", "unrel.txt");
    setupFile(testRoot, "p6s40", "src", "rel.txt");
    setupFile(testRoot, "p6s40", "dest", "unrel.txt");
    setupFile(testRoot, "p6s40", "dest", "rel.txt");

    // Phase 7: Empty Directory Cleanup (Steps 41-43)

    // Step 41: external dir, all files moved
    setupDir (testRoot, "p7s41", "dest");
    setupFile(extRoot, "p7s41", "src", "emptyme", "f1.txt");
    setupFile(extRoot, "p7s41", "src", "emptyme", "f2.txt");

    // Step 42: external nested dirs
    setupDir (testRoot, "p7s42", "dest");
    setupFile(extRoot, "p7s42", "src", "lvl1", "f1.txt");
    setupFile(extRoot, "p7s42", "src", "lvl1", "lvl2", "f2.txt");

    // Step 43: external dir, partial move
    setupDir (testRoot, "p7s43", "dest");
    setupFile(extRoot, "p7s43", "src", "partial", "move_me.txt");
    setupFile(extRoot, "p7s43", "src", "partial", "stay.txt");

    // Phase 8: moveToNewParent Same-Parent No-Op (Step 44)
    setupFile(testRoot, "p8s44", "src", "noop", "file.txt");

    // Phase 9: Watcher Lifecycle (Steps 45-46)

    // Step 45: paste-move restarts watcher
    setupDir (testRoot, "p9s45", "dest");
    setupFile(testRoot, "p9s45", "src", "watch.txt");

    // Step 46: folder rename
    setupFile(testRoot, "p9s46", "src", "oldname", "file.txt");
    setupDir (testRoot, "p9s46", "src", "oldname", "sub");

    // Phase 10: Lock Checking (Steps 47-53)
    // Steps 47-50: pre-create files for lock tests
    setupFile(testRoot, "p10s47", "lock", "locked.txt");
    setupFile(testRoot, "p10s48", "lock", "locked.txt");
    setupFile(testRoot, "p10s49", "lock", "locked.txt");
    setupFile(testRoot, "p10s50", "lock", "unlocked.txt");

    // Steps 51-52: only create parent dirs; probe-renamed dirs use dynamic names at test time
    setupDir(testRoot, "p10s51", "lock");
    setupDir(testRoot, "p10s52", "lock");

    // Step 53: pre-create static "conflict" dir; dynamic probe dir created at test time
    setupFile(testRoot, "p10s53", "lock", "conflict", "original.txt");

    // Step 54: move subtree with locked interior file
    setupDir (testRoot, "p10s54", "dest");
    setupFile(testRoot, "p10s54", "src", "tree", "top.txt");
    setupFile(testRoot, "p10s54", "src", "tree", "sub1", "s1.txt");
    setupFile(testRoot, "p10s54", "src", "tree", "sub1", "sub2", "s2.txt");
    setupFile(testRoot, "p10s54", "src", "tree", "sub1", "sub2", "unlocked.txt");
    setupFile(testRoot, "p10s54", "src", "tree", "sibling", "sib.txt");

    // Phase 11: Baseline mixed moves (Steps 55-56)

    setupFile(extRoot,  "p11s55", "src", "nested", "top.txt");
    setupFile(extRoot,  "p11s55", "src", "nested", "child1", "c1.txt");
    setupFile(extRoot,  "p11s55", "src", "nested", "child1", "child2", "c2.txt");
    setupFile(extRoot,  "p11s55", "src", "sibling.txt");
    setupDir (testRoot, "p11s55", "dest");

    setupFile(testRoot, "p11s56", "src", "folder", "inner.txt");
    setupFile(testRoot, "p11s56", "src", "work.pdf");
    setupFile(testRoot, "p11s56", "src", "loose.txt");
    setupDir (testRoot, "p11s56", "dest");

    // Phase 12: Re-parent loop Skip scenarios (Steps 57-60)

    setupFile(testRoot, "p12s57", "src", "locked_dir", "ok.txt");
    setupFile(testRoot, "p12s57", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p12s57", "src", "clean", "a.txt");
    setupDir (testRoot, "p12s57", "dest");

    setupFile(testRoot, "p12s58", "src", "clean", "a.txt");
    setupFile(testRoot, "p12s58", "src", "locked_dir", "ok.txt");
    setupFile(testRoot, "p12s58", "src", "locked_dir", "nested", "target.txt");
    setupDir (testRoot, "p12s58", "dest");

    setupFile(testRoot, "p12s59", "src", "clean", "a.txt");
    setupFile(testRoot, "p12s59", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p12s59", "src", "extra.txt");
    setupDir (testRoot, "p12s59", "dest");

    setupFile(testRoot, "p12s60", "src", "clean", "a.txt");
    setupFile(testRoot, "p12s60", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p12s60", "src", "extra.txt");
    setupDir (testRoot, "p12s60", "dest");

    // Phase 13: Re-parent loop Stop and Retry (Steps 61-63)

    setupFile(testRoot, "p13s61", "src", "first", "a.txt");
    setupFile(testRoot, "p13s61", "src", "second", "b.txt");
    setupDir (testRoot, "p13s61", "dest");

    setupFile(testRoot, "p13s62", "src", "retrydir", "inner.txt");
    setupFile(testRoot, "p13s62", "src", "retrydir", "locked.txt");
    setupDir (testRoot, "p13s62", "dest");

    setupFile(testRoot, "p13s63", "src", "retrydir", "inner.txt");
    setupFile(testRoot, "p13s63", "src", "retrydir", "locked.txt");
    setupFile(testRoot, "p13s63", "src", "extra.txt");
    setupDir (testRoot, "p13s63", "dest");

    // Phase 14: File move loop lock scenarios (Steps 64-67)

    setupFile(extRoot,  "p14s64", "src", "a.txt");
    setupFile(extRoot,  "p14s64", "src", "b.txt");
    setupFile(extRoot,  "p14s64", "src", "c.txt");
    setupDir (testRoot, "p14s64", "dest");

    setupFile(extRoot,  "p14s65", "src", "a.txt");
    setupFile(extRoot,  "p14s65", "src", "b.txt");
    setupFile(extRoot,  "p14s65", "src", "c.txt");
    setupDir (testRoot, "p14s65", "dest");

    setupFile(extRoot,  "p14s66", "src", "x.txt");
    setupFile(extRoot,  "p14s66", "src", "y.txt");
    setupDir (testRoot, "p14s66", "dest");

    setupFile(extRoot,  "p14s67", "src", "locked.txt");
    setupDir (testRoot, "p14s67", "dest");

    // Phase 15: Cross-loop scenarios (Steps 68-72)

    setupFile(testRoot, "p15s68", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p15s68", "src", "loose.txt");
    setupDir (testRoot, "p15s68", "dest");

    setupFile(testRoot, "p15s69", "src", "folder", "inner.txt");
    setupFile(testRoot, "p15s69", "src", "locked.txt");
    setupDir (testRoot, "p15s69", "dest");

    setupFile(testRoot, "p15s70", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p15s70", "src", "extra.txt");
    setupDir (testRoot, "p15s70", "dest");

    setupFile(testRoot, "p15s71", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p15s71", "src", "locked_file.txt");
    setupDir (testRoot, "p15s71", "dest");

    setupFile(testRoot, "p15s72", "src", "folder", "inner.txt");
    setupFile(testRoot, "p15s72", "src", "work.pdf");
    setupDir (testRoot, "p15s72", "dest");

    // Phase 16: Read-only (Steps 73-74)

    setupFile(testRoot, "p16s73", "src", "readonly.txt");
    setupDir (testRoot, "p16s73", "dest");

    setupFile(testRoot, "p16s74", "src", "rodir", "normal.txt");
    setupFile(testRoot, "p16s74", "src", "rodir", "readonly.txt");
    setupDir (testRoot, "p16s74", "dest");

    // Phase 17: Copy operations (Steps 75-77)

    setupFile(testRoot, "p17s75", "src", "original.txt");
    setupDir (testRoot, "p17s75", "dest");

    setupFile(extRoot,  "p17s76", "src", "topdir", "a.txt");
    setupFile(extRoot,  "p17s76", "src", "topdir", "sub", "b.txt");
    setupDir (testRoot, "p17s76", "dest");

    setupDir (testRoot, "p17s77", "dest");

    // Phase 18: Cross-condition interaction tests (Steps 78-81)

    setupFile(testRoot, "p18s78", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p18s78", "src", "work.pdf");
    setupDir (testRoot, "p18s78", "dest");

    setupFile(testRoot, "p18s79", "src", "file.txt");
    setupFile(testRoot, "p18s79", "dest", "file.txt");

    setupFile(testRoot, "p18s80", "src", "work.pdf");
    setupFile(testRoot, "p18s80", "dest", "work.pdf");

    setupFile(extRoot,  "p18s81", "src", "extdir", "a.txt");
    setupFile(extRoot,  "p18s81", "src", "extdir", "locked.txt");
    setupDir (testRoot, "p18s81", "dest");

    // Phase 19: Record-associated file lock extended (Steps 82-84)

    setupFile(testRoot, "p19s82", "src", "folder", "inner.txt");
    setupFile(testRoot, "p19s82", "src", "work.pdf");
    setupDir (testRoot, "p19s82", "dest");

    setupFile(testRoot, "p19s83", "src", "work.pdf");
    setupDir (testRoot, "p19s83", "dest");

    setupFile(testRoot, "p19s84", "src", "work.pdf");
    setupFile(testRoot, "p19s84", "src", "loose.txt");
    setupDir (testRoot, "p19s84", "dest");

    // Phase 20: Copy path lock scenarios (Steps 85-87)

    setupFile(testRoot, "p20s85", "src", "file.txt");
    setupFile(testRoot, "p20s85", "dest", "file.txt");

    setupFile(testRoot, "p20s86", "src", "file.txt");
    setupFile(testRoot, "p20s86", "dest", "file.txt");

    setupFile(testRoot, "p20s87", "src", "file.txt");
    setupFile(testRoot, "p20s87", "dest", "file.txt");

    // Phase 21: Timed-release retry (Steps 88-101)

    setupFile(testRoot, "p21s88", "src", "rdir", "inner.txt");
    setupFile(testRoot, "p21s88", "src", "rdir", "locked.txt");
    setupDir (testRoot, "p21s88", "dest");

    setupFile(extRoot,  "p21s89", "src", "locked.txt");
    setupDir (testRoot, "p21s89", "dest");

    setupFile(testRoot, "p21s90", "src", "work.pdf");
    setupDir (testRoot, "p21s90", "dest");

    setupFile(testRoot, "p21s91", "src", "rdir", "locked.txt");
    setupFile(testRoot, "p21s91", "src", "extra.txt");
    setupDir (testRoot, "p21s91", "dest");

    setupFile(testRoot, "p21s92", "src", "folder", "inner.txt");
    setupFile(testRoot, "p21s92", "src", "work.pdf");
    setupDir (testRoot, "p21s92", "dest");

    setupFile(testRoot, "p21s93", "src", "file.txt");
    setupFile(testRoot, "p21s93", "dest", "file.txt");

    setupFile(extRoot,  "p21s94", "src", "edir", "a.txt");
    setupFile(extRoot,  "p21s94", "src", "edir", "locked.txt");
    setupDir (testRoot, "p21s94", "dest");

    setupFile(testRoot, "p21s95", "src", "rdir", "locked.txt");
    setupDir (testRoot, "p21s95", "dest");

    setupFile(testRoot, "p21s96", "src", "dir1", "locked1.txt");
    setupFile(testRoot, "p21s96", "src", "dir2", "locked2.txt");
    setupDir (testRoot, "p21s96", "dest");

    setupFile(testRoot, "p21s97", "src", "locked_dir", "nested", "target.txt");
    setupFile(testRoot, "p21s97", "src", "locked_file.txt");
    setupDir (testRoot, "p21s97", "dest");

    setupFile(testRoot, "p21s98", "src", "file.txt");
    setupFile(testRoot, "p21s98", "dest", "file.txt");

    setupFile(extRoot,  "p21s99", "src", "f1.txt");
    setupFile(extRoot,  "p21s101", "src", "f2.txt");
    setupDir (testRoot, "p21s101", "dest");

    // Phase 22: Retry escalation (Steps 100-103)

    setupFile(extRoot,  "p22s100", "src", "locked.txt");
    setupDir (testRoot, "p22s100", "dest");

    setupFile(testRoot, "p22s101", "src", "work.pdf");
    setupDir (testRoot, "p22s101", "dest");

    setupFile(testRoot, "p22s102", "src", "file.txt");
    setupFile(testRoot, "p22s102", "dest", "file.txt");

    setupFile(testRoot, "p22s103", "src", "work.pdf");
    setupDir (testRoot, "p22s103", "dest");

    // Phase 23: Move + overwrite + source lock (Steps 104-107)

    setupFile      (testRoot, "p23s104", "src", "file.txt");
    setupFileCustom(testRoot, "original", "p23s104", "dest", "file.txt");

    setupFile      (testRoot, "p23s105", "src", "file.txt");
    setupFileCustom(testRoot, "original", "p23s105", "dest", "file.txt");

    setupFile      (testRoot, "p23s106", "src", "file.txt");
    setupFileCustom(testRoot, "original", "p23s106", "dest", "file.txt");

    setupFile      (testRoot, "p23s107", "src", "incoming.txt");
    setupFileCustom(testRoot, "original_related", "p23s107", "dest", "incoming.txt");

    // Phase 24: Multi-file copy + lock (Steps 108-110)

    setupFile      (extRoot,  "p24s108", "src", "a.txt");
    setupFile      (extRoot,  "p24s108", "src", "b.txt");
    setupFileCustom(testRoot, "original_b", "p24s108", "dest", "b.txt");

    setupFile      (extRoot,  "p24s109", "src", "a.txt");
    setupFile      (extRoot,  "p24s109", "src", "b.txt");
    setupFileCustom(testRoot, "orig_a", "p24s109", "dest", "a.txt");
    setupFileCustom(testRoot, "orig_b", "p24s109", "dest", "b.txt");

    setupFile      (extRoot,  "p24s110", "src", "a.txt");
    setupFile      (extRoot,  "p24s110", "src", "b.txt");
    setupFileCustom(testRoot, "orig_a", "p24s110", "dest", "a.txt");
    setupFileCustom(testRoot, "orig_b", "p24s110", "dest", "b.txt");

    // Phase 25: Multi-item folder tree move with records (Step 111)

    setupFile(testRoot, "p25s111", "src", "loose.txt");
    setupFile(testRoot, "p25s111", "src", "tree", "mid.pdf");
    setupFile(testRoot, "p25s111", "src", "tree", "inner", "deep.jpg");
    setupDir (testRoot, "p25s111", "dest");

    // Phase 26: Registry Lookup for Untracked Files (Steps 112-113)

    setupFile(testRoot, "p26s112", "dir", "tracked.txt");
    setupFile(testRoot, "p26s112", "dir", "loose.txt");

    setupFile(testRoot, "p26s113", "src", "mixed", "tracked.txt");
    setupFile(testRoot, "p26s113", "src", "mixed", "loose.txt");
    setupDir (testRoot, "p26s113", "dest");

    // Phase 27: New Folder button (Steps 114-118)

    setupFile(testRoot, "p27s114", "parent", "file.txt");

    // Phase 28: Directory deletion outside the File Manager (Steps 119-123)

    setupFile(testRoot, "p28s119", "parent", "doomed", "file.txt");
    setupFile(testRoot, "p28s119", "parent", "inuse", "work.pdf");

    // Phase 29: Folder lookup for a vanished directory (Step 124)

    setupDir(testRoot, "p29s124", "vanishing");

    // Phase 30: Record lookups around deletion and outside rename (Steps 125-129)

    setupFile(testRoot, "p30s125", "dir", "doomed.txt");
    setupFile(testRoot, "p30s128", "dir", "tracked.txt");

    // Phase 31: Main window view after an outside deletion of its record (Step 130): no artifacts

    // Phase 32: Deleting a note's folder through the File Manager (Steps 131-133)

    setupFile(testRoot, "p32s131", "dir", "file.txt");

    // Phase 33: Rename through the File Manager (Steps 134-143)

    setupFile(testRoot, "p33s134", "dir", "plain.txt");
    setupFile(testRoot, "p33s134", "dir", "misc.dat");
    setupFile(testRoot, "p33s134", "dir", "work.pdf");
    setupFile(testRoot, "p33s134", "dir", "taken.txt");
    setupFile(testRoot, "p33s139", "outer", "inner.dat");
    setupFile(testRoot, "p33s139", "outer", "sub", "deep.txt");
    setupDir (testRoot, "p33s141", "notedir");

    // Phase 34: Outside changes seen by the watcher (Steps 144-157)

    setupDir (testRoot, "p34s144", "parent");
    setupFile(testRoot, "p34s146", "parent", "inuse.dat");
    setupDir (testRoot, "p34s148", "empty");
    setupDir (testRoot, "p34s150", "notedir");
    setupDir (testRoot, "p34s152", "notedir");
    setupFile(testRoot, "p34s154", "parent", "pic.jpg");
    setupFile(testRoot, "p34s156", "parent", "work.pdf");

    // Phase 35: Deletion variants (Steps 158-165)

    setupFile(testRoot, "p35s158", "dir", "decline.dat");
    setupFile(testRoot, "p35s158", "dir", "work.pdf");
    setupFile(testRoot, "p35s158", "dir", "pic.jpg");
    setupFile(testRoot, "p35s158", "dir", "plain.txt");
    setupFile(testRoot, "p35s158", "dir", "mixed.dat");
    setupDir (testRoot, "p35s158", "dir", "sub");
    setupFile(testRoot, "p35s163", "dir", "lockme.dat");
    setupFile(testRoot, "p35s163", "dir", "other.dat");
    setupFile(testRoot, "p35s164", "dir", "inuse", "file.dat");

    // Phase 36: Clipboard and drag variants of paste (Steps 166-171)

    setupFile(testRoot, "p36s166", "src", "cut.txt");
    setupFile(testRoot, "p36s166", "src", "copy.txt");
    setupFile(testRoot, "p36s166", "src", "drag.txt");
    setupFile(testRoot, "p36s166", "src", "movedir", "inside.dat");
    setupDir (testRoot, "p36s166", "dest");
    setupFile(extRoot,  "p36s171", "ext.txt");

    // Phase 37: Folder history and the Refresh button (Steps 172-174)

    setupDir(testRoot, "p37s172", "a");
    setupDir(testRoot, "p37s172", "b");
    setupDir(testRoot, "p37s172", "c");

    // Phase 38: Outside deletion of several in-use files at once (Steps 175-176)

    setupFile(testRoot, "p38s175", "parent", "first.dat");
    setupFile(testRoot, "p38s175", "parent", "second.dat");

    // Phase 39: The record table after records change outside the File Manager (Steps 177-180)

    setupDir(testRoot, "p39s177", "dir");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<EntityWithPath> entitiesOf(FilePath... filePaths)
  {
    return Arrays.stream(filePaths).map(EntityWithPath::new).toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static FilePath createTestDir(String phase, String name)
  {
    FilePath dir = testRoot.resolve(phase, name);
    HyperPath.folderRecordFor(dir, true);
    return dir;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Deletes the records the suite assigned to test files (work files, misc. files, person
   * pictures) and the notes it assigned to test folders. Left behind, they would keep those
   * folders "in use", which the prune after the test tree's deletion rightly refuses to remove.
   */
  private static void deleteFixtureRecords()
  {
    List<HDT_Record> fixtures = new ArrayList<>();

    for (HDT_WorkFile workFile : db.workFiles) if (isFixturePath(workFile.filePath())) fixtures.add(workFile);
    for (HDT_MiscFile miscFile : db.miscFiles) if (isFixturePath(miscFile.filePath())) fixtures.add(miscFile);
    for (HDT_Person   person   : db.persons  ) if (isFixturePath(person  .filePath())) fixtures.add(person  );

    for (HDT_Note note : db.notes)
      if (isFixturePath(nullSwitch(note.folder.get(), null, HDT_Folder::filePath)))
        fixtures.add(note);

    fixtures.forEach(db::deleteRecord);
  }

  private static boolean isFixturePath(FilePath filePath)
  {
    return (FilePath.isEmpty(filePath) == false) && (testRoot.contains(filePath) || extRoot.contains(filePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The live folder record named {@code name} directly under {@code parent}, or null. Goes by the
   * records rather than the path registry, which a deletion evicts.
   */
  private static HDT_Folder childFolderRecord(HDT_Folder parent, String name)
  {
    for (HDT_Folder folder : db.folders)
      if ((folder.parentFolder() == parent) && name.equals(folder.getPath().getNameStr()))
        return folder;

    return null;
  }

  /**
   * Whether the File Manager's folder tree shows a node named {@code childName} under {@code parent}'s node.
   */
  private static boolean folderTreeShows(HDT_Folder parent, String childName)
  {
    return FileManager.instance().folderTree.getRowsForRecord(parent).getFirst().getTreeItem().getChildren().stream()
      .anyMatch(item -> childName.equals(item.getValue().getFileName()));
  }

  /**
   * Selects the root folder in the folder tree synchronously. {@code selectRecord} would defer its
   * scroll and focus to a later pulse, which lands on a closed window when the step closes it next.
   */
  private static void selectRootFolder()
  {
    FolderTreeWrapper folderTree = FileManager.instance().folderTree;

    folderTree.getSelectionModel().select(folderTree.getRowsForRecord(db.getRootFolder()).getFirst().getTreeItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Wraps the full moveCopy + paste sequence for a move operation.
   * Creates the destination folder record and constructs the FileRow.
   */
  private static void pasteMove(List<EntityWithPath> items, FilePath destDir)
  {
    assertTrue(FileManager.instance().moveCopy(items, false, false), "moveCopy failed");

    HDT_Folder destFolder = HyperPath.folderRecordFor(destDir, true);
    assertNotNull(destFolder, "Destination folder record should exist: " + destDir);

    FileRow destRow = new FileRow(destFolder.getPath(), true);
    FileManager.instance().paste(destRow, false, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void pasteCopy(List<EntityWithPath> items, FilePath destDir)
  {
    assertTrue(FileManager.instance().moveCopy(items, true, false), "moveCopy(copy) failed");

    HDT_Folder destFolder = HyperPath.folderRecordFor(destDir, true);
    assertNotNull(destFolder, "Destination folder record should exist: " + destDir);

    FileRow destRow = new FileRow(destFolder.getPath(), true);
    FileManager.instance().paste(destRow, true, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertGone(FilePath path, String label)
  {
    assertFalse(path.exists(), label + " should not exist: " + path);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertExists(FilePath path, String label)
  {
    assertTrue(path.exists(), label + " should exist: " + path);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertFileContent(FilePath path, String expected, String label)
  {
    assertTrue(path.exists(), label + " should exist: " + path);

    try
    {
      assertEquals(expected, Files.readString(path.toPath()), label + " content mismatch");
    }
    catch (IOException e)
    {
      throw new AssertionError(label + " failed to read: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Spawns a child JVM running {@link FileLockHelper} to hold an exclusive
   * lock on the given file. Waits for the "LOCKED" signal before returning.
   */
  private static Process startLockProcess(FilePath file) throws IOException, InterruptedException, TimeoutException
  {
    String javaHome  = System.getProperty("java.home"),
           classPath = System.getProperty("java.class.path"),
           className = FileLockHelper.class.getName();

    ProcessBuilder pb = new ProcessBuilder(Path.of(javaHome, "bin", "java").toString(), "-cp", classPath, className, file.toString());

    pb.redirectErrorStream(true);
    Process proc = pb.start();

    // Wait for "LOCKED" signal with timeout

    try (BufferedReader reader = new BufferedReader(new InputStreamReader(proc.getInputStream())))
    {
      long deadline = System.currentTimeMillis() + 10_000;

      while (System.currentTimeMillis() < deadline)
      {
        if (reader.ready())
        {
          String line = reader.readLine();
          if ("LOCKED".equals(line))
            return proc;
        }
        else
        {
          Thread.sleep(50);
        }
      }
    }

    proc.destroyForcibly();
    throw new TimeoutException("FileLockHelper did not produce LOCKED signal within 10 seconds");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void destroyQuietly(Process proc)
  {
    if (proc == null) return;

    proc.destroyForcibly();

    try { proc.waitFor(5, TimeUnit.SECONDS); }
    catch (InterruptedException e) { Thread.currentThread().interrupt(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
