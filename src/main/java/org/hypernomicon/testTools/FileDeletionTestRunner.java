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

import static org.hypernomicon.App.app;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;
import static org.junit.jupiter.api.Assertions.*;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.BatchBuilder;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;

//---------------------------------------------------------------------------

/**
 * Test runner for the FileDeletion API, launched from the Test Console's Deletion tab.
 * All test files are created under the transient database's root folder so that
 * {@code anyUnderDbRoot} returns true, exercising the full watcher stop/restart
 * pipeline. Locked-file tests use programmatic file locking via {@link RandomAccessFile}
 * to hold Windows file handles.
 *
 * @see FileDeletion
 */
public final class FileDeletionTestRunner
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String TEST_DIR_NAME = "_test_deletion";

  private static FilePath testRoot;

//---------------------------------------------------------------------------

  private FileDeletionTestRunner() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Specifies what kind of test target to create and which factory method to use.
   */
  private enum TestTarget
  {
    FILE,              // Creates a file; uses ofFile() / ofFiles()
    DIR_WITH_CONTENTS, // Creates a dir with child files; uses ofDirWithContents() / ofDirsWithContents()
    DIR_CONTENTS_ONLY, // Creates a dir with child files; uses ofDirContentsOnly() / ofDirsContentsOnly()
    FODWC_AS_FILE,     // Creates a file; uses ofFileOrDirWithContents() / ofFilesOrDirsWithContents()
    FODWC_AS_DIR       // Creates a dir with child files; uses ofFileOrDirWithContents() / ofFilesOrDirsWithContents()
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run the full FileDeletion test suite as an FXTestSequencer sequence.
   */
  public static void runTests(FilePath dbRoot)
  {
    testRoot = dbRoot.resolve(TEST_DIR_NAME);

    try { Files.createDirectories(testRoot.toPath()); }
    catch (IOException e)
    {
      e.printStackTrace();
      System.err.println("Failed to create test root directory.");
      return;
    }

    FilePath logFile = new FilePath(app.prefs.get(PrefKey.LOG_PATH, ""));

    if (FilePath.isEmpty(logFile) || (logFile.exists() == false))
    {
      System.err.println("Log file path must be configured in Settings before running tests.");
      return;
    }

    PopupRobot.setActive(true);
    PopupRobot.clear();

    FXTestSequencer seq = new FXTestSequencer();

    seq.setDelayMS(100);

    seq.setFinalizer(() -> runDelayedInFXThread(1, 100, () ->
    {
      PopupRobot.setActive(false);
      PopupRobot.clear();

      // Best-effort cleanup (locked PDFs may prevent full cleanup until viewer is closed)

      if (testRoot != null)
        FileDeletion.ofDirContentsOnly(testRoot).nonInteractiveFailureOK().execute();
    }));

    // -----------------------------------------------------------------------
    // Phase 1: Single-item non-interactive deletion
    // -----------------------------------------------------------------------

    // Step 1: Single file

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "file1.txt");
        DeletionResult result = FileDeletion.ofFile(filePath).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 1 result");
        assertGone(filePath, "Step 1 file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 2: Dir with contents

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir1 = testRoot.resolve("dir1");
        createTestFile(dir1, "sub/file.txt");

        DeletionResult result = FileDeletion.ofDirWithContents(dir1).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 2 result");
        assertGone(dir1, "Step 2 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 3: Dir contents only

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir2 = testRoot.resolve("dir2");
        createTestFile(dir2, "a.txt");
        createTestFile(dir2, "sub/b.txt");

        DeletionResult result = FileDeletion.ofDirContentsOnly(dir2).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 3 result");
        assertExists(dir2, "Step 3 dir");
        assertDirEmpty(dir2, "Step 3 dir");

        // Clean up the now-empty directory

        FileDeletion.ofDirWithContents(dir2).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 4: FILE_OR_DIR on file

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "file4.txt");
        DeletionResult result = FileDeletion.ofFileOrDirWithContents(filePath).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 4 result");
        assertGone(filePath, "Step 4 file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 5: FILE_OR_DIR on directory

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir3 = testRoot.resolve("dir3");
        createTestFile(dir3, "sub/file.txt");

        DeletionResult result = FileDeletion.ofFileOrDirWithContents(dir3).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 5 result");
        assertGone(dir3, "Step 5 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 2: Deeply nested tree
    // -----------------------------------------------------------------------

    // Step 6: 12 levels deep

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath deep = testRoot.resolve("deep");
        createTestFile(deep, "l1/l2/l3/l4/l5/l6/l7/l8/l9/l10/l11/leaf.txt");

        DeletionResult result = FileDeletion.ofDirWithContents(deep).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 6 result");
        assertGone(deep, "Step 6 deep dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 3: Non-existent target behavior (idempotent deletion)
    // -----------------------------------------------------------------------

    // Step 7: Non-existent file with nonInteractive (SUCCESS: deletion is idempotent)

    seq.thenRunAfterDelay(() ->
    {
      FilePath nonexistent = testRoot.resolve("nonexistent.txt");

      DeletionResult result = FileDeletion.ofFile(nonexistent).nonInteractive().execute();

      assertEquals(SUCCESS, result, "Step 7 result");
    });

    // Step 8: nonInteractiveFailureOK on non-existent (SUCCESS)

    seq.thenRunAfterDelay(() ->
    {
      FilePath nonexistent = testRoot.resolve("nonexistent.txt");

      DeletionResult result = FileDeletion.ofFile(nonexistent).nonInteractiveFailureOK().execute();

      assertEquals(SUCCESS, result, "Step 8 result");
    });

    // Step 9: nonInteractiveLogErrors on non-existent (SUCCESS, nothing to log)

    seq.thenRunAfterDelay(() ->
    {
      FilePath nonexistent = testRoot.resolve("nonexistent.txt");

      DeletionResult result = FileDeletion.ofFile(nonexistent).nonInteractiveLogErrors().execute();

      assertEquals(SUCCESS, result, "Step 9 result");
    });

    // -----------------------------------------------------------------------
    // Phase 4: Batch operations
    // -----------------------------------------------------------------------

    // Step 10: Batch all succeed

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f1 = createTestFile(testRoot, "batch1.txt"),
                 f2 = createTestFile(testRoot, "batch2.txt"),
                 f3 = createTestFile(testRoot, "batch3.txt");

        var batch = FileDeletion.ofFiles(List.of(f1, f2, f3)).nonInteractive();
        DeletionResult result = batch.execute();

        assertEquals(SUCCESS, result, "Step 10 result");
        assertGone(f1, "Step 10 f1");
        assertGone(f2, "Step 10 f2");
        assertGone(f3, "Step 10 f3");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 10 failedPaths should be empty");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 11: Batch with non-existent path silently skipped

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath real = createTestFile(testRoot, "batch_real.txt");
        FilePath ghost = testRoot.resolve("batch_ghost.txt");

        var batch = FileDeletion.ofFiles(List.of(real, ghost)).nonInteractive();
        DeletionResult result = batch.execute();

        assertEquals(SUCCESS, result, "Step 11 result");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 11 failedPaths should be empty");
        assertGone(real, "Step 11 real file should be deleted");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 12: Batch null element throws NullPointerException

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath real = createTestFile(testRoot, "batch_skip.txt");

        List<FilePath> list = new ArrayList<>();
        list.add(real);
        list.add(null);

        try
        {
          FileDeletion.ofFiles(list).nonInteractive().execute();
          fail("Step 12: Expected NullPointerException");
        }
        catch (NullPointerException e)
        {
          assertTrue(e.getMessage().contains("Null element"), "Step 12 exception message");
        }
        finally
        {
          FileDeletion.ofFile(real).nonInteractiveFailureOK().execute();
        }
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 13: Type validation — directory passed to ofFiles

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("type_val_dir");
        Files.createDirectories(dir.toPath());

        try
        {
          FileDeletion.ofFiles(List.of(dir)).nonInteractive().execute();
          fail("Step 13: Expected IllegalArgumentException");
        }
        catch (IllegalArgumentException e)
        {
          assertTrue(e.getMessage().contains("Expected file, got directory"), "Step 13 exception message");
        }
        finally
        {
          FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute();
        }
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 14: Type validation — file passed to ofDirsWithContents

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "type_val_file.txt");

        try
        {
          FileDeletion.ofDirsWithContents(List.of(filePath)).nonInteractive().execute();
          fail("Step 14: Expected IllegalArgumentException");
        }
        catch (IllegalArgumentException e)
        {
          assertTrue(e.getMessage().contains("Expected directory, got file"), "Step 14 exception message");
        }
        finally
        {
          FileDeletion.ofFile(filePath).nonInteractiveFailureOK().execute();
        }
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 5: Interactive mode, unlocked files
    // -----------------------------------------------------------------------

    // Step 15: Interactive success on unlocked file (no popup expected)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "interactive_unlocked.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);  // Safety net; should not be used

        DeletionResult result = FileDeletion.ofFile(filePath).interactive().execute();

        assertEquals(SUCCESS, result, "Step 15 result");
        assertGone(filePath, "Step 15 file");
        assertNull(PopupRobot.getLastMessage(), "Step 15 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 6: Interactive mode with locked files (Windows only)
    // -----------------------------------------------------------------------

    // Step 16: Interactive single, cancel

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step16_locked.txt");
        raf = lockFile(locked);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        DeletionResult result = FileDeletion.ofFile(locked).interactive().execute();

        assertEquals(CANCELLED, result, "Step 16 result");
        assertExists(locked, "Step 16 locked file should still exist");
        assertEquals(1, PopupRobot.getInvocationCount(), "Step 16 popup count");
        assertTrue(PopupRobot.getLastMessage().contains("Unable to delete"), "Step 16 popup message");

        System.out.println("Step 16: Phase 2 cancel OK");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // Step 17: Batch interactive, skip locked

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step17_locked.txt"),
                 u1     = createTestFile(testRoot, "u1.txt");
        raf = lockFile(locked);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrIgnore);

        var batch = FileDeletion.ofFiles(List.of(locked, u1)).interactive();
        DeletionResult result = batch.execute();

        assertEquals(PARTIAL, result, "Step 17 result");
        assertTrue(batch.getFailedPaths().contains(locked), "Step 17 failedPaths should contain locked");
        assertGone(u1, "Step 17 u1 should be deleted");
        assertNotNull(PopupRobot.getLastMessage(), "Step 17 popup expected");
        System.out.println("Step 17: batch skip locked OK");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // Step 18: Batch interactive, retry then skip

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step18_locked.txt"),
                 u2     = createTestFile(testRoot, "u2.txt");
        raf = lockFile(locked);

        PopupRobot.clear();
        PopupRobot.enqueueResponses(mrRetry);    // First prompt: retry
        PopupRobot.setDefaultResponse(mrIgnore);  // Second prompt: skip

        var batch = FileDeletion.ofFiles(List.of(locked, u2)).interactive();
        DeletionResult result = batch.execute();

        assertEquals(PARTIAL, result, "Step 18 result");
        assertTrue(batch.getFailedPaths().contains(locked), "Step 18 failedPaths should contain locked");
        assertGone(u2, "Step 18 u2 should be deleted");
        System.out.println("Step 18: retry then skip OK");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // Step 19: Batch interactive, cancel

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step19_locked.txt");
        raf = lockFile(locked);

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        var batch = FileDeletion.ofFiles(List.of(locked)).interactive();
        DeletionResult result = batch.execute();

        assertEquals(CANCELLED, result, "Step 19 result");
        assertExists(locked, "Step 19 locked file should still exist");

        System.out.println("Step 19: CANCELLED as expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // Step 20: nonInteractiveLogErrors on locked file logs error

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step20_locked.txt");
        raf = lockFile(locked);

        long startPos = logFile.toFile().length();

        System.out.println("An error stack trace is expected below.");

        FileDeletion.ofFile(locked).nonInteractiveLogErrors().execute();

        assertExists(locked, "Step 20 locked file should still exist");
        assertLogContains(logFile, startPos, locked.getNameOnly().toString(), "Step 20 log output");

        System.out.println("Step 20: nonInteractiveLogErrors logged error as expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // -----------------------------------------------------------------------
    // Phase 7: Special filenames
    // -----------------------------------------------------------------------

    // Step 21: Unicode filename

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "caf\u00e9_\u00fc\u00f1.txt");
        DeletionResult result = FileDeletion.ofFile(filePath).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 21 result");
        assertGone(filePath, "Step 21 unicode file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 22: Filename with spaces

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "file with spaces.txt");
        DeletionResult result = FileDeletion.ofFile(filePath).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 22 result");
        assertGone(filePath, "Step 22 file with spaces");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 8: Symlinks
    // -----------------------------------------------------------------------

    // Step 23: Symlink to file (deletes link, preserves target)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath target = createTestFile(testRoot, "symlink_target.txt");
        FilePath link = testRoot.resolve("symlink_to_file");

        try
        {
          Files.createSymbolicLink(link.toPath(), target.toPath());
        }
        catch (IOException | UnsupportedOperationException | SecurityException e)
        {
          System.out.println("Step 23 SKIPPED (symlink creation not supported: " + e.getMessage() + ')');
          FileDeletion.ofFile(target).nonInteractiveFailureOK().execute();
          return;
        }

        DeletionResult result = FileDeletion.ofFile(link).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 23 result");
        assertGone(link, "Step 23 symlink should be deleted");
        assertExists(target, "Step 23 target should still exist");

        FileDeletion.ofFile(target).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 9: Wide directory tree
    // -----------------------------------------------------------------------

    // Step 24: Directory with many sibling files

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath wideDir = testRoot.resolve("wide");

        for (int ndx = 0; ndx < 50; ndx++)
          createTestFile(wideDir, "file_" + ndx + ".txt");

        DeletionResult result = FileDeletion.ofDirWithContents(wideDir).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 24 result");
        assertGone(wideDir, "Step 24 wide dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 10: Idempotent re-deletion
    // -----------------------------------------------------------------------

    // Step 25: Batch delete, recreate, delete again

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f1 = createTestFile(testRoot, "idem1.txt"),
                 f2 = createTestFile(testRoot, "idem2.txt");

        // First deletion

        DeletionResult result1 = FileDeletion.ofFiles(List.of(f1, f2)).nonInteractive().execute();
        assertEquals(SUCCESS, result1, "Step 25 first deletion result");

        // Recreate

        createTestFile(testRoot, "idem1.txt");
        createTestFile(testRoot, "idem2.txt");

        // Second deletion

        DeletionResult result2 = FileDeletion.ofFiles(List.of(f1, f2)).nonInteractive().execute();
        assertEquals(SUCCESS, result2, "Step 25 second deletion result");

        assertGone(f1, "Step 25 f1 after second deletion");
        assertGone(f2, "Step 25 f2 after second deletion");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 11: Empty directory deletion
    // -----------------------------------------------------------------------

    // Step 26: Empty directory with ofDirWithContents

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath emptyDir = testRoot.resolve("empty_dir");
        Files.createDirectories(emptyDir.toPath());

        DeletionResult result = FileDeletion.ofDirWithContents(emptyDir).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 26 result");
        assertGone(emptyDir, "Step 26 empty dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 27: Empty directory with ofDirContentsOnly (directory remains)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath emptyDir = testRoot.resolve("empty_dir_contents");
        Files.createDirectories(emptyDir.toPath());

        DeletionResult result = FileDeletion.ofDirContentsOnly(emptyDir).nonInteractive().execute();

        assertEquals(SUCCESS, result, "Step 27 result");
        assertExists(emptyDir, "Step 27 empty dir should remain");
        assertDirEmpty(emptyDir, "Step 27 dir");

        FileDeletion.ofDirWithContents(emptyDir).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 12: Auto-detection consistency
    // -----------------------------------------------------------------------

    // Step 28: ofFileOrDirWithContents matches ofDirWithContents on same directory structure

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir1 = testRoot.resolve("autodetect1"),
                 dir2 = testRoot.resolve("autodetect2");

        createTestFile(dir1, "sub/file.txt");
        createTestFile(dir2, "sub/file.txt");

        DeletionResult result1 = FileDeletion.ofDirWithContents(dir1).nonInteractive().execute();
        DeletionResult result2 = FileDeletion.ofFileOrDirWithContents(dir2).nonInteractive().execute();

        assertEquals(SUCCESS, result1, "Step 28 ofDirWithContents result");
        assertEquals(SUCCESS, result2, "Step 28 ofFileOrDirWithContents result");
        assertGone(dir1, "Step 28 dir1");
        assertGone(dir2, "Step 28 dir2");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 13: Duplicate paths in batch
    // -----------------------------------------------------------------------

    // Step 29: Same file path twice in batch (second task sees file already gone)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath filePath = createTestFile(testRoot, "duplicate.txt");

        var batch = FileDeletion.ofFiles(List.of(filePath, filePath)).nonInteractive();
        DeletionResult result = batch.execute();

        assertEquals(SUCCESS, result, "Step 29 result");
        assertGone(filePath, "Step 29 file");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 29 no failures");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 30: Same directory path twice in batch

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("duplicate_dir");
        createTestFile(dir, "child.txt");

        var batch = FileDeletion.ofDirsWithContents(List.of(dir, dir)).nonInteractive();
        DeletionResult result = batch.execute();

        assertEquals(SUCCESS, result, "Step 30 result");
        assertGone(dir, "Step 30 dir");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 30 no failures");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 14: Builder × FOK/LOG on actual files (steps 31-38)
    // -----------------------------------------------------------------------

    // Step 31: FILE × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "fok_file.txt");
        assertEquals(SUCCESS, FileDeletion.ofFile(f).nonInteractiveFailureOK().execute(), "Step 31 result");
        assertGone(f, "Step 31 file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 32: FILE × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "log_file.txt");
        assertEquals(SUCCESS, FileDeletion.ofFile(f).nonInteractiveLogErrors().execute(), "Step 32 result");
        assertGone(f, "Step 32 file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 33: DWC × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("fok_dwc");
        createTestFile(dir, "sub/file.txt");
        assertEquals(SUCCESS, FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute(), "Step 33 result");
        assertGone(dir, "Step 33 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 34: DWC × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("log_dwc");
        createTestFile(dir, "sub/file.txt");
        assertEquals(SUCCESS, FileDeletion.ofDirWithContents(dir).nonInteractiveLogErrors().execute(), "Step 34 result");
        assertGone(dir, "Step 34 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 35: DCO × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("fok_dco");
        createTestFile(dir, "a.txt");
        createTestFile(dir, "sub/b.txt");
        assertEquals(SUCCESS, FileDeletion.ofDirContentsOnly(dir).nonInteractiveFailureOK().execute(), "Step 35 result");
        assertExists(dir, "Step 35 dir");
        assertDirEmpty(dir, "Step 35 dir");
        FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 36: DCO × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("log_dco");
        createTestFile(dir, "a.txt");
        createTestFile(dir, "sub/b.txt");
        assertEquals(SUCCESS, FileDeletion.ofDirContentsOnly(dir).nonInteractiveLogErrors().execute(), "Step 36 result");
        assertExists(dir, "Step 36 dir");
        assertDirEmpty(dir, "Step 36 dir");
        FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 37: FODWC × FOK (auto-detect as dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("fok_fodwc_dir");
        createTestFile(dir, "sub/file.txt");
        assertEquals(SUCCESS, FileDeletion.ofFileOrDirWithContents(dir).nonInteractiveFailureOK().execute(), "Step 37 result");
        assertGone(dir, "Step 37 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 38: FODWC × LOG (auto-detect as file)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "log_fodwc_file.txt");
        assertEquals(SUCCESS, FileDeletion.ofFileOrDirWithContents(f).nonInteractiveLogErrors().execute(), "Step 38 result");
        assertGone(f, "Step 38 file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 15: Builder × interactive on actual unlocked dirs (steps 39-41)
    // -----------------------------------------------------------------------

    // Step 39: DWC × I

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("i_dwc");
        createTestFile(dir, "sub/file.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        assertEquals(SUCCESS, FileDeletion.ofDirWithContents(dir).interactive().execute(), "Step 39 result");
        assertGone(dir, "Step 39 dir");
        assertNull(PopupRobot.getLastMessage(), "Step 39 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 40: DCO × I

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("i_dco");
        createTestFile(dir, "a.txt");
        createTestFile(dir, "sub/b.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        assertEquals(SUCCESS, FileDeletion.ofDirContentsOnly(dir).interactive().execute(), "Step 40 result");
        assertExists(dir, "Step 40 dir");
        assertDirEmpty(dir, "Step 40 dir");
        assertNull(PopupRobot.getLastMessage(), "Step 40 no popup expected");
        FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 41: FODWC × I (auto-detect as dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir = testRoot.resolve("i_fodwc_dir");
        createTestFile(dir, "sub/file.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        assertEquals(SUCCESS, FileDeletion.ofFileOrDirWithContents(dir).interactive().execute(), "Step 41 result");
        assertGone(dir, "Step 41 dir");
        assertNull(PopupRobot.getLastMessage(), "Step 41 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 16: Batch × DCO/FODWC × NI (steps 42-43)
    // -----------------------------------------------------------------------

    // Step 42: Batch DCO × NI

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("batch_dco1"),
                 d2 = testRoot.resolve("batch_dco2");
        createTestFile(d1, "a.txt");
        createTestFile(d2, "b.txt");

        var batch = FileDeletion.ofDirsContentsOnly(List.of(d1, d2)).nonInteractive();
        assertEquals(SUCCESS, batch.execute(), "Step 42 result");
        assertExists(d1, "Step 42 d1");
        assertDirEmpty(d1, "Step 42 d1");
        assertExists(d2, "Step 42 d2");
        assertDirEmpty(d2, "Step 42 d2");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 42 no failures");
        FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 43: Batch FODWC × NI (one file + one dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "batch_fodwc_file.txt");
        FilePath dir = testRoot.resolve("batch_fodwc_dir");
        createTestFile(dir, "child.txt");

        var batch = FileDeletion.ofFilesOrDirsWithContents(List.of(f, dir)).nonInteractive();
        assertEquals(SUCCESS, batch.execute(), "Step 43 result");
        assertGone(f, "Step 43 file");
        assertGone(dir, "Step 43 dir");
        assertTrue(batch.getFailedPaths().isEmpty(), "Step 43 no failures");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 17: Batch × FOK/LOG on actual files (steps 44-51)
    // -----------------------------------------------------------------------

    // Step 44: Batch FILE × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f1 = createTestFile(testRoot, "bfok1.txt"),
                 f2 = createTestFile(testRoot, "bfok2.txt"),
                 f3 = createTestFile(testRoot, "bfok3.txt");

        assertEquals(SUCCESS, FileDeletion.ofFiles(List.of(f1, f2, f3)).nonInteractiveFailureOK().execute(), "Step 44 result");
        assertGone(f1, "Step 44 f1");
        assertGone(f2, "Step 44 f2");
        assertGone(f3, "Step 44 f3");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 45: Batch FILE × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f1 = createTestFile(testRoot, "blog1.txt"),
                 f2 = createTestFile(testRoot, "blog2.txt"),
                 f3 = createTestFile(testRoot, "blog3.txt");

        assertEquals(SUCCESS, FileDeletion.ofFiles(List.of(f1, f2, f3)).nonInteractiveLogErrors().execute(), "Step 45 result");
        assertGone(f1, "Step 45 f1");
        assertGone(f2, "Step 45 f2");
        assertGone(f3, "Step 45 f3");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 46: Batch DWC × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("bfok_dwc1"),
                 d2 = testRoot.resolve("bfok_dwc2");
        createTestFile(d1, "sub/file.txt");
        createTestFile(d2, "sub/file.txt");

        assertEquals(SUCCESS, FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveFailureOK().execute(), "Step 46 result");
        assertGone(d1, "Step 46 d1");
        assertGone(d2, "Step 46 d2");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 47: Batch DWC × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("blog_dwc1"),
                 d2 = testRoot.resolve("blog_dwc2");
        createTestFile(d1, "sub/file.txt");
        createTestFile(d2, "sub/file.txt");

        assertEquals(SUCCESS, FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveLogErrors().execute(), "Step 47 result");
        assertGone(d1, "Step 47 d1");
        assertGone(d2, "Step 47 d2");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 48: Batch DCO × FOK

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("bfok_dco1"),
                 d2 = testRoot.resolve("bfok_dco2");
        createTestFile(d1, "a.txt");
        createTestFile(d2, "b.txt");

        assertEquals(SUCCESS, FileDeletion.ofDirsContentsOnly(List.of(d1, d2)).nonInteractiveFailureOK().execute(), "Step 48 result");
        assertExists(d1, "Step 48 d1");
        assertDirEmpty(d1, "Step 48 d1");
        assertExists(d2, "Step 48 d2");
        assertDirEmpty(d2, "Step 48 d2");
        FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 49: Batch DCO × LOG

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("blog_dco1"),
                 d2 = testRoot.resolve("blog_dco2");
        createTestFile(d1, "a.txt");
        createTestFile(d2, "b.txt");

        assertEquals(SUCCESS, FileDeletion.ofDirsContentsOnly(List.of(d1, d2)).nonInteractiveLogErrors().execute(), "Step 49 result");
        assertExists(d1, "Step 49 d1");
        assertDirEmpty(d1, "Step 49 d1");
        assertExists(d2, "Step 49 d2");
        assertDirEmpty(d2, "Step 49 d2");
        FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 50: Batch FODWC × FOK (file + dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "bfok_fodwc_file.txt");
        FilePath dir = testRoot.resolve("bfok_fodwc_dir");
        createTestFile(dir, "child.txt");

        assertEquals(SUCCESS, FileDeletion.ofFilesOrDirsWithContents(List.of(f, dir)).nonInteractiveFailureOK().execute(), "Step 50 result");
        assertGone(f, "Step 50 file");
        assertGone(dir, "Step 50 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 51: Batch FODWC × LOG (file + dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "blog_fodwc_file.txt");
        FilePath dir = testRoot.resolve("blog_fodwc_dir");
        createTestFile(dir, "child.txt");

        assertEquals(SUCCESS, FileDeletion.ofFilesOrDirsWithContents(List.of(f, dir)).nonInteractiveLogErrors().execute(), "Step 51 result");
        assertGone(f, "Step 51 file");
        assertGone(dir, "Step 51 dir");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 18: Batch × interactive clean success (steps 52-55)
    // -----------------------------------------------------------------------

    // Step 52: Batch FILE × I

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f1 = createTestFile(testRoot, "bi_f1.txt"),
                 f2 = createTestFile(testRoot, "bi_f2.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        var batch = FileDeletion.ofFiles(List.of(f1, f2)).interactive();
        assertEquals(SUCCESS, batch.execute(), "Step 52 result");
        assertGone(f1, "Step 52 f1");
        assertGone(f2, "Step 52 f2");
        assertNull(PopupRobot.getLastMessage(), "Step 52 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 53: Batch DWC × I

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("bi_dwc1"),
                 d2 = testRoot.resolve("bi_dwc2");
        createTestFile(d1, "sub/file.txt");
        createTestFile(d2, "sub/file.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        var batch = FileDeletion.ofDirsWithContents(List.of(d1, d2)).interactive();
        assertEquals(SUCCESS, batch.execute(), "Step 53 result");
        assertGone(d1, "Step 53 d1");
        assertGone(d2, "Step 53 d2");
        assertNull(PopupRobot.getLastMessage(), "Step 53 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 54: Batch DCO × I

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath d1 = testRoot.resolve("bi_dco1"),
                 d2 = testRoot.resolve("bi_dco2");
        createTestFile(d1, "a.txt");
        createTestFile(d2, "b.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        var batch = FileDeletion.ofDirsContentsOnly(List.of(d1, d2)).interactive();
        assertEquals(SUCCESS, batch.execute(), "Step 54 result");
        assertExists(d1, "Step 54 d1");
        assertDirEmpty(d1, "Step 54 d1");
        assertExists(d2, "Step 54 d2");
        assertDirEmpty(d2, "Step 54 d2");
        assertNull(PopupRobot.getLastMessage(), "Step 54 no popup expected");
        FileDeletion.ofDirsWithContents(List.of(d1, d2)).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // Step 55: Batch FODWC × I (file + dir)

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "bi_fodwc_file.txt");
        FilePath dir = testRoot.resolve("bi_fodwc_dir");
        createTestFile(dir, "child.txt");

        PopupRobot.clear();
        PopupRobot.setDefaultResponse(mrCancel);

        var batch = FileDeletion.ofFilesOrDirsWithContents(List.of(f, dir)).interactive();
        assertEquals(SUCCESS, batch.execute(), "Step 55 result");
        assertGone(f, "Step 55 file");
        assertGone(dir, "Step 55 dir");
        assertNull(PopupRobot.getLastMessage(), "Step 55 no popup expected");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 18B: Multi-pass directory tree walk — cloud-storage simulation (step 56)
    //
    // Exercises the 3-pass retry in deleteDirectoryTreeWalk by having a background
    // thread inject a new file into a subdirectory while deletion is in progress.
    // This simulates a cloud-storage sync daemon (e.g. Dropbox on macOS) that
    // creates metadata files between walk passes. The subdirectory is populated
    // with many files so the deletion loop takes long enough for the injection
    // to land reliably during the first pass. The test always asserts SUCCESS;
    // the injected.get() log count reports whether the race was actually exercised.
    // -----------------------------------------------------------------------

    // Step 56: DCO multi-pass — injected file cleaned up by retry

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath dir    = testRoot.resolve("multipass"),
                 subdir = dir.resolve("sub");

        for (int ndx = 0; ndx < 100; ndx++)
          createTestFile(subdir, "file_" + ndx + ".txt");

        AtomicInteger injected = new AtomicInteger();
        Path subdirPath = subdir.toPath();

        try (ScheduledExecutorService exec = Executors.newSingleThreadScheduledExecutor())
        {
          // Inject a file shortly after deletion begins to simulate a sync daemon
          // creating a metadata file between walk passes. The 5ms delay targets the
          // deletion phase (after the walk has closed its stream) so the injected file
          // is not in the collected path list and survives pass 1.

          exec.schedule(() ->
          {
            try
            {
              if (Files.exists(subdirPath))
              {
                Files.writeString(subdirPath.resolve("injected.txt"), "injected");
                injected.incrementAndGet();
              }
            }
            catch (IOException ignored) { }
          }, 5, TimeUnit.MILLISECONDS);

          DeletionResult result = FileDeletion.ofDirContentsOnly(dir).nonInteractive().execute();

          assertEquals(SUCCESS, result, "Step 56 result");
          assertExists(dir,   "Step 56 dir should survive (DCO)");
          assertDirEmpty(dir, "Step 56 dir should be empty");
        }

        System.out.println("Step 56: multi-pass walk OK (files injected mid-deletion: " + injected.get() + ')');
        FileDeletion.ofDirWithContents(dir).nonInteractiveFailureOK().execute();
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 19: FAILED result via nonInteractive (steps 57-59, Windows-only)
    // -----------------------------------------------------------------------

    // Step 57: Builder NI on locked file → FAILED

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked = createTestFile(testRoot, "step57_locked.txt");
        raf = lockFile(locked);

        assertEquals(FAILED, FileDeletion.ofFile(locked).nonInteractive().execute(), "Step 57 result");
        assertExists(locked, "Step 57 locked file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // Step 58: Batch NI on two locked files → FAILED

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf1 = null, raf2 = null;

      try
      {
        FilePath locked1 = createTestFile(testRoot, "step58_locked1.txt"),
                 locked2 = createTestFile(testRoot, "step58_locked2.txt");
        raf1 = lockFile(locked1);
        raf2 = lockFile(locked2);

        var batch = FileDeletion.ofFiles(List.of(locked1, locked2)).nonInteractive();
        assertEquals(FAILED, batch.execute(), "Step 58 result");
        assertTrue(batch.getFailedPaths().contains(locked1), "Step 58 locked1 in failedPaths");
        assertTrue(batch.getFailedPaths().contains(locked2), "Step 58 locked2 in failedPaths");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf1); closeQuietly(raf2); }
    }).windowsOnly();

    // Step 59: Batch NI on locked + unlocked → PARTIAL

    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try
      {
        FilePath locked   = createTestFile(testRoot, "step59_locked.txt"),
                 unlocked = createTestFile(testRoot, "step59_unlocked.txt");
        raf = lockFile(locked);

        var batch = FileDeletion.ofFiles(List.of(locked, unlocked)).nonInteractive();
        assertEquals(PARTIAL, batch.execute(), "Step 59 result");
        assertTrue(batch.getFailedPaths().contains(locked), "Step 59 locked in failedPaths");
        assertGone(unlocked, "Step 59 unlocked file");
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally { closeQuietly(raf); }
    }).windowsOnly();

    // -----------------------------------------------------------------------
    // Phase 20: Batch type validation gap (step 60)
    // -----------------------------------------------------------------------

    // Step 60: ofDirsContentsOnly with file path → IllegalArgumentException

    seq.thenRunAfterDelay(() ->
    {
      try
      {
        FilePath f = createTestFile(testRoot, "type_val_dco.txt");

        try
        {
          FileDeletion.ofDirsContentsOnly(List.of(f)).nonInteractive().execute();
          fail("Step 60: Expected IllegalArgumentException");
        }
        catch (IllegalArgumentException e)
        {
          assertTrue(e.getMessage().contains("Expected directory, got file"), "Step 60 exception message");
        }
        finally
        {
          FileDeletion.ofFile(f).nonInteractiveFailureOK().execute();
        }
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    });

    // -----------------------------------------------------------------------
    // Phase 21-22: Timed-release tests (steps 61-162, Windows-only)
    // -----------------------------------------------------------------------
    //
    // These tests use RandomAccessFile("rw") to hold Windows file handles,
    // preventing Files.delete() from succeeding. ScheduledExecutorService
    // releases the handles at specified times to test retry progressions.
    // On non-Windows, all timed steps are SKIPPED because POSIX doesn't
    // prevent deletion of files with open handles.

    // Builder timed tests: 5 targets × 10 progressions = 50 tests (steps 61-110)

    TestTarget[] builderTargets =
    {
      TestTarget.FILE, TestTarget.DIR_WITH_CONTENTS, TestTarget.DIR_CONTENTS_ONLY,
      TestTarget.FODWC_AS_FILE, TestTarget.FODWC_AS_DIR
    };

    int stepNum = 61;

    for (TestTarget target : builderTargets)
    {
      // NI: fail → success within 5s
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          1500, SUCCESS, true, false);

      // NI: fail → timeout → FAILED
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          -1, FAILED, false, false);

      // FOK: fail → success within 5s
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "FOK",
          1500, SUCCESS, true, false);

      // FOK: fail → timeout → SUCCESS (file stays)
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "FOK",
          -1, SUCCESS, false, false);

      // LOG: fail → success within 5s (no log)
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "LOG",
          1500, SUCCESS, true, false);

      // LOG: fail → timeout → SUCCESS (logged)
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "LOG",
          -1, SUCCESS, false, true);

      // I: auto-retry success (no prompt)
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          1500, SUCCESS, true, false, mrCancel);

      // I: prompt → retry → success
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          4000, SUCCESS, true, false, mrRetry, mrCancel);

      // I: prompt → retry → prompt → retry → success
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          7000, SUCCESS, true, false, mrRetry, mrRetry, mrCancel);

      // I: prompt → cancel → CANCELLED
      addBuilderTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          -1, CANCELLED, false, false, mrCancel);
    }

    // Batch timed tests: 4 targets × 13 progressions = 52 tests (steps 111-162)

    TestTarget[] batchTargets =
    {
      TestTarget.FILE, TestTarget.DIR_WITH_CONTENTS, TestTarget.DIR_CONTENTS_ONLY,
      TestTarget.FODWC_AS_FILE
    };

    for (TestTarget target : batchTargets)
    {
      // NI: staggered all succeed
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          3, new long[] { 500, 1500, 2500 }, SUCCESS, 0, false);

      // NI: immediate partial + stall + success
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          3, new long[] { 0, 1500, 3000 }, SUCCESS, 0, false);

      // NI: some succeed + timeout → PARTIAL
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          3, new long[] { 1000, -1, -1 }, PARTIAL, 2, false);

      // NI: all timeout → FAILED
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "NI",
          3, new long[] { -1, -1, -1 }, FAILED, 3, false);

      // FOK: staggered all succeed
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "FOK",
          3, new long[] { 500, 1500, 2500 }, SUCCESS, 0, false);

      // FOK: all fail → SUCCESS (items remain)
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "FOK",
          3, new long[] { -1, -1, -1 }, SUCCESS, 0, false);

      // LOG: staggered all succeed (no log)
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "LOG",
          3, new long[] { 500, 1500, 2500 }, SUCCESS, 0, false);

      // LOG: all fail → SUCCESS (logged)
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "LOG",
          3, new long[] { -1, -1, -1 }, SUCCESS, 0, true);

      // I: auto-retry all succeed (no prompt)
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          3, new long[] { 500, 1000, 1500 }, SUCCESS, 0, false, mrCancel);

      // I: 1 retry round → all succeed
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          3, new long[] { 1000, 4000, 4500 }, SUCCESS, 0, false, mrRetry, mrCancel);

      // I: 2 retry rounds → all succeed
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          3, new long[] { 1000, 4000, 7000 }, SUCCESS, 0, false, mrRetry, mrRetry, mrCancel);

      // I: partial + user skip → PARTIAL
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          2, new long[] { 1000, -1 }, PARTIAL, 1, false, mrIgnore);

      // I: all fail + user cancel → CANCELLED
      addBatchTimedStep(seq, testRoot, logFile, "Step " + stepNum++, target, "I",
          2, new long[] { -1, -1 }, CANCELLED, 0, false, mrCancel);
    }

    // -----------------------------------------------------------------------
    // Phase 23: Builder × Interactive Phase 1 Cancel (steps 163-167, Windows-only)
    //
    // Tests that cancelling the progress dialog (Phase 1) returns CANCELLED
    // without ever reaching Phase 2. One test per TestTarget.
    // -----------------------------------------------------------------------

    for (TestTarget target : builderTargets)
    {
      final String label = "Step " + stepNum++;

      seq.thenRunAfterDelay(() ->
      {
        RandomAccessFile raf = null;

        try
        {
          FilePath targetPath = createTarget(testRoot, label.replace(' ', '_') + "_P1_" + target, target);
          raf = lockTarget(targetPath, target);

          PopupRobot.clear();
          PopupRobot.setCancelNextProgressDialog(true);
          PopupRobot.setDefaultResponse(mrCancel);  // safety net

          DeletionResult result = executeBuilder(targetPath, target, "I");

          assertEquals(CANCELLED, result, label + " result (" + target + ')');
          assertTargetPresent(targetPath, target, label);
          assertEquals(0, PopupRobot.getInvocationCount(), label + " popup count (Phase 2 should not be reached)");

          System.out.println(label + ": Phase 1 cancel " + target + " OK");
        }
        catch (IOException e) { throw new UncheckedIOException(e); }
        finally
        {
          closeQuietly(raf);
        }
      }).windowsOnly();
    }

    // -----------------------------------------------------------------------
    // Phase 24A: Batch × Interactive Phase 1 Cancel, all locked (steps 168-171, Windows-only)
    //
    // Both items locked, cancel at progress dialog. Verifies CANCELLED result,
    // both targets present, zero popups, empty failedPaths.
    // -----------------------------------------------------------------------

    for (TestTarget target : batchTargets)
    {
      final String label = "Step " + stepNum++;

      seq.thenRunAfterDelay(() ->
      {
        RandomAccessFile raf1 = null, raf2 = null;

        try
        {
          FilePath path1 = createTarget(testRoot, label.replace(' ', '_') + "_P1A_" + target + "_1", target),
                   path2 = createTarget(testRoot, label.replace(' ', '_') + "_P1A_" + target + "_2", target);

          raf1 = lockTarget(path1, target);
          raf2 = lockTarget(path2, target);

          PopupRobot.clear();
          PopupRobot.setCancelNextProgressDialog(true);
          PopupRobot.setDefaultResponse(mrCancel);  // safety net

          var batch = executeBatch(List.of(path1, path2), target, "I");
          DeletionResult result = batch.execute();

          assertEquals(CANCELLED, result, label + " result (" + target + ')');
          assertTargetPresent(path1, target, label + " item 1");
          assertTargetPresent(path2, target, label + " item 2");
          assertEquals(0, PopupRobot.getInvocationCount(), label + " popup count");
          assertTrue(batch.getFailedPaths().isEmpty(), label + " failedPaths should be empty on CANCELLED");

          System.out.println(label + ": Phase 1 cancel batch " + target + " (all locked) OK");
        }
        catch (IOException e) { throw new UncheckedIOException(e); }
        finally
        {
          closeQuietly(raf1);
          closeQuietly(raf2);
        }
      }).windowsOnly();
    }

    // -----------------------------------------------------------------------
    // Phase 24B: Batch × Interactive Phase 1 Cancel, partial progress (steps 172-175, Windows-only)
    //
    // Two items locked; first lock released at 100ms so it's deleted during
    // the pre-dialog retry window. Second stays locked. Progress dialog cancel
    // fires at 300ms. Verifies first item gone, second present, zero popups.
    // -----------------------------------------------------------------------

    for (TestTarget target : batchTargets)
    {
      final String label = "Step " + stepNum++;

      seq.thenRunAfterDelay(() ->
      {
        RandomAccessFile raf1 = null, raf2 = null;

        try (ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor())
        {
          FilePath path1 = createTarget(testRoot, label.replace(' ', '_') + "_P1B_" + target + "_1", target),
                   path2 = createTarget(testRoot, label.replace(' ', '_') + "_P1B_" + target + "_2", target);

          raf1 = lockTarget(path1, target);
          raf2 = lockTarget(path2, target);

          // Release first lock at 100ms so it's deleted during retry window
          final RandomAccessFile handle1 = raf1;
          executor.schedule(() -> closeQuietly(handle1), 100, TimeUnit.MILLISECONDS);

          PopupRobot.clear();
          PopupRobot.setCancelNextProgressDialog(true);
          PopupRobot.setDefaultResponse(mrCancel);  // safety net

          var batch = executeBatch(List.of(path1, path2), target, "I");
          DeletionResult result = batch.execute();

          assertEquals(CANCELLED, result, label + " result (" + target + ')');
          assertTargetGone(path1, target, label + " item 1 (released)");
          assertTargetPresent(path2, target, label + " item 2 (locked)");
          assertEquals(0, PopupRobot.getInvocationCount(), label + " popup count");
          assertTrue(batch.getFailedPaths().isEmpty(), label + " failedPaths should be empty on CANCELLED");

          raf1 = null;  // already released by scheduler

          System.out.println(label + ": Phase 1 cancel batch " + target + " (partial progress) OK");
        }
        catch (IOException e) { throw new UncheckedIOException(e); }
        finally
        {
          closeQuietly(raf1);
          closeQuietly(raf2);
        }
      }).windowsOnly();
    }

    // -----------------------------------------------------------------------
    // Phase 25: External deletion during interactive retry (steps 176-180, Windows-only)
    //
    // Target is locked, then lock released + target deleted externally at 600ms.
    // The retry loop discovers the target is gone and returns SUCCESS.
    // Exercises different code paths per target type:
    //   FILE/FODWC_AS_FILE: deleteIfExists returns false (file already gone)
    //   DWC/FODWC_AS_DIR: Files.walk throws; catch block sees exists()==false
    //   DCO: tree walk finds directory empty after external content deletion
    // -----------------------------------------------------------------------

    for (TestTarget target : builderTargets)
    {
      final String label = "Step " + stepNum++;

      seq.thenRunAfterDelay(() ->
      {
        RandomAccessFile raf = null;

        try (ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor())
        {
          FilePath targetPath = createTarget(testRoot, label.replace(' ', '_') + "_ExtDel_" + target, target);
          raf = lockTarget(targetPath, target);

          final RandomAccessFile handle = raf;
          executor.schedule(() ->
          {
            closeQuietly(handle);
            deleteTargetExternally(targetPath, target);
          }, 600, TimeUnit.MILLISECONDS);

          PopupRobot.clear();
          PopupRobot.setDefaultResponse(mrCancel);  // safety net

          DeletionResult result = executeBuilder(targetPath, target, "I");

          assertEquals(SUCCESS, result, label + " result (" + target + ')');
          assertTargetGone(targetPath, target, label);
          assertEquals(0, PopupRobot.getInvocationCount(), label + " no popup expected");

          raf = null;  // already released by scheduler

          System.out.println(label + ": external deletion during retry " + target + " OK");
        }
        catch (IOException e) { throw new UncheckedIOException(e); }
        finally
        {
          closeQuietly(raf);
        }
      }).windowsOnly();
    }

    seq.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Builder timed-release test step adder ---

  /**
   * Add a Builder timed-release test step to the sequencer. Creates a target,
   * locks it, schedules release, executes deletion, asserts result.
   * Windows-only; SKIPPED on POSIX.
   *
   * @param responses For interactive mode: the first N-1 are enqueued, the last is the default.
   *                  For non-interactive: ignored (pass nothing).
   */
  private static void addBuilderTimedStep
  ( FXTestSequencer seq, FilePath localTestRoot, FilePath logFile,
    String stepLabel, TestTarget target, String retryMode,
    long releaseMs, DeletionResult expected, boolean expectGone,
    boolean expectLog, DialogResult... responses)
  {
    seq.thenRunAfterDelay(() ->
    {
      RandomAccessFile raf = null;

      try (ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor())
      {
        FilePath targetPath = createTarget(localTestRoot, stepLabel.replace(' ', '_') + '_' + target, target);
        raf = lockTarget(targetPath, target);

        if (releaseMs >= 0)
        {
          final RandomAccessFile handle = raf;
          executor.schedule(() -> closeQuietly(handle), releaseMs, TimeUnit.MILLISECONDS);
        }

        // Configure PopupRobot for interactive mode

        if (responses.length > 0)
        {
          PopupRobot.clear();

          if (responses.length > 1)
            PopupRobot.enqueueResponses(Arrays.copyOf(responses, responses.length - 1));

          PopupRobot.setDefaultResponse(responses[responses.length - 1]);
        }

        // Record log position for LOG mode assertions

        long logStartPos = expectLog ? logFile.toFile().length() : 0;

        if (expectLog)
          System.out.println("An error stack trace is expected below.");

        // Execute

        DeletionResult result = executeBuilder(targetPath, target, retryMode);

        // Assert result

        assertEquals(expected, result, stepLabel + " result (" + target + " × " + retryMode + ')');

        // Assert file state

        if (expectGone)
          assertTargetGone(targetPath, target, stepLabel);
        else
          assertTargetPresent(targetPath, target, stepLabel);

        // Assert log output

        if (expectLog)
          assertLogContains(logFile, logStartPos, targetPath.getNameOnly().toString(), stepLabel + " log");

        // Assert popup invocation count for interactive mode

        if (responses.length > 0)
        {
          int expectedPopups = (expected == SUCCESS) ? responses.length - 1 : responses.length;
          assertEquals(expectedPopups, PopupRobot.getInvocationCount(),
              stepLabel + " popup count (" + target + " × " + retryMode + ')');
        }

        // Mark lock as consumed by scheduled release (if applicable)

        if (releaseMs >= 0)
          raf = null;
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally
      {
        closeQuietly(raf);
      }
    }).windowsOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Batch timed-release test step adder ---

  /**
   * Add a Batch timed-release test step to the sequencer. Creates multiple targets,
   * locks them, schedules staggered releases, executes batch deletion, asserts result.
   * Windows-only; SKIPPED on POSIX.
   *
   * @param responses For interactive mode: the first N-1 are enqueued, the last is the default.
   */
  @SuppressWarnings("resource") // locks are closed in the finally block via closeQuietly
  private static void addBatchTimedStep
  ( FXTestSequencer seq, FilePath localTestRoot, FilePath logFile,
    String stepLabel, TestTarget target, String retryMode,
    int itemCount, long[] releaseMs, DeletionResult expected,
    int expectedFailCount, boolean expectLog, DialogResult... responses)
  {
    seq.thenRunAfterDelay(() ->
    {
      List<RandomAccessFile> locks = new ArrayList<>();

      try (ScheduledExecutorService executor = Executors.newSingleThreadScheduledExecutor())
      {
        List<FilePath> paths = new ArrayList<>();

        for (int ndx = 0; ndx < itemCount; ndx++)
        {
          FilePath targetPath = createTarget(localTestRoot,
              stepLabel.replace(' ', '_') + '_' + target + '_' + ndx, target);
          paths.add(targetPath);

          RandomAccessFile raf = lockTarget(targetPath, target);
          locks.add(raf);

          if (releaseMs[ndx] >= 0)
          {
            final RandomAccessFile handle = raf;
            executor.schedule(() -> closeQuietly(handle), releaseMs[ndx], TimeUnit.MILLISECONDS);
          }
        }

        // Configure PopupRobot

        if (responses.length > 0)
        {
          PopupRobot.clear();

          if (responses.length > 1)
            PopupRobot.enqueueResponses(Arrays.copyOf(responses, responses.length - 1));

          PopupRobot.setDefaultResponse(responses[responses.length - 1]);
        }

        // Record log position

        long logStartPos = expectLog ? logFile.toFile().length() : 0;

        if (expectLog)
        {
          long lockedCount = Arrays.stream(releaseMs).filter(ms -> ms < 0).count();
          System.out.println(lockedCount + " error stack trace(s) expected below.");
        }

        // Execute

        BatchBuilder batch = executeBatch(paths, target, retryMode);
        DeletionResult result = batch.execute();

        // Assert result

        assertEquals(expected, result,
            stepLabel + " result (" + target + " × " + retryMode + ')');

        // Assert failed paths count (for NI modes that track failures)

        if ((expected == PARTIAL) || (expected == FAILED))
          assertEquals(expectedFailCount, batch.getFailedPaths().size(),
              stepLabel + " failedPaths count");

        // Assert log output

        if (expectLog)
        {
          // For LOG mode, we need to verify that errors were actually logged.
          // The log file should contain references to at least one of the target paths.

          boolean foundAny = false;

          for (FilePath p : paths)
          {
            try
            {
              assertLogContains(logFile, logStartPos, p.getNameOnly().toString(), stepLabel + " log");
              foundAny = true;
              break;
            }
            catch (AssertionError ignored) { }
          }

          assertTrue(foundAny, stepLabel + " expected log output for at least one path");
        }

        // Assert popup invocation count for interactive mode

        if (responses.length > 0)
        {
          int expectedPopups = (expected == SUCCESS) ? responses.length - 1 : responses.length;
          assertEquals(expectedPopups, PopupRobot.getInvocationCount(),
              stepLabel + " popup count (" + target + " × " + retryMode + ')');
        }

        // Assert failedPaths content for PARTIAL/FAILED results

        if ((expected == PARTIAL) || (expected == FAILED))
        {
          Set<FilePath> failedSet = batch.getFailedPaths();

          for (int ndx = 0; ndx < itemCount; ndx++)
          {
            if (releaseMs[ndx] == -1)
              assertTrue(failedSet.contains(paths.get(ndx)),
                  stepLabel + " failedPaths should contain item " + ndx);
            else
              assertFalse(failedSet.contains(paths.get(ndx)),
                  stepLabel + " failedPaths should not contain item " + ndx);
          }
        }

      }
      catch (IOException e) { throw new UncheckedIOException(e); }
      finally
      {
        for (RandomAccessFile raf : locks)
          closeQuietly(raf);
      }
    }).windowsOnly();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Target creation ---

  /**
   * Create a test target (file or dir with children) and return its root path.
   */
  private static FilePath createTarget(FilePath parentDir, String name, TestTarget target) throws IOException
  {
    return switch (target)
    {
      case FILE, FODWC_AS_FILE ->
        createTestFile(parentDir, name + ".txt");

      case DIR_WITH_CONTENTS, DIR_CONTENTS_ONLY, FODWC_AS_DIR ->
      {
        FilePath dir = parentDir.resolve(name);
        createTestFile(dir, "child.txt");
        yield dir;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Lock the target by opening a RandomAccessFile on the relevant file.
   * For file targets, locks the file itself.
   * For directory targets, locks the child file within the directory.
   */
  private static RandomAccessFile lockTarget(FilePath targetPath, TestTarget target) throws IOException
  {
    return switch (target)
    {
      case FILE, FODWC_AS_FILE ->
        lockFile(targetPath);

      case DIR_WITH_CONTENTS, DIR_CONTENTS_ONLY, FODWC_AS_DIR ->
        lockFile(targetPath.resolve("child.txt"));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Delete a target externally, simulating deletion by another application or user.
   * For FILE targets, deletes the file. For DWC targets, deletes the entire directory
   * tree (so Files.walk throws on retry). For DCO, deletes contents only.
   */
  private static void deleteTargetExternally(FilePath target, TestTarget type)
  {
    try
    {
      Path root = target.toPath();

      switch (type)
      {
        case FILE, FODWC_AS_FILE -> Files.deleteIfExists(root);

        case DIR_WITH_CONTENTS, FODWC_AS_DIR ->
        {
          try (var walk = Files.walk(root))
          {
            for (Path p : walk.sorted(Comparator.reverseOrder()).toList())
              Files.deleteIfExists(p);
          }
        }

        case DIR_CONTENTS_ONLY ->
        {
          try (var walk = Files.walk(root))
          {
            for (Path p : walk.sorted(Comparator.reverseOrder()).toList())
            {
              if (p.equals(root) == false)
                Files.deleteIfExists(p);
            }
          }
        }
      }
    }
    catch (IOException e) { e.printStackTrace(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Execution helpers ---

  /**
   * Execute a Builder deletion for the given target and retry mode.
   */
  private static DeletionResult executeBuilder(FilePath path, TestTarget target, String retryMode)
  {
    var builder = switch (target)
    {
      case FILE              -> FileDeletion.ofFile(path);
      case DIR_WITH_CONTENTS -> FileDeletion.ofDirWithContents(path);
      case DIR_CONTENTS_ONLY -> FileDeletion.ofDirContentsOnly(path);
      case FODWC_AS_FILE, FODWC_AS_DIR -> FileDeletion.ofFileOrDirWithContents(path);
    };

    return switch (retryMode)
    {
      case "NI"  -> builder.nonInteractive().execute();
      case "FOK" -> builder.nonInteractiveFailureOK().execute();
      case "LOG" -> builder.nonInteractiveLogErrors().execute();
      case "I"   -> builder.interactive().execute();
      default    -> throw new IllegalArgumentException("Unknown retry mode: " + retryMode);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a Batch deletion builder for the given paths, target type, and retry mode.
   * Returns the BatchBuilder so the caller can inspect getFailedPaths() after execute().
   */
  private static BatchBuilder executeBatch(List<FilePath> paths, TestTarget target, String retryMode)
  {
    var builder = switch (target)
    {
      case FILE              -> FileDeletion.ofFiles(paths);
      case DIR_WITH_CONTENTS -> FileDeletion.ofDirsWithContents(paths);
      case DIR_CONTENTS_ONLY -> FileDeletion.ofDirsContentsOnly(paths);
      case FODWC_AS_FILE, FODWC_AS_DIR -> FileDeletion.ofFilesOrDirsWithContents(paths);
    };

    return switch (retryMode)
    {
      case "NI"  -> builder.nonInteractive();
      case "FOK" -> builder.nonInteractiveFailureOK();
      case "LOG" -> builder.nonInteractiveLogErrors();
      case "I"   -> builder.interactive();
      default    -> throw new IllegalArgumentException("Unknown retry mode: " + retryMode);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Target assertion helpers ---

  /**
   * Assert that the target was successfully deleted.
   * For DCO targets: directory still exists but is empty.
   * For all other targets: path does not exist.
   */
  private static void assertTargetGone(FilePath path, TestTarget target, String label)
  {
    if (target == TestTarget.DIR_CONTENTS_ONLY)
    {
      assertExists(path, label + " DCO dir should still exist");
      assertDirEmpty(path, label + " DCO dir should be empty");

      // Clean up the empty directory
      FileDeletion.ofDirWithContents(path).nonInteractiveFailureOK().execute();
    }
    else
      assertGone(path, label + " target");
  }

//---------------------------------------------------------------------------

  /**
   * Assert that the target still exists (deletion failed or was not attempted).
   * For DCO targets: directory exists and still has contents (locked child).
   * For all other targets: path exists.
   */
  private static void assertTargetPresent(FilePath path, TestTarget target, String label)
  {
    assertExists(path, label + " target should still exist");

    if (target == TestTarget.DIR_CONTENTS_ONLY)
    {
      // DCO target that failed deletion should still have contents (the locked child)

      try
      {
        assertTrue(path.dirContainsAnyFiles(true), label + " DCO dir should still have contents: " + path);
      }
      catch (IOException e) { throw new UncheckedIOException(e); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- File creation and locking utilities ---

  private static FilePath createTestFile(FilePath parent, String name) throws IOException
  {
    FilePath filePath = parent.resolve(name);
    filePath.getDirOnly().createDirectories();
    Files.writeString(filePath.toPath(), name);
    return filePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Open a RandomAccessFile in "rw" mode to hold a Windows file handle.
   * On Windows, this prevents Files.delete() from succeeding because
   * the file handle is opened without FILE_SHARE_DELETE.
   */
  private static RandomAccessFile lockFile(FilePath filePath) throws IOException
  {
    return new RandomAccessFile(filePath.toFile(), "rw");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Close a Closeable, suppressing any IOException.
   */
  private static void closeQuietly(Closeable c)
  {
    if (c != null)
    {
      try { c.close(); }
      catch (IOException ignored) { }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Assertion helpers ---

  private static void assertGone(FilePath filePath, String label)
  {
    assertFalse(filePath.exists(), label + " should not exist: " + filePath);
  }

//---------------------------------------------------------------------------

  private static void assertExists(FilePath filePath, String label)
  {
    assertTrue(filePath.exists(), label + " should exist: " + filePath);
  }

//---------------------------------------------------------------------------

  private static void assertDirEmpty(FilePath dir, String label)
  {
    try
    {
      assertFalse(dir.dirContainsAnyFiles(true), label + " should be empty: " + dir);
    }
    catch (IOException e) { throw new UncheckedIOException(e); }
  }

//---------------------------------------------------------------------------

  /**
   * Assert that the log file's tail (lines appended since {@code startPos})
   * contains the given substring. Flushes stderr first so the tee has written
   * through to the file.
   */
  private static void assertLogContains(FilePath logFile, long startPos, String substring, String label)
  {
    System.err.flush();

    try (RandomAccessFile raf = new RandomAccessFile(logFile.toFile(), "r"))
    {
      long length = raf.length();

      assertTrue(length > startPos, label + ": no new log output was written");

      raf.seek(startPos);

      byte[] buf = new byte[(int) (length - startPos)];
      raf.readFully(buf);

      String tail = new String(buf);

      assertTrue(tail.contains(substring), label + ": log tail does not contain \"" + substring + '"');
    }
    catch (IOException e) { throw new UncheckedIOException(e); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
