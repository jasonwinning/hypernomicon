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

package org.hypernomicon.util.file.deletion;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.prefs.Preferences;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import org.hypernomicon.App;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Tests for {@link org.hypernomicon.util.file.deletion.FileDeletion} API contract enforcement.
 * <p>
 * These tests verify: null rejection, factory method validation, mutual exclusivity
 * of retry mode options, prevention of double execution, post-execution modification
 * guards, execution results for nonexistent and empty inputs, defensive copying,
 * {@code getFailedPaths} unmodifiability, and ancestor/descendant batch behavior.
 * <p>
 * Most tests use non-existent paths to avoid filesystem operations. Factory method
 * validation tests use real paths (hdb file and directory from app preferences) to verify type checks.
 */
class FileDeletionTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Non-existent path used for builder validation tests (no filesystem operations occur) */
  private static final FilePath NONEXISTENT = new FilePath("_nonexistent_test_path_");

  /** Known file and directory from app preferences; null if not configured or missing */
  private static final FilePath HDB_FILE, HDB_DIR;

  static
  {
    FilePath file = null, dir = null;

    try
    {
      Preferences prefs = Preferences.userNodeForPackage(App.class);
      String srcPath = prefs.get("sourcePath", ""),
             srcName = prefs.get("sourceFile", "");

      if ((srcPath.isBlank() == false) && (srcName.isBlank() == false))
      {
        FilePath candidate = new FilePath(srcPath).resolve(srcName);

        if (candidate.exists())
        {
          file = candidate;
          dir = new FilePath(srcPath);
        }
      }
    }
    catch (SecurityException ignored) { }

    HDB_FILE = file;
    HDB_DIR = dir;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

//region Null Rejection
//---------------------------------------------------------------------------

  @Test
  void ofFile_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofFile(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirWithContents_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirWithContents(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirContentsOnly_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirContentsOnly(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofFileOrDirWithContents_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofFileOrDirWithContents(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofFiles_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofFiles(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirsWithContents_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirsWithContents(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirsContentsOnly_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirsContentsOnly(null));
  }

//---------------------------------------------------------------------------

  @Test
  void ofFilesOrDirsWithContents_rejectsNull()
  {
    assertThrows(NullPointerException.class, () -> FileDeletion.ofFilesOrDirsWithContents(null));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsNullElement()
  {
    List<FilePath> list = new ArrayList<>();
    list.add(NONEXISTENT);
    list.add(null);

    assertThrows(NullPointerException.class, () -> FileDeletion.ofFiles(list).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsNullElement_dirsWithContents()
  {
    List<FilePath> list = new ArrayList<>();
    list.add(NONEXISTENT);
    list.add(null);

    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirsWithContents(list).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsNullElement_dirsContentsOnly()
  {
    List<FilePath> list = new ArrayList<>();
    list.add(NONEXISTENT);
    list.add(null);

    assertThrows(NullPointerException.class, () -> FileDeletion.ofDirsContentsOnly(list).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsNullElement_filesOrDirsWithContents()
  {
    List<FilePath> list = new ArrayList<>();
    list.add(NONEXISTENT);
    list.add(null);

    assertThrows(NullPointerException.class, () -> FileDeletion.ofFilesOrDirsWithContents(list).nonInteractive().execute());
  }

//---------------------------------------------------------------------------
//endregion
//region Builder Retry Mode Mutual Exclusivity
//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_nonInteractiveThenFailureOK()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_nonInteractiveThenInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_nonInteractiveThenLogErrors()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_interactiveThenNonInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_interactiveThenFailureOK()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_interactiveThenLogErrors()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_failureOKThenInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_failureOKThenNonInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_failureOKThenLogErrors()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_logErrorsThenFailureOK()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_logErrorsThenNonInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsSettingRetryModeTwice_logErrorsThenInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsExecuteWithoutRetryMode()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT);

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("Retry mode must be set"));
  }

//---------------------------------------------------------------------------
//endregion
//region Builder Double Execution Prevention
//---------------------------------------------------------------------------

  @Test
  void builder_rejectsDoubleExecution_failureOK()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK();

    // First execution should work (returns SUCCESS because path doesn't exist)
    builder.execute();

    // Second execution should throw
    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsDoubleExecution_nonInteractive()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractive();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------

  @Test
  void builder_rejectsDoubleExecution_logErrors()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveLogErrors();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------
//endregion
//region Builder Post-Execution Modification Prevention
//---------------------------------------------------------------------------

  @Test
  void builder_rejectsRetryModeAfterExecution()
  {
    Builder builder = FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------
//endregion
//region BatchBuilder Retry Mode Mutual Exclusivity
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_nonInteractiveThenFailureOK()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_nonInteractiveThenInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_nonInteractiveThenLogErrors()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_interactiveThenNonInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_interactiveThenFailureOK()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_interactiveThenLogErrors()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).interactive();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_failureOKThenNonInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_failureOKThenInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_failureOKThenLogErrors()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveFailureOK();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveLogErrors);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_logErrorsThenNonInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_logErrorsThenInteractive()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::interactive);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsSettingRetryModeTwice_logErrorsThenFailureOK()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveLogErrors();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("Retry mode already set"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsExecuteWithoutRetryMode()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of());

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("Retry mode must be set"));
  }

//---------------------------------------------------------------------------
//endregion
//region BatchBuilder Double Execution Prevention
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsDoubleExecution()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();

    // First execution should work (returns SUCCESS because list is empty)
    builder.execute();

    // Second execution should throw
    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsDoubleExecution_failureOK()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveFailureOK();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsDoubleExecution_logErrors()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractiveLogErrors();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::execute);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------
//endregion
//region BatchBuilder Post-Execution Modification Prevention
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_rejectsRetryModeAfterExecution()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();
    builder.execute();

    IllegalStateException ex = assertThrows(IllegalStateException.class, builder::nonInteractiveFailureOK);
    assertTrue(ex.getMessage().contains("already been executed"));
  }

//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
//endregion
//region Factory Method Validation
//---------------------------------------------------------------------------

  @Test
  void ofFile_rejectsDirectory()
  {
    if (HDB_DIR == null)
      return;

    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> FileDeletion.ofFile(HDB_DIR));
    assertTrue(ex.getMessage().contains("requires a file, not a directory"));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirWithContents_rejectsFile()
  {
    if (HDB_FILE == null)
      return;

    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> FileDeletion.ofDirWithContents(HDB_FILE));
    assertTrue(ex.getMessage().contains("requires a directory, not a file"));
  }

//---------------------------------------------------------------------------

  @Test
  void ofDirContentsOnly_rejectsFile()
  {
    if (HDB_FILE == null)
      return;

    IllegalArgumentException ex = assertThrows(IllegalArgumentException.class, () -> FileDeletion.ofDirContentsOnly(HDB_FILE));
    assertTrue(ex.getMessage().contains("requires a directory, not a file"));
  }

//---------------------------------------------------------------------------

  @Test
  void ofFileOrDirWithContents_acceptsFile()
  {
    if (HDB_FILE == null)
      return;

    assertDoesNotThrow(() -> FileDeletion.ofFileOrDirWithContents(HDB_FILE));
  }

//---------------------------------------------------------------------------

  @Test
  void ofFileOrDirWithContents_acceptsDirectory()
  {
    if (HDB_DIR == null)
      return;

    assertDoesNotThrow(() -> FileDeletion.ofFileOrDirWithContents(HDB_DIR));
  }

//---------------------------------------------------------------------------
//endregion
//region Execution Results (No Filesystem)
//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentPath_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFile(NONEXISTENT).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentPath_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFile(NONEXISTENT).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentPath_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFile(NONEXISTENT).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentDirWithContents_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirWithContents(NONEXISTENT).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentDirContentsOnly_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirContentsOnly(NONEXISTENT).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentFileOrDirWithContents_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFileOrDirWithContents(NONEXISTENT).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyList_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFiles(List.of()).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyList_getFailedPaths_isEmpty()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();
    builder.execute();

    assertTrue(builder.getFailedPaths().isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistent_returnsSuccessWithEmptyFailedPaths()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    BatchBuilder batch = FileDeletion.ofFiles(List.of(ghost1, ghost2)).nonInteractive();

    assertEquals(FileDeletion.DeletionResult.SUCCESS, batch.execute());
    assertTrue(batch.getFailedPaths().isEmpty());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistent_failureOK_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFiles(List.of(ghost1, ghost2)).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistent_logErrors_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFiles(List.of(ghost1, ghost2)).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsWithContents_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of()).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsContentsOnly_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of()).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyFilesOrDirsWithContents_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of()).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  // Builder Nonexistent Path x FOK/LOG

  @Test
  void builder_nonexistentDirWithContents_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirWithContents(NONEXISTENT).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentDirWithContents_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirWithContents(NONEXISTENT).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentDirContentsOnly_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirContentsOnly(NONEXISTENT).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentDirContentsOnly_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirContentsOnly(NONEXISTENT).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentFileOrDirWithContents_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFileOrDirWithContents(NONEXISTENT).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void builder_nonexistentFileOrDirWithContents_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFileOrDirWithContents(NONEXISTENT).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  // Batch Empty List x FOK/LOG

  @Test
  void batchBuilder_emptyFiles_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFiles(List.of()).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyFiles_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFiles(List.of()).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsWithContents_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of()).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsWithContents_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of()).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsContentsOnly_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of()).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyDirsContentsOnly_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of()).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyFilesOrDirsWithContents_failureOK_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of()).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_emptyFilesOrDirsWithContents_logErrors_returnsSuccess()
  {
    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of()).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  // Batch Nonexistent Paths x Other Types

  @Test
  void batchBuilder_allNonexistentDirsWithContents_nonInteractive_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of(ghost1, ghost2)).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentDirsWithContents_failureOK_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of(ghost1, ghost2)).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentDirsWithContents_logErrors_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of(ghost1, ghost2)).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentDirsContentsOnly_nonInteractive_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of(ghost1, ghost2)).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentDirsContentsOnly_failureOK_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of(ghost1, ghost2)).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentDirsContentsOnly_logErrors_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of(ghost1, ghost2)).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentFilesOrDirsWithContents_nonInteractive_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of(ghost1, ghost2)).nonInteractive().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentFilesOrDirsWithContents_failureOK_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of(ghost1, ghost2)).nonInteractiveFailureOK().execute());
  }

//---------------------------------------------------------------------------

  @Test
  void batchBuilder_allNonexistentFilesOrDirsWithContents_logErrors_returnsSuccess()
  {
    FilePath ghost1 = new FilePath("_nonexistent_1_"),
             ghost2 = new FilePath("_nonexistent_2_");

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofFilesOrDirsWithContents(List.of(ghost1, ghost2)).nonInteractiveLogErrors().execute());
  }

//---------------------------------------------------------------------------
//endregion
//region Pre-Execution State
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_getFailedPaths_beforeExecute_isEmpty()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();

    assertTrue(builder.getFailedPaths().isEmpty());
  }

//---------------------------------------------------------------------------
//endregion
//region BatchBuilder Defensive Copy
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_defensivelyCopiesCollection()
  {
    List<FilePath> list = new ArrayList<>();

    BatchBuilder batch = FileDeletion.ofFiles(list).nonInteractive();

    // Mutate original list after builder creation
    list.add(null);

    // If defensively copied, execute sees the original empty list (SUCCESS).
    // Without defensive copy, execute would throw NPE from the null element.
    assertEquals(FileDeletion.DeletionResult.SUCCESS, batch.execute());
  }

//---------------------------------------------------------------------------
//endregion
//region getFailedPaths Unmodifiability
//---------------------------------------------------------------------------

  @Test
  void batchBuilder_getFailedPaths_isUnmodifiable()
  {
    BatchBuilder builder = FileDeletion.ofFiles(List.of()).nonInteractive();
    builder.execute();

    assertThrows(UnsupportedOperationException.class,
        () -> builder.getFailedPaths().add(NONEXISTENT));
  }

//---------------------------------------------------------------------------
//endregion
//region Ancestor/Descendant Batch
//---------------------------------------------------------------------------

  /**
   * Helper: creates parent/child/file.txt and parent/other.txt under tempDir.
   * Returns [parent, child] as FilePaths.
   */
  private static FilePath[] createNestedDirs(Path tempDir) throws IOException
  {
    Path parent = tempDir.resolve("parent"),
         child  = parent.resolve("child");

    Files.createDirectories(child);
    Files.writeString(child.resolve("file.txt"), "data");
    Files.writeString(parent.resolve("other.txt"), "data");

    return new FilePath[] { new FilePath(parent), new FilePath(child) };
  }

//---------------------------------------------------------------------------

  @Test
  void batchDirsWithContents_parentThenChild_succeeds(@TempDir Path tempDir) throws IOException
  {
    FilePath[] dirs = createNestedDirs(tempDir);
    FilePath parent = dirs[0], child = dirs[1];

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of(parent, child)).nonInteractive().execute());

    assertFalse(parent.exists(), "parent should be gone");
    assertFalse(child.exists(), "child should be gone");
  }

//---------------------------------------------------------------------------

  @Test
  void batchDirsWithContents_childThenParent_succeeds(@TempDir Path tempDir) throws IOException
  {
    FilePath[] dirs = createNestedDirs(tempDir);
    FilePath parent = dirs[0], child = dirs[1];

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsWithContents(List.of(child, parent)).nonInteractive().execute());

    assertFalse(parent.exists(), "parent should be gone");
    assertFalse(child.exists(), "child should be gone");
  }

//---------------------------------------------------------------------------

  @Test
  void batchDirsContentsOnly_parentThenChild_succeeds(@TempDir Path tempDir) throws IOException
  {
    FilePath[] dirs = createNestedDirs(tempDir);
    FilePath parent = dirs[0], child = dirs[1];

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of(parent, child)).nonInteractive().execute());

    assertTrue(parent.exists(), "parent should survive (DCO preserves root)");
    assertFalse(child.exists(), "child should be gone (deleted as parent's content)");
  }

//---------------------------------------------------------------------------

  @Test
  void batchDirsContentsOnly_childThenParent_succeeds(@TempDir Path tempDir) throws IOException
  {
    FilePath[] dirs = createNestedDirs(tempDir);
    FilePath parent = dirs[0], child = dirs[1];

    assertEquals(FileDeletion.DeletionResult.SUCCESS,
        FileDeletion.ofDirsContentsOnly(List.of(child, parent)).nonInteractive().execute());

    assertTrue(parent.exists(), "parent should survive (DCO preserves root)");
    assertFalse(child.exists(), "child should be gone (deleted as parent's content)");
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
