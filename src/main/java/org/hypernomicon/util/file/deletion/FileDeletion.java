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

import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionType.*;

import java.util.Collection;
import java.util.Objects;

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Unified API for file and directory deletion with retry support
 * for Windows file handle delays.
 *
 * <h2>Factory Methods</h2>
 *
 * <h3>Single-Item Operations</h3>
 * <ul>
 *   <li>{@link #ofFile(FilePath)}: Delete a single file. Throws if path is a directory.</li>
 *   <li>{@link #ofDirWithContents(FilePath)}: Delete a directory and all its contents. Throws if path is a file.</li>
 *   <li>{@link #ofDirContentsOnly(FilePath)}: Delete directory contents only, keeping the directory. Throws if path is a file.</li>
 *   <li>{@link #ofFileOrDirWithContents(FilePath)}: Deletes file or directory with contents (auto-detected).</li>
 * </ul>
 *
 * <h3>Batch Operations</h3>
 * <ul>
 *   <li>{@link #ofFiles(Collection)}: Delete multiple files. Validates all are files at execution time.</li>
 *   <li>{@link #ofDirsWithContents(Collection)}: Delete multiple directories with contents. Validates all are directories.</li>
 *   <li>{@link #ofDirsContentsOnly(Collection)}: Delete contents of multiple directories. Validates all are directories.</li>
 *   <li>{@link #ofFilesOrDirsWithContents(Collection)}: Mixed files and directories, auto-detected per item.</li>
 * </ul>
 *
 * <p>All factory methods throw {@code NullPointerException} if the path or collection argument is null.
 * Null elements within a batch collection also throw {@code NullPointerException} at execution time.</p>
 *
 * <h2>Builder Options (Single-Item)</h2>
 *
 * <h3>Retry Mode (mutually exclusive, required)</h3>
 * <p>All modes run synchronously and block the calling thread until completion. Retry timeouts apply only
 * on Windows where file handles can linger after processes close them. On Mac/Linux, deletions are attempted
 * once since POSIX systems don't have this issue.</p>
 * <ul>
 *   <li>{@code nonInteractive()}: On Windows, retry silently for up to 5 seconds with 250ms delays. On Mac/Linux,
 *       attempts once. For background operations.</li>
 *   <li>{@code interactive()}: On Windows, auto-retry for up to 3 seconds with progress dialog, then prompt
 *       user with [Try Again] [Cancel] options. On Mac/Linux, attempts once and shows error dialog immediately
 *       if failed (no progress dialog). Loops until success or user cancels (returns {@code CANCELLED}).</li>
 *   <li>{@code nonInteractiveFailureOK()}: Best-effort cleanup. On Windows, retries for up to 5 seconds. On Mac/Linux,
 *       attempts once. Never throws, never prompts. Always returns SUCCESS.</li>
 *   <li>{@code nonInteractiveLogErrors()}: Same as {@code nonInteractiveFailureOK()}, but logs errors via
 *       {@code logThrowable()} if deletion still fails after timeout (Windows) or first attempt (Mac/Linux).</li>
 * </ul>
 *
 * <p>Non-existent targets always return {@code SUCCESS} (idempotent, like {@code Files.deleteIfExists}).</p>
 *
 * <h2>BatchBuilder Options</h2>
 *
 * <h3>Retry Mode (mutually exclusive, required)</h3>
 * <p>Same options as single-item. Retry mode must be set explicitly before calling {@code execute()}.</p>
 *
 * <h3>Results</h3>
 * <ul>
 *   <li>{@code getFailedPaths()}: After {@code execute()}, returns the set of paths that could not be deleted.</li>
 * </ul>
 *
 * <p>Non-existent paths in the collection are silently skipped.</p>
 *
 * <h2>Return Values</h2>
 * <ul>
 *   <li>{@code SUCCESS}: All items deleted successfully (or didn't exist).</li>
 *   <li>{@code PARTIAL}: (Batch only) Some items deleted, some skipped or failed.</li>
 *   <li>{@code FAILED}: Deletion failed after all retry attempts (non-failure-OK modes only).</li>
 *   <li>{@code CANCELLED}: User cancelled the operation (interactive mode only).</li>
 * </ul>
 * <p>
 * Note: {@code nonInteractiveFailureOK()} and {@code nonInteractiveLogErrors()} always return {@code SUCCESS}
 * regardless of actual outcome.
 *
 * <h2>Usage Examples</h2>
 * <pre>{@code
 * // Simple file deletion with non-interactive retry (good for background operations)
 * FileDeletion.ofFile(filePath)
 *   .nonInteractive()
 *   .execute();
 *
 * // Interactive directory deletion with auto-retry then prompt
 * FileDeletion.ofDirWithContents(folderPath)
 *   .interactive()
 *   .execute();
 *
 * // Best-effort cleanup of temp directory (fire-and-forget)
 * FileDeletion.ofDirWithContents(tempDir)
 *   .nonInteractiveFailureOK()
 *   .execute();
 *
 * // Best-effort cleanup with error logging
 * FileDeletion.ofFile(tempFile)
 *   .nonInteractiveLogErrors()
 *   .execute();
 *
 * // Batch delete empty source folders after move (non-interactive retry)
 * FileDeletion.ofDirsWithContents(emptySourceDirs)
 *   .nonInteractive()
 *   .execute();
 *
 * // Batch interactive delete with failure tracking
 * BatchBuilder batch = FileDeletion.ofFilesOrDirsWithContents(selectedItems)
 *   .interactive();
 *
 * DeletionResult result = batch.execute();
 *
 * if (result == DeletionResult.PARTIAL)
 *   System.out.println("Some items failed: " + batch.getFailedPaths());
 * }</pre>
 *
 * <h2>FolderTreeWatcher and FileManager Refresh</h2>
 * <p>
 * The {@code FolderTreeWatcher} is automatically stopped before deletion and restarted afterward
 * when the target path is under the database root folder. No caller action is needed. Watcher
 * management is skipped when: the path is outside the database root, no database is loaded, or the
 * deletion is running on the watcher's own thread.
 * <p>
 * Similarly, {@code FileManager.setNeedRefresh()} is automatically called after successful deletion
 * of paths under the database root folder. Paths outside the database root do not trigger a refresh
 * since they are not displayed in the FileManager.
 *
 * <h2>Thread Safety</h2>
 * <ul>
 *   <li>{@code nonInteractive()}, {@code nonInteractiveFailureOK()}, and {@code nonInteractiveLogErrors()} are safe from any thread.</li>
 *   <li>{@code interactive()} must be called from the JavaFX Application Thread (shows dialogs).</li>
 * </ul>
 *
 * <h2>Platform Behavior</h2>
 * <p>
 * <b>Windows:</b> Retry timeouts are enabled because file handles can linger after processes close them
 * (antivirus scanners, Explorer thumbnails, Windows Search indexer, etc.). All deletion uses Java's
 * {@code Files} API with per-file granularity; directory trees are walked bottom-up so that each retry
 * attempt makes maximum progress as lingering handles close.
 * <p>
 * <b>Mac/Linux:</b> Retry is disabled (single attempt) because POSIX systems use "unlink" semantics where
 * files can be deleted even while open. If a deletion fails, retrying won't help; the cause is typically
 * permissions, non-empty directories, or filesystem errors.
 */
public final class FileDeletion
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Specifies what type of deletion to perform. Internal use only.
   */
  enum DeletionType
  {
    /** Delete a single file only */
    FILE_ONLY,

    /** Delete a directory and all its contents */
    DIR_WITH_CONTENTS,

    /** Delete only the contents of a directory, keeping the directory itself */
    DIR_CONTENTS_ONLY,

    /** Auto-detect: file or directory with contents */
    FILE_OR_DIR_WITH_CONTENTS
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Specifies how to handle deletion failures.
   */
  public enum RetryMode
  {
    /** Non-interactive retry with delays (5s timeout on Windows, single attempt on POSIX) */
    NON_INTERACTIVE,

    /** Auto-retry first (3s on Windows, single attempt on POSIX), then prompt user */
    INTERACTIVE,

    /** Non-interactive, failure OK: suppress all errors; best-effort cleanup with retry */
    NON_INTERACTIVE_FAILURE_OK
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Result of a deletion operation.
   */
  public enum DeletionResult
  {
    /** All items deleted successfully */
    SUCCESS,

    /** Some items deleted, some skipped (batch only) */
    PARTIAL,

    /** Deletion failed after all retry attempts */
    FAILED,

    /** User cancelled the operation */
    CANCELLED
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FileDeletion() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

// -------------------------------------------------------------------------
// Single-item factory methods
// -------------------------------------------------------------------------

  /**
   * Create a builder for deleting a single file.
   * @param filePath The file to delete (must not be null or a directory)
   * @return A new Builder instance
   * @throws NullPointerException if filePath is null
   * @throws IllegalArgumentException if filePath is a directory
   * @see FileDeletion
   */
  public static Builder ofFile(FilePath filePath)
  {
    Objects.requireNonNull(filePath, "filePath must not be null");

    if (filePath.isDirectory())
      throw new IllegalArgumentException("ofFile() requires a file, not a directory: " + filePath);

    return new Builder(filePath, FILE_ONLY);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting a directory and all its contents.
   * @param filePath The directory to delete (must not be null; must be a directory if it exists)
   * @return A new Builder instance
   * @throws NullPointerException if filePath is null
   * @throws IllegalArgumentException if filePath exists and is not a directory
   * @see FileDeletion
   */
  public static Builder ofDirWithContents(FilePath filePath)
  {
    Objects.requireNonNull(filePath, "filePath must not be null");

    if (filePath.exists() && (filePath.isDirectory() == false))
      throw new IllegalArgumentException("ofDirWithContents() requires a directory, not a file: " + filePath);

    return new Builder(filePath, DIR_WITH_CONTENTS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting only the contents of a directory, keeping the directory itself.
   * @param filePath The directory whose contents to delete (must not be null; must be a directory if it exists)
   * @return A new Builder instance
   * @throws NullPointerException if filePath is null
   * @throws IllegalArgumentException if filePath exists and is not a directory
   * @see FileDeletion
   */
  public static Builder ofDirContentsOnly(FilePath filePath)
  {
    Objects.requireNonNull(filePath, "filePath must not be null");

    if (filePath.exists() && (filePath.isDirectory() == false))
      throw new IllegalArgumentException("ofDirContentsOnly() requires a directory, not a file: " + filePath);

    return new Builder(filePath, DIR_CONTENTS_ONLY);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting a file or directory (auto-detected).
   * If the path is a directory, it and all its contents will be deleted.
   * @param filePath The file or directory to delete (must not be null)
   * @return A new Builder instance
   * @throws NullPointerException if filePath is null
   * @see FileDeletion
   */
  public static Builder ofFileOrDirWithContents(FilePath filePath)
  {
    Objects.requireNonNull(filePath, "filePath must not be null");

    return new Builder(filePath, FILE_OR_DIR_WITH_CONTENTS);
  }

// -------------------------------------------------------------------------
// Batch factory methods
// -------------------------------------------------------------------------

  /**
   * Create a builder for deleting multiple files.
   * @param filePaths The files to delete (must not be null; all elements must be files, not directories)
   * @return A new BatchBuilder instance
   * @throws NullPointerException if filePaths is null
   * @see FileDeletion
   */
  public static BatchBuilder ofFiles(Collection<FilePath> filePaths)
  {
    Objects.requireNonNull(filePaths, "filePaths must not be null");

    return new BatchBuilder(filePaths, FILE_ONLY);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting multiple directories with all their contents.
   * @param filePaths The directories to delete (must not be null; all elements must be directories)
   * @return A new BatchBuilder instance
   * @throws NullPointerException if filePaths is null
   * @see FileDeletion
   */
  public static BatchBuilder ofDirsWithContents(Collection<FilePath> filePaths)
  {
    Objects.requireNonNull(filePaths, "filePaths must not be null");

    return new BatchBuilder(filePaths, DIR_WITH_CONTENTS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting the contents of multiple directories, keeping the directories.
   * @param filePaths The directories whose contents to delete (must not be null; all elements must be directories)
   * @return A new BatchBuilder instance
   * @throws NullPointerException if filePaths is null
   * @see FileDeletion
   */
  public static BatchBuilder ofDirsContentsOnly(Collection<FilePath> filePaths)
  {
    Objects.requireNonNull(filePaths, "filePaths must not be null");

    return new BatchBuilder(filePaths, DIR_CONTENTS_ONLY);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a builder for deleting multiple files and/or directories (auto-detected per item).
   * Directories will be deleted with all their contents.
   * @param filePaths The files and/or directories to delete (must not be null)
   * @return A new BatchBuilder instance
   * @throws NullPointerException if filePaths is null
   * @see FileDeletion
   */
  public static BatchBuilder ofFilesOrDirsWithContents(Collection<FilePath> filePaths)
  {
    Objects.requireNonNull(filePaths, "filePaths must not be null");

    return new BatchBuilder(filePaths, FILE_OR_DIR_WITH_CONTENTS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
