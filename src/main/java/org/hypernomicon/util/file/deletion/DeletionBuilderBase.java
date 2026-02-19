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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.RetryMode.*;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion.*;

import javafx.concurrent.Worker.State;

//---------------------------------------------------------------------------

/**
 * Base class for deletion builders, providing shared configuration,
 * validation, retry logic, and the execution pipeline.
 */
abstract class DeletionBuilderBase<T extends DeletionBuilderBase<T>>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final DeletionType type;
  protected final Set<FilePath> failedPaths = new LinkedHashSet<>();
  List<DeletionTask> tasks;
  protected int totalCount;

  private RetryMode retryMode;

  private boolean retryModeSet = false,
                  executed     = false,
                  logErrors    = false;

//---------------------------------------------------------------------------

  // Retry timing constants: Windows needs retry for lingering file handles; POSIX doesn't

  private static final int NON_INTERACTIVE_TIMEOUT_MS = IS_OS_WINDOWS ? 5000 : 0,
                           INTERACTIVE_AUTO_RETRY_MS  = IS_OS_WINDOWS ? 3000 : 0,
                           RETRY_DELAY_MS = 250;

//---------------------------------------------------------------------------

  DeletionBuilderBase(DeletionType type)
  {
    this.type = type;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected abstract T self();

  /**
   * Return a stream of DeletionTasks to execute. Called at execute() time
   * so that filesystem state checks reflect the moment of deletion. The stream
   * is collected into a single mutable list that is used throughout execution.
   */
  abstract Stream<DeletionTask> taskStream();

  /**
   * Execute the interactive retry strategy. Subclasses implement single-item
   * vs. batch interactive behavior.
   */
  protected abstract DeletionResult executeInteractive();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Validation ---

  private void checkNotExecuted()
  {
    if (executed)
      throw new IllegalStateException("Builder has already been executed");
  }

//---------------------------------------------------------------------------

  private void checkRetryModeNotSet()
  {
    checkNotExecuted();

    if (retryModeSet)
      throw new IllegalStateException("Retry mode already set to " + retryMode);

    retryModeSet = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // --- Retry mode (mutually exclusive) ---

  /** Non-interactive retry with timeout (5s on Windows, single attempt on POSIX) */
  public T nonInteractive() { checkRetryModeNotSet(); retryMode = NON_INTERACTIVE; return self(); }

  /** Auto-retry with timeout then prompt user (3s on Windows, single attempt on POSIX) */
  public T interactive() { checkRetryModeNotSet(); retryMode = INTERACTIVE; return self(); }

  /** Non-interactive, failure OK: suppress all errors, best-effort with retry */
  public T nonInteractiveFailureOK() { checkRetryModeNotSet(); retryMode = NON_INTERACTIVE_FAILURE_OK; return self(); }

  /** Non-interactive, failure OK with logging: suppress errors but log them */
  public T nonInteractiveLogErrors() { checkRetryModeNotSet(); retryMode = NON_INTERACTIVE_FAILURE_OK; logErrors = true; return self(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Execute the deletion operation.
   * @return The result of the deletion
   * @throws IllegalStateException if called more than once or if retry mode has not been set
   */
  public final DeletionResult execute()
  {
    checkNotExecuted();

    if (retryModeSet == false)
      throw new IllegalStateException("Retry mode must be set before executing deletion. " +
          "Call nonInteractive(), interactive(), nonInteractiveFailureOK(), or nonInteractiveLogErrors().");

    executed = true;

    tasks = taskStream().collect(Collectors.toCollection(ArrayList::new));
    totalCount = tasks.size();

    if (totalCount == 0)
      return SUCCESS;

    boolean underDbRoot = anyUnderDbRoot(tasks),
            startWatcher = underDbRoot && (folderTreeWatcher.isOnWatcherThread() == false) && folderTreeWatcher.stop();

    try
    {
      DeletionResult result = executeWithRetry();

      if (((result == SUCCESS) || (result == PARTIAL)) && underDbRoot)
        FileManager.setNeedRefresh();

      return result;
    }
    finally
    {
      if (startWatcher)
        folderTreeWatcher.createNewWatcherAndStart();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DeletionResult executeWithRetry()
  {
    return switch (retryMode)
    {
      case NON_INTERACTIVE            -> executeNonInteractive();
      case INTERACTIVE                -> executeInteractive();
      case NON_INTERACTIVE_FAILURE_OK -> executeNonInteractiveFailureOK();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Non-interactive retry with timeout.
   */
  private DeletionResult executeNonInteractive()
  {
    if (retryWithTimeout(NON_INTERACTIVE_TIMEOUT_MS, DeletionTask::attemptOnce) == SUCCESS)
      return SUCCESS;

    for (DeletionTask task : tasks)
      failedPaths.add(task.getFilePath());

    return (failedPaths.size() < totalCount) ? PARTIAL : FAILED;
  }

//---------------------------------------------------------------------------

  /**
   * Non-interactive failure-OK retry. If logErrors is true, logs errors for items
   * that still fail after timeout.
   */
  private DeletionResult executeNonInteractiveFailureOK()
  {
    retryWithTimeout(NON_INTERACTIVE_TIMEOUT_MS, DeletionTask::attemptOnceSilent);

    if (logErrors)
      for (DeletionTask task : tasks)
        if (task.attemptOnce() == false)
          logThrowable(task.getLastError());

    return SUCCESS;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Core retry loop: removes successful tasks from the task list, retries with timeout.
   * The attemptFn parameter determines which attempt method to use
   * ({@code attemptOnce} or {@code attemptOnceSilent}).
   * Tasks that succeed are removed; tasks that remain after timeout are still
   * in the list for the caller to handle.
   */
  private DeletionResult retryWithTimeout(int timeoutMillis, Predicate<DeletionTask> attemptFn)
  {
    long startTime = System.currentTimeMillis();

    while (tasks.isEmpty() == false)
    {
      tasks.removeIf(attemptFn);

      if (tasks.isEmpty())
        return SUCCESS;

      if ((System.currentTimeMillis() - startTime) >= timeoutMillis)
        return FAILED;

      try { Thread.sleep(RETRY_DELAY_MS); }
      catch (InterruptedException e)
      {
        Thread.currentThread().interrupt();
        return FAILED;
      }
    }

    return SUCCESS;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Phase 1 of interactive mode: auto-retry with indeterminate progress dialog.
   * On Windows, retries for up to 3 seconds to handle lingering file handles.
   * On non-Windows with the main UI present, runs a single attempt via HyperTask so the
   * JavaFX thread stays responsive and the delayed-show progress dialog appears for slow
   * operations (e.g. cloud-storage paths like Dropbox). The timeout of 0 means the loop
   * exits after one pass regardless of outcome, which is correct for POSIX semantics.
   * Without the main UI (e.g. standalone tools), attempts directly without a dialog.
   * Returns SUCCESS if all tasks complete, CANCELLED if user cancels, FAILED if timeout/failure.
   */
  protected DeletionResult executeInteractiveAutoRetryPhase()
  {
    // Without UI: attempt directly (no progress dialog, no cancel support)

    if ((INTERACTIVE_AUTO_RETRY_MS == 0) && (ui == null))
    {
      tasks.removeIf(DeletionTask::attemptOnce);

      return tasks.isEmpty() ? SUCCESS : FAILED;
    }

    // Without the main UI (Windows): retry without progress dialog (no cancel support)

    if (ui == null)
      return retryWithTimeout(INTERACTIVE_AUTO_RETRY_MS, DeletionTask::attemptOnce);

    // With main UI: run via HyperTask to keep JavaFX thread responsive.
    // On Windows: retries for up to INTERACTIVE_AUTO_RETRY_MS milliseconds.
    // On non-Windows: INTERACTIVE_AUTO_RETRY_MS is 0, so the timeout check fires
    // immediately after one pass; exactly one attempt, but on a background thread.

    final long[] startTime = { System.currentTimeMillis() };
    final DeletionResult[] result = { FAILED };

    HyperTask hyperTask = new HyperTask("FileDeletion", "Deleting...", false)
    {
      @Override protected void call() throws CancelledTaskException
      {
        while (tasks.isEmpty() == false)
        {
          // Attempt each pending task

          Iterator<DeletionTask> it = tasks.iterator();

          while (it.hasNext())
          {
            throwExceptionIfCancelled(this);

            DeletionTask task = it.next();
            if (task.attemptOnce())
              it.remove();
          }

          if (tasks.isEmpty())
          {
            result[0] = SUCCESS;
            return;
          }

          // Check timeout

          if ((System.currentTimeMillis() - startTime[0]) >= INTERACTIVE_AUTO_RETRY_MS)
          {
            result[0] = FAILED;
            return;
          }

          // Sleep before retry

          sleepForMillis(RETRY_DELAY_MS);
        }

        result[0] = SUCCESS;
      }
    };

    State state = hyperTask.runWithProgressDialog();

    if (state == State.CANCELLED)
      return CANCELLED;

    return result[0];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determine whether a path is under the database root folder. Used to decide whether
   * the FolderTreeWatcher should be managed and whether {@code FileManager.setNeedRefresh()}
   * should be called after successful deletion.
   */
  private static boolean isUnderDbRoot(FilePath filePath)
  {
    FilePath rootPath = ((db == null) || db.isOffline()) ? null : db.getRootPath();
    return (FilePath.isEmpty(rootPath) == false) && rootPath.contains(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean anyUnderDbRoot(Collection<DeletionTask> tasks)
  {
    return tasks.stream().anyMatch(task -> isUnderDbRoot(task.getFilePath()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
