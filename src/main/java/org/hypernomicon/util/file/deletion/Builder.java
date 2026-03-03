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
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;

import java.util.stream.Stream;

import org.hypernomicon.HyperTask;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionType;

//---------------------------------------------------------------------------

/**
 * Builder for single-path deletion operations.
 */
public class Builder extends DeletionBuilderBase<Builder>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final FilePath filePath;

//---------------------------------------------------------------------------

  Builder(FilePath filePath, DeletionType type)
  {
    super(type);
    this.filePath = filePath;
  }

//---------------------------------------------------------------------------

  @Override protected Builder self() { return this; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override Stream<DeletionTask> taskStream()
  {
    return FilePath.isEmpty(filePath) || (filePath.exists() == false) ?
      Stream.empty()
    :
      Stream.of(new DeletionTask(filePath, type));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Interactive retry: auto-retry with progress dialog, then prompt user.
   */
  @Override protected DeletionResult executeInteractive()
  {
    DeletionTask task = tasks.getFirst();

    // Scan for locked files only on the first failure. On subsequent retries the user has
    // already been informed; re-scanning would repeat the probe-rename unnecessarily, which
    // can trigger cloud-sync activity (e.g. OneDrive) and make the next attempt more likely
    // to fail.

    boolean firstFailure = true;
    FilePath lockedFile = null;

    while (true)
    {
      // Phase 1: Auto-retry with indeterminate progress dialog

      DeletionResult phaseOneResult = executeInteractiveAutoRetryPhase();

      if (phaseOneResult != FAILED)
        return phaseOneResult;

      // Phase 2: On Windows, scan for locked files to provide diagnostic info (first failure only)

      if (firstFailure)
      {
        lockedFile = scanForLockedFile();
        firstFailure = false;
      }

      // Phase 3: Prompt user with specific or generic error message

      String errorMsg = task.getLastError() != null ? getThrowableMessage(task.getLastError()) : "",
             message;

      if (lockedFile != null)
      {
        String lockedItemType = lockedFile.isDirectory() ? "folder" : "file";
        message = "Unable to delete \"" + task.getFilePath() + "\". The " + lockedItemType + " is locked:\n\""
          + lockedFile + "\"\n\nClose the application using this " + lockedItemType + ", then try again.";
      }
      else if (errorMsg.isEmpty() == false)
        message = "Unable to delete \"" + task.getFilePath() + "\":\n" + errorMsg + "\n\nTry again?";
      else
        message = "Unable to delete \"" + task.getFilePath() + "\". Try again?";

      DialogResult response = new PopupDialog(message)
        .addDefaultButton("Try Again", DialogResult.mrRetry)
        .addButton("Cancel", DialogResult.mrCancel)
        .showModal();

      if (response != DialogResult.mrRetry)
        return ABORTED;

      // Loop back to Phase 1
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scans for a locked file inside the target directory. On POSIX, returns
   * {@code null} immediately. On Windows, runs the scan on a background
   * thread with an indeterminate progress dialog.
   */
  private FilePath scanForLockedFile()
  {
    if ((IS_OS_WINDOWS == false) || (filePath.isDirectory() == false))
      return null;

    if (ui == null)
      return filePath.findLockedFileInDir();

    FilePath[] result = { null };

    new HyperTask("LockedFileScan", "Scanning for locked files...", false)
    {
      @Override protected void call()
      {
        result[0] = filePath.findLockedFileInDir();
      }
    }.runWithProgressDialog();

    return result[0];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
