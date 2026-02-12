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

import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionType.*;

import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionType;

//---------------------------------------------------------------------------

/**
 * Builder for batch deletion operations.
 */
public class BatchBuilder extends DeletionBuilderBase<BatchBuilder>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Collection<FilePath> filePaths;

//---------------------------------------------------------------------------

  BatchBuilder(Collection<FilePath> filePaths, DeletionType type)
  {
    super(type);
    this.filePaths = new ArrayList<>(filePaths);
  }

//---------------------------------------------------------------------------

  @Override protected BatchBuilder self() { return this; }

//---------------------------------------------------------------------------

  /**
   * Get the paths that failed to delete after execute() completes.
   * @return Set of paths that could not be deleted
   */
  public Set<FilePath> getFailedPaths() { return Collections.unmodifiableSet(failedPaths); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override Stream<DeletionTask> taskStream()
  {
    if (filePaths.isEmpty())
      return Stream.empty();

    return filePaths.stream()
      .peek  (filePath -> Objects.requireNonNull(filePath, "Null element in file path collection"))
      .filter(filePath -> (FilePath.isEmpty(filePath) == false) && filePath.exists())
      .map   (filePath ->
      {
        // Validate directory operations are actually on directories
        // This prevents catastrophic bugs where getDirOnly() on a file returns the parent

        if (((type == DIR_WITH_CONTENTS) || (type == DIR_CONTENTS_ONLY)) && (filePath.isDirectory() == false))
          throw new IllegalArgumentException("Expected directory, got file: " + filePath);

        // Validate file operations are actually on files

        if ((type == FILE_ONLY) && filePath.isDirectory())
          throw new IllegalArgumentException("Expected file, got directory: " + filePath);

        return new DeletionTask(filePath, type);
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Batch interactive deletion with auto-retry phase then user prompts.
   */
  @Override protected DeletionResult executeInteractive()
  {
    while (true)
    {
      // Phase 1: Auto-retry with progress dialog

      DeletionResult phaseOneResult = executeInteractiveAutoRetryPhase();

      if ((phaseOneResult == SUCCESS) || (phaseOneResult == CANCELLED))
        return phaseOneResult;

      // Phase 2: Prompt user about all failures together

      switch (promptBatchFailures(tasks.size(), totalCount))
      {
        case mrRetry:

          // Loop back to Phase 1

          continue;

        case mrIgnore:

          // Skip remaining; record as failed

          tasks.forEach(task -> failedPaths.add(task.getFilePath()));

          return PARTIAL;

        default:

          return CANCELLED;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Show batch failure prompt dialog.
   */
  private static DialogResult promptBatchFailures(int failedCount, int totalCount)
  {
    String message = failedCount + " of " + totalCount + " items could not be deleted.";

    return new PopupDialog(message)
      .addDefaultButton("Try Again", DialogResult.mrRetry)
      .addButton("Skip", DialogResult.mrIgnore)
      .addButton("Cancel", DialogResult.mrCancel)
      .showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
