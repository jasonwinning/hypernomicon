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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult.*;

import java.util.stream.Stream;

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

    while (true)
    {
      // Phase 1: Auto-retry with indeterminate progress dialog

      DeletionResult phaseOneResult = executeInteractiveAutoRetryPhase();

      if (phaseOneResult != FAILED)
        return phaseOneResult;

      // Phase 2: Prompt user

      String errorMsg = task.getLastError() != null ? getThrowableMessage(task.getLastError()) : "";

      String message = "Unable to delete \"" + task.getFilePath() + '"' + (errorMsg.isEmpty() == false ?
        (":\n" + errorMsg + "\n\nTry again?")
      :
        ". Try again?");

      DialogResult response = new PopupDialog(message)
        .addDefaultButton("Try Again", DialogResult.mrRetry)
        .addButton("Cancel", DialogResult.mrCancel)
        .showModal();

      if (response != DialogResult.mrRetry)
        return CANCELLED;

      // Loop back to Phase 1
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
