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

import org.hypernomicon.util.PopupDialog.DialogResult;

//---------------------------------------------------------------------------

/**
 * Tracks overwrite-or-skip state for a series of file conflicts.
 * <p>
 * Each instance manages one independent state machine with three states:
 * {@code check} (ask user each time), {@code overwriteAll} (auto-yes),
 * {@code overwriteNone} (auto-skip). The state advances when the user
 * responds with "Yes to all" or "No to all".
 * <p>
 * Used by {@code FileManager.doPasteChecks()} with two independent instances:
 * one for unrelated files (no database record at destination) and one for
 * related files (destination is associated with a database record).
 */
class OverwriteResolver
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum Decision { OVERWRITE, SKIP }

  private enum State { check, overwriteNone, overwriteAll }

  private State state = State.check;

//---------------------------------------------------------------------------

  /**
   * Returns the decision for a file conflict, advancing state as needed.
   *
   * @param userResponse the dialog result; ignored when state is already
   *                     {@code overwriteAll} or {@code overwriteNone}
   * @return {@link Decision#OVERWRITE} to proceed, {@link Decision#SKIP} to remove the entry
   */
  Decision resolve(DialogResult userResponse)
  {
    return switch (state)
    {
      case check -> switch (userResponse)
      {
        case mrNo      -> Decision.SKIP;
        case mrNoToAll ->
        {
          state = State.overwriteNone;
          yield Decision.SKIP;
        }
        case mrYesToAll ->
        {
          state = State.overwriteAll;
          yield Decision.OVERWRITE;
        }
        default -> Decision.OVERWRITE;  // mrYes or anything else
      };

      case overwriteNone -> Decision.SKIP;
      case overwriteAll  -> Decision.OVERWRITE;
    };
  }

//---------------------------------------------------------------------------

  /**
   * Returns {@code true} if this resolver is still in the initial {@code check}
   * state and needs a user prompt for the next conflict.
   */
  boolean needsUserPrompt()
  {
    return state == State.check;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
