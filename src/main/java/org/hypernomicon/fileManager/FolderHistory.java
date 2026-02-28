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

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.records.HDT_Folder;

import javafx.scene.control.Button;

//---------------------------------------------------------------------------

final class FolderHistory
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<FolderHistoryItem> history = new ArrayList<>();
  private final Button btnForward, btnBack;

  private int cursorNdx = -1;

//---------------------------------------------------------------------------

  FolderHistory(Button btnForward, Button btnBack)
  {
    this.btnForward = btnForward;
    this.btnBack    = btnBack;

    clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    history.clear();
    cursorNdx = -1;
    updateButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Folder folder)
  {
    if (folder == null) return;

    for (int ndx = 0; ndx < history.size(); ndx++)
    {
      FolderHistoryItem item = history.get(ndx);

      if (item.folder() == folder)
      {
        // Update cursorNdx if it points to or follows the deleted entry

        if (ndx <= cursorNdx)
          cursorNdx--;

        // Remove the matching item

        history.remove(ndx);
        ndx--; // Adjust index to recheck after removal
      }
      else if ((ndx > 0) && (history.get(ndx - 1).folder() == item.folder()))
      {
        // Update cursorNdx if it points to or follows the removed duplicate

        if (ndx <= cursorNdx)
          cursorNdx--;

        // Remove adjacent duplicates

        history.remove(ndx);
        ndx--; // Adjust index for recheck
      }
    }

    // Ensure cursorNdx is valid after all deletions

    if (cursorNdx < 0)
      cursorNdx = 0; // Default to the first folder if no valid preceding folder exists
    else if (cursorNdx >= history.size())
      cursorNdx = history.size() - 1; // Default to the last folder if out of bounds

    // Update navigation buttons

    updateButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This gets called by the change listener for the folder tree selected property
   * and is called only if the selection change was user-initiated
   * @param newItem the new history item
   */
  void add(FolderHistoryItem newItem)
  {
    while (history.size() > (cursorNdx + 1))
      history.remove(cursorNdx + 1);

    history.add(newItem);
    cursorNdx++;
    updateButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateButtons()
  {
    btnBack   .setDisable(cursorNdx < 1);
    btnForward.setDisable(cursorNdx == (history.size() - 1));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when the user navigates backward in the history
   * @return the resulting selected history item
   */
  FolderHistoryItem back()
  {
    cursorNdx--;
    updateButtons();
    return history.get(cursorNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when the user navigates forward in the history
   * @return the resulting selected history item
   */
  FolderHistoryItem forward()
  {
    cursorNdx++;
    updateButtons();
    return history.get(cursorNdx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called by the change listeners for the file table and record table
   * @param newItem the new history item
   */
  void updateCurrent(FolderHistoryItem newItem)
  {
    history.set(cursorNdx, newItem);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
