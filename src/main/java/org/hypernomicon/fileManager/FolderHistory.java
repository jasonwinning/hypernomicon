/*
 * Copyright 2015-2025 Jason Winning
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
  private int ndx = -1;

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
    ndx = -1;
    updateButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Folder folder)
  {
    if (folder == null) return;

    for (int i = 0; i < history.size(); i++)
    {
      FolderHistoryItem item = history.get(i);

      if (item.folder == folder)
      {
        // Update ndx if it points to or follows the deleted entry
        if (i <= ndx)
          ndx--;

        // Remove the matching item
        history.remove(i);
        i--; // Adjust index to recheck after removal
      }
      else if ((i > 0) && (history.get(i - 1).folder == item.folder))
      {
        // Remove adjacent duplicates
        history.remove(i);
        i--; // Adjust index for recheck
      }
    }

    // Ensure ndx is valid after all deletions
    if (ndx < 0)
      ndx = 0; // Default to the first folder if no valid preceding folder exists
    else if (ndx >= history.size())
      ndx = history.size() - 1; // Default to the last folder if out of bounds

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
    while (history.size() > (ndx + 1))
      history.remove(ndx + 1);

    history.add(newItem);
    ndx++;
    updateButtons();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateButtons()
  {
    btnBack   .setDisable(ndx == 0);
    btnForward.setDisable(ndx == (history.size() - 1));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when the user navigates backward in the history
   * @return the resulting selected history item
   */
  FolderHistoryItem back()
  {
    ndx--;
    updateButtons();
    return history.get(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when the user navigates forward in the history
   * @return the resulting selected history item
   */
  FolderHistoryItem forward()
  {
    ndx++;
    updateButtons();
    return history.get(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called by the change listeners for the file table and record table
   * @param newItem the new history item
   */
  void updateCurrent(FolderHistoryItem newItem)
  {
    history.set(ndx, newItem);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
