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

package org.hypernomicon.view.tableCells;

import javafx.scene.Cursor;
import javafx.scene.control.TableCell;

//---------------------------------------------------------------------------

public abstract class CursorAwareCell<S, T> extends TableCell<S, T>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CursorAwareCell()
  {
    hoverProperty().addListener((obs, wasHover, isHover) ->
    {
      setCursor(isHover && (isEditing() == false) && (isEmpty() == false) && (getTableRow() != null) && (getTableRow().getItem() != null) ?
        getMouseCursor()
      :
        null);  // Either not hovered, or currently editing: let default/editor cursor show
    });

    // Also listen for editing state changes, so cursor resets immediately
    editingProperty().addListener((obs, wasEditing, isEditing) ->
    {
      if (isEditing)
        setCursor(null); // hand control to the editor
      else if (isHover())
        setCursor(getMouseCursor());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected abstract Cursor getMouseCursor();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
