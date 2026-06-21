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

package org.hypernomicon.query.ui;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.view.HyperFavorites.QueryFavorite;

import javafx.fxml.FXML;
import javafx.scene.control.*;

//---------------------------------------------------------------------------

public class NewQueryFavDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnRemove;
  @FXML private CheckBox chkAutoExec;
  @FXML private Label lblInfo;
  @FXML private TextField tfName;

  /** The favorite being edited, or {@code null} in create mode. In edit mode,
   *  {@link #isValid()} applies the new name to this object directly. */
  private final QueryFavorite favoriteBeingEdited;

  private boolean removeClicked = false;

//---------------------------------------------------------------------------

  /** Create-mode constructor: presents an empty (or pre-suggested) name plus
   *  the auto-execute checkbox. Caller reads {@link #getNewName()} and
   *  {@link #getAutoExec()} after a successful {@link #showModal()} to
   *  construct a new {@link QueryFavorite}. */
  NewQueryFavDlgCtrlr(String newName)
  {
    super("query/NewQueryFavDlg", "Add Query Favorite", true, true);

    favoriteBeingEdited = null;

    tfName.setText(newName);

    removeFromParent(btnRemove);
    removeFromParent(lblInfo);

    onShown = this::sizeAndFocus;
  }

//---------------------------------------------------------------------------

  /** Edit-mode constructor: shows the favorite's current name (editable) and
   *  current auto-execute setting (editable), plus a Remove button. On
   *  successful {@link #showModal()} the favorite's name and autoexec are
   *  updated in place; if the user clicked Remove, the dialog returns false
   *  from {@code showModal()} but {@link #wasRemoveClicked()} returns true. */
  NewQueryFavDlgCtrlr(QueryFavorite favoriteBeingEdited)
  {
    super("query/NewQueryFavDlg", "Edit Query Favorite", true, true);

    this.favoriteBeingEdited = favoriteBeingEdited;

    tfName.setText(favoriteBeingEdited.name);
    chkAutoExec.setSelected(favoriteBeingEdited.autoexec);

    btnRemove.setOnAction(event -> btnRemoveClick());

    onShown = this::sizeAndFocus;
  }

//---------------------------------------------------------------------------

  String getNewName()         { return tfName.getText().strip(); }
  boolean getAutoExec()       { return chkAutoExec.isSelected(); }
  boolean wasRemoveClicked()  { return removeClicked; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sizeAndFocus()
  {
    stage.sizeToScene();
    stage.setMinHeight(stage.getHeight());
    stage.setMaxHeight(stage.getHeight());
    safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnRemoveClick()
  {
    removeClicked = true;
    btnCancelClick();  // close dialog without setting okClicked; caller checks wasRemoveClicked()
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    String newName = getNewName();

    if (strNullOrBlank(newName))
      return falseWithErrorPopup("Name cannot be blank.", tfName);

    // In edit mode, the current name is allowed (it's just keeping the same name).
    // In create mode, or when renaming to a different name, the name must not collide.

    boolean checkForDuplicateName = (favoriteBeingEdited == null) || (newName.equals(favoriteBeingEdited.name) == false);

    if (checkForDuplicateName && ui.favorites.queryFavNameExists(newName))
      return falseWithErrorPopup("A favorite with that name already exists.", tfName);

    if (favoriteBeingEdited != null)
    {
      favoriteBeingEdited.rename(newName);
      favoriteBeingEdited.autoexec = chkAutoExec.isSelected();
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
