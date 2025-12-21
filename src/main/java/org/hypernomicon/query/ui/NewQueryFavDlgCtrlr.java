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

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class NewQueryFavDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TextField tfName;
  @FXML private CheckBox chkAutoExec;

//---------------------------------------------------------------------------

  NewQueryFavDlgCtrlr(String newName)
  {
    super("query/NewQueryFavDlg", "Add Query Favorite", true, true);

    tfName.setText(newName);

    onShown = () -> safeFocus(tfName);
  }

//---------------------------------------------------------------------------

  String getNewName()   { return tfName.getText().strip(); }
  boolean getAutoExec() { return chkAutoExec.isSelected(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (strNullOrBlank(tfName.getText()))
      return falseWithErrorPopup("Name cannot be blank.", tfName);

    if (ui.favorites.queryFavNameExists(tfName.getText().strip()))
      return falseWithErrorPopup("A favorite with that name already exists.", tfName);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
