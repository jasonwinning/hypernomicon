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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;

import org.hypernomicon.util.DesktopUtil;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;

//---------------------------------------------------------------------------

public class NewVersionDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnDownload;
  @FXML private CheckBox chkNoShow;

//---------------------------------------------------------------------------

  public NewVersionDlgCtrlr()
  {
    super("NewVersionDlg", "A New Version Is Available", false);

    btnDownload.setOnAction(event ->
    {
      DesktopUtil.openWebLink("https://sourceforge.net/projects/hypernomicon/files/latest/download");

      btnOkClick();
    });

    ui.windows.runInFXThreadAfterModalPopups(3000, this::showModal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (chkNoShow.isSelected())
      app.prefs.putBoolean(PREF_KEY_CHECK_FOR_NEW_VERSION, false);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
