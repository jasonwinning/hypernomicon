/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;

public class NewVersionDlgCtrlr extends HyperDlg
{
  @FXML private Button btnDownload;
  @FXML private CheckBox chkNoShow;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewVersionDlgCtrlr build()
  {
    return ((NewVersionDlgCtrlr) create("NewVersionDlg.fxml", "A New Version Is Available", false)).init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewVersionDlgCtrlr init()
  {
    btnDownload.setOnAction(event ->
    {
      openWebLink("https://sourceforge.net/projects/hypernomicon/files/latest/download");

      btnOkClick();
    });

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (chkNoShow.isSelected())
      appPrefs.putBoolean(PREF_KEY_CHECK_FOR_NEW_VERSION, false);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
