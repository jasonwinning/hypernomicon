/*
 * Copyright 2015-2019 Jason Winning
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

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public class NewLinkDlgCtrlr extends HyperDlg
{
  @FXML public TextField tfDisplayText, tfURL;
  @FXML private Button btnOk, btnCancel;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfDisplayText.getText().trim().length() == 0)
    {
      messageDialog("Enter the text to be displayed.", mtError);
      safeFocus(tfDisplayText);
      return false;
    }

    if (tfURL.getText().trim().length() == 0)
    {
      messageDialog("Enter a web address (URL).", mtError);
      safeFocus(tfURL);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewLinkDlgCtrlr create(String title, String selText)
  {
    NewLinkDlgCtrlr nld = HyperDlg.create("NewLinkDlg.fxml", title, true);
    nld.init(selText);
    return nld;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(String selText)
  {
    String clipText = getClipboardText(true).trim();
    selText = selText.trim();

    if (isStringUrl(selText))
    {
      tfURL.setText(selText);
      tfDisplayText.setText(clipText.length() > 0 ? clipText : selText);
      return;
    }

    if (isStringUrl(clipText))
      tfURL.setText(clipText);

    tfDisplayText.setText(selText.length() > 0 ? selText : clipText);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
