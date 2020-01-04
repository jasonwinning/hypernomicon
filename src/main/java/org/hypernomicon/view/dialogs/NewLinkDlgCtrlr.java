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

import javafx.fxml.FXML;
import javafx.scene.control.TextField;

import static org.hypernomicon.util.Util.*;

public class NewLinkDlgCtrlr extends HyperDlg
{
  @FXML public TextField tfDisplayText, tfURL;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfDisplayText.getText().trim().isEmpty())
      return falseWithErrorMessage("Enter the text to be displayed.", tfDisplayText);

    if (tfURL.getText().trim().isEmpty())
      return falseWithErrorMessage("Enter a web address (URL).", tfURL);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewLinkDlgCtrlr create(String selText)
  {
    NewLinkDlgCtrlr nld = HyperDlg.create("NewLinkDlg.fxml", "Insert Link", true);
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
