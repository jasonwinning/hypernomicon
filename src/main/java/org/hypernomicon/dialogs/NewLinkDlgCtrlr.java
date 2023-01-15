/*
 * Copyright 2015-2023 Jason Winning
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

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

public class NewLinkDlgCtrlr extends HyperDlg
{
  @FXML public TextField tfDisplayText, tfURL;
  @FXML private Button btnPaste;

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

  public static NewLinkDlgCtrlr build(String selText)
  {
    return ((NewLinkDlgCtrlr) create("NewLinkDlg", "Insert Link", true)).init(selText);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewLinkDlgCtrlr init(String selText)
  {
    String clipText = getClipboardText(true).trim();
    selText = selText.trim();

    btnPaste.setOnAction(event -> tfURL.setText(ultraTrim(getClipboardText(true))));
    setToolTip(btnPaste, "Paste text from clipboard");

    if (isStringUrl(selText))
    {
      tfURL.setText(selText);
      tfDisplayText.setText(clipText.length() > 0 ? clipText : selText);
      return this;
    }

    if (isStringUrl(clipText))
      tfURL.setText(clipText);

    tfDisplayText.setText(selText.length() > 0 ? selText : clipText);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
