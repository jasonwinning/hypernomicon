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

package org.hypernomicon.util;

import java.util.LinkedHashMap;
import java.util.Map;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.ButtonType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class PopupDialog
{
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static enum DialogResult
  {
    mrUnknown, mrNone, mrOk,       mrCancel,   mrYes, mrNo,      mrAbort,    mrRetry, mrIgnore, mrFail,
    mrClose,   mrHelp, mrTryAgain, mrContinue, mrAll, mrNoToAll, mrYesToAll, mrMove,  mrCopy
  }

//---------------------------------------------------------------------------

  private final Alert dlg = new Alert(AlertType.CONFIRMATION);
  private final Map<ButtonType, DialogResult> bTypeToResult = new LinkedHashMap<>();

  public DialogResult showModal() { return bTypeToResult.get(showAndWait(dlg)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PopupDialog addButton(String caption, DialogResult result)
  {
    ButtonType bType = new ButtonType(caption);
    bTypeToResult.put(bType, result);
    dlg.getButtonTypes().add(bType);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PopupDialog(String message)
  {
    dlg.setTitle(appTitle);
    dlg.setHeaderText(message);
    dlg.setContentText("Please select an option.");
    dlg.setResizable(false);
    dlg.getButtonTypes().clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
