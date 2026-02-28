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

package org.hypernomicon.util;

import java.util.LinkedHashMap;

import com.google.common.collect.BiMap;
import com.google.common.collect.HashBiMap;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

public class PopupDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum DialogResult
  {
    mrUnknown, mrNone, mrOk,       mrCancel,   mrYes, mrNo,      mrAbort,    mrRetry, mrIgnore, mrFail,
    mrClose,   mrHelp, mrTryAgain, mrContinue, mrAll, mrNoToAll, mrYesToAll, mrMove,  mrCopy
  }

//---------------------------------------------------------------------------

  private final String message;
  private final LinkedHashMap<String, DialogResult> buttons = new LinkedHashMap<>();
  private final LinkedHashMap<String, String> tooltips = new LinkedHashMap<>();

  private DialogResult defaultButton;

//---------------------------------------------------------------------------

  public PopupDialog(String message)
  {
    this.message = message;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PopupDialog addDefaultButton(String caption, DialogResult result)
  {
    return addDefaultButton(caption, result, null);
  }

//---------------------------------------------------------------------------

  public PopupDialog addDefaultButton(String caption, DialogResult result, String tooltip)
  {
    defaultButton = result;

    return addButton(caption, result, tooltip);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PopupDialog addButton(String caption, DialogResult result)
  {
    return addButton(caption, result, null);
  }

//---------------------------------------------------------------------------

  public PopupDialog addButton(String caption, DialogResult result, String tooltip)
  {
    if (buttons.containsKey(caption))
      throw new IllegalArgumentException("Duplicate button caption: " + caption);

    buttons.put(caption, result);

    if (tooltip != null)
      tooltips.put(caption, tooltip);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public DialogResult showModal()
  {
    // PopupRobot interception for direct PopupDialog callers (e.g., FileDeletion).
    // UIUtil wrappers (confirmDialog, yesNoCancelDialog, etc.) intercept earlier with
    // their own fallback logic and never reach this point.

    if (PopupRobot.isActive())
    {
      PopupRobot.record(message, AlertType.CONFIRMATION);
      DialogResult response = PopupRobot.getDefaultResponse();

      // Return the response if it matches a configured button

      if (buttons.containsValue(response))
        return response;

      // Otherwise return the default button's result, or the first configured button

      return defaultButton != null ? defaultButton : buttons.values().iterator().next();
    }

    DialogResult[] result = { null };

    runInFXThread(() ->
    {
      Alert dlg = new Alert(AlertType.CONFIRMATION);
      dlg.setTitle(appTitle);
      dlg.setHeaderText(message);
      dlg.setContentText("Please select an option.");
      dlg.setResizable(false);
      dlg.getButtonTypes().clear();

      BiMap<ButtonType, DialogResult> bTypeToResult = HashBiMap.create(new LinkedHashMap<>());

      buttons.forEach((caption, dialogResult) ->
      {
        ButtonType bType = new ButtonType(caption);
        bTypeToResult.put(bType, dialogResult);
        dlg.getButtonTypes().add(bType);

        setToolTip((Control) dlg.getDialogPane().lookupButton(bType), tooltips.get(caption));
      });

      if (defaultButton != null)
        Platform.runLater(() -> dlg.getDialogPane().lookupButton(bTypeToResult.inverse().get(defaultButton)).requestFocus());

      result[0] = bTypeToResult.get(showAndWait(dlg));
    }, true);

    return result[0];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
