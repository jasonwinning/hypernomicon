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

package org.hypernomicon.dialogs;

import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.layout.HBox;

import org.hypernomicon.dialogs.base.ModalDialog;

//---------------------------------------------------------------------------

/**
 * Dialog for displaying a long message in a scrollable text area with an optional
 * header section containing an icon and summary message, similar to standard Alert dialogs.
 */
public class LongMessageDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private HBox hboxHeader;
  @FXML private Label lblHeaderText;
  @FXML private TextArea taMessage;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  /**
   * Creates a dialog with just a text area (no header).
   */
  public LongMessageDlgCtrlr(String title, String text)
  {
    this(title, null, null, text);
  }

//---------------------------------------------------------------------------

  /**
   * Creates a dialog with an icon and header message above the text area.
   * @param title The window title
   * @param alertType The type of alert icon to display (WARNING, ERROR, INFORMATION, etc.), or null for no icon
   * @param headerText The header message to display next to the icon, or null for no header
   * @param text The detailed text to display in the scrollable text area
   */
  public LongMessageDlgCtrlr(String title, AlertType alertType, String headerText, String text)
  {
    super("LongMessageDlg", title, true);

    if ((alertType != null) && (headerText != null))
    {
      // Get the standard icon from a temporary Alert (icon goes on the right, like real Alerts)
      Node graphic = getAlertGraphic(alertType);
      if (graphic != null)
        hboxHeader.getChildren().add(graphic);

      lblHeaderText.setText(headerText);
    }
    else
    {
      hboxHeader.setVisible(false);
      hboxHeader.setManaged(false);
    }

    taMessage.setText(text);
  }

//---------------------------------------------------------------------------

  /**
   * Extracts the standard graphic/icon from a JavaFX Alert of the given type.
   */
  private static Node getAlertGraphic(AlertType alertType)
  {
    Alert tempAlert = new Alert(alertType);
    tempAlert.getDialogPane().applyCss();
    return tempAlert.getDialogPane().getGraphic();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
