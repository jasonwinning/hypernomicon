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

import static org.hypernomicon.util.UIUtil.*;

import java.util.function.Supplier;

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

  @FXML private Button btnRefresh;
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
   * Creates a dialog with a text area (no header) and a Refresh button. The
   * initial text comes from {@code textSupplier}, and each Refresh click
   * replaces the displayed text with a fresh call to it.
   * @param title The window title
   * @param textSupplier Supplies the text, re-invoked on every Refresh click
   */
  public LongMessageDlgCtrlr(String title, Supplier<String> textSupplier)
  {
    this(title, null, null, textSupplier.get());

    bindManagedToVisible(btnRefresh);
    btnRefresh.setVisible(true);
    btnRefresh.setOnAction(event -> taMessage.setText(textSupplier.get()));
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

    bindManagedToVisible(hboxHeader);

    if ((alertType != null) && (headerText != null))
    {
      // Standard alert icon, on the right like real Alerts
      Node graphic = getAlertGraphic(alertType);
      if (graphic != null)
        hboxHeader.getChildren().add(graphic);

      lblHeaderText.setText(headerText);
    }
    else
      hboxHeader.setVisible(false);

    taMessage.setText(text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
