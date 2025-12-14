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
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;

import org.hypernomicon.dialogs.base.ModalDialog;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public class HelpDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private WebView webView;

//---------------------------------------------------------------------------

  public HelpDlgCtrlr()
  {
    super("HelpDlg", appTitle, true);

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    StringBuilder html = new StringBuilder();

    try
    {
      readResourceTextFile("resources/Shortcuts.html", html, true);
      webView.getEngine().loadContent(html.toString());
    }
    catch (IOException e)
    {
      onShown = () ->
      {
        errorPopup("Unable to show help content: " + getThrowableMessage(e));
        Platform.runLater(stage::close);
      };
    }
  }

//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
