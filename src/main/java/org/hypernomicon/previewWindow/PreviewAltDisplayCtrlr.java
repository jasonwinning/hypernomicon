/*
 * Copyright 2015-2024 Jason Winning
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

package org.hypernomicon.previewWindow;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.util.filePath.FilePath;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;

//---------------------------------------------------------------------------

public class PreviewAltDisplayCtrlr
{

//---------------------------------------------------------------------------

  private @FXML Label lblMessage;
  private @FXML ProgressBar progressBar;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setGenerating(FilePath filePath)
  {
    progressBar.setProgress(0.0);
    progressBar.setProgress(-1.0);
    lblMessage.setText("Generating preview for file: " + htmlEscaper.escape(filePath.toString()));
    setInfoIconVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setUnable(FilePath filePath)
  {
    setUnable(filePath.toString());
  }

  public void setUnable(String pathStr)
  {
    lblMessage.setText("Unable to preview the file: " + htmlEscaper.escape(pathStr));
    setInfoIconVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setNoOfficeInstallation()
  {
    lblMessage.setText("To preview this type of file, enter the installation path for LibreOffice or OpenOffice in the Settings dialog.");
    setInfoIconVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setInfoIconVisible(boolean visible)
  {
    if (visible)
    {
      if (lblMessage.getStyleClass().contains("dialog-pane") == false)
        lblMessage.getStyleClass().add("dialog-pane");
    }
    else
    {
      lblMessage.getStyleClass().remove("dialog-pane");
    }

    progressBar.setVisible(visible == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
