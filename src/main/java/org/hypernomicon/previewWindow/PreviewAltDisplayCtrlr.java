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

package org.hypernomicon.previewWindow;

import org.hypernomicon.util.file.FilePath;

import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;

//---------------------------------------------------------------------------

public class PreviewAltDisplayCtrlr
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private @FXML Label lblMessage;
  private @FXML ProgressBar progressBar;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setGenerating(FilePath filePath, boolean dontRestartProgressIfSamePreview)
  {
    String msg = OfficePreviewer.getFirstConversion() ?
      "Starting office document previewer and generating preview for file: " + filePath.toString()
    :
      "Generating preview for file: " + filePath.toString();

    if (dontRestartProgressIfSamePreview && lblMessage.getText().equals(msg))
      return;

    progressBar.setProgress(0.0);
    progressBar.setProgress(-1.0);
    lblMessage.setText(msg);
    setInfoIconVisible(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setStartingConverter()
  {
    progressBar.setProgress(0.0);
    progressBar.setProgress(-1.0);
    lblMessage.setText("Starting office document previewer...");
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
    lblMessage.setText("Unable to preview the file: " + pathStr);
    setInfoIconVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setNoOfficeInstallation()
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
