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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.dialogs.RenameDialogController.NameType.*;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.util.filePath.FilePath;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class RenameDialogController extends HyperDialog
{
  public static enum NameType
  {
    ntRecord,
    ntFile,
    ntFolder
  }

  @FXML private Button btnOk;
  @FXML private Button btnCancel;
  @FXML private Label lblInvalid;
  @FXML private TextField tfName;

  private NameType nameType;
  private String oldName;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getNewName() { return tfName.getText(); }

  @Override protected boolean isValid()
  {
    if (tfName.getText().length() == 0)
    {
      messageDialog("Name cannot be zero-length.", mtError);
      safeFocus(tfName);
      return false;
    }

    if (nameType != ntRecord)
    {
      if (FilenameUtils.equalsNormalizedOnSystem(oldName, tfName.getText()))
      {
        messageDialog("Original name and new name are the same.", mtError);
        safeFocus(tfName);
        return false;
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RenameDialogController create(String title, NameType nameType, String oldName)
  {
    RenameDialogController rdc = HyperDialog.create("RenameDialog.fxml", title, true);
    rdc.init(nameType, oldName);
    return rdc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(NameType nameType, String oldName)
  {
    this.nameType = nameType;
    this.oldName = oldName;
    tfName.setText(oldName);

    if (nameType == ntFolder)
      lblInvalid.setText("Invalid folder name!");

    tfName.focusedProperty().addListener((ov, oldValue, newValue) ->
    {
      Platform.runLater(() ->
      {
        if (tfName.isFocused() && !tfName.getText().isEmpty())
        {
          if ((nameType != NameType.ntRecord) && (FilenameUtils.indexOfExtension(tfName.getText()) >= 0))
            tfName.selectRange(0, FilenameUtils.indexOfExtension(tfName.getText()));
          else
            tfName.selectAll();
        }
      });
    });

    tfName.textProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      if (oldValue != null)
        if (oldValue.equals(newValue)) return;

      if (newValue.length() == 0)
      {
        lblInvalid.setVisible(false);
        btnOk.setDisable(true);
        return;
      }

      if (nameType == ntRecord)
      {
        btnOk.setDisable(false);
        return;
      }

      if (FilePath.isFilenameValid(newValue))
      {
        lblInvalid.setVisible(false);
        btnOk.setDisable(false);
      }
      else
      {
        lblInvalid.setVisible(true);
        btnOk.setDisable(true);
      }
    });

    onShown = () -> safeFocus(tfName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
