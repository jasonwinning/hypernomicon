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

import static org.hypernomicon.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import org.apache.commons.io.FilenameUtils;

import org.hypernomicon.util.filePath.FilePath;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class RenameDlgCtrlr extends HyperDlg
{
  public enum NameType
  {
    ntRecord,
    ntFile,
    ntFolder
  }

  @FXML private Button btnOk;
  @FXML private Label lblInvalid;
  @FXML private TextField tfName;

  private final NameType nameType;
  private final String oldName;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getNewName() { return tfName.getText(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public RenameDlgCtrlr(String title, NameType nameType, String oldName)
  {
    super("RenameDlg", title, true);

    this.nameType = nameType;
    this.oldName = oldName;
    tfName.setText(oldName);

    if (nameType == ntFolder)
      lblInvalid.setText("Invalid folder name!");

    tfName.focusedProperty().addListener((ob, oldValue, newValue) -> Platform.runLater(() ->
    {
      if (tfName.isFocused() && !tfName.getText().isEmpty())
      {
        try
        {
          if ((nameType != ntRecord) && (FilenameUtils.indexOfExtension(tfName.getText()) >= 0))
            tfName.selectRange(0, FilenameUtils.indexOfExtension(tfName.getText()));
          else
            tfName.selectAll();
        }
        catch (IllegalArgumentException e) { noOp(); }
      }
    }));

    tfName.textProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || newValue.equals(oldValue)) return;

      if (newValue.isEmpty())
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

  @Override protected boolean isValid()
  {
    if (tfName.getText().isEmpty())
      return falseWithErrorPopup("Name cannot be zero-length.", tfName);

    if (nameType != ntRecord)
    {
      if (FilenameUtils.equalsNormalizedOnSystem(oldName, tfName.getText()))
        return falseWithErrorPopup("Original name and new name are the same.", tfName);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
