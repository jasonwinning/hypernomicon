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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.model.HyperDB.*;

import org.hypernomicon.model.records.HDT_Record;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.TransferMode;

public class ChangeParentDlgCtrlr extends HyperDlg
{
  @FXML private Button btnCopy;
  @FXML private Label label1, label2, label3;
  @FXML private TextField tfChild, tfNewParent, tfOldParent;

  private TransferMode transferMode = null;

  public TransferMode getTransferMode() { return transferMode; }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ChangeParentDlgCtrlr build(HDT_Record oldParent, HDT_Record newParent, HDT_Record child, boolean copyIsOK)
  {
    return ((ChangeParentDlgCtrlr) create("ChangeParentDlg.fxml", "Copy or Move Record to Destination", true)).init(oldParent, newParent, child, copyIsOK);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ChangeParentDlgCtrlr init(HDT_Record oldParent, HDT_Record newParent, HDT_Record child, boolean copyIsOK)
  {
    label1.setText("The " + db.getTypeName(child.getType()) + " record:");
    label2.setText("will be attached under the " + db.getTypeName(newParent.getType()) + " record:");

    label3.setText(copyIsOK ?
      "Select [ Move ] if it should also be unattached from the " + db.getTypeName(oldParent.getType()) + " record:"
    :
      "and will be unattached from the " + db.getTypeName(oldParent.getType()) + " record:");

    tfChild.setText(child.name());
    tfOldParent.setText(oldParent.name());
    tfNewParent.setText(newParent.name());

    btnCopy.setDisable(copyIsOK == false);

    transferMode = null;

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnMoveClick()
  {
    transferMode = TransferMode.MOVE;
    btnOkClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCopyClick()
  {
    transferMode = TransferMode.COPY;
    btnOkClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
