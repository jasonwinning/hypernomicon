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

package org.hypernomicon.tree;

import static org.hypernomicon.model.HyperDB.*;

import org.hypernomicon.dialogs.base.ModalDialog;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.VBox;

//---------------------------------------------------------------------------

public class ChangeParentDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private VBox vbMustDetach, vbOptionDetach;
  @FXML private Label lblChild, lblNewParent, lblMustDetach, lblOptionDetach;
  @FXML private TextField tfChild, tfNewParent, tfMustDetach, tfOptionDetach;
  @FXML private Button btnDetachAlso, btnOnlyAttach;

  private boolean detachDragSource = false;

  boolean detachDragSource() { return detachDragSource; }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  ChangeParentDlgCtrlr(RecordTreeEdge dragTargetEdge, RecordTreeEdge dragSourceEdge, RecordTreeEdge otherEdgeToDetach)
  {
    super("tree/ChangeParentDlg", "Attach Record to New Parent", true, true);

    String childTypeName     = getTypeName(dragTargetEdge.child .getType()),
           newParentTypeName = getTypeName(dragTargetEdge.parent.getType()),
           srcParentTypeName = getTypeName(dragSourceEdge.parent.getType());

    // Set up child and new parent labels and fields
    lblChild.setText("Selected " + childTypeName + " record:");
    tfChild.setText(dragTargetEdge.child.name());

    lblNewParent.setText("Will become attached under the " + newParentTypeName + " record:");
    tfNewParent.setText(dragTargetEdge.parent.name());

    // Handle "must be detached from" section (otherEdgeToDetach)
    if (otherEdgeToDetach == null)
    {
      vbMustDetach.setVisible(false);
      vbMustDetach.setManaged(false);
    }
    else
    {
      String otherParentTypeName = getTypeName(otherEdgeToDetach.parent.getType());
      lblMustDetach.setText("And must therefore be detached from " + otherParentTypeName + " record:");
      tfMustDetach.setText(otherEdgeToDetach.parent.name());
    }

    // Handle "option to detach" section and edge cases
    boolean mustDetach = dragSourceEdge.mustDetachIfAttaching(dragTargetEdge),
            canDetach = dragSourceEdge.canDetach();

    if (mustDetach)
    {
      // Must detach - change label and hide "Only Attach" button
      lblOptionDetach.setText("And will be detached from " + srcParentTypeName + " record:");
      tfOptionDetach.setText(dragSourceEdge.parent.name());
      btnOnlyAttach.setVisible(false);
      btnOnlyAttach.setManaged(false);
      btnDetachAlso.setText("OK");
    }
    else if (!canDetach)
    {
      // Cannot detach - hide the entire option section and "Yes, Detach Also" button
      vbOptionDetach.setVisible(false);
      vbOptionDetach.setManaged(false);
      btnDetachAlso.setVisible(false);
      btnDetachAlso.setManaged(false);
      btnOnlyAttach.setText("OK");
    }
    else
    {
      // Normal case - show option and both buttons
      lblOptionDetach.setText("Also detach it from this " + srcParentTypeName + " record?");
      tfOptionDetach.setText(dragSourceEdge.parent.name());
    }

    // Resize window to fit content, then lock vertical size
    onShown = () ->
    {
      stage.sizeToScene();
      stage.setMinHeight(stage.getHeight());
      stage.setMaxHeight(stage.getHeight());
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnDetachAlsoClick()
  {
    detachDragSource = true;
    btnOkClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnOnlyAttachClick()
  {
    detachDragSource = false;
    btnOkClick();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
