/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.dialogs.HyperDlg;

import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class ChangeParentDlgCtrlr extends HyperDlg
{
  @FXML private CheckBox chkDetach1, chkDetach2;
  @FXML private Label label1, label2, label3;
  @FXML private TextField tfChild, tfNewParent, tfOldParent1, tfOldParent2;

  boolean detachDragSource() { return chkDetach1.isSelected(); }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ChangeParentDlgCtrlr build(RecordTreeEdge dragTargetEdge, RecordTreeEdge dragSourceEdge, RecordTreeEdge otherEdgeToDetach)
  {
    return ((ChangeParentDlgCtrlr) createUsingFullPath("tree/ChangeParentDlg", "Copy or Move Record to Destination", true))
                                  .init(dragTargetEdge, dragSourceEdge, otherEdgeToDetach);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ChangeParentDlgCtrlr init(RecordTreeEdge dragTargetEdge, RecordTreeEdge dragSourceEdge, RecordTreeEdge otherEdgeToDetach)
  {
    label1.setText("The " + db.getTypeName(dragTargetEdge.child.getType()) + " record:");
    label2.setText("will be attached under the " + db.getTypeName(dragTargetEdge.parent.getType()) + " record:");

    tfChild.setText(dragTargetEdge.child.name());
    tfNewParent.setText(dragTargetEdge.parent.name());

    tfOldParent1.setText(dragSourceEdge.parent.name());

    if (dragSourceEdge.mustDetachIfAttaching(dragTargetEdge))
    {
      chkDetach1.setDisable(true);
      chkDetach1.setSelected(true);
    }
    else if (dragSourceEdge.canDetach() == false)
    {
      chkDetach1.setDisable(true);
      chkDetach1.setSelected(false);
    }

    if (otherEdgeToDetach == null)
      setAllVisible(false, chkDetach2, tfOldParent2);
    else
      tfOldParent2.setText(otherEdgeToDetach.parent.name());

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
