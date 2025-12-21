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

package org.hypernomicon.dialogs.base;

import static org.hypernomicon.App.ui;
import static org.hypernomicon.util.UIUtil.*;

import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.ComboBox;
import javafx.stage.Modality;
import javafx.stage.StageStyle;

//---------------------------------------------------------------------------

public abstract class ModalDialog extends DialogBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean doShow = true;
  protected boolean okClicked = false;

//---------------------------------------------------------------------------

  protected ModalDialog(String loc, String title, boolean resizable)
  {
    this(loc, title, resizable, false);
  }

  protected ModalDialog(String loc, String title, boolean resizable, boolean fullPath)
  {
    super(loc, title, resizable, StageStyle.UTILITY, Modality.APPLICATION_MODAL, fullPath);
  }

//---------------------------------------------------------------------------

  protected final void abort() { doShow = false; }

  protected abstract boolean isValid();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected final void doAdditionalOnShown()
  {
    if (onShown != null) onShown.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showModal()
  {
    if (doShow)
    {
      ui.windows.push(stage);

      stage.showAndWait();

      ui.windows.pop();
    }
    else
    {
      okClicked = false;
    }

    return okClicked;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML protected void btnOkClick()
  {
    Node node = stage.getScene().getFocusOwner();

    // The next check is necessary due to https://bugs.openjdk.org/browse/JDK-8229924
    // If you hit enter in the combobox, the key event gets consumed by the Scene before it ever gets to the text edit control. The HyperCB onaction gets
    // triggered in that case because the Scene did not mark the event as consumed after processing it (which is also a related JavaFX bug).

    if ((node instanceof ComboBox) && (getNodeUserObj(node, NodeUserDataType.HypercCB) != null))
      return;

    if (isValid() == false)
      return;

    okClicked = true;
    stage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML protected void btnCancelClick()
  {
    okClicked = false;
    stage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
