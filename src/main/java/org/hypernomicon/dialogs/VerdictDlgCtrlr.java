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

package org.hypernomicon.dialogs;

import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;

public class VerdictDlgCtrlr extends HyperDlg
{
  @FXML private Label lblParentType, lblParentName;
  @FXML private ComboBox<HyperTableCell> cbVerdict;

  public HyperCB hcbVerdict;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static VerdictDlgCtrlr build(String title, HDT_Record parent)
  {
    return ((VerdictDlgCtrlr) create("VerdictDlg", title, false)).init(parent);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private VerdictDlgCtrlr init(HDT_Record parent)
  {
    lblParentName.setText(parent.getCBText());

    if (parent.getType() == hdtPosition)
    {
      lblParentType.setText("Target Position:");
      hcbVerdict = new HyperCB(cbVerdict, ctDropDownList, new StandardPopulator(hdtPositionVerdict));
    }
    else if (parent.getType() == hdtArgument)
    {
      lblParentType.setText("Target Argument:");
      hcbVerdict = new HyperCB(cbVerdict, ctDropDownList, new StandardPopulator(hdtArgumentVerdict));
    }
    else
    {
      messageDialog("Internal Error: 90902", mtError);
    }

    hcbVerdict.addBlankEntry();

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return hcbVerdict.selectedID() > 0 ? true : falseWithErrorMessage("You must select a verdict.", cbVerdict);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
