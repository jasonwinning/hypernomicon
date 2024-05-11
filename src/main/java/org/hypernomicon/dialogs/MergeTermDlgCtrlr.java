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

package org.hypernomicon.dialogs;

import static org.hypernomicon.view.MainCtrlr.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.records.HDT_Term;
import javafx.fxml.FXML;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;

public class MergeTermDlgCtrlr extends HyperDlg
{
  @FXML private RadioButton rbName1, rbName2, rbName3, rbKey1, rbKey2, rbKey3;
  @FXML private TextField tfName1, tfName2, tfName3, tfKey1, tfKey2, tfKey3;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (getKey().replace("^", "").replace("$", "").trim().length() < 3)
    {
      errorPopup("Search key of a term record cannot be zero-length.");

      if      (rbKey1.isSelected()) safeFocus(tfKey1);
      else if (rbKey2.isSelected()) safeFocus(tfKey2);
      else                          safeFocus(tfKey3);

      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MergeTermDlgCtrlr(HDT_Term term1, HDT_Term term2)
  {
    super("MergeTermDlg", "Specify How to Merge Fields", true);

    String name1 = term1.listName(),
           name2 = term2.listName(),
           key1  = term1.getSearchKey(),
           key2  = term2.getSearchKey();

    tfName1.setText(name1);
    tfName2.setText(name2);

    if (name1.isEmpty() && (name2.length() > 0))
      rbName2.setSelected(true);

    tfKey1.setText(key1);
    tfKey2.setText(key2);

    if (key1.isEmpty() && (key2.length() > 0))
      rbKey2.setSelected(true);

    tfName3.textProperty().addListener((ob, oldValue, newValue) -> rbName3.setSelected(true));
    tfKey3 .textProperty().addListener((ob, oldValue, newValue) -> rbKey3 .setSelected(true));

    setSearchKeyToolTip(tfKey1);
    setSearchKeyToolTip(tfKey2);
    setSearchKeyToolTip(tfKey3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getName()
  {
    if (rbName1.isSelected()) return tfName1.getText();
    if (rbName2.isSelected()) return tfName2.getText();
                              return tfName3.getText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getKey()
  {
    if (rbKey1.isSelected()) return tfKey1.getText();
    if (rbKey2.isSelected()) return tfKey2.getText();
                             return tfKey3.getText();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
