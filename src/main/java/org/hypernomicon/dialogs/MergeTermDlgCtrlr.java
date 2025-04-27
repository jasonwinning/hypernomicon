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

import static org.hypernomicon.view.MainCtrlr.*;

import org.hypernomicon.dialogs.base.ModalDialog;

import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.HDT_Term;

import javafx.fxml.FXML;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class MergeTermDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private RadioButton rbName1, rbName2, rbName3, rbKey1, rbKey2, rbKey3;
  @FXML private TextField tfName1, tfName2, tfName3, tfKey1, tfKey2, tfKey3;

  private final HDT_Term term1, term2;

//---------------------------------------------------------------------------

  public MergeTermDlgCtrlr(HDT_Term term1, HDT_Term term2)
  {
    super("MergeTermDlg", "Specify How to Merge Fields", true);

    this.term1 = term1;
    this.term2 = term2;

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

  private TextField selectedKeyField()
  {
    if (rbKey1.isSelected()) return tfKey1;
    if (rbKey2.isSelected()) return tfKey2;
                             return tfKey3;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TextField selectedNameField()
  {
    if (rbName1.isSelected()) return tfName1;
    if (rbName2.isSelected()) return tfName2;
                              return tfName3;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    TextField nameField = selectedNameField(),
              keyField  = selectedKeyField();

    if (nameField.getText().isBlank())
      return falseWithErrorPopup("Unable to merge terms: Term cannot be blank.", nameField);

    if (keyField.getText().isBlank())
      return falseWithErrorPopup("Unable to merge terms: Search key cannot be blank.", keyField);

    try
    {
      HDT_Term.merge(term1, term2, nameField.getText(), keyField.getText());
    }
    catch (SearchKeyException e)
    {
      return falseWithErrorPopup(e instanceof SearchKeyTooShortException ?
        "Unable to merge terms. Search key must have at least 3 characters: " + e.getKey()
      :
        "Unable to merge terms. Search key already exists: " + e.getKey(), keyField);
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
