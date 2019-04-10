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

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Institution;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_InstitutionType;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;

public class NewInstDlgCtrlr extends HyperDlg
{
  public HyperCB hcbParent, hcbType;

  @FXML public TextField tfName, tfNewParentName;
  @FXML private ComboBox<HyperTableCell> cbType, cbParent;
  @FXML public RadioButton rbNew;
  @FXML private RadioButton rbExisting;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewInstDlgCtrlr create(String title, HDT_Institution parent, String newName, boolean isParent)
  {
    NewInstDlgCtrlr ndc = HyperDlg.create("NewInstDlg.fxml", title, true);
    ndc.init(parent, newName, isParent);
    return ndc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Institution parent, String newName, boolean isParent)
  {
    Predicate<HDT_Record> popFilter = record -> HDT_Institution.class.cast(record).subInstitutions.size() > 0;

    hcbParent = new HyperCB(cbParent, ctDropDownList, new StandardPopulator(hdtInstitution, popFilter, true), null, false);
    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtInstitutionType), null, false);

    hcbParent.dontCreateNewRecord = true;

    cbParent.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> rbExisting.setSelected(true));

    tfNewParentName.textProperty().addListener((observable, oldValue, newValue) -> rbNew.setSelected(true));

    hcbParent.addAndSelectEntryOrBlank(parent, HDT_Record::name);

    if (newName.length() > 0)
    {
      if (isParent)
      {
        if (parent == null)
          tfNewParentName.setText(newName);
      }
      else
        tfName.setText(newName);
    }

    hcbType.selectID(HDT_InstitutionType.DEPARTMENT_INST_TYPE_ID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbType.selectedID() >= 1) return true;

    messageDialog("You must select a type.", mtError);
    cbType.requestFocus();
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
