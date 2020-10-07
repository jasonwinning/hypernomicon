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

package org.hypernomicon.dialogs;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Institution;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.SubjectPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;

public class NewInstDlgCtrlr extends HyperDlg
{
  public HyperCB hcbParent, hcbExisting, hcbType;

  @FXML public TextField tfName, tfNewParentName;
  @FXML private ComboBox<HyperTableCell> cbType, cbParent, cbExisting;
  @FXML public RadioButton rbNewInst, rbNewDiv;
  @FXML private RadioButton rbExistingInst, rbExistingDiv;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewInstDlgCtrlr build(HDT_Institution parent, String newName, boolean isParent)
  {
    return ((NewInstDlgCtrlr) create("NewInstDlg", "New Institution or Institutional Division", true)).init(parent, newName, isParent);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewInstDlgCtrlr init(HDT_Institution parent, String newName, boolean isParent)
  {
    Predicate<Integer> popFilter = id -> db.institutions.getByID(id).subInstitutions.size() > 0;

    hcbParent = new HyperCB(cbParent, ctDropDownList, new StandardPopulator(hdtInstitution, popFilter, true));
    hcbExisting = new HyperCB(cbExisting, ctDropDownList, new SubjectPopulator(rtParentInstOfInst, false), true);
    hcbType = new HyperCB(cbType, ctDropDownList, new StandardPopulator(hdtInstitutionType));

    hcbParent.dontCreateNewRecord = true;

    hcbParent.addListener((oldValue, newValue) ->
    {
      if (isEmpty(newValue) || (getCellID(oldValue) == getCellID(newValue))) return;

      rbExistingInst.setSelected(true);
        
      ((SubjectPopulator)hcbExisting.getPopulator()).setObj(null, getRecord(newValue));
      if (getCellID(oldValue) > 0)
        hcbExisting.selectID(-1);
    });

    hcbExisting.addListener((oldValue, newValue) ->
    {
      if (isEmpty(newValue) || (getCellID(oldValue) == getCellID(newValue))) return;
      
      rbExistingInst.setSelected(true);
      rbExistingDiv .setSelected(true);
    });

    rbExistingDiv.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        rbExistingInst.setSelected(true);
    });

    rbNewInst.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        rbNewDiv.setSelected(true);
    });

    tfNewParentName.textProperty().addListener((ob, oldValue, newValue) -> rbNewInst.setSelected(true));
    tfName         .textProperty().addListener((ob, oldValue, newValue) -> rbNewDiv .setSelected(true));

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

    hcbType.selectID(HDT_Institution.DEPARTMENT_INST_TYPE_ID);

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (rbNewInst.isSelected())
    {
      if (tfNewParentName.getText().isBlank())
        return falseWithErrorMessage("You must enter a name for the new institution.", tfNewParentName);
    }
    else if (hcbParent.selectedRecord() == null)
      return falseWithErrorMessage("Select an institution record.", cbParent);

    if (rbNewDiv.isSelected())
    {
      if (hcbType.selectedID() < 1)
        return falseWithErrorMessage("You must select a type.", cbType);

      if (tfName.getText().isBlank())
        return falseWithErrorMessage("You must enter a division name.", tfName);
    }
    else if (hcbExisting.selectedRecord() == null)
      return falseWithErrorMessage("Select a division.", cbExisting);


    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
