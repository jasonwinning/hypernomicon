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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;

public class NewCategoryDlgCtrlr extends HyperDlg
{
  public final HyperCB hcbRecordType;
  private final HyperCB hcbCompare;

  @FXML private ComboBox<HyperTableCell> cbRecordType, cbCompare;
  @FXML private TextField tfCompareID, tfCompareKey;
  @FXML public TextField tfNewName, tfNewID, tfNewKey;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public NewCategoryDlgCtrlr(RecordType recordType, boolean canChangeType)
  {
    super("NewCategoryDlg", "New Category", true);

    hcbRecordType = new HyperCB(cbRecordType, ctDropDownList, new RecordTypePopulator(hdtField, hdtCountry, hdtRank, hdtPersonStatus));

    hcbCompare = new HyperCB(cbCompare, ctDropDownList, new RecordByTypePopulator());

    hcbRecordType.addListener((oldValue, newValue) ->
    {
      RecordType newType = HyperTableCell.getCellType(newValue);

      if (HyperTableCell.getCellType(oldValue) == newType) return;

      ((RecordByTypePopulator) hcbCompare.getPopulator()).setRecordType(newType);
      hcbCompare.selectID(-1);

      tfNewID.setText(newType == hdtNone ? "" : String.valueOf(db.getNextID(newType)));
    });

    hcbCompare.addListener((oldValue, newValue) ->
    {
      int newID = HyperTableCell.getCellID(newValue);
      RecordType type = hcbRecordType.selectedType();

      if (HyperTableCell.getCellID(oldValue) == newID) return;

      if ((newID < 1) || (type == hdtNone))
      {
        tfCompareID.clear();
        tfCompareKey.clear();
        return;
      }

      HDT_Record record = db.records(hcbRecordType.selectedType()).getByID(newID);
      tfCompareID.setText(String.valueOf(newID));
      tfCompareKey.setText(record.getSortKeyAttr());
    });

    hcbRecordType.selectType(recordType);

    cbRecordType.setDisable(canChangeType == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfNewName.getText().isBlank())
      return falseWithErrorPopup("Record name cannot be blank.", tfNewName);

    if (tfNewKey.getText().isBlank())
      return falseWithErrorPopup("Sort key cannot be blank.", tfNewKey);

    if (hcbRecordType.selectedType() == hdtNone)
      return falseWithErrorPopup("You must select a record type.", cbRecordType);

    for (HDT_Record record : db.records(hcbRecordType.selectedType()))
      if (record.getSortKeyAttr().equals(tfNewKey.getText()))
        return falseWithErrorPopup("Another record already has that sort key.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
