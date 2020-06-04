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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.EnumSet;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;

public class NewCategoryDlgCtrlr extends HyperDlg
{
  public HyperCB hcbRecordType, hcbCompare;
  private RecordTypePopulator typePopulator;

  @FXML private ComboBox<HyperTableCell> cbRecordType, cbCompare;
  @FXML private TextField tfCompareID, tfCompareKey;
  @FXML public TextField tfNewName, tfNewID, tfNewKey;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewCategoryDlgCtrlr build(HDT_RecordType recordType)
  {
    return ((NewCategoryDlgCtrlr) create("NewCategoryDlg", "New Category", true)).init(recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private NewCategoryDlgCtrlr init(HDT_RecordType recordType)
  {
    typePopulator = new RecordTypePopulator(EnumSet.of(hdtField, hdtCountry, hdtRank, hdtPersonStatus));
    hcbRecordType = new HyperCB(cbRecordType, ctDropDownList, typePopulator);

    hcbCompare = new HyperCB(cbCompare, ctDropDownList, new RecordByTypePopulator());

    cbRecordType.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      HDT_RecordType oldType = HyperTableCell.getCellType(oldValue),
                     newType = HyperTableCell.getCellType(newValue);

      if (oldType == newType) return;

      ((RecordByTypePopulator) hcbCompare.getPopulator()).setRecordType(Populator.dummyRow, newType);
      hcbCompare.selectID(-1);

      tfNewID.setText(newType == hdtNone ? "" : String.valueOf(db.getNextID(newType)));
    });

    cbCompare.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      int oldID = HyperTableCell.getCellID(oldValue),
          newID = HyperTableCell.getCellID(newValue);
      HDT_RecordType type = hcbRecordType.selectedType();

      if (oldID == newID) return;

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

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfNewName.getText().isEmpty())
      return falseWithErrorMessage("Record name cannot be blank.", tfNewName);

    if (tfNewKey.getText().isEmpty())
      return falseWithErrorMessage("Sort key cannot be blank.", tfNewKey);

    if (hcbRecordType.selectedType() == hdtNone)
      return falseWithErrorMessage("You must select a record type.", cbRecordType);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
