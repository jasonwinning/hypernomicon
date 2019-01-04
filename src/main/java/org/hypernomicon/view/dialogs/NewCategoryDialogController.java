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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.EnumSet;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;

public class NewCategoryDialogController extends HyperDialog
{
  public HyperCB hcbRecordType, hcbCompare;
  private RecordTypePopulator typePopulator;

  @FXML private ComboBox<HyperTableCell> cbRecordType;
  @FXML private ComboBox<HyperTableCell> cbCompare;
  @FXML private TextField tfCompareID;
  @FXML private TextField tfCompareKey;
  @FXML public TextField tfNewName;
  @FXML public TextField tfNewID;
  @FXML public TextField tfNewKey;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static NewCategoryDialogController create(String title, HDT_RecordType recordType)
  {
    NewCategoryDialogController ncd = HyperDialog.create("NewCategoryDialog.fxml", title, true);
    ncd.init(recordType);
    return ncd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_RecordType recordType)
  {
    EnumSet<HDT_RecordType> typeSet = EnumSet.noneOf(HDT_RecordType.class);

    typePopulator = new RecordTypePopulator();
    hcbRecordType = new HyperCB(cbRecordType, ctDropDownList, typePopulator, null, false);

    hcbCompare = new HyperCB(cbCompare, ctDropDownList, new RecordByTypePopulator(), null);

    typeSet.add(hdtField);
    typeSet.add(hdtCountry);
    typeSet.add(hdtRank);
    typeSet.add(hdtPersonStatus);

    typePopulator.setTypes(typeSet);

    cbRecordType.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      HDT_RecordType oldType = HyperTableCell.getCellType(oldValue),
                     newType = HyperTableCell.getCellType(newValue);

      if (oldType == newType) return;

      ((RecordByTypePopulator) hcbCompare.getPopulator()).setRecordType(Populator.dummyRow, newType);
      hcbCompare.selectID(-1);

      tfNewID.setText("" + (newType == hdtNone ? "" : db.getNextID(newType)));
    });

    cbCompare.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
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

      HDT_Base record = db.records(hcbRecordType.selectedType()).getByID(newID);
      tfCompareID.setText("" + newID);
      tfCompareKey.setText(record.getSortKeyAttr());
    });

    hcbRecordType.selectType(recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (tfNewName.getText().length() == 0)
    {
      messageDialog("Record name cannot be blank.", mtError);
      safeFocus(tfNewName);
      return false;
    }

    if (tfNewKey.getText().length() == 0)
    {
      messageDialog("Sort key cannot be blank.", mtError);
      safeFocus(tfNewKey);
      return false;
    }

    if (hcbRecordType.selectedType() == hdtNone)
    {
      messageDialog("You must select a record type.", mtError);
      safeFocus(cbRecordType);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
