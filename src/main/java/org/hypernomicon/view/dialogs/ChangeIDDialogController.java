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
import static org.hypernomicon.model.records.HDT_RecordType.hdtNone;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.ctDropDownList;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class ChangeIDDialogController extends HyperDialog
{
  public HyperCB hcbType, hcbRecord;

  @FXML private ComboBox<HyperTableCell> cbType, cbRecord;
  @FXML public TextField tfOldID, tfNewID;
  @FXML private Button btnNextID;
  @FXML private Label lblNotAvailable;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ChangeIDDialogController create(String title)
  {
    ChangeIDDialogController cid = HyperDialog.create("ChangeIDDialog.fxml", title, true);
    cid.init();
    return cid;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    hcbType = new HyperCB(cbType, ctDropDownList, new RecordTypePopulator(), null, false);
    hcbRecord = new HyperCB(cbRecord, ctDropDownList, new RecordByTypePopulator(), null);

    cbType.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      HDT_RecordType oldType = HyperTableCell.getCellType(oldValue),
                     newType = HyperTableCell.getCellType(newValue);

      if (oldType == newType) return;

      ((RecordByTypePopulator) hcbRecord.getPopulator()).setRecordType(Populator.dummyRow, newType);
      hcbRecord.selectID(-1);
    });

    cbRecord.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      int oldID = HyperTableCell.getCellID(oldValue),
          newID = HyperTableCell.getCellID(newValue);
      HDT_RecordType type = hcbType.selectedType();

      if (oldID == newID) return;

      lblNotAvailable.setVisible(false);
      tfNewID.clear();

      if ((newID < 1) || (type == hdtNone))
      {
        tfOldID.clear();
        return;
      }

      tfOldID.setText("" + newID);
    });

    btnNextID.setOnAction(event ->
    {
      if (hcbRecord.selectedID() > 0)
        tfNewID.setText("" + db.getNextID(hcbRecord.selectedType()));
    });

    tfNewID.textProperty().addListener((observable, oldValue, newValue) ->
    {
      int id = parseInt(newValue, -1);
      HDT_RecordType type = hcbRecord.selectedType();

      if (id > 0)
        if (db.idAvailable(type, id) == false)
        {
          lblNotAvailable.setVisible(true);
          return;
        }

      lblNotAvailable.setVisible(false);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbRecord.selectedID() < 1)
    {
      messageDialog("You must select a record.", mtError);
      safeFocus(cbRecord);
      return false;
    }

    if ((parseInt(tfNewID.getText(), -1) < 1) || (lblNotAvailable.isVisible()))
    {
      messageDialog("You must enter a valid numeric ID.", mtError);
      safeFocus(tfNewID);
      return false;
    }

    HDT_Base record = db.records(hcbRecord.selectedType()).getByID(parseInt(tfOldID.getText(), -1));

    if ((record == null) || (record.changeID(parseInt(tfNewID.getText(), -1)) == false))
      return falseWithErrorMessage("Unable to change record ID.");

    messageDialog("The record ID was changed successfully.", mtInformation);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
