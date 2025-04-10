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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

//---------------------------------------------------------------------------

public class ChangeIDDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperCB hcbType;
  public final HyperCB hcbRecord;

  @FXML private ComboBox<HyperTableCell> cbType, cbRecord;
  @FXML public TextField tfOldID, tfNewID;
  @FXML private Button btnNextID;
  @FXML private Label lblNotAvailable;

//---------------------------------------------------------------------------

  public ChangeIDDlgCtrlr()
  {
    super("ChangeIDDlg", "Change Record ID", true);

    hcbType   = new HyperCB(cbType  , ctEditableLimitedDropDown, new RecordTypePopulator(false));
    hcbRecord = new HyperCB(cbRecord, ctEditableLimitedDropDown, new RecordByTypePopulator());

    hcbType.addListener((oldValue, newValue) ->
    {
      RecordType oldType = HyperTableCell.getCellType(oldValue),
                 newType = HyperTableCell.getCellType(newValue);

      if (oldType == newType) return;

      ((RecordByTypePopulator) hcbRecord.getPopulator()).setRecordType(newType);
      hcbRecord.selectID(-1);
    });

    hcbRecord.addListener((oldValue, newValue) ->
    {
      int oldID = HyperTableCell.getCellID(oldValue),
          newID = HyperTableCell.getCellID(newValue);

      if (oldID == newID) return;

      lblNotAvailable.setVisible(false);
      tfNewID.clear();

      if ((newID < 1) || (hcbType.selectedType() == hdtNone))
        tfOldID.clear();
      else
        tfOldID.setText(String.valueOf(newID));
    });

    btnNextID.setOnAction(event ->
    {
      if (hcbRecord.selectedID() > 0)
        tfNewID.setText(String.valueOf(db.getNextID(hcbRecord.selectedType())));
    });

    tfNewID.textProperty().addListener((ob, oldValue, newValue) ->
    {
      int id = parseInt(newValue, -1);

      if ((id > 0) && (db.idAvailable(hcbRecord.selectedType(), id) == false))
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
    HDT_Record record = hcbRecord.selectedRecord();

    if (record == null)
      return falseWithErrorPopup("You must select a record.", cbRecord);

    if (HDT_Record.isEmpty(record, false))
      return falseWithErrorPopup("Unable to change record ID.", cbRecord);

    if ((parseInt(tfNewID.getText(), -1) < 1) || lblNotAvailable.isVisible())
      return falseWithErrorPopup("You must enter a valid numeric ID.", tfNewID);

    try
    {
      db.changeRecordID(record, parseInt(tfNewID.getText(), -1));
    }
    catch (HyperDataException e)
    {
      return falseWithErrorPopup("Unable to change record ID: " + getThrowableMessage(e));
    }

    infoPopup("The record ID was changed successfully.");
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
