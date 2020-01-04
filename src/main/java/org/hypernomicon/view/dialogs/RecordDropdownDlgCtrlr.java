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

package org.hypernomicon.view.dialogs;

import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.model.HyperDB.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;

public class RecordDropdownDlgCtrlr<HDT_T extends HDT_Record> extends HyperDlg
{
  @FXML private ComboBox<HyperTableCell> cbRecord;

  private HyperCB hcbRecord;
  private String typeName;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_T getRecord() { return hcbRecord.selectedRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <HDT_T2 extends HDT_Record> RecordDropdownDlgCtrlr<HDT_T2> create(HDT_RecordType recordType)
  {
    RecordDropdownDlgCtrlr<HDT_T2> rdd = HyperDlg.create("RecordDropdownDlg.fxml", "Select a Term Record to Merge With", true);
    rdd.init(recordType);
    return rdd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_RecordType recordType)
  {
    hcbRecord = new HyperCB(cbRecord, ctDropDownList, new StandardPopulator(recordType));
    typeName = db.getTypeName(recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbRecord.selectedID() >= 1) return true;

    return falseWithInfoMessage("Select a " + typeName + " record.", cbRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
