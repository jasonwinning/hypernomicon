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

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.function.Predicate;

import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;

//---------------------------------------------------------------------------

public class RecordDropdownDlgCtrlr<HDT_T extends HDT_Record> extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private ComboBox<HyperTableCell> cbRecord;

  private final HyperCB hcbRecord;
  private final String typeName;

//---------------------------------------------------------------------------

  public HDT_T getRecord() { return hcbRecord.selectedRecord(); }

//---------------------------------------------------------------------------

  public RecordDropdownDlgCtrlr(RecordType recordType, Predicate<Integer> idFilter)
  {
    super("RecordDropdownDlg", "Select a Term Record to Merge With", true);

    hcbRecord = new HyperCB(cbRecord, ctEditableLimitedDropDown, new StandardPopulator(recordType, idFilter));
    typeName = getTypeName(recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return (hcbRecord.selectedID() >= 1) || falseWithInfoPopup("Select a " + typeName + " record.", cbRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
