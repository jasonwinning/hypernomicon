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

import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ComboBox;

public class SelectWorkDlgCtrlr extends HyperDlg
{
  @FXML private ComboBox<HyperTableCell> cbAuthor, cbWork;
  @FXML private Button btnOK, btnCancel;

  private HyperCB hcbAuthor, hcbWork;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Work getWork() { return hcbWork.selectedRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SelectWorkDlgCtrlr create(String title, HDT_Person author)
  {
    SelectWorkDlgCtrlr swd = HyperDlg.create("SelectWorkDlg.fxml", title, true);
    swd.init(author);
    return swd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Person author)
  {
    hcbAuthor = new HyperCB(cbAuthor, ctDropDownList, new StandardPopulator(hdtPerson), null, false);
    hcbWork = new HyperCB(cbWork, ctDropDownList, new HybridSubjectPopulator(rtAuthorOfWork), null, false);

    cbAuthor.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;

      if (HyperTableCell.getCellID(oldValue) != HyperTableCell.getCellID(newValue))
      {
        ((HybridSubjectPopulator)hcbWork.getPopulator()).setObj(Populator.dummyRow, getRecord(newValue));
        hcbWork.selectID(-1);
      }
    });

    hcbAuthor.addAndSelectEntryOrBlank(author, HDT_Record::getCBText);

    hcbWork.addBlankEntry();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    if (hcbWork.selectedID() < 1)
    {
      messageDialog("Select a work record.", mtInformation);
      safeFocus(cbWork);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
