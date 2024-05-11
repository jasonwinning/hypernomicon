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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.populators.HybridSubjectPopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperCB;
import org.hypernomicon.view.wrappers.HyperTableCell;

import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;

//---------------------------------------------------------------------------

public class SearchKeySelectDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private ComboBox<HyperTableCell> cbType, cbAuthor, cbRecord;
  @FXML private ComboBox<String> cbKey;

//---------------------------------------------------------------------------

  public SearchKeySelectDlgCtrlr()
  {
    super("SearchKeySelectDlg", "Select Search Key", true);

    VariablePopulator recordPop = new VariablePopulator();

    HyperCB hcbType   = new HyperCB(cbType  , ctDropDownList, new RecordTypePopulator(hdtPerson, hdtInvestigation, hdtWork, hdtMiscFile, hdtDebate, hdtPosition, hdtArgument, hdtNote, hdtTerm)),
            hcbAuthor = new HyperCB(cbAuthor, ctDropDownList, new StandardPopulator  (hdtPerson)),
            hcbRecord = new HyperCB(cbRecord, ctDropDownList, recordPop);

//---------------------------------------------------------------------------

    hcbType.addListener((oldValue, newValue) ->
    {
      if (newValue == null) return;

      RecordType oldType = HyperTableCell.getCellType(oldValue),
                 newType = HyperTableCell.getCellType(newValue);

      if (oldType == newType) return;

      cbAuthor.setDisable((newType != hdtWork) && (newType != hdtMiscFile));

      if (newType == hdtWork)
      {
        HybridSubjectPopulator workPop = new HybridSubjectPopulator(rtAuthorOfWork);
        recordPop.setPopulator(workPop);
        if (hcbAuthor.selectedRecord() != null)
          workPop.setObj(hcbAuthor.selectedRecord());
      }
      else if (newType == hdtMiscFile)
      {
        HybridSubjectPopulator filePop = new HybridSubjectPopulator(rtAuthorOfFile, id -> db.miscFiles.getByID(id).getSearchKey().isBlank() == false);
        recordPop.setPopulator(filePop);
        if (hcbAuthor.selectedRecord() != null)
          filePop.setObj(hcbAuthor.selectedRecord());
      }
      else if ((newType == null) || (newType == hdtNone))
      {
        recordPop.setPopulator(new StandardPopulator(newType, id -> false));
      }
      else
      {
        recordPop.setPopulator(new StandardPopulator(newType, id -> db.records(newType).getByID(id).getSearchKey().isBlank() == false));
      }

      if (oldType != hdtNone)
        hcbRecord.selectID(-1);
    });

//---------------------------------------------------------------------------

    hcbAuthor.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      ((HybridSubjectPopulator) ((VariablePopulator)hcbRecord.getPopulator()).getPopulator()).setObj(HyperTableCell.getRecord(newCell));
      hcbRecord.selectID(-1);
    });

//---------------------------------------------------------------------------

    hcbRecord.addListener((oldCell, newCell) ->
    {
      if (HyperTableCell.getCellID(oldCell) == HyperTableCell.getCellID(newCell)) return;

      cbKey.getItems().clear();

      HDT_Record record = HyperTableCell.getRecord(newCell);
      if (record == null) return;

      record.getSearchKeys().forEach(keyword -> cbKey.getItems().add(keyword.text));

      if (cbKey.getItems().size() == 1)
        cbKey.getSelectionModel().clearAndSelect(0);
    });

//---------------------------------------------------------------------------

    hcbType.selectType(hdtWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getKeyword()
  {
    return cbKey.getSelectionModel().getSelectedItem();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return (safeStr(getKeyword()).isBlank() == false) || falseWithErrorPopup("No search key is selected.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
