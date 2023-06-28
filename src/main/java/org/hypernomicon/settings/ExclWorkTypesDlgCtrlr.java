/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.settings;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;

import java.util.Set;
import java.util.stream.Stream;

import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.fxml.FXML;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public class ExclWorkTypesDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TableView<HyperTableRow> tv;

  private final HyperTable hyperTable;

//---------------------------------------------------------------------------

  public ExclWorkTypesDlgCtrlr(Set<HDT_WorkType> set, String componentType)
  {
    super("settings/ExclWorkTypesDlg", "Excluded Work Types" + (componentType.isBlank() ? "" : (" - " + componentType)), true, true);

    hyperTable = new HyperTable(tv, 1, false, "");

    hyperTable.addCheckboxCol();
    hyperTable.addLabelCol(hdtWorkType);

    hyperTable.buildRows(db.workTypes.stream().filter(workType -> workType.enumVal() != WorkTypeEnum.wtUnenteredSet), (row, workType) ->
    {
      row.setCheckboxValue(0, set.contains(workType));
      row.setCellValue(1, workType, workType.name());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Stream<HDT_WorkType> exclTypes()
  {
    return hyperTable.dataRowStream().filter(row -> row.getCheckboxValue(0)).map(HyperTableRow::getRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
