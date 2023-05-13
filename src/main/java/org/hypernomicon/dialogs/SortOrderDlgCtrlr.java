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

package org.hypernomicon.dialogs;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import java.util.HashSet;
import java.util.Set;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.TableView;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import org.hypernomicon.model.HyperDB;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class SortOrderDlgCtrlr extends HyperDlg
{
  @FXML private TableView<HyperTableRow> tvRecords;

  private final HyperTable htRecords;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public SortOrderDlgCtrlr(RecordType recordType)
  {
    super("SortOrderDlg", "Change " + typeName(recordType) + " Listing Order", true);

    htRecords = new HyperTable(tvRecords, 1, false, "");

    htRecords.addLabelCol(hdtNone, smNumeric);
    htRecords.addLabelCol(recordType, smTextSimple);
    htRecords.addTextEditCol(recordType, false, smTextSimple);

    @SuppressWarnings("unchecked")
    Iterable<HDT_Record> iterable = (Iterable<HDT_Record>) db.records(recordType);

    htRecords.buildRows(iterable, (row, record) ->
    {
      row.setCellValue(0, record, String.valueOf(record.getID()));
      row.setCellValue(1, record, record.listName());
      row.setCellValue(2, record, record.getSortKeyAttr());
    });

    Platform.runLater(() -> tvRecords.getColumns().get(0).setResizable(false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    Set<String> keys = new HashSet<>();

    for (HyperTableRow row : htRecords.dataRows())
    {
      String sortKey = row.getText(2);

      if (sortKey.isBlank())
        return falseWithErrorMessage("One or more of the sort keys is blank.");

      if (keys.add(sortKey) == false)
        return falseWithErrorMessage("Multiple records cannot have the same sort key.");
    }

    for (HyperTableRow row : htRecords.dataRows())
      row.getRecord().setSortKeyAttr(row.getText(2));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String typeName(RecordType recordType)
  {
    switch (recordType)
    {
      case hdtPersonStatus    : return "Person Status";
      case hdtField           : return "Academic Field";
      case hdtRank            : return "Academic Rank";
      case hdtWorkType        : return "Work Type";
      case hdtArgumentVerdict : return "Argument Verdict";
      case hdtPositionVerdict : return "Position Verdict";

      default                 : return HyperDB.getTypeName(recordType);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
