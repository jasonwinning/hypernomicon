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

package org.hypernomicon.view.wrappers;

import javafx.geometry.Pos;
import javafx.scene.control.CheckBox;
import javafx.scene.control.TableCell;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;

import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;

class CheckboxCell extends TableCell<HyperTableRow, HyperTableCell>
{
  private final HyperTable table;
  private final CheckBox chk;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  CheckboxCell(HyperTable table)
  {
    this.table = table;
    chk = new CheckBox();

    emptyProperty().addListener((ob, oldValue, newValue) -> chk.setVisible(newValue == false));

    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      HyperTableRow row = getTableRow().getItem();
      if (row == null) return;

      row.setCellValue(getTableView().getColumns().indexOf(getTableColumn()), GenericNonRecordHTC.fromBoolean(Boolean.TRUE.equals(newValue)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateItem(HyperTableCell val, boolean empty)
  {
    super.updateItem(val, empty);

    if (empty)
    {
      setGraphic(null);
      return;
    }

    setGraphic(chk);
    setAlignment(Pos.CENTER);
    chk.setSelected(getCellID(val) == TRUE_ID);

    chk.setDisable(GenericNonRecordHTC.isEmpty(nullSwitch(getTableRow(), null, tableRow ->
                                               nullSwitch(tableRow.getItem(), null, row ->
                                               row.getCell(table.getMainColNdx())))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
