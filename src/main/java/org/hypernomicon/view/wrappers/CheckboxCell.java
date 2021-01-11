/*
 * Copyright 2015-2021 Jason Winning
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

import javafx.scene.control.CheckBox;
import javafx.scene.control.TableCell;

import static org.hypernomicon.util.Util.*;

class CheckboxCell extends TableCell<HyperTableRow, Boolean>
{
  final private HyperTable table;
  final private CheckBox chk;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  CheckboxCell(HyperTable table)
  {
    super();

    this.table = table;
    chk = new CheckBox();

    emptyProperty().addListener((ob, oldValue, newValue) -> chk.setVisible(newValue.booleanValue() == false));

    chk.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      HyperTableRow row = getTableRow().getItem();
      if (row == null) return;

      HyperTableCell cell = Boolean.TRUE.equals(newValue) ? HyperTableCell.trueCheckboxCell : HyperTableCell.falseCheckboxCell;

      row.setCellValue(getTableView().getColumns().indexOf(getTableColumn()), cell);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateItem(Boolean val, boolean empty)
  {
    super.updateItem(val, empty);

    if (empty)
    {
      setGraphic(null);
      return;
    }

    setGraphic(chk);
    chk.setSelected(val);

    chk.setDisable(HyperTableCell.isEmpty((HyperTableCell)nullSwitch(getTableRow(), null, tableRow ->
                                          nullSwitch(tableRow.getItem(), null, row ->
                                          row.getCell(table.getMainColNdx())))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
