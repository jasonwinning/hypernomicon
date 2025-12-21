/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.view.tableCells;

import javafx.geometry.Pos;
import javafx.scene.control.*;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;

import java.util.function.Function;

import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class CheckboxCell extends TableCell<HyperTableRow, HyperTableCell>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperTable table;
  private final CheckBox chk;
  private final Function<HyperTableRow, Tooltip> toolTipHndlr;

//---------------------------------------------------------------------------

  public CheckboxCell(HyperTable table, Function<HyperTableRow, Tooltip> toolTipHndlr)
  {
    this.table = table;
    this.toolTipHndlr = toolTipHndlr;

    chk = new CheckBox();

    emptyProperty().addListener((ob, oldValue, newValue) -> chk.setVisible(newValue == false));

    chk.setOnAction(event ->
    {
      HyperTableRow row = getTableRow().getItem();
      if (row == null) return;

      row.setCellValue(getTableView().getColumns().indexOf(getTableColumn()), GenericNonRecordHTC.fromBoolean(chk.isSelected()));
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
      setTooltip(null);
      return;
    }

    setGraphic(chk);
    chk.setTooltip(toolTipHndlr == null ? null : toolTipHndlr.apply(getTableRow().getItem()));

    setAlignment(Pos.CENTER);
    chk.setSelected(getCellID(val) == GenericNonRecordHTC.TRUE_ID);

    chk.setDisable(HyperTableCell.isEmpty(nullSwitch(getTableRow(), null, tableRow ->
                                          nullSwitch(tableRow.getItem(), null, row ->
                                          row.getCell(table.getMainColNdx())))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
