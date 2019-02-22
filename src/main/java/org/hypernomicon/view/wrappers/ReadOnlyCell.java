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

package org.hypernomicon.view.wrappers;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.view.wrappers.RecordListView.RecordHandler;

import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.control.Tooltip;
import javafx.scene.input.MouseButton;

public class ReadOnlyCell extends TableCell<HyperTableRow, HyperTableCell>
{
  private final boolean incremental;
  private final HyperTable table;
  private final HyperTableColumn col;
  public static final int INCREMENTAL_ROWS = 20;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ReadOnlyCell(HyperTable table, HyperTableColumn col, boolean incremental)
  {
    super();

    this.incremental = incremental;
    this.table = table;
    this.col = col;

    setOnMouseClicked(mouseEvent ->
    {
      if ((mouseEvent.getButton().equals(MouseButton.PRIMARY)) && (mouseEvent.getClickCount() == 2))
      {
        HyperTableCell cellItem = getItem();
        if (cellItem == null) return;

        HDT_Base record = HyperTableCell.getRecord(cellItem);
        if (record == null) return;

        if (table.dblClickHandler != null)
          handleRecord(table.dblClickHandler, record);
        else
          ui.goToRecord(record, true);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private static <HDT_T extends HDT_Base> void handleRecord(RecordHandler<HDT_T> handler, HDT_Base record)
  {
    handler.handle((HDT_T) record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateItem(HyperTableCell cell, boolean empty)
  {
    super.updateItem(cell, empty);

    if (empty)
    {
      setText(null);
      setGraphic(null);
      setTooltip(null);
    }
    else
    {
      if ((incremental) && (col.wasMoreButtonClicked() == false))
      {
        HyperTableRow row = (HyperTableRow) getTableRow().getItem();

        if (row == null)
        {
          setText("");
          setGraphic(null);
          setTooltip(null);
          return;
        }

        if (HyperTableCell.getCellType(cell) == hdtAuxiliary)
        {
          setText("");
          setTooltip(null);
          Button cellButton = HyperTableColumn.makeButton(this);
          cellButton.setText("Show more");
          cellButton.setOnAction(event ->
          {
            if (table.onShowMore != null)
              table.onShowMore.run();
          });

          setGraphic(cellButton);
          table.showMoreRow = row;
          return;
        }
      }

      String text = HyperTableCell.getCellText(cell);

      setText(text);
      setTooltip(text.length() == 0 ? null : new Tooltip(text));
      setGraphic(null);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
