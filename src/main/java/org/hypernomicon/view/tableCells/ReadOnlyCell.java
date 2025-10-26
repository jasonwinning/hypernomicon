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

package org.hypernomicon.view.tableCells;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.function.Consumer;
import java.util.function.Function;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.*;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.input.MouseButton;

//---------------------------------------------------------------------------

public class ReadOnlyCell extends TableCell<HyperTableRow, HyperTableCell>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperTable table;
  private final HyperTableColumn col;
  private final Function<HyperTableRow, Node> graphicProvider;

  public static final int INCREMENTAL_ROWS = 20;

//---------------------------------------------------------------------------

  public ReadOnlyCell(HyperTable table, HyperTableColumn col, Function<HyperTableRow, Node> graphicProvider)
  {
    this.table = table;
    this.col = col;
    this.graphicProvider = graphicProvider;

    setEditable(false);

    if (col.getAlignment() != null)
      setAlignment(col.getAlignment());

    setOnMouseClicked(mouseEvent -> nullSwitch(getItem(), cellItem -> nullSwitch(cellItem.getRecord(), (HDT_Record record) ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
        handleRecord(table.getDblClickHandler(), record);
    })));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <HDT_T extends HDT_Record> void handleRecord(Consumer<HDT_T> handler, HDT_Record record)
  {
    if (handler != null)
      handler.accept((HDT_T) record);
    else
      ui.goToRecord(record, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateItem(HyperTableCell cell, boolean empty)
  {
    super.updateItem(cell, empty);

    HyperTableRow row;

    if (empty || ((row = getTableRow().getItem()) == null))
    {
      setText("");
      setGraphic(null);
      setTooltip(null);
      return;
    }

    if ((col.getCtrlType() == HyperCtrlType.ctIncremental) && (HyperTableCell.getCellType(cell) == hdtAuxiliary))
    {
      setText("");
      setTooltip(null);
      Button cellButton = HyperTableColumn.makeButton(this);
      cellButton.setText("Show more");
      cellButton.setOnAction(event ->
      {
        if (table.getOnShowMore() != null)
        {
          table.getTV().requestFocus();

          table.getOnShowMore().run();
        }
      });

      setGraphic(cellButton);
      table.setShowMoreRow(row);
      return;
    }

    if ((cell.getRecord() == null) && (graphicProvider != null))
    {
      setText("");
      setTooltip(null);
      setGraphic(graphicProvider.apply(row));
      return;
    }

    String text = HyperTableCell.getCellText(cell);

    setText(text);
    setToolTip(this, text);
    setGraphic(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
