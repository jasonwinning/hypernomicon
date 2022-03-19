/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.function.Consumer;

import org.hypernomicon.model.records.HDT_Record;

import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.input.MouseButton;

public class ReadOnlyCell extends TableCell<HyperTableRow, HyperTableCell>
{
  private final boolean incremental;
  private final HyperTable table;
  private final CustomAddNewGraphicProvider graphicProvider;
  public static final int INCREMENTAL_ROWS = 20;

  @FunctionalInterface public interface CustomAddNewGraphicProvider { Node provide(HyperTableRow row); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ReadOnlyCell(HyperTable table, boolean incremental, CustomAddNewGraphicProvider graphicProvider)
  {
    super();

    this.incremental = incremental;
    this.table = table;
    this.graphicProvider = graphicProvider;

    setOnMouseClicked(mouseEvent -> nullSwitch(getItem(), cellItem -> nullSwitch(cellItem.getRecord(), (HDT_Record record) ->
    {
      if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
        handleRecord(table.dblClickHandler, record);
    })));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  static <HDT_T extends HDT_Record> void handleRecord(Consumer<HDT_T> handler, HDT_Record record)
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

    if (incremental && (HyperTableCell.getCellType(cell) == hdtAuxiliary))
    {
      setText("");
      setTooltip(null);
      Button cellButton = HyperTableColumn.makeButton(this);
      cellButton.setText("Show more");
      cellButton.setOnAction(event ->
      {
        if (table.onShowMore != null)
        {
          table.getTV().requestFocus();

          table.onShowMore.run();
        }
      });

      setGraphic(cellButton);
      table.showMoreRow = row;
      return;
    }

    if ((cell.getRecord() == null) && (graphicProvider != null))
    {
      setText("");
      setTooltip(null);
      setGraphic(graphicProvider.provide(row));
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
