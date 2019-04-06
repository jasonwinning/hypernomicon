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
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.function.Function;

import org.apache.commons.lang3.mutable.MutableBoolean;

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TableCell;
import javafx.scene.input.KeyCode;

//---------------------------------------------------------------------------

public class ComboBoxCell extends TableCell<HyperTableRow, HyperTableCell> implements CommitableWrapper
{
  private ComboBox<HyperTableCell> cB;
  private HyperCB hCB;
  private HyperCtrlType ctrlType;
  private final Populator populator;
  private final EventHandler<ActionEvent> onAction;
  private final HyperTable table;
  private final MutableBoolean dontCreateNewRecord;
  private final Function<HyperTableRow, String> textHndlr;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ComboBoxCell(HyperTable table, HyperCtrlType ctrlType, Populator populator, EventHandler<ActionEvent> onAction,
               MutableBoolean dontCreateNewRecord, Function<HyperTableRow, String> textHndlr)
  {
    super();
    this.table = table;
    this.ctrlType = ctrlType;
    this.populator = populator;
    this.onAction = onAction;
    this.dontCreateNewRecord = dontCreateNewRecord;
    this.textHndlr = textHndlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void startEdit()
  {
    if (isEmpty()) return;

    super.startEdit();
    createComboBox();

    hCB.populate(false);

    HyperTableCell cell = getItem();

    cB.setValue(cell);
    if (cell != null)
      cB.getSelectionModel().select(cell);

    setGraphic(cB);
    cB.show();

    runDelayedInFXThread(6, 50, event ->
    {
      cB.requestFocus();
      AutoCompleteCB.scrollToValue(cB);

      cB.getEditor().selectAll();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commitEdit(HyperTableCell newValue)
  {
    super.cancelEdit();
    setGraphic(null);

    HyperTableRow row = getTableRow().getItem();
    int colNdx = getTableView().getColumns().indexOf(getTableColumn());

    if (hCB.somethingWasTyped)
      if (hCB.typedMatch != null)
        newValue = hCB.typedMatch;

    row.setCellValue(colNdx, newValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateItem(HyperTableCell item, boolean empty)
  {
    super.updateItem(item, empty);

    if (empty)
    {
      setText(null);
      setItem(null);
      setGraphic(null);
      return;
    }

    if (isEditing())
    {
      if (hCB != null)
        hCB.populate(false);

      setText(null);
      setGraphic(cB);
    }
    else
    {
      setItem(item);
      setText(getString());
      setGraphic(null);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createComboBox()
  {
    cB = new ComboBox<>();
    cB.setMaxWidth(Double.MAX_VALUE);
    cB.setPrefWidth(getWidth() - getGraphicTextGap() * 2);
    cB.setMinHeight(18.0 * displayScale);
    cB.setPrefHeight(18.0 * displayScale);
    cB.setMaxHeight(18.0 * displayScale);

    HyperTableRow row = getTableRow().getItem();

    if (populator.getValueType() == cvtVaries)
    {
      VariablePopulator vp = (VariablePopulator)populator;
      ctrlType = vp.getRestricted(row) ? ctDropDownList : ctDropDown;
    }

    hCB = new HyperCB(cB, ctrlType, populator, row, false, table);

    hCB.dontCreateNewRecord = dontCreateNewRecord.booleanValue();

    hCB.setOnAction(onAction);

    cB.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (!cB.isFocused())
        commit();
    });

    cB.setOnKeyPressed(event ->
    {
      if (event.getCode() == KeyCode.ESCAPE)
      {
        HyperTableCell item = getItem();
        hCB.selectID(item.getID());
        commitEdit(item);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getString()
  {
    if (textHndlr != null)
      return textHndlr.apply(getTableView().getItems().get(getTableRow().getIndex()));

    return HyperTableCell.getCellText(getItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    if (getGraphic() == cB)
      commitEdit(hCB.selectedHTC());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
