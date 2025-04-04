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

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.function.Function;
import java.util.function.Supplier;

import org.apache.commons.lang3.mutable.MutableBoolean;

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;

import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TableCell;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

//---------------------------------------------------------------------------

public class ComboBoxCell extends TableCell<HyperTableRow, HyperTableCell> implements CommitableWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ComboBox<HyperTableCell> cb;
  private HyperCB hcb;
  private HyperCtrlType ctrlType;
  private final Populator populator;
  private final EventHandler<ActionEvent> onAction;
  private final HyperTable table;
  private final MutableBoolean dontCreateNewRecord;
  private final Supplier<HDT_Work> workSupplier;
  private final Function<HyperTableRow, String> textHndlr;

//---------------------------------------------------------------------------

  ComboBoxCell(HyperTable table, HyperCtrlType ctrlType, Populator populator, EventHandler<ActionEvent> onAction,
               MutableBoolean dontCreateNewRecord, Function<HyperTableRow, String> textHndlr, Supplier<HDT_Work> workSupplier)
  {
    this.table = table;
    this.ctrlType = ctrlType;
    this.populator = populator;
    this.onAction = onAction;
    this.dontCreateNewRecord = dontCreateNewRecord;
    this.workSupplier = workSupplier;
    this.textHndlr = textHndlr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void startEdit()
  {
    if (isEmpty()) return;

    super.startEdit();

    createComboBox(table.getTV().getColumns().indexOf(this.getTableColumn()));

    hcb.populate(false);

    HyperTableCell cell = getItem();

    cb.setValue(cell);
    setGraphic(cb);

    if (cell != null)
      cb.getSelectionModel().select(cell);

    cb.show();

    runDelayedInFXThread(6, 50, () ->
    {
      cb.requestFocus();  // If this is not called explicitly, the ComboBox will not "officially" ever receive focus even though it is
                          // outlined in blue and the user can type in it. And it will not receive an event when it loses focus (see createComboBox),
                          // which means the value doesn't get committed to the table when the user clicks off of it.

      if (cb.isEditable() == false)
        return;

      AutoCompleteCBHelper.scrollToValue(cb);

      cb.getEditor().selectAll();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commitEdit(HyperTableCell newValue)
  {
    super.cancelEdit();
    setGraphic(null);

    HyperTableRow row = getTableRow().getItem();
    if (row == null) return;

    if (hcb.getTypedMatch() != null)
      newValue = hcb.getTypedMatch();

    row.setCellValue(getTableView().getColumns().indexOf(getTableColumn()), newValue);
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
      if (hcb != null)
        hcb.populate(false);

      setText(null);
      setGraphic(cb);
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

  private void createComboBox(int colNdx)
  {
    cb = new ComboBox<>();
    cb.setMaxWidth(Double.MAX_VALUE);
    cb.setPrefWidth(getWidth() - getGraphicTextGap() * 2);

    setHeights(cb, scalePropertyValueForDPI(18));

    HyperTableRow row = getTableRow().getItem();

    if (populator.getValueType() == cvtVaries)
      ctrlType = ((VariablePopulator)populator).getRestricted(row) ? ctEditableLimitedDropDown : ctEditableUnlimitedDropDown;

    hcb = new HyperCB(cb, ctrlType, populator, row, false, table, colNdx);

    hcb.dontCreateNewRecord = dontCreateNewRecord.booleanValue();
    hcb.workSupplier = workSupplier;

    hcb.setOnAction(onAction);

    cb.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (cb.isFocused() == false)  // See ComboBoxCell.startEdit; cb.requestFocus() has to be called
        commit();                   // explicitly for this to work.
    });

    cb.addEventHandler(KeyEvent.KEY_PRESSED, event ->
    {
      if (event.getCode() == KeyCode.ESCAPE)
      {
        HyperTableCell item = getItem();
        hcb.select(item);
        commitEdit(item);
        event.consume();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getString()
  {
    return textHndlr != null ?
      textHndlr.apply(getTableView().getItems().get(getTableRow().getIndex()))
    :
      HyperTableCell.getCellText(getItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    if (getGraphic() == cb)
    {
      commitEdit(hcb.selectedHTC());
      cb.commitValue();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
