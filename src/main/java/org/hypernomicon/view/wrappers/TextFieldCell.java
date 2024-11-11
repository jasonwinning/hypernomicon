/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.cellValues.PageRangeHTC;
import org.hypernomicon.view.cellValues.RecordHTC;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod;

import javafx.beans.property.Property;
import javafx.scene.control.TableCell;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

//---------------------------------------------------------------------------

class TextFieldCell extends TableCell<HyperTableRow, HyperTableCell> implements CommitableWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TextField textField;
  private final MutableBoolean canEditIfEmpty;
  private final Property<CellSortMethod> sortMethod;
  private final CellValueType cellValueType;
  private final HyperTable table;

//---------------------------------------------------------------------------

  private String getString() { return nullSwitch(getItem(), "", HyperTableCell::getCellText); }

//---------------------------------------------------------------------------

  TextFieldCell(HyperTable table, CellValueType cellValueType, MutableBoolean canEditIfEmpty, Property<CellSortMethod> sortMethod)
  {
    this.table = table;

    this.canEditIfEmpty = canEditIfEmpty;
    this.sortMethod = sortMethod;
    this.cellValueType = cellValueType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void startEdit()
  {
    if (isEmpty())
      return;

    if (canEditIfEmpty.isFalse() && (table.dataRowCount() <= getTableRow().getIndex()))
      return;

    super.startEdit();
    createTextField();
    setText(null);
    setGraphic(textField);
    safeFocus(textField);
    textField.selectAll();
    table.doExternalRefresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateItem(HyperTableCell item, boolean empty)
  {
    super.updateItem(item, empty);

    if (empty)
    {
      setText(null);
      setGraphic(null);
      return;
    }

    if (isEditing())
    {
      if (textField != null)
        textField.setText(getString());

      setText(null);
      setGraphic(textField);
    }
    else
    {
      setText(getString());
      setGraphic(null);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commitEdit(HyperTableCell newValue)
  {
    super.cancelEdit();
    setGraphic(null);

    getTableRow().getItem().setCellValue(getTableView().getColumns().indexOf(getTableColumn()), newValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createTextField()
  {
    textField = new TextField(getString());
    textField.setMinWidth(getWidth() - getGraphicTextGap() * 2);
    textField.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue) == false)
        commit();
    });

    if (sortMethod.getValue() == smNumeric)
    {
      textField.setTextFormatter(new TextFormatter<>(change ->
      {
        if (change.getText().matches(".*[^0-9].*") && change.isAdded())
          change.setText("");

        return change;
      }));
    }

    textField.addEventHandler(KeyEvent.KEY_PRESSED, event ->
    {
      if (event.getCode() == KeyCode.ESCAPE)
      {
        HyperTableCell item = getItem();
        textField.setText(getCellText(item));
        commitEdit(item);
        event.consume();
      }
      else if (event.getCode() == KeyCode.ENTER)
      {
        commit();
        event.consume();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    if (getGraphic() != textField) return;

    HyperTableCell oldCell = getItem(),
                   newCell = switch (cellValueType)
    {
      case cvtPageRange -> new PageRangeHTC(getRecord(oldCell), textField.getText()                      );
      default           -> new RecordHTC   (getCellID(oldCell), textField.getText(), getCellType(oldCell));
    };

    commitEdit(newCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
