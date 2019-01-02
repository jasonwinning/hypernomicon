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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import java.util.function.UnaryOperator;

import org.apache.commons.lang3.mutable.MutableBoolean;

import javafx.scene.control.TableCell;
import javafx.scene.control.TextField;
import javafx.scene.control.TextFormatter;
import javafx.scene.input.KeyCode;
import org.hypernomicon.view.wrappers.HyperTableCell;

public class TextFieldCell extends TableCell<HyperTableRow, HyperTableCell> implements CommitableWrapper
{
  private TextField textField;
  private MutableBoolean canEditIfEmpty, isNumeric;
  private HyperTable table;

//---------------------------------------------------------------------------
  
  private String getString() { return getItem() == null ? "" : getItem().getText(); }

//---------------------------------------------------------------------------
  
  public TextFieldCell(HyperTable table, MutableBoolean canEditIfEmpty, MutableBoolean isNumeric)
  {
    this.table = table;

    this.canEditIfEmpty = canEditIfEmpty;
    this.isNumeric = isNumeric;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override public void startEdit()
  {
    if (!isEmpty())
    {
      if (canEditIfEmpty.isFalse())
        if (table.getDataRowCount() <= getTableRow().getIndex()) return;
      
      super.startEdit();
      createTextField();
      setText(null);
      setGraphic(textField);
      safeFocus(textField);
      textField.selectAll();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override public void cancelEdit()
  {
    super.cancelEdit();

    setText(getItem().getText());
    setGraphic(null);
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
    }
    else
    {
      if (isEditing())
      {
        if (textField != null)
        {
          textField.setText(getString());
        }
        setText(null);
        setGraphic(textField);
      }
      else
      {
        setText(getString());
        setGraphic(null);
      }
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void commitEdit(HyperTableCell newValue)
  {
    super.cancelEdit();
    setGraphic(null);
          
    HyperTableRow row = (HyperTableRow) getTableRow().getItem();
    int colNdx = getTableView().getColumns().indexOf(getTableColumn());
    
    row.setCellValue(colNdx, newValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private void createTextField()
  {
    textField = new TextField(getString());
    textField.setMinWidth(this.getWidth() - this.getGraphicTextGap() * 2);
    textField.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == false)
        commit();
    });
    
    if (isNumeric.isTrue())
    {
      UnaryOperator<TextFormatter.Change> filter = (change) ->
      {
        if (change.isReplaced())
          if (change.getText().matches("[^0-9]"))
            change.setText(change.getControlText().substring(change.getRangeStart(), change.getRangeEnd()));
      

        if (change.isAdded()) 
          if (change.getText().matches("[^0-9]"))
            change.setText("");
        
        return change;
      };
           
      textField.setTextFormatter(new TextFormatter<>(filter));
    }

    textField.setOnKeyPressed(event ->
    {
      if (event.getCode() == KeyCode.ESCAPE)
      {
        HyperTableCell item = getItem();
        textField.setText(item.getText());
        commitEdit(item);
      }
      
      else if (event.getCode() == KeyCode.ENTER)
        commit(); 
    });

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void commit()
  {
    HyperTableCell oldItem = getItem();
    HyperTableCell newItem = new HyperTableCell(getCellID(oldItem), textField.getText(), getCellType(oldItem));
    commitEdit(newItem);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}