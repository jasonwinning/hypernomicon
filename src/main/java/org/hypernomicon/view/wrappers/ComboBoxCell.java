/*
 * Copyright 2015-2018 Jason Winning
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

import org.apache.commons.lang3.mutable.MutableBoolean;

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.util.AutoCompleteCB;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType;
import org.hypernomicon.view.wrappers.HyperCB.CellTextHandler;
import javafx.collections.ObservableList;
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
  private Populator populator;
  private EventHandler<ActionEvent> onAction;
  private HyperTable table;
  private MutableBoolean dontCreateNewRecord;
  private CellTextHandler textHndlr;
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public ComboBoxCell(HyperTable table, HyperCtrlType ctrlType, Populator populator, EventHandler<ActionEvent> onAction, 
                      MutableBoolean dontCreateNewRecord, CellTextHandler textHndlr)
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
    if (!isEmpty())
    {
      super.startEdit();
      createComboBox();
      table.cellBeingEdited = this;
      
      hCB.populate(false);
      
      HyperTableCell cell = getItem();
      
      cB.setValue(cell);
      if (cell != null)
        cB.getSelectionModel().select(cell);
      
      setGraphic(cB);
      safeFocus(cB);
      AutoCompleteCB.scrollToValue(cB);
      cB.show();
    }
  }  

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void cancelEdit()
  { 
    table.cellBeingEdited = null;
    return;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void commitEdit(HyperTableCell newValue)
  {
    int rowNdx, colNdx;
    
    super.cancelEdit();
    setGraphic(null);
    
    ObservableList<HyperTableRow> rows = this.getTableView().getItems();
    
    rowNdx = getTableRow().getIndex();
    colNdx = getTableView().getColumns().indexOf(getTableColumn());
    
    if (hCB.somethingWasTyped)
      if (hCB.typedMatch != null)
        newValue = hCB.typedMatch;
    
    rows.get(rowNdx).updateCell(colNdx, newValue);
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
    }
    else
    {    
      if (this.isEditing())
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
  }  

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void createComboBox()
  {
    cB = new ComboBox<HyperTableCell>();
    cB.setPrefWidth(this.getWidth() - this.getGraphicTextGap() * 2);
    cB.setMinHeight(18.0 * displayScale);
    cB.setPrefHeight(18.0 * displayScale);
    cB.setMaxHeight(18.0 * displayScale);
    
    HyperTableRow row = (HyperTableRow) this.getTableRow().getItem();
    
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
      if (newValue == false)    
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
      return textHndlr.getText(getTableView().getItems().get(getTableRow().getIndex()));

    return getItem() == null ? "" : getItem().getText();
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void commit()
  {
    if (isEditing())
      commitEdit(hCB.selectedHTC());
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
