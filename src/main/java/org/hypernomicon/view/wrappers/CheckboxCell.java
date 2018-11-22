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

import javafx.scene.control.CheckBox;
import javafx.scene.control.TableCell;

public class CheckboxCell extends TableCell<HyperTableRow, Boolean>
{
  private HyperTable table;
  private CheckBox chk;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public CheckboxCell(HyperTable table) 
  {
    super();
    
    this.table = table;
    chk = new CheckBox();
    
    emptyProperty().addListener((observable, oldValue, newValue) -> chk.setVisible(newValue.booleanValue() == false));
    
    chk.selectedProperty().addListener((ov, oldValue, newValue) ->
    {      
      HyperTableRow row = (HyperTableRow) getTableRow().getItem();
      if (row == null) return;
           
      HyperTableCell cell = newValue.booleanValue() ? HyperTableCell.trueCell : HyperTableCell.falseCell;
      
      int colNdx = getTableView().getColumns().indexOf(getTableColumn());
      row.setCellValue(colNdx, cell);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
  
  //Display check box if the row is not empty
  @Override protected void updateItem(Boolean val, boolean empty)
  {
    super.updateItem(val, empty);
    
    if (empty) return;

    setGraphic(chk);
    boolean disable = true;
    chk.setSelected(val);
    
    if (getTableRow() != null)
    {
      HyperTableRow row = (HyperTableRow)getTableRow().getItem();
      if (row != null)
      {
        int colNdx = table.getMainColNdx();
        if ((row.getID(colNdx) > 0) || (row.getText(colNdx).length() > 0))
          disable = false;
      }
    }
    
    chk.setDisable(disable);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

}
