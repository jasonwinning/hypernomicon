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

package org.hypernomicon.view.workMerge;

import static org.hypernomicon.util.Util.deleteGridPaneColumn;

import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibData.BibFieldEnum;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;

public class MergeWorksSLController
{
  @FXML private RadioButton rb1;
  @FXML private RadioButton rb2;
  @FXML private RadioButton rb3;
  @FXML private RadioButton rb4;

  @FXML private TextField tf1;
  @FXML private TextField tf2;
  @FXML private TextField tf3;
  @FXML private TextField tf4;
  
  @FXML private GridPane gp;
  
  @FXML private Label lbl;
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void init(BibFieldEnum bibFieldEnum, BibData bd1, BibData bd2, BibData bd3, BibData bd4)
  {
    lbl.setText(BibData.getFieldName(bibFieldEnum));
    
    if (bd4 == null)
    {
      deleteGridPaneColumn(gp, 3);
    }
    else
    {
      if (bd4.fieldNotEmpty(bibFieldEnum))
      {
        tf4.setText(bd4.getStr(bibFieldEnum));
        rb4.setSelected(true);
      }
    }
    
    if (bd3 == null)
    {
      deleteGridPaneColumn(gp, 2);
    }
    else
    {
      if (bd3.fieldNotEmpty(bibFieldEnum))
      {
        tf3.setText(bd3.getStr(bibFieldEnum));
        rb3.setSelected(true);      
      }
    }
    
    if (bd2.fieldNotEmpty(bibFieldEnum))
    {
      tf2.setText(bd2.getStr(bibFieldEnum));
      rb2.setSelected(true);      
    }
    
    if (bd1.fieldNotEmpty(bibFieldEnum))
    {
      tf1.setText(bd1.getStr(bibFieldEnum));
      rb1.setSelected(true);      
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public String toString()
  {
    if      (rb1.isSelected()) return tf1.getText();
    else if (rb2.isSelected()) return tf2.getText();
    else if (rb3.isSelected()) return tf3.getText();
    else                       return tf4.getText();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
