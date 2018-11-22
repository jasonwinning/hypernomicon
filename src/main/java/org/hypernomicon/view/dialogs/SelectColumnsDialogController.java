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

package org.hypernomicon.view.dialogs;

import java.util.ArrayList;

import org.hypernomicon.view.wrappers.ResultsTable.ColumnGroup;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ScrollPane;
import javafx.scene.layout.AnchorPane;
import static org.hypernomicon.view.wrappers.ResultsTable.*;

public class SelectColumnsDialogController extends HyperDialog
{
  @FXML private CheckBox chkSelectAll;
  @FXML private CheckBox chkSelectNone;
  @FXML private CheckBox chkFirstType;
  @FXML private CheckBox chkFirstField;
  @FXML private CheckBox chkSecondType;
  @FXML private Button btnOk;
  @FXML private AnchorPane innerPane;
  @FXML private ScrollPane scrollPane;

//---------------------------------------------------------------------------
  
  public static class TypeCheckBox extends CheckBox
  {
    public TypeCheckBox(String caption) { super(caption); }
    public ArrayList<CheckBox> children = new ArrayList<CheckBox>();
  }
  
//---------------------------------------------------------------------------
  
  public static class ColumnCheckBox extends CheckBox
  {
    public ColumnCheckBox(String caption) { super(caption); }
    public TypeCheckBox parent;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean noListen = false;
  
  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static SelectColumnsDialogController create(String title)
  {
    SelectColumnsDialogController scd = HyperDialog.create("SelectColumnsDialog.fxml", title, true);
    scd.init();
    return scd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    double typeLeft = chkFirstType.getLayoutX(),
           fieldLeft = chkFirstField.getLayoutX(),
           itemMargin = chkFirstField.getLayoutY() - chkFirstType.getLayoutY(),
           typeMargin = chkSecondType.getLayoutY() - chkFirstField.getLayoutY(),
           posY = chkFirstType.getLayoutY() - typeMargin;
    
    btnOk.setOnAction(event -> getStage().close());
    
    innerPane.getChildren().remove(chkFirstType);
    innerPane.getChildren().remove(chkFirstField);
    innerPane.getChildren().remove(chkSecondType);
    
    chkSelectAll.setSelected(true);
    
    for (ColumnGroup group : colGroups)
    {
      if (group.items.size() == 0) continue;
      
      posY += typeMargin;
      TypeCheckBox chkType = new TypeCheckBox(group.caption);
      group.checkBox = chkType;
      chkType.setLayoutX(typeLeft);
      chkType.setLayoutY(posY);
      chkType.setSelected(true);
      innerPane.getChildren().add(chkType);
      
      chkSelectAll.selectedProperty().addListener((observable, oldValue, newValue) ->
      {
        if (noListen) return;
        noListen = true;
        
        if (newValue)
        {
          chkSelectNone.setSelected(false);
          
          for (ColumnGroup grp : colGroups)
          {
            if (grp.items.size() == 0) continue;
            
            TypeCheckBox tcb = grp.checkBox;
            tcb.setSelected(true);
            
            tcb.children.forEach(ccb -> ccb.setSelected(true));
          }
        }
        else
          chkSelectAll.setSelected(true);
        
        noListen = false;
      });

      chkSelectNone.selectedProperty().addListener((observable, oldValue, newValue) ->
      {
        if (noListen) return;
        noListen = true;
        
        if (newValue)
        {
          chkSelectAll.setSelected(false);
          
          for (ColumnGroup grp : colGroups)
          {
            if (grp.items.size() == 0) continue;
            
            TypeCheckBox tcb = grp.checkBox;
            tcb.setSelected(false);
            
            tcb.children.forEach(ccb -> ccb.setSelected(false));
          }
        }
        else
          chkSelectNone.setSelected(true);
        
        noListen = false;
      });
      
      
      chkType.selectedProperty().addListener((observable, oldValue, newValue) ->
      {
        if (noListen) return;
        noListen = true;

        if (newValue)
        {         
          chkSelectNone.setSelected(false);
          chkType.children.forEach(chk -> chk.setSelected(true));
        }
        else
        {
          chkSelectAll.setSelected(false);
          chkType.children.forEach(chk -> chk.setSelected(false));
        }        
        
        noListen = false;
      });
      
      
      for (ColumnGroupItem item : group.items)
      {
        if (item.col != null)
        {
          posY += itemMargin;
          ColumnCheckBox chkField = new ColumnCheckBox(item.caption);
          chkType.children.add(chkField);
          chkField.parent = chkType;
          chkField.setLayoutX(fieldLeft);
          chkField.setLayoutY(posY);
          chkField.selectedProperty().bindBidirectional(item.col.visibleProperty());
          innerPane.getChildren().add(chkField);
          
          chkField.selectedProperty().addListener((observable, oldValue, newValue) ->
          {
            if (noListen) return;
            noListen = true;
            
            boolean allSelected = true;
            boolean noneSelected = true;
            TypeCheckBox tcb = chkField.parent;
            
            if (newValue)
            {
              chkSelectNone.setSelected(false);
              for (CheckBox cb : tcb.children)
              {
                if (cb.isSelected() == false)
                  allSelected = false;
              }
              if (allSelected) tcb.setSelected(true);
            }
            else
            {
              chkSelectAll.setSelected(false);
              for (CheckBox cb : tcb.children)
              {
                if (cb.isSelected())
                  noneSelected = false;
              }
              if (noneSelected) tcb.setSelected(false);
            }
            
            noListen = false;           
          });   
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
