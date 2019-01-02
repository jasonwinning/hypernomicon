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

package org.hypernomicon.view.dialogs;

import org.hypernomicon.model.records.HDT_Investigation;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.CheckBoxListCell;

//---------------------------------------------------------------------------  

public class InvestigationsDialogController extends HyperDialog
{ 
//---------------------------------------------------------------------------  
  public static class InvestigationSetting
  {
    private final SimpleBooleanProperty selected;
    public HDT_Investigation inv;

    public InvestigationSetting(boolean selected, HDT_Investigation inv)
    {
      this.selected = new SimpleBooleanProperty(selected);
      this.inv = inv;
    }

    @Override public String toString()              { return inv.getCBText(); }    
    
    public boolean getSelected()                    { return selected.get(); }
    public SimpleBooleanProperty selectedProperty() { return selected; }
  }
 
//---------------------------------------------------------------------------  
  
  @FXML private Button btnOk;
  @FXML private Button btnCancel;
  @FXML private TextField tfNew;
  @FXML private CheckBox chkNew;
  @FXML public ListView<InvestigationSetting> listView;
  
  @Override protected boolean isValid() { return true; }
  public String newName()               { return tfNew.getText(); }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public static InvestigationsDialogController create(String title, HDT_Work work, HDT_Person curPerson)
  {
    InvestigationsDialogController idc = HyperDialog.create("InvestigationsDialog.fxml", title, true);
    idc.init(work, curPerson);
    return idc;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  private void init(HDT_Work work, HDT_Person curPerson)
  {
    final ObservableList<InvestigationSetting> data = FXCollections.observableArrayList();

    for (HDT_Investigation inv : curPerson.investigations)
      data.add(new InvestigationSetting(work.investigations.contains(inv), inv));
    
    listView.setItems(data);   
    listView.setCellFactory(CheckBoxListCell.forListView(invSetting -> invSetting.selectedProperty()));
    
    tfNew.textProperty().addListener((observable, oldValue, newValue) ->
    {
      chkNew.setSelected(newValue.trim().length() > 0);
    });
    
    onShown = tfNew::requestFocus;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public boolean hasNew()
  {
    if (tfNew.getText().trim().length() == 0) return false;
    return chkNew.isSelected();
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
