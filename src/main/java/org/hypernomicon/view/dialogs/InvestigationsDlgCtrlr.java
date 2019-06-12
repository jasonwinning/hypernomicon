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
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.CheckBoxListCell;

//---------------------------------------------------------------------------

public class InvestigationsDlgCtrlr extends HyperDlg
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

  @FXML private TextField tfNew;
  @FXML private CheckBox chkNew;
  @FXML public ListView<InvestigationSetting> listView;

  @Override protected boolean isValid() { return true; }
  public String newName()               { return tfNew.getText(); }
  public boolean hasNew()               { return tfNew.getText().trim().length() == 0 ? false : chkNew.isSelected(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static InvestigationsDlgCtrlr create(HDT_Work work, HDT_Person curPerson)
  {
    InvestigationsDlgCtrlr idc = HyperDlg.create("InvestigationsDlg.fxml", "Assign Investigations - " + work.name(), true);
    idc.init(work, curPerson);
    return idc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init(HDT_Work work, HDT_Person curPerson)
  {
    final ObservableList<InvestigationSetting> data = FXCollections.observableArrayList();

    curPerson.investigations.forEach(inv -> data.add(new InvestigationSetting(work.investigations.contains(inv), inv)));

    listView.setItems(data);
    listView.setCellFactory(CheckBoxListCell.forListView(InvestigationSetting::selectedProperty));

    tfNew.textProperty().addListener((ob, ov, nv) -> chkNew.setSelected(nv.trim().length() > 0));

    onShown = tfNew::requestFocus;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
