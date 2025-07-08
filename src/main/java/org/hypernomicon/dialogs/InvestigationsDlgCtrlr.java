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

package org.hypernomicon.dialogs;

import java.util.Set;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.HDT_Investigation;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.tabs.InvestigationView;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.CheckBox;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.CheckBoxListCell;

//---------------------------------------------------------------------------

public class InvestigationsDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class InvestigationSetting
  {
    private final SimpleBooleanProperty selected;
    private final String name;
    public final HDT_Investigation inv;

    private InvestigationSetting(boolean selected, HDT_Investigation inv, String name)
    {
      this.selected = new SimpleBooleanProperty(selected);
      this.inv = inv;
      this.name = name;
    }

    @Override public String toString()               { return name; }

    public boolean getSelected()                     { return selected.get(); }
    private SimpleBooleanProperty selectedProperty() { return selected; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TextField tfNew;
  @FXML private CheckBox chkNew;
  @FXML public ListView<InvestigationSetting> listView;

  @Override protected boolean isValid() { return true; }

  public String newName() { return tfNew.getText(); }
  public boolean hasNew() { return (tfNew.getText().isBlank() == false) && chkNew.isSelected(); }

//---------------------------------------------------------------------------

  public InvestigationsDlgCtrlr(HDT_Work work, Iterable<InvestigationView> invViews, HDT_Person curPerson)
  {
    super("InvestigationsDlg", "Assign Investigations - " + work.name(), true);

    final ObservableList<InvestigationSetting> data = FXCollections.observableArrayList();
    Set<HDT_Investigation> investigations = work.investigationSet();

    curPerson.investigations.forEach(inv ->
    {
      String name = findFirst(invViews, invView -> invView.record == inv, invView -> invView.tfName.getText());
      data.add(new InvestigationSetting(investigations.contains(inv), inv, name));
    });

    listView.setItems(data);
    listView.setCellFactory(CheckBoxListCell.forListView(InvestigationSetting::selectedProperty));

    tfNew.textProperty().addListener((ob, ov, nv) -> chkNew.setSelected(strNotNullOrBlank(nv)));

    onShown = () -> safeFocus(tfNew);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
