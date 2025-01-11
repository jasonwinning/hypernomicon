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

import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.cell.CheckBoxListCell;

import java.util.Collection;
import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------

public class UpdateISBNsDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class UpdateISBNSetting
  {
    private final SimpleBooleanProperty selected;
    private final HDT_Work work;

    private UpdateISBNSetting(HDT_Work work)
    {
      this.selected = new SimpleBooleanProperty(true);
      this.work = work;
    }

    @Override public String toString()               { return work.getCBText(); }

    private boolean getSelected()                    { return selected.get(); }
    private HDT_Work getWork()                       { return work; }
    private SimpleBooleanProperty selectedProperty() { return selected; }
  }

//---------------------------------------------------------------------------

  @FXML private Label lblOrigWork;
  @FXML private ListView<UpdateISBNSetting> listView;

//---------------------------------------------------------------------------

  public UpdateISBNsDlgCtrlr(HDT_Work origWork, Collection<HDT_Work> works)
  {
    super("UpdateISBNsDlg", "Update ISBNs", true);

    lblOrigWork.setText("Should the following related works be updated to have the same ISBN(s) as the work: " + origWork.getCBText());

    listView.setItems(FXCollections.observableArrayList(works.stream().map(UpdateISBNSetting::new).toList()));
    listView.setCellFactory(CheckBoxListCell.forListView(UpdateISBNSetting::selectedProperty));
  }

//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<HDT_Work> worksToUpdate()
  {
    return listView.getItems().stream().filter(UpdateISBNSetting::getSelected).map(UpdateISBNSetting::getWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
