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

import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.HyperFavorites.*;

public class FavOrderDlgCtrlr extends HyperDlg
{
  @FXML private ListView<MenuItem> lvRecord, lvQuery;
  @FXML private Button btnRecordUp, btnRecordDown, btnQueryUp, btnQueryDown, btnOk;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FavOrderDlgCtrlr create(String title)
  {
    FavOrderDlgCtrlr fod = HyperDlg.create("FavOrderDlg.fxml", title, true);
    fod.init();
    return fod;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    lvRecord.setCellFactory(listView -> new ListCell<MenuItem>()
    {
      @Override public void updateItem(MenuItem item, boolean empty)
      {
        super.updateItem(item, empty);
        setText(item == null ? "" : item.getText());
      }
    });

    lvRecord.setItems(new FilteredList<>(ui.mnuFavorites.getItems(), item -> ui.mnuFavorites.getItems().indexOf(item) >= FIRST_FAV_MENU_ITEM_NDX));

    btnRecordUp.setOnAction(event -> moveRecord(-1));
    btnRecordDown.setOnAction(event -> moveRecord(1));

    lvQuery.setCellFactory(listView -> new ListCell<MenuItem>()
    {
      @Override public void updateItem(MenuItem item, boolean empty)
      {
        super.updateItem(item, empty);
        setText(item == null ? "" : item.getText());
      }
    });

    lvQuery.setItems(new FilteredList<>(ui.mnuQueries.getItems(), item -> ui.mnuQueries.getItems().indexOf(item) >= 0));

    btnQueryUp.setOnAction(event -> moveQuery(-1));
    btnQueryDown.setOnAction(event -> moveQuery(1));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void moveRecord(int diff)
  {
    int oldNdx = lvRecord.getSelectionModel().getSelectedIndex() + FIRST_FAV_MENU_ITEM_NDX;
    int newNdx = oldNdx + diff;
    ObservableList<MenuItem> items = ui.mnuFavorites.getItems();

    if ((newNdx >= FIRST_FAV_MENU_ITEM_NDX) && (newNdx < items.size()))
    {
      MenuItem item = items.remove(oldNdx);
      items.add(newNdx, item);
      lvRecord.getSelectionModel().select(item);
    }

    safeFocus(lvRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void moveQuery(int diff)
  {
    int oldNdx = lvQuery.getSelectionModel().getSelectedIndex();
    int newNdx = oldNdx + diff;
    ObservableList<MenuItem> items = ui.mnuQueries.getItems();

    if ((newNdx >= 0) && (newNdx < items.size()))
    {
      MenuItem item = items.remove(oldNdx);
      items.add(newNdx, item);
      lvQuery.getSelectionModel().select(item);
    }

    safeFocus(lvQuery);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
