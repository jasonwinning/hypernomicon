/*
 * Copyright 2015-2023 Jason Winning
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

import javafx.collections.transformation.FilteredList;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.util.Callback;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.view.HyperFavorites.*;

import java.util.List;

public class FavOrderDlgCtrlr extends HyperDlg
{
  @FXML private ListView<MenuItem> lvRecord, lvQuery;
  @FXML private Button btnRecordUp, btnRecordDown, btnQueryUp, btnQueryDown;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FavOrderDlgCtrlr build()
  {
    return ((FavOrderDlgCtrlr) create("FavOrderDlg", "Change Order of Favorites", true)).init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FavOrderDlgCtrlr init()
  {
    Callback<ListView<MenuItem>, ListCell<MenuItem>> factory = listView -> new ListCell<>()
    {
      @Override public void updateItem(MenuItem item, boolean empty)
      {
        super.updateItem(item, empty);
        setText(item == null ? "" : item.getText());
      }
    };

    lvRecord.setCellFactory(factory);

    lvRecord.setItems(new FilteredList<>(ui.mnuFavorites.getItems(), item -> ui.mnuFavorites.getItems().indexOf(item) >= FIRST_FAV_MENU_ITEM_NDX));

    btnRecordUp.setOnAction(event -> moveRecord(-1));
    btnRecordDown.setOnAction(event -> moveRecord(1));

    lvQuery.setCellFactory(factory);

    lvQuery.setItems(new FilteredList<>(ui.mnuQueries.getItems(), item -> ui.mnuQueries.getItems().contains(item)));

    btnQueryUp.setOnAction(event -> moveQuery(-1));
    btnQueryDown.setOnAction(event -> moveQuery(1));

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveRecord(int diff)
  {
    if (lvRecord.getSelectionModel().getSelectedIndex() < 0) return;

    int oldNdx = lvRecord.getSelectionModel().getSelectedIndex() + FIRST_FAV_MENU_ITEM_NDX,
        newNdx = oldNdx + diff;

    List<MenuItem> items = ui.mnuFavorites.getItems();

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

  private void moveQuery(int diff)
  {
    int oldNdx = lvQuery.getSelectionModel().getSelectedIndex(),
        newNdx = oldNdx + diff;

    if (oldNdx < 0) return;

    List<MenuItem> items = ui.mnuQueries.getItems();

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
