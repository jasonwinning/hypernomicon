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

package org.hypernomicon.view.wrappers;

import java.util.HashMap;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Record;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

class TreeCB
{
  private final ComboBox<TreeRow> cb;
  private final HashMap<HDT_Record, TreeRow> recordToRow;
  private final ObservableList<TreeRow> rows;
  private boolean sorted = false;
  private final TreeWrapper tree;

//---------------------------------------------------------------------------

  TreeCB(ComboBox<TreeRow> comboBox, TreeWrapper tree)
  {
    this.cb = comboBox;
    this.tree = tree;
    recordToRow = new HashMap<>();
    rows = FXCollections.observableArrayList();
    cb.setItems(rows);

    comboBox.setEditable(true);

    comboBox.setOnShowing(event ->
    {
      if (sorted) return;

      HDT_Record record = tree.selectedRecord();

      comboBox.setItems(null);
      rows.sort((row1, row2) -> row1.getCBText().toLowerCase().compareTo(row2.getCBText().toLowerCase()));
      comboBox.setItems(rows);

      if (record != null)
        select(record);

      sorted = true;

      event.consume();

      Platform.runLater(comboBox::show);
    });

    clear();

  //---------------------------------------------------------------------------

    comboBox.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null)
        tree.selectRecord(null, -1, true);
      else if (newValue.getRecordID() > -1)
        tree.selectRecord(newValue.getRecord(), -1, true);
    });

  //---------------------------------------------------------------------------

    comboBox.setConverter(new StringConverter<TreeRow>()
    {
      @Override public String toString(TreeRow row)
      {
        return nullSwitch(row, "", TreeRow::getCBText);
      }

      @Override public TreeRow fromString(String string)
      {
        if (comboBox.getItems() == null)
          return new TreeRow(string);

        return nullSwitch(findFirst(comboBox.getItems(), row -> string.equals(row.getCBText())), new TreeRow(string));
      }
    });

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    rows.clear();
    recordToRow.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void add(HDT_Record record)
  {
    if (recordToRow.containsKey(record)) return;

    TreeRow row = new TreeRow(record, null);
    recordToRow.put(record, row);
    rows.add(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void checkIfShouldBeRemoved(HDT_Record record)
  {
    if (tree.getRowsForRecord(record).isEmpty() == false) return;

    rows.remove(recordToRow.get(record));

    recordToRow.remove(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearSelection()
  {
    cb.getSelectionModel().clearSelection();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh()
  {
    sorted = false;

    nullSwitch(nullSwitch(tree.selectedRecord(), db.debates.getByID(1)), this::select);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void select(HDT_Record record)
  {
    clearSelection();

    nullSwitch(recordToRow.get(record), row -> cb.getSelectionModel().select(row));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
