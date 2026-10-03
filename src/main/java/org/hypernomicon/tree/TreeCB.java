/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.tree;

import java.util.*;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Record;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.util.StringConverter;

//---------------------------------------------------------------------------

class TreeCB
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ComboBox<TreeRow> cb;
  private final Map<HDT_Record, TreeRow> recordToRow;
  private final ObservableList<TreeRow> rows;
  private boolean itemsAreCurrent = false, changeIsProgrammatic = false;
  private final TreeWrapper tree;

//---------------------------------------------------------------------------

  TreeCB(ComboBox<TreeRow> comboBox, TreeWrapper tree)
  {
    cb = comboBox;
    this.tree = tree;
    recordToRow = new HashMap<>();
    rows = FXCollections.observableArrayList();
    cb.setItems(rows);

    comboBox.setEditable(true);

    // The items are filled in here, when the dropdown opens, not as records enter the tree.
    // Every change to the items makes the ComboBox skin convert the editor's text back into
    // a value with the converter below, which compares the text with the display text of
    // every row; filling the items record by record while the tree model is being built
    // (see the relation change handlers in TreeModel) scanned the whole list for each record.

    comboBox.setOnShowing(event ->
    {
      if (itemsAreCurrent) return;

      HDT_Record record = tree.selectedRecord();

      changeIsProgrammatic = true;

      comboBox.setItems(null);
      populateRows();
      comboBox.setItems(rows);

      changeIsProgrammatic = false;

      if (record != null)
        select(record);

      itemsAreCurrent = true;

      event.consume();

      Platform.runLater(comboBox::show);
    });

    clear();

  //---------------------------------------------------------------------------

    comboBox.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (changeIsProgrammatic) return;

      if (newValue == null)
        tree.selectRecord(null, -1, true);
      else if (newValue.getRecordID() > -1)
        tree.selectRecord(newValue.getRecord(), -1, true);
    });

  //---------------------------------------------------------------------------

    comboBox.setConverter(new StringConverter<>()
    {
      @Override public String toString(TreeRow row)
      {
        return nullSwitch(row, "", TreeRow::getDisplayText);
      }

      @Override public TreeRow fromString(String string)
      {
        if (strNullOrEmpty(string) || (comboBox.getItems() == null))  // No row's display text is empty
          return new TreeRow(string);

        TreeRow value = comboBox.getValue();  // The skin converts the editor's own text back whenever the items change

        if ((value != null) && string.equals(value.getDisplayText()))
          return value;

        return nullSwitch(findFirst(comboBox.getItems(), row -> string.equals(row.getDisplayText())), new TreeRow(string));
      }
    });

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------

  public HDT_Record selectedRecord() { return nullSwitch(cb.getSelectionModel().getSelectedItem(), null, TreeRow::getRecord); }
  public String getText()            { return cb.getEditor().getText(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    changeIsProgrammatic = true;
    rows.clear();
    changeIsProgrammatic = false;

    recordToRow.clear();
    itemsAreCurrent = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void add(HDT_Record record)
  {
    if (recordToRow.containsKey(record)) return;

    recordToRow.put(record, new TreeRow(record, null));
    itemsAreCurrent = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void checkIfShouldBeRemoved(HDT_Record record)
  {
    if (tree.getRowsForRecord(record).isEmpty() == false) return;

    TreeRow row = recordToRow.remove(record);

    if (row == null) return;

    changeIsProgrammatic = true;
    rows.remove(row);  // Not necessarily present: records added since the dropdown last opened are not in the items yet
    changeIsProgrammatic = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearSelection()
  {
    changeIsProgrammatic = true;
    cb.getSelectionModel().clearSelection();
    changeIsProgrammatic = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh()
  {
    itemsAreCurrent = false;

    HDT_Debate rootDebate = db.debates.getByID(1);                           // If these two lines are combined into one, there will be
    nullSwitch(nullSwitch(tree.selectedRecord(), rootDebate), this::select); // false-positive build errors
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void select(HDT_Record record)
  {
    clearSelection();

    changeIsProgrammatic = true;
    nullSwitch(recordToRow.get(record), cb.getSelectionModel()::select);
    changeIsProgrammatic = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Fills the items with a row for every record in the tree, in the order the dropdown lists
   * them. Each display text is computed once: computed per comparison instead, the texts would
   * be most of the cost of opening the dropdown, since a work's is built from its authors, year
   * and title. Called while the items are detached from the ComboBox, so nothing observes the
   * fill or the sort.
   */
  private void populateRows()
  {
    Map<TreeRow, String> sortKeys = new HashMap<>();

    recordToRow.values().forEach(row -> sortKeys.put(row, row.getDisplayText().toLowerCase()));

    rows.setAll(recordToRow.values());
    rows.sort(Comparator.comparing(sortKeys::get));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
