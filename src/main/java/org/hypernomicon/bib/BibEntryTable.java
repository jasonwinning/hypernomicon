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

package org.hypernomicon.bib;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.view.wrappers.HasRightClickableRows;
import org.hypernomicon.view.wrappers.HyperTable;

import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.text.Text;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_Record.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class BibEntryTable extends HasRightClickableRows<BibEntryRow>
{

//---------------------------------------------------------------------------

  private final ObservableList<BibEntryRow> rows;
  private final Map<String, BibEntryRow> keyToRow;
  private final TableView<BibEntryRow> tv;

  void updateKey(String oldKey, String newKey) { keyToRow.put(newKey, keyToRow.remove(oldKey)); }
  boolean containsKey(String bibEntryKey)      { return keyToRow.containsKey(bibEntryKey); }
  void clear()                                 { rows.clear(); keyToRow.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked") BibEntryTable(TableView<BibEntryRow> tv, String prefID)
  {
    this.tv = tv;
    rows = FXCollections.observableArrayList();
    keyToRow = new HashMap<>();

    if (prefID.length() > 0)
      HyperTable.registerTable(tv, prefID, bibManagerDlg);

    tv.setItems(rows);
    tv.setPlaceholder(new Text("There are no entries in the current view."));

    TableColumn<BibEntryRow, String> tcEntryKey    = (TableColumn<BibEntryRow, String>) tv.getColumns().get(0),
                                     tcType        = (TableColumn<BibEntryRow, String>) tv.getColumns().get(1),
                                     tcAuthors     = (TableColumn<BibEntryRow, String>) tv.getColumns().get(2),
                                     tcTitle       = (TableColumn<BibEntryRow, String>) tv.getColumns().get(3),
                                     tcYear        = (TableColumn<BibEntryRow, String>) tv.getColumns().get(4),
                                     tcAssocRecord = (TableColumn<BibEntryRow, String>) tv.getColumns().get(5),
                                     tcPublishedIn = (TableColumn<BibEntryRow, String>) tv.getColumns().get(6);

    tcEntryKey   .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getEntryKey()));
    tcType       .setCellValueFactory(cellData -> new SimpleStringProperty(BibUtils.getEntryTypeName(cellData.getValue().getEntry().getEntryType())));
    tcAuthors    .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getAuthors().getStr()));
    tcTitle      .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfTitle)));
    tcYear       .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfYear)));
    tcPublishedIn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfContainerTitle)));

    tcTitle.setComparator((str1, str2) -> makeSortKeyByType(str1, hdtWork).compareTo(makeSortKeyByType(str2, hdtWork)));

    Comparator<String> cmp = (str1, str2) ->
    {
      int int1 = parseInt(str1, -1);

      if (int1 >= 0)
      {
        int int2 = parseInt(str2, -1);

        if (int2 >= 0)
          return int1 - int2;
      }

      return str1.compareTo(str2);
    };

    tcYear.setComparator(cmp);
    tcAssocRecord.setComparator(cmp);

    tcAssocRecord.setCellValueFactory(cellData -> new SimpleStringProperty
      (nullSwitch(db.getWorkByBibEntryKey(cellData.getValue().getEntry().getEntryKey()), "", work -> String.valueOf(work.getID()))));

    tv.setRowFactory(thisTV ->
    {
      TableRow<BibEntryRow> row = new TableRow<>();

      row.itemProperty().addListener((o, ov, nv) -> row.setContextMenu(nv == null ? null : createContextMenu(nv)));

      row.setOnMouseClicked(mouseEvent ->
      {
        if ((mouseEvent.getButton().equals(MouseButton.PRIMARY)) && (mouseEvent.getClickCount() == 2))
          nullSwitch(row.getItem(), item -> nullSwitch(item.getWork(), work -> ui.goToRecord(work, true)));
      });

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh(Set<? extends BibEntry> entries)
  {
    entries.forEach(entry ->
    {
      if (entry.getEntryType() != EntryType.etOther)
        if (keyToRow.containsKey(entry.getEntryKey()) == false)
        {
          BibEntryRow row = new BibEntryRow(entry);
          rows.add(row);
          keyToRow.put(entry.getEntryKey(), row);
        }
    });

    rows.removeIf(row ->
    {
      if (entries.contains(row.getEntry()) == false)
      {
        keyToRow.remove(row.getEntry().getEntryKey());
        return true;
      }

      return false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(Set<? extends BibEntry> entries)
  {
    clear();

    entries.forEach(entry ->
    {
      if (entry.getEntryType() == EntryType.etOther) return;

      BibEntryRow row = new BibEntryRow(entry);
      rows.add(row);
      keyToRow.put(entry.getEntryKey(), row);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void selectKey(String bibEntryKey)
  {
    tv.getSelectionModel().select(keyToRow.get(bibEntryKey));
    HyperTable.scrollToSelection(tv, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
