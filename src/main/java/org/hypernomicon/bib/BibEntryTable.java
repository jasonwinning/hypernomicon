/*
 * Copyright 2015-2024 Jason Winning
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

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.util.BasicTextMatcher;
import org.hypernomicon.view.wrappers.HasRightClickableRows;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.ObjectCellValue;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.collections.transformation.FilteredList;
import javafx.collections.transformation.SortedList;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.text.Text;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

class BibEntryTable extends HasRightClickableRows<BibEntryRow>
{

//---------------------------------------------------------------------------

  private final ObservableList<BibEntryRow> rows;
  private final FilteredList<BibEntryRow> filteredRows;
  private final Map<String, BibEntryRow> keyToRow;
  private final TableView<BibEntryRow> tv;
  private boolean noRefresh = false;

  void updateKey(String oldKey, String newKey) { keyToRow.put(newKey, keyToRow.remove(oldKey)); }
  boolean containsKey(String bibEntryKey)      { return keyToRow.containsKey(bibEntryKey); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked") BibEntryTable(TableView<BibEntryRow> tv, BibManager dlg)
  {
    this.tv = tv;
    rows = FXCollections.observableArrayList();
    keyToRow = new HashMap<>();

    HyperTable.registerTable(tv, PREF_KEY_HT_BIB_ENTRIES, dlg);

    filteredRows = new FilteredList<>(rows, row -> true);

    SortedList<BibEntryRow> sortedRows = new SortedList<>(filteredRows);
    sortedRows.comparatorProperty().bind(tv.comparatorProperty());

    tv.setItems(sortedRows);
    tv.setPlaceholder(new Text("There are no entries in the current view."));

    TableColumn<BibEntryRow, String> tcEntryKey    = (TableColumn<BibEntryRow, String>) tv.getColumns().get(0),
                                     tcType        = (TableColumn<BibEntryRow, String>) tv.getColumns().get(1),
                                     tcAuthors     = (TableColumn<BibEntryRow, String>) tv.getColumns().get(2),
                                     tcTitle       = (TableColumn<BibEntryRow, String>) tv.getColumns().get(3),
                                     tcAssocRecord = (TableColumn<BibEntryRow, String>) tv.getColumns().get(5),
                                     tcPublishedIn = (TableColumn<BibEntryRow, String>) tv.getColumns().get(6),
                                     tcPublisher   = (TableColumn<BibEntryRow, String>) tv.getColumns().get(7);

    TableColumn<BibEntryRow, ObjectCellValue<BibliographicDate>> tcBibDate = (TableColumn<BibEntryRow, ObjectCellValue<BibliographicDate>>) tv.getColumns().get(4);

    tcEntryKey   .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getKey()));
    tcType       .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getEntryType().getUserFriendlyName()));
    tcAuthors    .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getAuthors().getStr()));
    tcTitle      .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfTitle)));
    tcPublishedIn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfContainerTitle)));
    tcPublisher  .setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getEntry().getStr(bfPublisher)));

    tcBibDate.setCellValueFactory(cellData ->
    {
      BibliographicDate bibDate = cellData.getValue().getEntry().getDate();

      return new SimpleObjectProperty<>(new ObjectCellValue<>(bibDate.displayToUser(), bibDate));
    });

    tcTitle.setComparator(Comparator.comparing(str -> makeSortKeyByType(str, hdtWork)));

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

    tcAssocRecord.setComparator(cmp);

    tcAssocRecord.setCellValueFactory(cellData -> new SimpleStringProperty
      (nullSwitch(db.getWorkByBibEntryKey(cellData.getValue().getEntry().getKey()), "", work -> String.valueOf(work.getID()))));

    tv.setRowFactory(thisTV ->
    {
      TableRow<BibEntryRow> row = new TableRow<>();

      row.itemProperty().addListener((ob, ov, nv) -> row.setContextMenu(createContextMenu(nv)));

      row.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
          nullSwitch(row.getItem(), item -> nullSwitch(item.getWork(), work -> ui.goToRecord(work, true)));
      });

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    noRefresh = true;
    rows.clear();
    keyToRow.clear();
    noRefresh = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh(Stream<? extends BibEntry<?, ?>> entries)
  {
    if (noRefresh) return;

    Set<BibEntry<?, ?>> entriesToRemove = rows.stream().map(BibEntryRow::getEntry).collect(Collectors.toSet());

    entries.forEachOrdered(entry ->
    {
      if (entry.getEntryType() == EntryType.etOther)
        return;

      entriesToRemove.remove(entry);

      if (keyToRow.containsKey(entry.getKey()) == false)
      {
        BibEntryRow row = new BibEntryRow(entry);
        rows.add(row);
        keyToRow.put(entry.getKey(), row);
      }
    });

    if (entriesToRemove.isEmpty()) return;

    rows.removeIf(row ->
    {
      if (entriesToRemove.contains(row.getEntry()) == false)
        return false;

      keyToRow.remove(row.getEntry().getKey());
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(Stream<? extends BibEntry<?, ?>> entries)
  {
    clear();

    entries.forEachOrdered(entry ->
    {
      if (entry.getEntryType() == EntryType.etOther) return;

      BibEntryRow row = new BibEntryRow(entry);
      rows.add(row);
      keyToRow.put(entry.getKey(), row);
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

  public void filter(String txt, boolean requireByDefault)
  {
    String text = ultraTrim(safeStr(txt).toLowerCase());

    if (text.isBlank())
    {
      filteredRows.setPredicate(row -> true);
      return;
    }

    BasicTextMatcher matcher = new BasicTextMatcher(text, false, requireByDefault);

    filteredRows.setPredicate(row ->
    {
      BibEntry<?, ?> entry = row.getEntry();

      String entryStr = entry.getCBText() + ' ' + entry.getStr(bfContainerTitle) + ' ' + entry.getStr(bfPublisher);

      return matcher.isMatch(entryStr);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
