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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.view.wrappers.HyperTable;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.text.Text;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_Record.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class BibEntryTable
{
  @FunctionalInterface public interface BibEntryRowHandler     { void handle(BibEntryRow row); }
  @FunctionalInterface public interface CondBibEntryRowHandler { boolean handle(BibEntryRow row); }

//---------------------------------------------------------------------------

  public static class BibEntryRowMenuItemSchema
  {
    public CondBibEntryRowHandler condHandler;
    public BibEntryRowHandler handler;
    public String caption;
    public boolean visible = true;
    public boolean disabled = false;

    public BibEntryRowMenuItemSchema(String caption) { this.caption = caption; }
  }

//---------------------------------------------------------------------------

  public static class BibEntryRowMenuItem extends MenuItem
  {
    public BibEntryRowMenuItem(String caption, BibEntryRowMenuItemSchema schema)
    {
      super(caption);
      this.schema = schema;
    }

    public BibEntryRowMenuItemSchema schema;
  }

//---------------------------------------------------------------------------

  private final ObservableList<BibEntryRow> rows;
  private final Map<String, BibEntryRow> keyToRow;
  private final TableView<BibEntryRow> tv;
  final List<BibEntryRowMenuItemSchema> contextMenuSchemata;

  public void updateKey(String oldKey, String newKey) { keyToRow.put(newKey, keyToRow.remove(oldKey)); }
  public boolean containsKey(String bibEntryKey)      { return keyToRow.containsKey(bibEntryKey); }
  public void clear()                                 { rows.clear(); keyToRow.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public BibEntryTable(TableView<BibEntryRow> tv, String prefID)
  {
    this.tv = tv;
    rows = FXCollections.observableArrayList();
    contextMenuSchemata = new ArrayList<>();
    keyToRow = new HashMap<>();

    if (prefID.length() > 0)
      HyperTable.registerTable(tv, prefID, bibManagerDlg);

    tv.setItems(rows);
    tv.setPlaceholder(new Text("There are no entries in the current view."));

    TableColumn<BibEntryRow, String> tcEntryKey    = (TableColumn<BibEntryRow, String>) tv.getColumns().get(0);
    TableColumn<BibEntryRow, String> tcType        = (TableColumn<BibEntryRow, String>) tv.getColumns().get(1);
    TableColumn<BibEntryRow, String> tcAuthors     = (TableColumn<BibEntryRow, String>) tv.getColumns().get(2);
    TableColumn<BibEntryRow, String> tcTitle       = (TableColumn<BibEntryRow, String>) tv.getColumns().get(3);
    TableColumn<BibEntryRow, String> tcYear        = (TableColumn<BibEntryRow, String>) tv.getColumns().get(4);
    TableColumn<BibEntryRow, String> tcAssocRecord = (TableColumn<BibEntryRow, String>) tv.getColumns().get(5);
    TableColumn<BibEntryRow, String> tcPublishedIn = (TableColumn<BibEntryRow, String>) tv.getColumns().get(6);

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

    tcAssocRecord.setCellValueFactory(cellData ->
    {
      int id = -1;
      String key = cellData.getValue().getEntry().getEntryKey();
      HDT_Work work = db.getWorkByBibEntryKey(key);

      if (work != null)
        id = work.getID();

      return new SimpleStringProperty(id < 1 ? "" : String.valueOf(id));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refresh(Set<? extends BibEntry> entries)
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

  public void update(Set<? extends BibEntry> entries)
  {
    clear();

    entries.forEach(entry ->
    {
      if (entry.getEntryType() != EntryType.etOther)
      {
        BibEntryRow row = new BibEntryRow(entry);
        rows.add(row);
        keyToRow.put(entry.getEntryKey(), row);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectKey(String bibEntryKey)
  {
    BibEntryRow row = keyToRow.get(bibEntryKey);

    tv.getSelectionModel().select(row);
    HyperTable.scrollToSelection(tv, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntryRowMenuItemSchema addContextMenuItem(String caption, BibEntryRowHandler handler)
  {
    return BibEntryRow.addCondContextMenuItem(caption, row -> true, handler, contextMenuSchemata);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntryRowMenuItemSchema addCondContextMenuItem(String caption, CondBibEntryRowHandler condHandler, BibEntryRowHandler handler)
  {
    return BibEntryRow.addCondContextMenuItem(caption, condHandler, handler, contextMenuSchemata);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
