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

import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Set;
import java.util.function.Function;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.view.dialogs.SelectColumnsDlgCtrlr;
import org.hypernomicon.view.dialogs.SelectColumnsDlgCtrlr.TypeCheckBox;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;

public final class ResultsTable extends HasRightClickableRows<ResultsRow>
{
  private final TableView<ResultsRow> tv;
  private boolean datesAdded = false;
  public static final ArrayList<ColumnGroup> colGroups = new ArrayList<>();
  private static ColumnGroup generalGroup;

  public TableView<ResultsRow> getTV() { return tv; }

//---------------------------------------------------------------------------

  public static final class ColumnGroupItem
  {
    private ColumnGroupItem(Tag tag, String caption)
    {
      this.tag = tag;
      this.caption = caption;
    }

    private final Tag tag;
    public final String caption;
    public TableColumn<ResultsRow, ? extends ResultCellValue<? extends Comparable<?>>> col;
  }

//---------------------------------------------------------------------------

  public static final class ColumnGroup
  {
    public final String caption;
    public final ArrayList<ColumnGroupItem> items = new ArrayList<>();
    public TypeCheckBox checkBox;

    public ColumnGroup(String caption, Set<Tag> tags)
    {
      this.caption = caption;

      tags.forEach(tag -> items.add(new ColumnGroupItem(tag, db.getTagHeader(tag))));
    }

    public <Comp_T extends Comparable<Comp_T>> void setColumns(TableColumn<ResultsRow, ResultCellValue<Comp_T>> col, Tag tag)
    {
      items.forEach(item -> {
        if (item.tag == tag)
          item.col = col; });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final class ResultCellValue<Comp_T extends Comparable<Comp_T>> implements Comparable<ResultCellValue<Comp_T>>
  {
    private final String text;
    private final Comparable<Comp_T> sortVal;
    private final Function<String, Comp_T> strToComp;

    ResultCellValue(String text, Comparable<Comp_T> sortVal)
    {
      this.text = text;
      this.sortVal = sortVal;
      strToComp = null;
    }

    ResultCellValue(String text, Function<String, Comp_T> strToComp)
    {
      this.text = text;
      this.strToComp = strToComp;
      sortVal = null;
    }

    private ObservableValue<ResultCellValue<Comp_T>> getObservable() { return new SimpleObjectProperty<>(this); }

    @Override public String toString() { return text; }

    @SuppressWarnings("unchecked")
    @Override public int compareTo(ResultCellValue<Comp_T> other)
    {
      if (strToComp != null)
        return strToComp.apply(text).compareTo(other.strToComp.apply(other.text));

      return sortVal.compareTo((Comp_T) other.sortVal);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ResultsTable(TableView<ResultsRow> tvResults)
  {
    tv = tvResults;

    tv.setItems(FXCollections.observableArrayList());

    tv.setPlaceholder(new Label("There are no query results to display."));
    tv.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

    initColumns();

    tv.setRowFactory(theTV ->
    {
      final TableRow<ResultsRow> row = new TableRow<>();

      row.setOnMouseClicked(mouseEvent ->
      {
        if ((mouseEvent.getButton().equals(MouseButton.PRIMARY)) && (mouseEvent.getClickCount() == 2))
          dblClick(row.getItem());
      });

      row.itemProperty().addListener((observable, oldValue, newValue) ->
      {
        if (newValue == null)
          row.setContextMenu(null);
        else
          row.setContextMenu(createContextMenu(newValue));
      });

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void dblClick(ResultsRow row)
  {
    if (row != null)
      ui.goToRecord(row.getRecord(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    colGroups.clear();
    tv.setContextMenu(null);

    initColumns();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <Comp_T extends Comparable<Comp_T>> ObservableValue<ResultCellValue<Comp_T>> getCustomCellValue(String str, Function<String, Comp_T> strToComp)
  {
    return new ResultCellValue<>(str, strToComp).getObservable();
  }

  private static final double RESULT_COL_MAX_WIDTH = 400.0;

  private void initColumns()
  {
    datesAdded = false;
    generalGroup = new ColumnGroup("General", EnumSet.noneOf(Tag.class));
    ColumnGroupItem item;

    colGroups.add(generalGroup);

    TableColumn<ResultsRow, ResultCellValue<Integer>> intCol;
    TableColumn<ResultsRow, ResultCellValue<String>> strCol;

    intCol = new TableColumn<>("ID");

    intCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordIDStr(), str -> Integer.valueOf(parseInt(str, -1))));
    intCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(intCol);

    item = new ColumnGroupItem(tagNone, "ID");
    generalGroup.items.add(item);
    item.col = intCol;

    strCol = new TableColumn<>("Name");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordName(), str -> makeSortKeyByType(str, hdtWork)));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);

    item = new ColumnGroupItem(tagNone, "Name");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new TableColumn<>("Type");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordTypeStr(), str -> str.trim().toLowerCase()));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);

    item = new ColumnGroupItem(tagNone, "Type");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new TableColumn<>("Search Key");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getSearchKey(), str -> str.trim().toLowerCase()));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);
    strCol.setVisible(false);

    item = new ColumnGroupItem(tagNone, "Search Key");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new TableColumn<>("Sort Key");
    strCol.setCellValueFactory(cellData ->
    {
      String sortKey = cellData.getValue().getSortKey();
      return new ResultCellValue<>(sortKey, sortKey).getObservable();
    });

    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);
    strCol.setVisible(false);

    item = new ColumnGroupItem(tagNone, "Sort Key");
    generalGroup.items.add(item);
    item.col = strCol;

    Node showHideColumnsButton = tv.lookup(".show-hide-columns-button");

    if (showHideColumnsButton != null)
      showHideColumnsButton.addEventFilter(MouseEvent.MOUSE_PRESSED, event ->
      {
        SelectColumnsDlgCtrlr.create("Select Columns").showModal();
        event.consume();
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDateColumns()
  {
    if (datesAdded) return;
    datesAdded = true;

    Platform.runLater(() ->
    {
      TableColumn<ResultsRow, ResultCellValue<Instant>> col;
      ColumnGroupItem item;

      col = new TableColumn<>("Date created");
      col.setCellValueFactory(cellData -> cellData.getValue().getCreationDateCellValue().getObservable());
      tv.getColumns().add(col);

      item = new ColumnGroupItem(tagNone, "Date created");
      generalGroup.items.add(item);
      item.col = col;

      col = new TableColumn<>("Date modified");
      col.setCellValueFactory(cellData -> cellData.getValue().getModifiedDateCellValue().getObservable());
      tv.getColumns().add(col);

      item = new ColumnGroupItem(tagNone, "Date modified");
      generalGroup.items.add(item);
      item.col = col;

      col = new TableColumn<>("Date accessed");
      col.setCellValueFactory(cellData -> cellData.getValue().getViewDateCellValue().getObservable());
      tv.getColumns().add(col);

      item = new ColumnGroupItem(tagNone, "Date accessed");
      generalGroup.items.add(item);
      item.col = col;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TableColumn<ResultsRow, ResultCellValue<String>> addTagColumn(Tag tag)
  {
    TableColumn<ResultsRow, ResultCellValue<String>> col = new TableColumn<>(db.getTagHeader(tag));
    Function<String, String> strToComp;

    if (tag == tagTitle)
      strToComp = str -> makeSortKeyByType(str, hdtWork);
    else
      strToComp = str -> str.trim().toLowerCase();

    col.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getTagText(tag), strToComp));

    col.setMaxWidth(RESULT_COL_MAX_WIDTH);

    tv.getColumns().add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Record selectedRecord()
  {
    return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, ResultsRow::getRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
