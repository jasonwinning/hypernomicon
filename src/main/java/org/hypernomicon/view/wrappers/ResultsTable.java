/*
 * Copyright 2015-2020 Jason Winning
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
import java.util.EnumMap;
import java.util.List;
import java.util.Set;
import java.util.function.Function;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.model.relations.RelationSet.RelationType;
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
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

public final class ResultsTable extends HasRightClickableRows<ResultsRow>
{
  private final TableView<ResultsRow> tv;
  private boolean datesAdded = false;
  public static final List<ColumnGroup> colGroups = new ArrayList<>();
  private static ColumnGroup generalGroup;

  public TableView<ResultsRow> getTV() { return tv; }
  public HDT_Record selectedRecord()   { return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, ResultsRow::getRecord); }

//---------------------------------------------------------------------------

  public static class ResultColumn<Comp_T extends Comparable<Comp_T>> extends TableColumn<ResultsRow, ResultCellValue<Comp_T>>
  {
    public ResultColumn(String caption) { super(caption); }

    public final EnumMap<HDT_RecordType, ColumnGroupItem> map = new EnumMap<>(HDT_RecordType.class);
  }

//---------------------------------------------------------------------------

  public static final class ColumnGroupItem
  {
    private ColumnGroupItem(Tag tag)
    {
      this.tag = tag;
      caption = db.getTagHeader(tag);
      relType = RelationType.rtNone;
    }

    private ColumnGroupItem(String caption)
    {
      tag = tagNone;
      this.caption = caption;
      relType = RelationType.rtNone;
    }

    private ColumnGroupItem(RelationType relType)
    {
      this.relType = relType;
      tag = relType.getSubjTag();
      caption = relType.getSubjTitle();
    }

    public final Tag tag;
    public final RelationType relType; // If relType != rtNone, then this is a column showing subjects for the row record (the object)
    public final String caption;
    public ResultColumn<? extends Comparable<?>> col;
  }

//---------------------------------------------------------------------------

  public static final class ColumnGroup
  {
    public final HDT_RecordType recordType;
    public final String caption;
    public final List<ColumnGroupItem> items = new ArrayList<>();
    public TypeCheckBox checkBox;

    public ColumnGroup(HDT_RecordType recordType, Set<Tag> tags)
    {
      this.recordType = recordType;
      caption = db.getTypeName(recordType);

      tags.forEach(tag -> items.add(new ColumnGroupItem(tag)));

      RelationSet.getRelationsForObjType(recordType).forEach(relType -> items.add(new ColumnGroupItem(relType)));
    }

    public ColumnGroup(String caption)
    {
      this.caption = caption;
      recordType = hdtNone;
    }
  }

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
      return strToComp != null ?
        strToComp.apply(text).compareTo(other.strToComp.apply(other.text))
      :
        sortVal.compareTo((Comp_T) other.sortVal);
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

    reset();

    tv.setRowFactory(theTV ->
    {
      final TableRow<ResultsRow> row = new TableRow<>();

      row.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
          nullSwitch(row.getItem(), rowItem -> ui.goToRecord(rowItem.getRecord(), false));
      });

      row.itemProperty().addListener((ob, oldValue, newValue) -> row.setContextMenu(newValue == null ? null : createContextMenu(newValue)));

      return row;
    });

    addDefaultMenuItems();

    addContextMenuItem("Remove from query results", HDT_Record.class, record ->
    {
      new ArrayList<>(tv.getSelectionModel().getSelectedItems()).forEach(tv.getItems()::remove);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private <Comp_T extends Comparable<Comp_T>> ObservableValue<ResultCellValue<Comp_T>> getCustomCellValue(String str, Function<String, Comp_T> strToComp)
  {
    return new ResultCellValue<>(str, strToComp).getObservable();
  }

  private static final double RESULT_COL_MAX_WIDTH = 400.0;

  public void reset()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    colGroups.clear();

    datesAdded = false;
    generalGroup = new ColumnGroup("General");
    ColumnGroupItem item;

    colGroups.add(generalGroup);

    ResultColumn<Integer> intCol;
    ResultColumn<String> strCol;

    intCol = new ResultColumn<>("ID");
    intCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordIDStr(), str -> Integer.valueOf(parseInt(str, -1))));
    intCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(intCol);

    item = new ColumnGroupItem("ID");
    generalGroup.items.add(item);
    item.col = intCol;

    strCol = new ResultColumn<>("Name");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordName(), str -> makeSortKeyByType(str, hdtWork)));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);

    item = new ColumnGroupItem("Name");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new ResultColumn<>("Type");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getRecordTypeStr(), str -> str.trim().toLowerCase()));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);

    item = new ColumnGroupItem("Type");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new ResultColumn<>("Search Key");
    strCol.setCellValueFactory(cellData -> getCustomCellValue(cellData.getValue().getSearchKey(), str -> str.trim().toLowerCase()));
    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);
    strCol.setVisible(false);

    item = new ColumnGroupItem("Search Key");
    generalGroup.items.add(item);
    item.col = strCol;

    strCol = new ResultColumn<>("Sort Key");
    strCol.setCellValueFactory(cellData ->
    {
      String sortKey = cellData.getValue().getSortKey();
      return new ResultCellValue<>(sortKey, sortKey).getObservable();
    });

    strCol.setMaxWidth(RESULT_COL_MAX_WIDTH);
    tv.getColumns().add(strCol);
    strCol.setVisible(false);

    item = new ColumnGroupItem("Sort Key");
    generalGroup.items.add(item);
    item.col = strCol;

    if (commencedAddingButton) return;

    commencedAddingButton = true;

    Thread thread = new HyperThread("ButtonAdder")
    {
      @Override public void run()
      {
        while (buttonAdded == false)
        {
          runInFXThread(() ->
          {
            buttonAdded = buttonAdded || !nullSwitch(tv.getScene(), false, scene ->
                                          nullSwitch(scene.getWindow(), false, Window::isShowing));
            if (buttonAdded) return;

            nullSwitch(tv.lookup(".show-hide-columns-button"), showHideColumnsButton ->
            {
              buttonAdded = true;

              showHideColumnsButton.addEventFilter(MouseEvent.MOUSE_PRESSED, event ->
              {
                SelectColumnsDlgCtrlr.build().showModal();
                event.consume();
              });
            });
          });

          sleepForMillis(50);
        }
      }
    };

    thread.setDaemon(true);
    thread.start();
  }

  private boolean buttonAdded = false, commencedAddingButton = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addDateColumn(Tag dateTag)
  {
    String header = db.getTagHeader(dateTag);

    ResultColumn<Instant> col = new ResultColumn<>(header);
    col.setCellValueFactory(cellData -> cellData.getValue().getDateCellValue(dateTag).getObservable());
    tv.getColumns().add(col);

    ColumnGroupItem item = new ColumnGroupItem(header);
    generalGroup.items.add(item);
    item.col = col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addDateColumns()
  {
    if (datesAdded) return;
    datesAdded = true;

    Platform.runLater(() ->
    {
      addDateColumn(tagCreationDate);
      addDateColumn(tagModifiedDate);
      addDateColumn(tagViewDate);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ResultColumn<String> addNonGeneralColumn(EnumMap<HDT_RecordType, ColumnGroupItem> map)
  {
    ColumnGroupItem firstItem = map.entrySet().iterator().next().getValue();

    ResultColumn<String> col = new ResultColumn<>(firstItem.caption);

    Function<String, String> strToComp = firstItem.tag == tagTitle ?
      str -> makeSortKeyByType(str, hdtWork)
    :
      str -> str.trim().toLowerCase();

    boolean visible = false;
    for (ColumnGroupItem item : map.values())
      if (item.relType == RelationType.rtNone)
        visible = true;

    col.setVisible(visible);

    col.setCellValueFactory(cellData ->
    {
      String str = "";
      HDT_Record record = cellData.getValue().getRecord();

      if (record != null)
      {
        ColumnGroupItem item = map.get(record.getType());

        if (item != null)
        {
          if (item.relType != RelationType.rtNone)
          {
            str = db.getSubjectList(item.relType, record).stream().map(HDT_Record::listName)
                                                                  .filter(oneStr -> oneStr.length() > 0)
                                                                  .limit(20)
                                                                  .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
          }
          else
            str = record.getResultTextForTag(item.tag);
        }
      }

      return getCustomCellValue(str, strToComp);
    });

    col.setMaxWidth(RESULT_COL_MAX_WIDTH);

    tv.getColumns().add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
