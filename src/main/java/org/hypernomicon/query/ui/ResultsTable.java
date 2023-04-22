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

package org.hypernomicon.query.ui;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.commons.lang3.mutable.MutableInt;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HDI_OnlinePointerMulti;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.wrappers.HasRightClickableRows;
import org.hypernomicon.view.wrappers.HyperTableCell;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.ui.ResultsTable.ResultCellValue.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableColumn.CellDataFeatures;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

final class ResultsTable extends HasRightClickableRows<ResultsRow>
{
  private final TableView<ResultsRow> tv;
  private boolean datesAdded = false;
  static final List<ColumnGroup> colGroups = new ArrayList<>();
  private static ColumnGroup generalGroup;

  public TableView<ResultsRow> getTV() { return tv; }
  public HDT_Record selectedRecord()   { return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, ResultsRow::getRecord); }

//---------------------------------------------------------------------------

  static final class ResultColumn extends TableColumn<ResultsRow, ResultCellValue>
  {
    ResultColumn(String caption)
    {
      this(caption, false);
    }

    private ResultColumn(String caption, boolean caseSensitive)
    {
      super(caption);

      setComparator((cell1, cell2) ->
      {
        if ((cell1 == null) && (cell2 == null)) return 0;
        if (cell1 == null) return -1;
        if (cell2 == null) return 1;

        if (caseSensitive)
          return safeStr(cell1.text).trim().compareTo(safeStr(cell2.text).trim());

        return safeStr(cell1.text).trim().compareToIgnoreCase(safeStr(cell2.text).trim());
      });
    }

    private ResultColumn(String caption, Comparator<HDT_Record> comparator)
    {
      this(caption, comparator, false);
    }

    private ResultColumn(String caption, Comparator<HDT_Record> comparator, boolean caseSensitive)
    {
      super(caption);

      setComparator((cell1, cell2) ->
      {
        if ((cell1 == null) && (cell2 == null)) return 0;
        if (cell1 == null) return -1;
        if (cell2 == null) return 1;

        if (HDT_Record.isEmpty(cell1.record) || HDT_Record.isEmpty(cell2.record))
        {
          if (caseSensitive)
            return safeStr(cell1.text).trim().compareTo(safeStr(cell2.text).trim());

          return safeStr(cell1.text).trim().compareToIgnoreCase(safeStr(cell2.text).trim());
        }

        return comparator.compare(cell1.record, cell2.record);
      });
    }

    private ResultColumn(String caption, Function<String, String> sortKeyFunction)
    {
      this(caption, sortKeyFunction, false);
    }

    private ResultColumn(String caption, Function<String, String> sortKeyFunction, boolean caseSensitive)
    {
      super(caption);

      setComparator((cell1, cell2) ->
      {
        if ((cell1 == null) && (cell2 == null)) return 0;
        if (cell1 == null) return -1;
        if (cell2 == null) return 1;

        if (caseSensitive)
          return sortKeyFunction.apply(safeStr(cell1.text).trim()).compareTo(sortKeyFunction.apply(safeStr(cell2.text).trim()));

        return sortKeyFunction.apply(safeStr(cell1.text).trim()).compareToIgnoreCase(sortKeyFunction.apply(safeStr(cell2.text).trim()));
      });
    }

    /**
     *
     * @param caption column header name
     * @param comparator how to compare the display text of the cells
     * @param klass is just there to disambiguate from the other constructor that takes a comparator; it doesn't do anything
     */
    private ResultColumn(String caption, Comparator<String> comparator, Class<String> klass)
    {
      super(caption);

      setComparator((cell1, cell2) ->
      {
        if ((cell1 == null) && (cell2 == null)) return 0;
        if (cell1 == null) return -1;
        if (cell2 == null) return 1;

        return comparator.compare(safeStr(cell1.text).trim(), safeStr(cell2.text).trim());
      });
    }

    final EnumMap<RecordType, ColumnGroupItem> map = new EnumMap<>(RecordType.class);
  }

//---------------------------------------------------------------------------

  static final class ResultCellValue
  {
    private final String text;
    private final HDT_Record record;

    private ResultCellValue(String text, HDT_Record record)
    {
      this.text = text;
      this.record = record;
    }

    private ObservableValue<ResultCellValue> getObservable() { return new SimpleObjectProperty<>(this); }

    @Override public String toString() { return text; }

    @Override public int hashCode()
    {
      return Objects.hash(safeStr(text).toLowerCase(), record);
    }

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;

      ResultCellValue other = (ResultCellValue)obj;

      if (record != other.record) return false;
      return safeStr(text).trim().equalsIgnoreCase(safeStr(other.text).trim());
    }

    static ObservableValue<ResultCellValue> getObservableCellValue(CellDataFeatures<ResultsRow, ResultCellValue> cellData, String str)
    {
      return new ResultCellValue(str, cellData.getValue().getRecord()).getObservable();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ResultsTable(TableView<ResultsRow> tvResults)
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

      row.itemProperty().addListener((ob, ov, nv) -> row.setContextMenu(createContextMenu(nv)));

      return row;
    });

    addDefaultMenuItems();

    addContextMenuItem("Remove from query results", HDT_Record.class, record ->
      tv.getItems().removeAll(List.copyOf(tv.getSelectionModel().getSelectedItems())));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    colGroups.clear();

    datesAdded = false;

    colGroups.add(generalGroup = new ColumnGroup("General"));

    ResultColumn intCol = new ResultColumn("ID", Comparator.comparingInt(HDT_Record::getID));
    intCol.setCellValueFactory(cellData -> getObservableCellValue(cellData, cellData.getValue().getRecordIDStr()));
    generalGroup.add(new ColumnGroupItem(intCol, tv, -1));

    ResultColumn strCol = new ResultColumn("Name", str -> makeSortKeyByType(str, hdtWork));
    strCol.setCellValueFactory(cellData -> getObservableCellValue(cellData, cellData.getValue().getRecordName()));
    generalGroup.add(new ColumnGroupItem(strCol, tv, -1));

    strCol = new ResultColumn("Type");
    strCol.setCellValueFactory(cellData -> getObservableCellValue(cellData, cellData.getValue().getRecordTypeStr()));
    generalGroup.add(new ColumnGroupItem(strCol, tv, -1));

    strCol = new ResultColumn("Search Key");
    strCol.setCellValueFactory(cellData -> getObservableCellValue(cellData, cellData.getValue().getSearchKey()));
    strCol.setVisible(false);
    generalGroup.add(new ColumnGroupItem(strCol, tv, -1));

    strCol = new ResultColumn("Sort Key", true);
    strCol.setCellValueFactory(cellData -> getObservableCellValue(cellData, cellData.getValue().getSortKey()));

    strCol.setVisible(false);
    generalGroup.add(new ColumnGroupItem(strCol, tv, -1));

    if (commencedAddingButton) return;

    commencedAddingButton = true;

    Thread thread = new HyperThread("ButtonAdder")
    {
      @Override public void run()
      {
        boolean buttonNotAdded;

        synchronized (buttonAdded) { buttonNotAdded = buttonAdded.isFalse(); }

        while (buttonNotAdded)
        {
          runInFXThread(() ->
          {
            synchronized (buttonAdded)
            {
              buttonAdded.setValue(buttonAdded.booleanValue() || !nullSwitch(tv.getScene(), false, scene ->
                                                                  nullSwitch(scene.getWindow(), false, Window::isShowing)));
              if (buttonAdded.isTrue()) return;

              nullSwitch(tv.lookup(".show-hide-columns-button"), showHideColumnsButton ->
              {
                buttonAdded.setTrue();

                showHideColumnsButton.addEventFilter(MouseEvent.MOUSE_PRESSED, event ->
                {
                  new SelectColumnsDlgCtrlr().showModal();
                  event.consume();
                });
              });
            }
          }, true);

          synchronized (buttonAdded) { buttonNotAdded = buttonAdded.isFalse(); }

          if (buttonNotAdded)
            sleepForMillis(50);
        }
      }
    };

    thread.setDaemon(true);
    thread.start();
  }

  private final MutableBoolean buttonAdded = new MutableBoolean(false);
  private boolean commencedAddingButton = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addDateColumn(Tag dateTag)
  {
    Comparator<HDT_Record> comparator = null;

    switch (dateTag)
    {
      case tagCreationDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getCreationDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getCreationDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      case tagModifiedDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getModifiedDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getModifiedDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      case tagViewDate :

        comparator = (record1, record2) ->
        {
          Instant i1 = nullSwitch(record1.getViewDate(), Instant.MIN);
          Instant i2 = nullSwitch(record2.getViewDate(), Instant.MIN);
          return i1.compareTo(i2);
        };

        break;

      default : break;
    }

    ResultColumn col = new ResultColumn(dateTag.header, comparator);

    col.setCellValueFactory(cellData ->
    {
      Instant i = null;
      HDT_Record record = cellData.getValue().getRecord();

      if ((record != null) && (record.getType() != hdtNone)) switch (dateTag)
      {
        case tagCreationDate : i = record.getCreationDate(); break;
        case tagModifiedDate : i = record.getModifiedDate(); break;
        case tagViewDate     : i = record.getViewDate    (); break;
        default              :                               break;
      }

      return getObservableCellValue(cellData, i == null ? "" : dateTimeToUserReadableStr(i));
    });

    generalGroup.add(new ColumnGroupItem(col, tv, firstNonGeneralColumnNdx()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addDateColumns()
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

  ResultColumn addNonGeneralColumn(EnumMap<RecordType, ColumnGroupItem> recordTypeToItem)
  {
    ColumnGroupItem firstItem = recordTypeToItem.entrySet().iterator().next().getValue();

    ResultColumn col;

    switch (firstItem.tag)
    {
      case tagTitle :

        col = new ResultColumn(firstItem.caption, str -> makeSortKeyByType(str, hdtWork));
        break;

      case tagYear :

        Comparator<String> strComparator = (str1, str2) ->
        {
          MutableInt result = new MutableInt();

          if (HyperTableCell.compareNumberStrings(str1, str2, result))
            return result.getValue();

          return str1.compareToIgnoreCase(str2);
        };

        col = new ResultColumn(firstItem.caption, strComparator, String.class);
        break;

      default :

        col = new ResultColumn(firstItem.caption);
        break;
    }

    boolean visible = false;
    for (ColumnGroupItem item : recordTypeToItem.values())
      if (item.relType == RelationType.rtNone)
      {
        visible = true;
        break;
      }

    col.setVisible(visible);

    col.setCellValueFactory(cellData ->
    {
      String str = "";
      HDT_Record record = cellData.getValue().getRecord();

      if (record != null)
      {
        ColumnGroupItem item = recordTypeToItem.get(record.getType());

        if (item != null)
        {
          str = item.relType != RelationType.rtNone ?
            HDI_OnlinePointerMulti.recordStreamResultText(db.getObjType(item.relType), db.getSubjectList(item.relType, record).stream())
          :
            record.resultTextForTag(item.tag);
        }
      }

      return getObservableCellValue(cellData, str);
    });

    col.setMaxWidth(ColumnGroupItem.RESULT_COL_MAX_WIDTH);

    if (EnumSet.of(tagAuthor, tagYear, tagWorkType, tagMainText).contains(firstItem.tag))
      tv.getColumns().add(firstNonGeneralColumnNdx(), col);
    else
      tv.getColumns().add(col);

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int firstNonGeneralColumnNdx()
  {
    List<TableColumn<ResultsRow, ?>> columns = tv.getColumns();
    int numColumns = columns.size();

    for (int ndx = 0; ndx < numColumns; ndx++)
    {
      TableColumn<ResultsRow, ?> col = columns.get(ndx);

      if (generalGroup.stream().noneMatch(item -> item.col == col))
        return ndx;
    }

    return numColumns;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
