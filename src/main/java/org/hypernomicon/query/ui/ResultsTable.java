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

package org.hypernomicon.query.ui;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.ui.ColumnGroupItem.NonGeneralColumnGroupItem;
import org.hypernomicon.view.wrappers.HasRightClickableRows;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.Multimap;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.query.ui.ResultColumn.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

//---------------------------------------------------------------------------

final class ResultsTable extends HasRightClickableRows<ResultRow>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final TableView<ResultRow> tv;
  private boolean datesAdded = false;
  static final Multimap<RecordType, AbstractColumnGroup<? extends ColumnGroupItem>> recordTypeToColumnGroups = LinkedHashMultimap.create();  // Has to be a multimap because there are two ColumnGroups for hdtWork
  private static ColumnGroup generalGroup;

//---------------------------------------------------------------------------

  ResultsTable(TableView<ResultRow> tvResults)
  {
    tv = tvResults;

    tv.setItems(FXCollections.observableArrayList());

    tv.setPlaceholder(new Label("There are no query results to display."));
    tv.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

    reset();

    tv.setRowFactory(theTV ->
    {
      final TableRow<ResultRow> row = new TableRow<>();

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

  /**
   * Similar to OneTouchExpandableWrapper.addOneTouchExpansion
   */
  void reset()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    recordTypeToColumnGroups.clear();

    datesAdded = false;

    recordTypeToColumnGroups.put(hdtNone, generalGroup = new ColumnGroup("General", this));

    generalGroup.addColumn(new RecordIDColumn  ());
    generalGroup.addColumn(new RecordNameColumn());
    generalGroup.addColumn(new RecordTypeColumn());
    generalGroup.addColumn(new SearchKeyColumn ());
    generalGroup.addColumn(new SortKeyColumn   ());

//---------------------------------------------------------------------------

    if (commencedAddingButton) return;

    commencedAddingButton = true;

    Thread thread = new HyperThread("ButtonAdder")
    {
      @Override public void run()
      {
        boolean buttonNotAdded;
        int sleepTotal = 0;

        synchronized (buttonAdded) { buttonNotAdded = buttonAdded.isFalse(); }

        while (buttonNotAdded && (sleepTotal < 1000))
        {
          runInFXThread(() ->
          {
            synchronized (buttonAdded)
            {
              Scene scene = tv.getScene();
              if (scene == null) return;

              Window window = scene.getWindow();
              if ((window == null) || (window.isShowing() == false)) return;

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
          {
            sleepForMillis(50);
            sleepTotal += 50;
          }
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

  void addDateColumns()
  {
    if (datesAdded) return;
    datesAdded = true;

    Platform.runLater(() ->
    {
      generalGroup.addColumn(new DateColumn(tagCreationDate.header, HDT_Record::getCreationDate), true);
      generalGroup.addColumn(new DateColumn(tagModifiedDate.header, HDT_Record::getModifiedDate), true);
      generalGroup.addColumn(new DateColumn(tagViewDate    .header, HDT_Record::getViewDate    ), true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  NonGeneralColumn addNonGeneralColumn(EnumMap<RecordType, NonGeneralColumnGroupItem> recordTypeToItem, EnumSet<RelationType> relationsToShow)
  {
    NonGeneralColumnGroupItem firstItem = recordTypeToItem.entrySet().iterator().next().getValue();

    NonGeneralColumn col = NonGeneralColumn.create(firstItem, recordTypeToItem, relationsToShow);

    addColumn(col, EnumSet.of(tagAuthor, tagBibDate, tagWorkType, tagMainText).contains(firstItem.tag));

    for (ColumnGroupItem otherItem : recordTypeToItem.values())
      otherItem.col = col;

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addColumn(ResultColumn newCol, boolean addToFront)
  {
    if (addToFront == false)
    {
      tv.getColumns().add(newCol);
      addCountColIfNeeded(newCol);
      return;
    }

    // In order to add column to front of non-general columns, determine first non-general column index

    List<TableColumn<ResultRow, ?>> columns = tv.getColumns();
    int numColumns = columns.size();
    int colNdx = numColumns;

    for (int ndx = 0; ndx < numColumns; ndx++)
    {
      TableColumn<ResultRow, ?> col = columns.get(ndx);

      if (generalGroup.stream().noneMatch(item -> item.col == col))
      {
        colNdx = ndx;
        break;
      }
    }

    columns.add(colNdx, newCol);

    addCountColIfNeeded(newCol);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a count column to the right of the specified target column if needed.
   * <p>
   * Some result columns display a list of items within a single cell. For such columns,
   * this method adds a count column immediately to the right of the target column,
   * which shows the count of items in each cell. The need for a count column is determined
   * based on specific criteria related to the type of the target column.
   * </p>
   *
   * @param targetCol The {@link ResultColumn} for which a count column may be needed.
   *                  If a count column is already associated with the target column,
   *                  this method does nothing.
   */
  void addCountColIfNeeded(ResultColumn targetCol)
  {
    if (targetCol.countCol != null)
      return;

    boolean needCountCol = false;

    if (targetCol instanceof NonGeneralColumn ngCol)
    {
      Iterator<Entry<RecordType, NonGeneralColumnGroupItem>> it = ngCol.map.entrySet().iterator();

      while ((needCountCol == false) && it.hasNext())
      {
        Entry<RecordType, NonGeneralColumnGroupItem> entry = it.next();
        NonGeneralColumnGroupItem item = entry.getValue();

        if ((item.relType != RelationType.rtNone) || (item.tag == tagKeyWork) || (item.tag == tagDisplayRecord) || (item.tag == tagISBN))
          needCountCol = true;
        else
        {
          HDI_Schema schema = db.getSchema(entry.getKey(), item.tag);

          if (schema != null)
          {
            switch (schema.category())
            {
              case hdcAuthors: case hdcPointerMulti:
                needCountCol = true;
                break;

              default:
                break;
            }
          }
        }
      }
    }
    else if (targetCol instanceof BibFieldColumn bfCol)
    {
      switch (bfCol.field.getType())
      {
        case bftAuthor: case bftMultiString:
          needCountCol = true;
          break;

        default:
          break;
      }
    }

    if (needCountCol == false)
      return;

    targetCol.countCol = new CountColumn(targetCol);
    List<TableColumn<ResultRow, ?>> columns = tv.getColumns();
    columns.add(columns.indexOf(targetCol) + 1, targetCol.countCol);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
