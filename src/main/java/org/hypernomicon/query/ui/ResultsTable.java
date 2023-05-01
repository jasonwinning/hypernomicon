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

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.ui.ColumnGroupItem.NonGeneralColumnGroupItem;
import org.hypernomicon.view.wrappers.HasRightClickableRows;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.query.ui.ResultColumn.*;
import static org.hypernomicon.util.Util.*;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.scene.control.Label;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.stage.Window;

final class ResultsTable extends HasRightClickableRows<ResultRow>
{

//---------------------------------------------------------------------------

  private final TableView<ResultRow> tv;
  private boolean datesAdded = false;
  final static Map<RecordType, AbstractColumnGroup<? extends ColumnGroupItem>> colGroups = new LinkedHashMap<>();
  private static ColumnGroup generalGroup;

  public HDT_Record selectedRecord()  { return nullSwitch(tv.getSelectionModel().getSelectedItem(), null, ResultRow::getRecord); }

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

  void reset()
  {
    tv.getColumns().clear();
    tv.getItems().clear();
    colGroups.clear();

    datesAdded = false;

    colGroups.put(hdtNone, generalGroup = new ColumnGroup("General", this));

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

  NonGeneralColumn addNonGeneralColumn(EnumMap<RecordType, NonGeneralColumnGroupItem> recordTypeToItem)
  {
    NonGeneralColumnGroupItem firstItem = recordTypeToItem.entrySet().iterator().next().getValue();

    NonGeneralColumn col = NonGeneralColumn.create(firstItem, recordTypeToItem);

    addColumn(col, EnumSet.of(tagAuthor, tagYear, tagWorkType, tagMainText).contains(firstItem.tag));

    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addColumn(ResultColumn newCol, boolean addToFront)
  {
    if (addToFront == false)
    {
      tv.getColumns().add(newCol);
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

    tv.getColumns().add(colNdx, newCol);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
