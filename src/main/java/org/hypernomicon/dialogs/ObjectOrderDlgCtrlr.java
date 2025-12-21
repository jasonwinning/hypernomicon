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

package org.hypernomicon.dialogs;

import static org.hypernomicon.util.UIUtil.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

//---------------------------------------------------------------------------

public class ObjectOrderDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnMoveUp, btnMoveDown;
  @FXML private TableView<HyperTableRow> tv;

  private final List<HyperTableRow> rows;

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------

  public ObjectOrderDlgCtrlr(HyperTable ht, List<HyperTableRow> rows)
  {
    super("ObjectOrderDlg", "Change Order of Rows", true);

    this.rows = rows;
    tv.getColumns().clear();

    Collection<TableColumn<HyperTableRow, ?>> tableCols = new ArrayList<>();

    ht.getColumns().forEach(hyperTableColumn -> { switch (hyperTableColumn.getCtrlType())
    {
      case ctEditableUnlimitedDropDown: case ctNoneditableDropDown: case ctEditableLimitedDropDown: case ctEdit: case ctNone:

        TableColumn<HyperTableRow, String> tableColumn = new TableColumn<>();

        tableColumn.setText(hyperTableColumn.getHeader());
        tableColumn.setSortable(false);
        tableColumn.setEditable(false);

        tableColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getText(hyperTableColumn.getColNdx())));

        tv.getColumns().add(tableColumn);
        tableCols.add(tableColumn);

        break;

      default: break;
    }});

    tv.itemsProperty().bindBidirectional(ht.getTV().itemsProperty());

    HyperTable.preventMovingColumns(tv, tableCols);

    stage.setOnHidden(event -> tv.itemsProperty().unbindBidirectional(ht.getTV().itemsProperty()));

    btnMoveUp  .setOnAction(event -> moveUp  ());
    btnMoveDown.setOnAction(event -> moveDown());

    setToolTip(btnMoveUp  , "Move Up"  );
    setToolTip(btnMoveDown, "Move Down");

    tv.getSelectionModel().selectFirst();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveUp()
  {
    int ndx = tv.getSelectionModel().getSelectedIndex();
    if (ndx < 1) return;

    HyperTableRow row = rows.remove(ndx);
    rows.add(ndx - 1, row);
    tv.getSelectionModel().select(row);
    safeFocus(tv);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void moveDown()
  {
    int ndx = tv.getSelectionModel().getSelectedIndex();
    if (ndx < 0) return;
    if (ndx == (tv.getItems().size() - 1)) return;

    HyperTableRow row = rows.remove(ndx);
    rows.add(ndx + 1, row);
    tv.getSelectionModel().select(row);
    safeFocus(tv);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
