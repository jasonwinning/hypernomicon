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

package org.hypernomicon.dialogs;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.util.List;
import java.util.function.Function;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.OmniFinder;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.fxml.FXML;
import javafx.scene.control.TableView;
import javafx.scene.text.Text;

public class RecordSelectDlgCtrlr extends HyperDlg
{
  @FXML private TableView<HyperTableRow> tvFind;

  private OmniFinder omniFinder;
  private HyperTable htFind;

  public <HDT_T extends HDT_Record> HDT_T getRecord() { return htFind.selectedRecord(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RecordSelectDlgCtrlr build(Populator populator, List<HyperTableCell> list, String queryStr)
  {
    return ((RecordSelectDlgCtrlr) create("RecordSelectDlg.fxml", "Choose a Record", true)).init(list, populator, queryStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RecordSelectDlgCtrlr init(List<HyperTableCell> list, Populator populator, String queryStr)
  {
    if (collEmpty(list)) return this;
    HDT_RecordType objType = HyperTableCell.getCellType(list.get(0));

    htFind = new HyperTable(tvFind, 1, false, ""); htFind.disableRefreshAfterCellUpdate = true;

    htFind.addIconCol();
    htFind.addCol(hdtNone, ctIncremental);
    htFind.addCol(hdtNone, ctNone);
    htFind.addCol(hdtNone, ctNone);

    htFind.setDblClickHandler(HDT_Record.class, work -> btnOkClick());

    omniFinder = new OmniFinder(htFind, objType, false);

    tvFind.setPlaceholder(new Text("Searching..."));

    Runnable runnable = () ->
    {
      omniFinder.doneHndlr = null;

      if (omniFinder.noResults())
      {
        Function<HyperTableCell, HDT_Record> function = cell -> cell.getRecord();
        omniFinder.setSourceAndStart(list.stream().map(function).iterator(), true);
      }
    };

    if ((populator instanceof StandardPopulator) || (populator instanceof RecordByTypePopulator))
    {
      omniFinder.doneHndlr = runnable;
      omniFinder.setQueryAndStart(queryStr, true);
    }
    else
    {
      Function<HyperTableCell, HDT_Record> function = cell -> cell.getRecord();
      omniFinder.setSourceAndStart(list.stream().map(function).iterator(), true);
    }

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid()
  {
    return htFind.selectedRecord() != null ? true : falseWithWarningMessage("Select a record.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
