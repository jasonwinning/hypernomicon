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

package org.hypernomicon.tree;

import java.util.List;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.view.wrappers.DragNDropContainer;

import javafx.application.Platform;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

public abstract class AbstractTreeWrapper<RowType extends AbstractTreeRow<? extends HDT_Record, RowType>> extends DragNDropContainer<RowType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean selectingFromCB = false;

  private static final int MAX_SELECT_ATTEMPTS = 100;  // safety cap so the showItem/select retry loop in selectRecord can never hang the FX thread

  protected abstract RowType newRow(HDT_Record rootRecord, TreeModel<RowType> treeModel);
  protected abstract TreeItem<RowType> getRoot();
  protected abstract SelectionModel<TreeItem<RowType>> getSelectionModel();
  protected abstract void scrollToNdx(int ndx);
  protected abstract List<RowType> getRowsForRecord(HDT_Record record); // should never return null
  protected abstract void expandMainBranches();

//---------------------------------------------------------------------------

  public final TreeItem<RowType> selectedItem() { return getSelectionModel().getSelectedItem(); }
  public final HDT_Record selectedRecord()      { return nullSwitch(selectedItem(), null, treeItem -> nullSwitch(treeItem.getValue(), null, RowType::getRecord)); }

  protected final void selectRow(TreeRow row, boolean fromCB) { selectRecord(row.getRecord(), getRowsForRecord(row.getRecord()).indexOf(row), fromCB); }
  protected TreeItem<RowType> getTreeItem(RowType treeRow)    { return treeRow.getTreeItem(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void selectRecord(HDT_Record record, int ndx, boolean fromCB)
  {
    if (record == null) return;

    if (ndx < 0)
    {
      TreeItem<RowType> selItem = getSelectionModel().getSelectedItem();

      if ((selItem != null) && (selItem.getValue().getRecord() == record))
      {
        safeFocus(getControl());
        return;
      }

      ndx = 0;
    }

    List<RowType> list = getRowsForRecord(record);
    if (list.isEmpty()) return;

    if (list.size() <= ndx) ndx = 0;

    if (fromCB)
      selectingFromCB = true;

    try
    {
      TreeItem<RowType> item = getTreeItem(list.get(ndx));

      showItem(item);

      if (getSelectionModel().getSelectedItem() != item)
      {
        getSelectionModel().clearSelection();

        // It takes a while for the "showItem" operation to work. Before it's done, "select(item)" will instead
        // select a *different* item (?!?!), so we retry. The attempt cap ensures this can never become an
        // infinite loop that freezes the FX thread (e.g. if the item never becomes selectable).

        for (int attempt = 0; (getSelectionModel().getSelectedItem() != item) && (attempt < MAX_SELECT_ATTEMPTS); attempt++)
          getSelectionModel().select(item);
      }

      getControl().layout();

      Platform.runLater(() ->
      {
        scrollToNdx(getSelectionModel().getSelectedIndex());

        safeFocus(getControl());

        selectingFromCB = false;
      });
    }
    catch (RuntimeException e)
    {
      selectingFromCB = false;  // make sure the flag is cleared even if selection fails; otherwise rowSelectionChanged
      throw e;                  // would stop syncing the combo box to the tree selection for the rest of the session
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showItem(TreeItem<RowType> item)
  {
    nullSwitch(item.getParent(), this::showItem);
    item.setExpanded(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
