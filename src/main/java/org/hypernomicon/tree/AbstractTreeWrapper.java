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

package org.hypernomicon.tree;

import java.util.List;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.view.wrappers.DragNDropContainer;

import javafx.scene.control.Control;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;

import static org.hypernomicon.util.Util.*;

public abstract class AbstractTreeWrapper<RowType extends AbstractTreeRow<? extends HDT_Record, RowType>> extends DragNDropContainer<RowType>
{
  protected boolean selectingFromCB = false;

  protected abstract RowType newRow(HDT_Record rootRecord, TreeModel<RowType> treeModel);
  protected abstract TreeItem<RowType> getTreeItem(RowType treeRow);
  protected abstract TreeItem<RowType> getRoot();
  protected abstract SelectionModel<TreeItem<RowType>> getSelectionModel();
  protected abstract void scrollToNdx(int ndx);
  protected abstract void clear();
  protected abstract List<RowType> getRowsForRecord(HDT_Record record); // should never return null
  protected abstract void focusOnTreeCtrl();
  protected abstract void expandMainBranches();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected AbstractTreeWrapper(Control ctrl)   { super(ctrl); }

  protected void reset()                        { clear(); }
  public final TreeItem<RowType> selectedItem() { return getSelectionModel().getSelectedItem(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HDT_Record selectedRecord()
  {
    return nullSwitch(selectedItem(), null, item -> item.getValue().getRecord());
  }

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
        focusOnTreeCtrl();
        return;
      }

      ndx = 0;
    }

    List<RowType> list = getRowsForRecord(record);
    if (list.isEmpty()) return;

    if (list.size() <= ndx) ndx = 0;

    if (fromCB)
      selectingFromCB = true;

    TreeItem<RowType> item = getTreeItem(list.get(ndx));

    showItem(item);

    if (getSelectionModel().getSelectedItem() != item)
    {
      getSelectionModel().clearSelection();

      while (getSelectionModel().getSelectedItem() != item) // It takes a while for the "showItem" operation to work. Before it's done,
        getSelectionModel().select(item);                   // "select(item)" will instead select a *different* item (?!?!)
    }

    scrollToNdx(getSelectionModel().getSelectedIndex());

    focusOnTreeCtrl();

    selectingFromCB = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final void showItem(TreeItem<RowType> item)
  {
    nullSwitch(item.getParent(), this::showItem);
    item.setExpanded(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
