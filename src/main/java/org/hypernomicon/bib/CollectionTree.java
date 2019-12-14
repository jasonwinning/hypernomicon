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

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;

import static org.hypernomicon.bib.CollectionTree.BibCollectionType.*;
import static org.hypernomicon.util.Util.*;

public class CollectionTree
{
  static enum BibCollectionType { bctAll, bctUnsorted, bctTrash, bctUser }

  private final TreeView<BibCollectionRow> treeView;
  private final Map<String, BibCollectionRow> keyToRow;

  private BibCollectionRow treeRowAllEntries, treeRowUnsorted, treeRowTrash;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void selectAllEntries() { treeView.getSelectionModel().select(treeRowAllEntries.getTreeItem()); }
  void selectTrash()      { treeView.getSelectionModel().select(treeRowTrash.getTreeItem()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  CollectionTree(TreeView<BibCollectionRow> treeView)
  {
    this.treeView = treeView;
    keyToRow = new HashMap<>();

    treeView.setCellFactory(theTreeView ->
    {
      TreeCell<BibCollectionRow> row = new TreeCell<>();

      row.itemProperty().addListener((ob, oldValue, newValue) ->
      {
        if (oldValue == newValue) return;

        if (newValue == null)
        {
          row.setText(null);
          row.setGraphic(null);
          row.setContextMenu(null);
        }
        else
          row.setText(newValue.getText());
      });

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    nullSwitch(treeView.getRoot(), root ->
    {
      root.getChildren().clear();
      treeView.setRoot(null);
    });

    keyToRow.clear();

    TreeItem<BibCollectionRow> root = new TreeItem<>(null);

    treeView.setRoot(root);
    treeView.setShowRoot(false);

    treeRowAllEntries = new BibCollectionRow(bctAll);
    treeRowUnsorted = new BibCollectionRow(bctUnsorted);
    treeRowTrash = new BibCollectionRow(bctTrash);

    List<TreeItem<BibCollectionRow>> children = root.getChildren();

    children.add(treeRowAllEntries.getTreeItem());
    children.add(treeRowUnsorted.getTreeItem());
    children.add(treeRowTrash.getTreeItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh(Map<String, BibCollection> keyToColl)
  {
    pruneNode(treeView.getRoot(), keyToColl);

    keyToColl.forEach((key, coll) ->
    {
      if (keyToRow.containsKey(key) == false)
        addToTree(key, coll, keyToColl);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<BibCollectionRow> addToTree(String childKey, BibCollection childColl, Map<String, BibCollection> keyToColl)
  {
    TreeItem<BibCollectionRow> parentItem;
    String parentKey = childColl.getParentKey();

    if (parentKey == null)
      parentItem = treeView.getRoot();
    else if (keyToRow.containsKey(parentKey))
      parentItem = keyToRow.get(parentKey).getTreeItem();
    else
      parentItem = addToTree(parentKey, keyToColl.get(parentKey), keyToColl);

    BibCollectionRow childRow = new BibCollectionRow(childColl);
    keyToRow.put(childKey, childRow);
    insertTreeItem(parentItem.getChildren(), childRow);

    return childRow.getTreeItem();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pruneNode(TreeItem<BibCollectionRow> treeItem, Map<String, BibCollection> keyToColl)
  {
    Iterator<TreeItem<BibCollectionRow>> it = treeItem.getChildren().iterator();

    while (it.hasNext())
    {
      boolean removed = false;
      TreeItem<BibCollectionRow> childItem = it.next();

      BibCollectionRow row = childItem.getValue();
      if (row.getType() == BibCollectionType.bctUser)
      {
        String key = row.getKey();

        if (keyToColl.containsKey(key) == false)
        {
          it.remove();
          keyToRow.remove(key);
          removed = true;
        }
        else
          row.updateCollObj(keyToColl.get(key));
      }

      if (removed == false)
        pruneNode(childItem, keyToColl);
    }
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void rebuild(Map<String, BibCollection> keyToColl)
  {
    clear();

    keyToColl.forEach((childKey, childColl)  ->
    {
      BibCollectionRow childRow = keyToRow.get(childKey);
      if (childRow == null)
      {
        childRow = new BibCollectionRow(childColl);
        keyToRow.put(childKey, childRow);
      }

      String parentKey = childColl.getParentKey();
      if (parentKey == null)
      {
        insertTreeItem(treeView.getRoot().getChildren(), childRow);
      }
      else
      {
        BibCollection parentColl = keyToColl.get(parentKey);
        BibCollectionRow parentRow = keyToRow.get(parentKey);
        if (parentRow == null)
        {
          parentRow = new BibCollectionRow(parentColl);
          keyToRow.put(parentKey, parentRow);
        }

        insertTreeItem(parentRow.getTreeItem().getChildren(), childRow);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void insertTreeItem(List<TreeItem<BibCollectionRow>> list, BibCollectionRow newRow)
  {
    for (int ndx = 0; ndx < list.size(); ndx++)
    {
      if (newRow.getSortKey().compareTo(list.get(ndx).getValue().getSortKey()) < 0)
      {
        list.add(ndx, newRow.getTreeItem());
        return;
      }
    }

    list.add(newRow.getTreeItem());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
