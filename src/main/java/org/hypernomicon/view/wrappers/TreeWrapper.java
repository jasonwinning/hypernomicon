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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Position;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.util.Util;
import org.hypernomicon.view.dialogs.ChangeParentDialogController;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.TreeTabController;
import org.hypernomicon.view.wrappers.DragNDropHoverHelper.DragNDropContainer;
import org.hypernomicon.view.wrappers.HyperTable.HyperMenuItem;
import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeSortMode;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
import javafx.scene.control.TreeTableColumn.SortType;

public class TreeWrapper extends AbstractTreeWrapper<TreeRow> implements RecordListView, DragNDropContainer<TreeRow>
{
  private final TreeTableView<TreeRow> ttv;
  private final boolean hasTerms;
  private final TreeCB tcb;
  private final List<HyperMenuItem<? extends HDT_Base>> contextMenuItems;
  private boolean searchingDown = true;
  private boolean searchingNameOnly = false;
  private TreeRow draggingRow = null;
  private final DragNDropHoverHelper<TreeRow> ddHoverHelper;
  public final TreeModel<TreeRow> debateTree, termTree, labelTree, noteTree;

//---------------------------------------------------------------------------

  public static class TreeTargetType
  {
    public TreeTargetType(RelationType relType, HDT_RecordType objType)
    {
      this.relType = relType;
      this.objType = objType;
    }

    public RelationType relType;
    public HDT_RecordType objType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TreeWrapper(TreeTableView<TreeRow> ttv, boolean hasTerms, ComboBox<TreeRow> comboBox)
  {
    this.ttv = ttv;
    this.hasTerms = hasTerms;

    tcb = new TreeCB(comboBox, this);
    ddHoverHelper = new DragNDropHoverHelper<>(ttv);

    debateTree = new TreeModel<>(this, tcb);
    noteTree = new TreeModel<>(this, tcb);
    termTree = new TreeModel<>(this, tcb);
    labelTree = new TreeModel<>(this, tcb);

    contextMenuItems = new ArrayList<>();

    clear();

    ttv.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue != null)
        if (newValue.getValue() != null)
        {
          TreeRow row = newValue.getValue();
          if (row.getRecordType() != hdtNone)
          {
            if (selectingFromCB == false)
              tcb.select(row.getRecord());
            return;
          }
        }

      if (selectingFromCB == false)
        tcb.clearSelection();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public TreeItem<TreeRow> getTreeItem(TreeRow treeRow)        { return treeRow.getTreeItem(); }
  @Override public TreeItem<TreeRow> getRoot()                           { return ttv.getRoot(); }
  @Override public void focusOnTreeCtrl()                                { safeFocus(ttv); }
  @Override public SelectionModel<TreeItem<TreeRow>> getSelectionModel() { return ttv.getSelectionModel(); }
  @Override public void scrollToNdx(int ndx)                             { ttv.scrollTo(ndx); }

  @Override public TreeRow newRow(HDT_Base record, TreeModel<TreeRow> treeModel) { return new TreeRow(record, treeModel); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expandMainBranches()
  {
    debateTree.expandMainBranch();
    noteTree.expandMainBranch();
    labelTree.expandMainBranch();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void removeRecord(HDT_Base record)
  {
    debateTree.removeRecord(record);
    noteTree.removeRecord(record);
    labelTree.removeRecord(record);

    if (hasTerms)
      termTree.removeRecord(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ArrayList<TreeRow> getRowsForRecord(HDT_Base record)
  {
    ArrayList<TreeRow> rows = new ArrayList<>();

    rows.addAll(debateTree.getRowsForRecord(record));
    rows.addAll(noteTree.getRowsForRecord(record));
    rows.addAll(labelTree.getRowsForRecord(record));

    if (this.hasTerms)
      rows.addAll(termTree.getRowsForRecord(record));

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    ui.ttDates.setText("No dates to show.");

    if (ttv.getRoot() != null)
    {
      ttv.getRoot().getChildren().clear();
      ttv.setRoot(null);
    }

    tcb.clear();

    ttv.setRoot(new TreeItem<>(null));
    ttv.setShowRoot(false);

    debateTree.clear();
    noteTree.clear();
    labelTree.clear();
    termTree.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void reset()
  {
    super.reset();

    debateTree.reset(db.debates.getByID(1));
    noteTree.reset(db.notes.getByID(1));
    labelTree.reset(db.workLabels.getByID(1));

    if (hasTerms)
      termTree.reset(db.glossaries.getByID(1));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addContextMenuItem(String caption, Class<HDT_T> klass, RecordHandler<HDT_T> handler)
  {
    return addCondContextMenuItem(caption, klass, record -> true, handler);
  }

  @Override public <HDT_T extends HDT_Base> HyperMenuItem<HDT_T> addCondContextMenuItem(String caption, Class<HDT_T> klass, CondRecordHandler<HDT_T> condHandler, RecordHandler<HDT_T> handler)
  {
    HyperMenuItem<HDT_T> mnu;

    mnu = new HyperMenuItem<>(caption);
    mnu.recordType = HDT_RecordType.typeByRecordClass(klass);
    mnu.condRecordHandler = condHandler;
    mnu.recordHandler = handler;

    contextMenuItems.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public void sort()
  {
    TreeTableColumn<TreeRow, ?> column = ttv.getColumns().get(0);

    column.setSortable(true);
    column.sortTypeProperty().set(SortType.ASCENDING);
    ttv.setSortMode(TreeSortMode.ALL_DESCENDANTS);

    ObservableList<TreeTableColumn<TreeRow, ?>> list = ttv.getSortOrder();

    if ((list.size() != 1) || (list.get(0) != column))
      list.setAll(column);

    tcb.refresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectNextInstance(boolean increment)
  {
    TreeRow row = selectedItem().getValue();

    ArrayList<TreeRow> list = getRowsForRecord(row.getRecord());
    int ndx = list.indexOf(row);

    ndx = ndx + (increment ? 1 : -1);
    if (ndx == list.size()) ndx = 0;
    if (ndx < 0) ndx = list.size() - 1;
    selectRecord(list.get(ndx).getRecord(), ndx, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void find(String text, boolean forward, boolean nameOnly)
  {
    text = text.toLowerCase();
    searchingDown = forward;
    searchingNameOnly = nameOnly;

    TreeItem<TreeRow> firstItem = nullSwitch(selectedItem(), ttv.getSelectionModel().getModelItem(0)),
                      item = firstItem;

    do
    {
      item = forward ? nullSwitch(getNext(item, false), getNext(ttv.getRoot(), false)) : getPrevious(item);

      TreeRow row = item.getValue();

      if (row.getName().toLowerCase().contains(text) ||
          ((searchingNameOnly == false) && (row.getDescString().toLowerCase().contains(text))))
      {
        TreeTabController.class.cast(HyperTab.getHyperTab(treeTab)).textToHilite = text;
        selectRecord(row.getRecord(), getRowsForRecord(row.getRecord()).indexOf(row), true);
        return;
      }

    } while (item != firstItem);

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<TreeRow> getPrevious(TreeItem<TreeRow> item)
  {
    TreeItem<TreeRow> prev = item.previousSibling();
    if (prev != null)
      return lastDescendant(prev);

    prev = item.getParent();

    if ((prev == null) || (prev == ttv.getRoot()))
      return lastDescendant(ttv.getRoot());

    return prev;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<TreeRow> lastDescendant(TreeItem<TreeRow> treeItem)
  {
    if (treeItem.getChildren().size() > 0)
      return lastDescendant(treeItem.getChildren().get(treeItem.getChildren().size() - 1));

    return treeItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<TreeRow> getNext(TreeItem<TreeRow> item, boolean fromChild)
  {
    if ((fromChild == false) && (item.getChildren().size() > 0))
      return item.getChildren().get(0);

    return nullSwitch(item.nextSibling(), nullSwitch(item.getParent(), null, parent -> getNext(parent, true)), Util::that);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void findAgain(String text)
  {
    find(text, searchingDown, searchingNameOnly);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void startDrag(TreeRow row)
  {
    draggingRow = row;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean acceptDrag(TreeRow targetRow, DragEvent dragEvent, TreeItem<TreeRow> treeItem)
  {
    ddHoverHelper.scroll(dragEvent);

    HDT_Base source = nullSwitch(draggingRow, null, row -> row.getRecord()),
             target = nullSwitch(targetRow, null, row -> row.getRecord());

    if ((source == null) || (target == null) || (source == target) ||
        (source.getType() == target.getType()) && (source.getID() == target.getID())) return false;

    if (nullSwitch(draggingRow.treeItem.getParent(), true, parent ->
        nullSwitch(parent.getValue(), true, value -> value.getRecord() == null)))
      return false;

    ddHoverHelper.expand(treeItem);

    if (targetRow.getTreeModel().hasParentChildRelation(target.getType(), source.getType()) == false)
      return false;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDroppedOnto(TreeRow targetRow)
  {
    ddHoverHelper.reset();

    HDT_Base subjRecord, objRecord,
             oldParent = draggingRow.treeItem.getParent().getValue().getRecord(),
             newParent = targetRow.getRecord(),
             child = draggingRow.getRecord();

    if (oldParent == newParent)
    {
      messageDialog("Unable to copy or move source record: It is already attached to destination record.", mtError);
      return;
    }

    MutableBoolean oldForward = new MutableBoolean(true),
                   newForward = new MutableBoolean(true);

    RelationType oldRelType = getParentChildRelation(oldParent.getType(), child.getType(), oldForward),
                 newRelType = getParentChildRelation(newParent.getType(), child.getType(), newForward);

    if ((oldRelType == rtNone) || (newRelType == rtNone))
    {
      messageDialog("Unable to copy or move source record: Internal error #33948.", mtError);
      return;
    }

    if (newForward.booleanValue())
    {
      subjRecord = child;
      objRecord = newParent;
    }
    else
    {
      subjRecord = newParent;
      objRecord = child;
    }

    if (db.getObjectList(newRelType, subjRecord, true).contains(objRecord))
    {
      messageDialog("Unable to copy or move source record: It is already attached to destination record.", mtError);
      return;
    }

    ChangeParentDialogController cpdc = ChangeParentDialogController.create("Copy or move record to destination",
        draggingRow.treeItem.getParent().getValue().getRecord(), targetRow.getRecord(), draggingRow.getRecord(), db.relationIsMulti(newRelType));

    if (cpdc.showModal())
    {
      try
      {
        HyperObjList<HDT_Base, HDT_Base> objList = db.getObjectList(newRelType, subjRecord, true);
        objList.add(objRecord);
        objList.throwLastException();

        if (cpdc.getTransferMode() == TransferMode.MOVE)
        {
          if (oldForward.booleanValue())
            db.getObjectList(oldRelType, child, true).remove(oldParent);
          else
            db.getObjectList(oldRelType, oldParent, true).remove(child);
        }
      }
      catch (RelationCycleException e)
      {
        messageDialog(e.getMessage(), mtError);
        return;
      }

      Platform.runLater(() ->
      {
        sort();
        ttv.getSelectionModel().select(getTreeItem(targetRow));
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean canDetach(boolean doDetach)
  {
    TreeItem<TreeRow> item = selectedItem();

    TreeRow parentRow = nullSwitch(nullSwitch(item, null, TreeItem::getParent), null, TreeItem::getValue);

    HDT_Base parent = nullSwitch(parentRow, null, TreeRow::getRecord),
             child = nullSwitch(nullSwitch(item, null, TreeItem::getValue), null, TreeRow::getRecord),
             subjRecord, objRecord, objToAdd = null;

    if ((parent == null) || (child == null)) return false;

    MutableBoolean forward = new MutableBoolean();
    RelationType relType = getParentChildRelation(parent.getType(), child.getType(), forward);

    if ((relType == rtNone) || (relType == rtUnited))
    {
      if (doDetach)
        messageDialog("Internal error #33948.", mtError);
      return false;
    }

    if (forward.booleanValue())
    {
      subjRecord = child;
      objRecord = parent;
    }
    else
    {
      subjRecord = parent;
      objRecord = child;
    }

    switch (subjRecord.getType())
    {
      case hdtDebate :

        if (relType == rtParentDebateOfDebate)
        {
          HDT_Debate debate = (HDT_Debate)subjRecord;
          if (debate.largerDebates.size() == 1)
          {
            if (debate.largerDebates.get(0).getID() == 1)
              return false;
            else
              objToAdd = db.debates.getByID(1);
          }
        }
        break;

      case hdtPosition :

        if (objRecord == db.debates.getByID(1)) return false;

        if ((relType == RelationType.rtDebateOfPosition) || (relType == RelationType.rtParentPosOfPos))
        {
          HDT_Position position = (HDT_Position)subjRecord;
          if ((position.debates.size() + position.largerPositions.size()) == 1)
            objToAdd = db.debates.getByID(1);
        }
        break;

      case hdtNote :

        if (objRecord == db.notes.getByID(1)) return false;

        if (relType == RelationType.rtParentNoteOfNote)
          if (db.getObjectList(relType, subjRecord, true).size() == 1)
            objToAdd = db.notes.getByID(1);

        break;

      case hdtGlossary :

        if (objRecord == db.glossaries.getByID(1)) return false;

        if (relType == RelationType.rtParentGlossaryOfGlossary)
          if (db.getObjectList(relType, subjRecord, true).size() == 1)
            objToAdd = db.glossaries.getByID(1);

        break;

      case hdtWorkLabel :

        if (objRecord == db.workLabels.getByID(1)) return false;

        if (relType == RelationType.rtParentLabelOfLabel)
          if (db.getObjectList(relType, subjRecord, true).size() == 1)
            objToAdd = db.workLabels.getByID(1);

        break;

      default : break;
    }

    if (doDetach)
    {
      db.getObjectList(relType, subjRecord, true).remove(objRecord);

      if (objToAdd != null)
        db.getObjectList(getRelation(subjRecord.getType(), objToAdd.getType()), subjRecord, true).add(objToAdd);

      Platform.runLater(() ->
      {
        sort();
        ttv.getSelectionModel().select(getTreeItem(parentRow));
      });
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RelationType getParentChildRelation(HDT_RecordType parentType, HDT_RecordType childType, MutableBoolean forward)
  {
    RelationType relType = getRelation(childType, parentType);

    if (relType == rtNone)
    {
      forward.setFalse();
      return getRelation(parentType, childType);
    }

    forward.setTrue();
    return relType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDone()
  {
    draggingRow = null;
    ddHoverHelper.reset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private <HDT_T extends HDT_Base> MenuItem createContextMenuItem(HyperMenuItem<HDT_T> hItem, HDT_Base record, ContextMenu rowMenu)
  {
    MenuItem newItem = new MenuItem(hItem.caption);

    rowMenu.getItems().add(newItem);

    newItem.setOnAction(event ->
    {
      rowMenu.hide();
      hItem.recordHandler.handle((HDT_T) record);
    });

    boolean visible = false;

    if ((hItem.recordType == hdtNone) || ((record != null) && (record.getType() == hItem.recordType)))
      visible = hItem.condRecordHandler.handle((HDT_T) record);

    newItem.setVisible(visible);

    return newItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ContextMenu createContextMenu(TreeRow treeRow)
  {
    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();
    HDT_Base record = treeRow.getRecord();

    for (HyperMenuItem<? extends HDT_Base> hItem : contextMenuItems)
      if (createContextMenuItem(hItem, record, rowMenu).isVisible()) noneVisible = false;

    if (treeRow.treeItem.isLeaf() == false)
    {
      noneVisible = false;

      MenuItem newItem = new MenuItem("Expand/Collapse");
      rowMenu.getItems().add(newItem);
      newItem.setOnAction(event -> treeRow.treeItem.setExpanded(!treeRow.treeItem.isExpanded()));

      newItem = new MenuItem("Expand All");
      rowMenu.getItems().add(newItem);
      newItem.setOnAction(event -> setAllExpanded(getTreeItem(treeRow), true));

      newItem = new MenuItem("Collapse All");
      rowMenu.getItems().add(newItem);
      newItem.setOnAction(event -> setAllExpanded(getTreeItem(treeRow), false));
    }

    return noneVisible ? null : rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
