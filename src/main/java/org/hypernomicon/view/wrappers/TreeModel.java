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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.util.BidiOneToManyRecordMap;

import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.SetMultimap;

import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

public class TreeModel<RowType extends AbstractTreeRow<? extends HDT_Base, RowType>>
{
  final private BidiOneToManyRecordMap parentToChildren;
  final private MappingFromRecordToRows recordToRows;
  final private AbstractTreeWrapper<RowType> treeWrapper;
  private RowType rootRow;
  final private Map<HDT_RecordType, Set<HDT_RecordType>> parentChildRelations;

  public boolean pruningOperationInProgress = false;

  public void expandMainBranch() { rootRow.treeItem.setExpanded(true); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private class MappingFromRecordToRows
  {
    final private SetMultimap<HDT_Base, RowType> recordToRows = LinkedHashMultimap.create();
    final private TreeCB tcb;

    //---------------------------------------------------------------------------

    private MappingFromRecordToRows(TreeCB tcb)            { this.tcb = tcb; }
    private Set<RowType> getRowsForRecord(HDT_Base record) { return recordToRows.get(record); }
    private void clear()                                   { recordToRows.clear(); }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void addRow(RowType row)
    {
      HDT_Base record = row.getRecord();

      if (recordToRows.containsKey(record) == false)
        if (tcb != null) tcb.add(record);

      recordToRows.put(record, row);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void removeRow(RowType row)
    {
      HDT_Base record = row.getRecord();

      if (recordToRows.remove(record, row) == false) return;

      if (tcb != null) tcb.checkIfShouldBeRemoved(record);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TreeModel(AbstractTreeWrapper<RowType> treeWrapper, TreeCB tcb)
  {
    parentToChildren = new BidiOneToManyRecordMap();
    recordToRows = new MappingFromRecordToRows(tcb);
    this.treeWrapper = treeWrapper;
    parentChildRelations = new HashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    parentToChildren.clear();
    recordToRows.clear();
    rootRow = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset(HDT_Base rootRecord)
  {
    clear();

    rootRow = treeWrapper.newRow(rootRecord, this);
    treeWrapper.getRoot().getChildren().add(treeWrapper.getTreeItem(rootRow));
    recordToRows.addRow(rootRow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeRecord(HDT_Base record)
  {
    parentToChildren.getForwardSet(record).forEach(child  -> unassignParent(child , record));
    parentToChildren.getReverseSet(record).forEach(parent -> unassignParent(record, parent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void copyTo(TreeModel<RowType> dest)
  {
    parentToChildren.getAllHeads().forEach(parent ->
      parentToChildren.getForwardSet(parent).forEach(child ->
        dest.assignParent(child, parent)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void unassignParent(HDT_Base child, HDT_Base parent)
  {
    if (parentToChildren.getForwardSet(parent).contains(child) == false) return;

    new ArrayList<>(recordToRows.getRowsForRecord(parent)).forEach(row -> row.treeItem.getChildren().removeIf(childItem ->
    {
      RowType childRow = childItem.getValue();

      if (childRow.getRecord() != child) return false;

      removeChildRows(childRow);
      recordToRows.removeRow(childRow);

      return pruningOperationInProgress == false;  // prevent ConcurrentModificationException
    }));

    parentToChildren.removeForward(parent, child);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeChildRows(RowType parentRow)
  {
    parentRow.treeItem.getChildren().removeIf(childItem ->
    {
      RowType childRow = childItem.getValue();

      removeChildRows(childRow);
      recordToRows.removeRow(childRow);

      return pruningOperationInProgress == false;  // prevent ConcurrentModificationException
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignParent(HDT_Base child, HDT_Base parent)
  {
    if (parentToChildren.getForwardSet(parent).contains(child)) return;

    parentToChildren.addForward(parent, child);

    new ArrayList<>(recordToRows.getRowsForRecord(parent)).forEach(row ->
    {
      RowType childRow = treeWrapper.newRow(child, this);

      insertTreeItem(treeWrapper.getTreeItem(row).getChildren(), childRow);
      recordToRows.addRow(childRow);
      addChildRows(childRow);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void insertTreeItem(ObservableList<TreeItem<RowType>> list, RowType newRow)
  {
    addToSortedList(list, treeWrapper.getTreeItem(newRow), (item1, item2) -> item1.getValue().compareTo(item2.getValue()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addChildRows(RowType parentRow)
  {
    parentToChildren.getForwardSet(parentRow.getRecord()).forEach(child ->
    {
      RowType childRow = treeWrapper.newRow(child, this);
      recordToRows.addRow(childRow);
      insertTreeItem(treeWrapper.getTreeItem(parentRow).getChildren(), childRow);
      addChildRows(childRow);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<RowType> getRowsForRecord(HDT_Base record)
  {
    return recordToRows.getRowsForRecord(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean hasParentChildRelation(HDT_RecordType parentType, HDT_RecordType childType)
  {
    return nullSwitch(parentChildRelations.get(parentType), false, rels -> rels.contains(childType));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addParentChildRelationMapping(HDT_RecordType parentType, HDT_RecordType childType)
  {
    Set<HDT_RecordType> childTypes;

    if (parentChildRelations.containsKey(parentType))
      childTypes = parentChildRelations.get(parentType);
    else
    {
      childTypes = EnumSet.noneOf(HDT_RecordType.class);
      parentChildRelations.put(parentType, childTypes);
    }

    childTypes.add(childType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addKeyWorkRelation(HDT_RecordType recordType, boolean forward)
  {
    if (forward)
    {
      addParentChildRelationMapping(hdtWork, recordType);
      addParentChildRelationMapping(hdtMiscFile, recordType);

      db.addKeyWorkHandler(recordType, (keyWork, record, affirm) ->
      {
        if (affirm) assignParent(keyWork, record);
        else        unassignParent(keyWork, record);
      });
    }
    else
    {
      addParentChildRelationMapping(recordType, hdtWork);
      addParentChildRelationMapping(recordType, hdtMiscFile);

      db.addKeyWorkHandler(recordType, (keyWork, record, affirm) ->
      {
        if (affirm) assignParent(record, keyWork);
        else        unassignParent(record, keyWork);
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addParentChildRelation(RelationType relType, boolean forward)
  {
    HDT_RecordType objType  = db.getObjType(relType),
                   subjType = db.getSubjType(relType);

    if (forward)
    {
      addParentChildRelationMapping(objType, subjType);

      db.addRelationChangeHandler(relType, (child, parent, affirm) ->
      {
        if (affirm) assignParent(child, parent);
        else        unassignParent(child, parent);
      });
    }
    else
    {
      addParentChildRelationMapping(subjType, objType);

      db.addRelationChangeHandler(relType, (child, parent, affirm) ->
      {
        if (affirm) assignParent(parent, child);
        else        unassignParent(parent, child);
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
