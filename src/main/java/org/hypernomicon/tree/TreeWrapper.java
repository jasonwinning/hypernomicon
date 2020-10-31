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

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.view.MainCtrlr;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.scene.control.ComboBox;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeSortMode;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableRow;
import javafx.scene.control.TreeTableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.control.TreeTableColumn.SortType;

public class TreeWrapper extends AbstractTreeWrapper<TreeRow>
{
  private final TreeTableView<TreeRow> ttv;
  private final Set<RecordType> recordTypesInTree = EnumSet.noneOf(RecordType.class);
  private final boolean hasTerms;
  private final TreeCB tcb;
  private boolean searchingDown = true, searchingNameOnly = false;
  private TreeRow draggingRow = null;
  final TreeModel<TreeRow> debateTree, termTree, labelTree, noteTree;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  TreeWrapper(TreeTableView<TreeRow> ttv, boolean hasTerms, ComboBox<TreeRow> comboBox, boolean limitedControl)
  {
    super(ttv);

    this.ttv = ttv;
    this.hasTerms = hasTerms;

    tcb = new TreeCB(comboBox, this);

    debateTree = new TreeModel<>(this, tcb);
    noteTree   = new TreeModel<>(this, tcb);
    termTree   = new TreeModel<>(this, tcb);
    labelTree  = new TreeModel<>(this, tcb);

    clear();

    ttv.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue != null) && (newValue.getValue() != null))
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

    if (limitedControl) return;

    ttv.setRowFactory(tTV ->
    {
      TreeTableRow<TreeRow> row = new TreeTableRow<>();

      setupDragHandlers(row);

      row.setOnMouseClicked(mouseEvent ->
      {
        nullSwitch(row.getItem(), treeRow -> nullSwitch(treeRow.treeItem, treeItem -> nullSwitch(treeRow.<HDT_Record>getRecord(), record ->
        {
          if (db.isLoaded() && mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2) && treeItem.isLeaf())
            ui.goToRecord(record, false);
        })));
      });

      row.itemProperty().addListener((ob, ov, nv) ->
      {
        if (nv == null)
          row.setGraphic(null);
        else
          nullSwitch(nv.treeItem, treeItem -> treeItem.expandedProperty().addListener((ob1, ov1, nv1) -> ttv.refresh()));

        row.setContextMenu(nullSwitch(nv, null, this::createContextMenu));
      });

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public TreeItem<TreeRow> getTreeItem(TreeRow treeRow)        { return treeRow.getTreeItem(); }
  @Override public TreeItem<TreeRow> getRoot()                           { return ttv.getRoot(); }
  @Override public void focusOnTreeCtrl()                                { safeFocus(ttv); }
  @Override public SelectionModel<TreeItem<TreeRow>> getSelectionModel() { return ttv.getSelectionModel(); }
  @Override public void scrollToNdx(int ndx)                             { ttv.scrollTo(ndx); }

  @Override public TreeRow newRow(HDT_Record record, TreeModel<TreeRow> treeModel) { return new TreeRow(record, treeModel); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expandMainBranches()
  {
    debateTree.expandMainBranch();
    noteTree  .expandMainBranch();
    labelTree .expandMainBranch();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void removeRecord(HDT_Record record)
  {
    debateTree.removeRecord(record);
    noteTree  .removeRecord(record);
    labelTree .removeRecord(record);

    if (hasTerms)
      termTree.removeRecord(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<TreeRow> getRowsForRecord(HDT_Record record)
  {
    List<TreeRow> rows = new ArrayList<>();

    rows.addAll(debateTree.getRowsForRecord(record));
    rows.addAll(noteTree  .getRowsForRecord(record));
    rows.addAll(labelTree .getRowsForRecord(record));

    if (hasTerms)
      rows.addAll(termTree.getRowsForRecord(record));

    return Collections.unmodifiableList(rows);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    ui.ttDates.setText(MainCtrlr.NO_DATES_TOOLTIP);

    if (ttv.getRoot() != null)
    {
      ttv.getRoot().getChildren().clear();
      ttv.setRoot(null);
    }

    tcb.clear();

    ttv.setRoot(new TreeItem<>(null));
    ttv.setShowRoot(false);

    debateTree.clear();
    noteTree  .clear();
    labelTree .clear();
    termTree  .clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void reset()
  {
    super.reset();

    debateTree.reset(HDT_Record.class.cast(db.debates   .getByID(1)));
    noteTree  .reset(HDT_Record.class.cast(db.notes     .getByID(1)));
    labelTree .reset(HDT_Record.class.cast(db.workLabels.getByID(1)));

    if (hasTerms)
      termTree.reset(HDT_Record.class.cast(db.glossaries.getByID(1)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked") void sort()
  {
    TreeTableColumn<TreeRow, ?> column = ttv.getColumns().get(0);

    column.setSortable(true);
    column.sortTypeProperty().set(SortType.ASCENDING);
    ttv.setSortMode(TreeSortMode.ALL_DESCENDANTS);

    ObservableList<TreeTableColumn<TreeRow, ?>> list = ttv.getSortOrder();

    if ((list.size() != 1) || (list.get(0) != column))
      list.setAll(column);
    else
      ttv.sort();

    tcb.refresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void selectNextInstance(boolean increment)
  {
    TreeRow row = selectedItem().getValue();

    List<TreeRow> list = getRowsForRecord(row.getRecord());
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
          ((searchingNameOnly == false) && row.getDescString().toLowerCase().contains(text)))
      {
        ui.treeHyperTab().textToHilite = text;
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

    return (prev == null) || (prev == ttv.getRoot()) ? lastDescendant(ttv.getRoot()) : prev;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<TreeRow> lastDescendant(TreeItem<TreeRow> treeItem)
  {
    return treeItem.getChildren().size() > 0 ?
      lastDescendant(treeItem.getChildren().get(treeItem.getChildren().size() - 1))
    :
      treeItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeItem<TreeRow> getNext(TreeItem<TreeRow> item, boolean fromChild)
  {
    return (fromChild == false) && (item.getChildren().size() > 0) ?
      item.getChildren().get(0)
    :
      nullSwitch(item.nextSibling(), nullSwitch(item.getParent(), null, parent -> getNext(parent, true)), UnaryOperator.identity());
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

  @Override public boolean isValidDragTarget(TreeRow targetRow, DragEvent dragEvent, TreeItem<TreeRow> treeItem)
  {
    scroll(dragEvent);

    HDT_Record source = nullSwitch(draggingRow, null, TreeRow::getRecord),
               target = nullSwitch(targetRow, null, TreeRow::getRecord);

    if ((source == null) || (target == null) || (source == target) ||
        (source.getType() == target.getType()) && (source.getID() == target.getID())) return false;

    if (nullSwitch(draggingRow.treeItem.getParent(), true, parent ->
        nullSwitch(parent.getValue(), true, value -> value.getRecord() == null)))
      return false;

    if ((source.getType() == hdtConcept) && (target.getType() == hdtGlossary))
    {
      HDT_Concept concept = (HDT_Concept)source;
      HDT_Glossary glossary = (HDT_Glossary)target;

      if (concept.term.get().getConcept(glossary) != null)
        return false;
    }

    expand(treeItem);

    return getValidTargetTypes(source.getType()).contains(target.getType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Set<RecordType> getValidTargetTypes(RecordType sourceType)
  {
    Set<RecordType> set = RelationSet.getRelationsForSubjType(sourceType).stream().map(db::getObjType).collect(Collectors.toSet());
    if (sourceType == hdtWork)
      set.add(hdtArgument);

    if (recordTypesInTree.isEmpty())
      List.of(debateTree, termTree, labelTree, noteTree).forEach(treeModel -> recordTypesInTree.addAll(treeModel.getRecordTypes()));

    set.retainAll(recordTypesInTree);

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDroppedOnto(TreeRow targetRow)
  {
    dragReset();

    RecordTreeEdge dragSourceEdge = new RecordTreeEdge(draggingRow.treeItem.getParent().getValue().getRecord(), draggingRow.getRecord()),
                   dragTargetEdge = new RecordTreeEdge(targetRow.getRecord(), draggingRow.getRecord()),
                   otherEdgeToDetach = dragTargetEdge.edgeToDetach();

    if (dragSourceEdge.equals(otherEdgeToDetach))
      otherEdgeToDetach = null;

    if (dragTargetEdge.canAttach(true) == false)
      return;

    if (dragSourceEdge.equals(dragTargetEdge))
    {
      messageDialog("Unable to copy or move source record: It is already attached to destination record.", mtError);
      return;
    }

    if (dragTargetEdge.relType == rtNone)
    {
      messageDialog("Unable to copy or move source record: Internal error #33948.", mtError);
      return;
    }

    ChangeParentDlgCtrlr cpdc = ChangeParentDlgCtrlr.build(dragTargetEdge, dragSourceEdge, otherEdgeToDetach);

    if (cpdc.showModal() == false)
      return;

    dragTargetEdge.attach(cpdc.detachDragSource() ? dragSourceEdge : null, true);

    Platform.runLater(() ->
    {
      sort();
      ttv.getSelectionModel().select(getTreeItem(targetRow));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean canDetach(TreeRow childRow, boolean doDetach)
  {
    if (childRow == null) return false;

    TreeItem<TreeRow> item = childRow.treeItem;

    TreeRow parentRow = nullSwitch(nullSwitch(item, null, TreeItem::getParent), null, TreeItem::getValue);

    HDT_Record parent = nullSwitch(parentRow, null, TreeRow::getRecord),
               child = childRow.getRecord();

    if ((parent == null) || (child == null)) return false;

    RecordTreeEdge edge = new RecordTreeEdge(parent, child);

    if (edge.canDetach() == false)
      return doDetach ? falseWithErrorMessage("Internal error #33948.") : false;

    boolean rv = edge.canDetachWithoutAttaching(doDetach);

    TreeItem<TreeRow> parentItem = getTreeItem(parentRow);

    if (rv && doDetach) Platform.runLater(() ->
    {
      sort();
      ttv.getSelectionModel().select(parentItem);
    });

    return rv;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDone()
  {
    draggingRow = null;
    dragReset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
