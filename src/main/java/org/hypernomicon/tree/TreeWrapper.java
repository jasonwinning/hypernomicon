/*
 * Copyright 2015-2025 Jason Winning
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
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import org.controlsfx.control.BreadCrumbBar;
import org.controlsfx.control.BreadCrumbBar.BreadCrumbButton;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.HDT_Concept;
import org.hypernomicon.model.records.HDT_Debate;
import org.hypernomicon.model.records.HDT_Glossary;
import org.hypernomicon.model.records.HDT_Note;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_WorkLabel;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet;
import org.hypernomicon.view.MainCtrlr;

import javafx.application.Platform;
import javafx.collections.ObservableList;
import javafx.geometry.Orientation;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Control;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeSortMode;
import javafx.scene.control.TreeTableColumn;
import javafx.scene.control.TreeTableRow;
import javafx.scene.control.TreeTableView;
import javafx.scene.input.DragEvent;
import javafx.scene.input.MouseButton;
import javafx.scene.control.TreeTableColumn.SortType;

//---------------------------------------------------------------------------

public class TreeWrapper extends AbstractTreeWrapper<TreeRow>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private TreeTableView<TreeRow> ttv;
  private final Map<TreeRow, Integer> breadCrumbRowToRecordNdx = new HashMap<>();
  private final Set<RecordType> recordTypesInTree = EnumSet.noneOf(RecordType.class);
  private final boolean hasTerms;
  private final TreeCB tcb;
  private final BreadCrumbBar<TreeRow> bcbPath;
  private boolean searchingDown = true, searchingNameOnly = false;
  private TreeRow draggingRow = null;
  private String lastSearchTerm = "";
  final TreeModel<TreeRow> debateTree, termTree, labelTree, noteTree;

//---------------------------------------------------------------------------

  TreeWrapper(BreadCrumbBar<TreeRow> bcbPath, boolean hasTerms, ComboBox<TreeRow> comboBox)
  {
    this.hasTerms = hasTerms;
    this.bcbPath = bcbPath;

    tcb = new TreeCB(comboBox, this);

    debateTree = new TreeModel<>(this, tcb);
    noteTree   = new TreeModel<>(this, tcb);
    termTree   = new TreeModel<>(this, tcb);
    labelTree  = new TreeModel<>(this, tcb);

  //---------------------------------------------------------------------------

    bcbPath.setCrumbFactory(crumb ->
    {
      TreeRow treeRow = crumb == null ? null : crumb.getValue();

      if (treeRow == null) return new BreadCrumbButton("");

      String caption = treeRow.getName();
      if (caption.isBlank() == false) caption = ' ' + caption + "  ";

      HDT_Record record = treeRow.getRecord();

      return record == null ?
        new BreadCrumbButton(caption)
      :
        new BreadCrumbButton(caption, imgViewForRecord(record));
    });

  //---------------------------------------------------------------------------

    bcbPath.setOnCrumbAction(event ->
      nullSwitch(event.getSelectedCrumb().getValue(), row -> selectRecord(row.getRecord(), breadCrumbRowToRecordNdx.get(row), false)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public TreeItem<TreeRow> getRoot()                           { return ttv.getRoot(); }
  @Override public Control getControl()                                  { return ttv; }
  @Override public SelectionModel<TreeItem<TreeRow>> getSelectionModel() { return ttv.getSelectionModel(); }
  @Override public void scrollToNdx(int ndx)                             { scrollToNdx(ttv, ndx); }

  @Override public TreeRow newRow(HDT_Record record, TreeModel<TreeRow> treeModel) { return new TreeRow(record, treeModel); }

  public boolean getHasTerms() { return hasTerms; }

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

  public void clear()
  {
    searchingDown = true;
    searchingNameOnly = false;
    lastSearchTerm = "";

    ui.ttDates.setText(MainCtrlr.NO_DATES_TOOLTIP);

    if (ttv != null)
    {
      if (ttv.getRoot() == null)
        ttv.setRoot(new TreeItem<>(null));
      else
        ttv.getRoot().getChildren().clear();
    }

    tcb.clear();

    debateTree.clear();
    noteTree  .clear();
    labelTree .clear();
    termTree  .clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reset(TreeTableView<TreeRow> ttv, boolean limitedControl, boolean initializeTTV)
  {
    if (initializeTTV)
    {
      clear();

      this.ttv = ttv;

      //---------------------------------------------------------------------------

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

    //---------------------------------------------------------------------------

      ttv.setRowFactory(tTV ->
      {
        TreeTableRow<TreeRow> row = new TreeTableRow<>();

        if (limitedControl == false)
        {
          setupDragHandlers(row);

          row.setOnMouseClicked(mouseEvent ->
            nullSwitch(row.getItem(), treeRow -> nullSwitch(treeRow.treeItem, treeItem -> nullSwitch(treeRow.<HDT_Record>getRecord(), record ->
            {
              if (db.isLoaded() && mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2) && treeItem.isLeaf())
                ui.goToRecord(record, false);
            }))));
        }

        row.itemProperty().addListener((ob, ov, nv) ->
        {
          if (nv == null)
            row.setGraphic(null);
          else
          {
            nullSwitch(nv.treeItem, treeItem ->
            {
              treeItem.expandedProperty().addListener((ob1, ov1, nv1) -> ttv.refresh());
              treeItem.setGraphic(nv.getGraphic());
            });
          }

          if (limitedControl == false)
            row.setContextMenu(createContextMenu(nv));
        });

        return row;
      });

    //---------------------------------------------------------------------------

      ttv.setShowRoot(false);
    }

    clear();

    HDT_Debate rootDebate = db.debates.getByID(1);      // If these two lines are combined into one, there will be
    debateTree.reset(rootDebate);                       // false-positive build errors

    HDT_Note rootNote = db.notes.getByID(1);            // Same as above
    noteTree.reset(rootNote);

    HDT_WorkLabel rootLabel = db.workLabels.getByID(1); // Same as above
    labelTree.reset(rootLabel);

    if (hasTerms)
      termTree.reset(db.glossaries.getByID(1));
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

  public void find(boolean forward, boolean nameOnly)
  {
    HDT_Record record = tcb.selectedRecord();
    String text;

    if (record != null)
    {
      if (record.getType() == hdtPerson)
        text = ((HDT_Person)record).getFullName(true);
      else
        text = record.name();

      text = removeAllParentheticals(text);
    }
    else
      text = tcb.getText();

    find(text, forward, nameOnly);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void find(String text, boolean forward, boolean nameOnly)
  {
    text = text.toLowerCase();
    lastSearchTerm = text;
    searchingDown = forward;
    searchingNameOnly = nameOnly;

    if (text.isBlank()) return;

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
        selectRow(row, true);
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

  private static TreeItem<TreeRow> lastDescendant(TreeItem<TreeRow> treeItem)
  {
    return treeItem.getChildren().size() > 0 ?
      lastDescendant(treeItem.getChildren().get(treeItem.getChildren().size() - 1))
    :
      treeItem;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TreeItem<TreeRow> getNext(TreeItem<TreeRow> item, boolean fromChild)
  {
    return (fromChild == false) && (item.getChildren().size() > 0) ?
      item.getChildren().get(0)
    :
      nullSwitch(item.nextSibling(), nullSwitch(item.getParent(), null, parent -> getNext(parent, true)), UnaryOperator.identity());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void findAgain()
  {
    if (lastSearchTerm.isBlank())
    {
      ui.goToRecord(tcb.selectedRecord(), false);
      return;
    }

    findAgain(searchingDown);
  }

  public void findAgain(boolean down)
  {
    find(lastSearchTerm, down, searchingNameOnly);
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
        ((source.getType() == target.getType()) && (source.getID() == target.getID()))) return false;

    if (nullSwitch(draggingRow.treeItem.getParent(), true, parent ->
        nullSwitch(parent.getValue(), true, value -> value.getRecord() == null)))
      return false;

    if ((source.getType() == hdtConcept) && ((target.getType() == hdtGlossary) || (target.getType() == hdtConcept)))
    {
      HDT_Concept sourceConcept = (HDT_Concept) source;
      HDT_Glossary targetGlossary = target.getType() == hdtGlossary ? (HDT_Glossary) target : ((HDT_Concept) target).glossary.get();

      if (sourceConcept.term.get().getConcept(targetGlossary, sourceConcept.sense.get()) != null)
        if ((target.getType() == hdtGlossary) || (sourceConcept.glossary.get() != targetGlossary))
          return false;
    }

    expand(treeItem);

    return getValidTargetTypes(source.getType()).contains(target.getType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Set<RecordType> getValidTargetTypes(RecordType sourceType)
  {
    Set<RecordType> set = RelationSet.getRelationsForSubjType(sourceType, false).stream().map(db::getObjType).collect(Collectors.toSet());
    if (sourceType == hdtWork)
      set.add(hdtArgument);

    if (recordTypesInTree.isEmpty())
      List.of(debateTree, termTree, labelTree, noteTree).forEach(treeModel -> recordTypesInTree.addAll(treeModel.getRecordTypes()));

    if ((sourceType == hdtWork) || (sourceType == hdtMiscFile))
    {
      set.add(hdtInvestigation);
      set.add(hdtWorkLabel);
    }

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

    if (dragTargetEdge.canAttach() == false)
      return;

    if (dragSourceEdge.equals(dragTargetEdge))
    {
      errorPopup("Unable to copy or move source record: It is already attached to destination record.");
      return;
    }

    if (dragTargetEdge.relType == rtNone)
    {
      errorPopup("Unable to copy or move source record: " + getThrowableMessage(new HDB_InternalError(33948)));
      return;
    }

    if ((draggingRow.getRecord().getType() == hdtConcept) && (dragTargetEdge.isConceptsInSameGlossary() == false))
    {
      DragConceptDlgCtrlr dcdc = new DragConceptDlgCtrlr(draggingRow.getRecord(), dragTargetEdge.parent);

      if (dcdc.showModal() == false)
        return;
    }
    else
    {
      ChangeParentDlgCtrlr cpdc = new ChangeParentDlgCtrlr(dragTargetEdge, dragSourceEdge, otherEdgeToDetach);

      if (cpdc.showModal() == false)
        return;

      dragTargetEdge.attach(cpdc.detachDragSource() ? dragSourceEdge : null);
    }

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
      return doDetach && falseWithInternalErrorPopup(33949);

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

  // TreeTableView.scrollTo could not be used because it is too buggy.
  // In java.scene.control.skin.VirtualFlow.adjustPositionToIndex, variable "estimatedSize" is often incorrectly set to 1,
  // which causes it to just scroll to the top regardless of the index passed in.

  // This algorithm is similar to HyperTable.scrollToNdx

  private static <RowType> void scrollToNdx(TreeTableView<RowType> ttv, int ndx)
  {
    ScrollBar sb = getScrollBar(ttv, Orientation.VERTICAL);
    if (sb == null) return;

    double rHeight = getRowHeight(ttv),

           allRowsHeight = rHeight * ttv.getExpandedItemCount(),
           vpHeight = allRowsHeight * sb.getVisibleAmount(),
           vpTop = (allRowsHeight - vpHeight) * sb.getValue(),

           y1 = ndx * rHeight,
           y2 = (ndx + 1) * rHeight;

    if (y2 > (vpTop + (vpHeight / 4.0)))
    {
      double scrollValue = y2 - (vpHeight * 0.25);
      if (scrollValue > allRowsHeight - vpHeight)
        scrollValue = allRowsHeight - vpHeight;

      sb.setValue(scrollValue / (allRowsHeight - vpHeight));
    }
    else if (y1 < vpTop) sb.setValue(y1 / (allRowsHeight - vpHeight));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // In this method, new TreeItem objects need to be created for the BreadCrumb control because
  // otherwise, it would always show a meaningless root node (parent node of the nodes representing
  // "All Debates", "All Notes", etc.) that is normally kept invisible in the TreeTableView.
  // The chain of TreeItems created for the BreadCrumb omits this root node.

  void setBreadCrumb(TreeItem<TreeRow> selItem)
  {
    TreeItem<TreeRow> curItem = selItem, head = null;
    TreeRow lastRow = null;
    breadCrumbRowToRecordNdx.clear();

    while ((curItem != null) && (curItem.getValue() != null) && (curItem.getValue().getRecord() != null))
    {
      TreeRow newRow = new TreeRow(curItem.getValue().getRecord(), null);
      breadCrumbRowToRecordNdx.put(newRow, getRowsForRecord(newRow.getRecord()).indexOf(curItem.getValue()));
      if (lastRow != null)
        newRow.treeItem.getChildren().add(lastRow.treeItem);

      lastRow = newRow;
      if (head == null) head = newRow.treeItem;

      curItem = curItem.getParent();
    }

    bcbPath.setSelectedCrumb(head);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
