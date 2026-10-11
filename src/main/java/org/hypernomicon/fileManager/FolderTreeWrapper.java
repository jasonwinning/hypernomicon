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

package org.hypernomicon.fileManager;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Comparator;
import java.util.List;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.tree.AbstractTreeWrapper;
import org.hypernomicon.tree.TreeModel;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.file.FilePath;

import static org.hypernomicon.model.records.RecordType.*;

import com.google.common.collect.ImmutableList;

import javafx.scene.control.*;
import javafx.scene.input.DragEvent;

//---------------------------------------------------------------------------

public class FolderTreeWrapper extends AbstractTreeWrapper<FileRow>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final TreeView<FileRow> tv;
  private final TreeModel<FileRow> treeModel;
  private final FileTable fileTable;
  private final FolderHistory folderHistory;

//---------------------------------------------------------------------------

  TreeModel<FileRow> getTreeModel()                                      { return treeModel; }

  @Override protected TreeItem<FileRow> getRoot()                        { return tv.getRoot(); }
  @Override public Control getControl()                                  { return tv; }
  @Override public void expandMainBranches()                             { treeModel.expandMainBranch(); }
  @Override public SelectionModel<TreeItem<FileRow>> getSelectionModel() { return tv.getSelectionModel(); }
  @Override public void scrollToNdx(int ndx)                             { tv.scrollTo(ndx); }
  @Override public List<FileRow> getRowsForRecord(HDT_Record record)     { return ImmutableList.copyOf(treeModel.getRowsForRecord(record)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FolderTreeWrapper(TreeView<FileRow> tv, FileTable fileTable, FolderHistory folderHistory)
  {
    this.tv = tv;
    this.fileTable = fileTable;
    this.folderHistory = folderHistory;

    treeModel = new TreeModel<>(this);

    clear();

    db.addCloseDBHandler    (this::reset);
    db.addPreDBChangeHandler(this::reset);
    db.addDeleteHandler     (this::recordDeleted);

    tv.setCellFactory(treeView ->
    {
      TreeCell<FileRow> row = new TreeCell<>()
      {
        @Override protected void updateItem(FileRow newValue, boolean empty)
        {
          super.updateItem(newValue, empty);

          if (empty || (newValue == null) || (newValue.getFilePath() == null))
          {
            setText(null);
            setGraphic(null);
            return;
          }

          String fileName = newValue.getFileName();
          setText(fileName);
          setToolTip(this, fileName.length() >= FILENAME_LENGTH_TO_SHOW_TOOLTIP ? fileName : null);

          setGraphic(imgViewForRecord(newValue.getRecord(), hdtFolder));
        }
      };

      setupDragHandlers(row);

      // A row whose folder record expired during deletion has no path and gets no menu

      buildContextMenuOnRequest(row, () -> nullSwitch(row.getItem(), null, fileRow -> fileRow.getFilePath() == null ? null : fileRow),
                                fileTable.getContextMenuSchemata());

      return row;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clear()
  {
    if (tv.getRoot() != null)
    {
      tv.getRoot().getChildren().clear();
      tv.setRoot(null);
    }

    tv.setRoot(new TreeItem<>(null));
    tv.setShowRoot(false);

    treeModel.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void reset()
  {
    clear();

    treeModel.reset(db.getRootFolder());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Purges a deleted folder from the tree model's record-keyed maps and from the folder history, by
   * identity, as anything holding record references must on deletion. During a prune the model leaves
   * the tree items to the prune's own pass; otherwise it removes the folder's node as well.
   */
  private void recordDeleted(HDT_Record record)
  {
    if (record instanceof HDT_Folder folder)
    {
      treeModel.removeRecord(folder);
      folderHistory.removeRecord(folder);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public FileRow newRow(HDT_Record record, TreeModel<FileRow> treeModel)
  {
    if (record.getType() == hdtFolder)
      return new FileRow(((HDT_RecordWithPath)record).getPath(), true, treeModel);

    internalErrorPopup(18726);
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refresh()
  {
    sortNode(getRoot());

    // Reset the cell factory to force the VirtualFlow to discard all cells and
    // create fresh ones. Otherwise JavaFX bugginess can leave cells with stale
    // indentation (wrong tree depth), producing click dead zones shifted by one
    // indentation level. Recreating cells from the final tree structure ensures
    // correct layout. Potentially related to:
    // https://bugs.openjdk.org/browse/JDK-8260977
    // https://bugs.openjdk.org/browse/JDK-8252811

    var factory = tv.getCellFactory();
    tv.setCellFactory(null);
    tv.setCellFactory(factory);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void sortNode(TreeItem<FileRow> nodeItem)
  {
    nodeItem.getChildren().forEach(this::sortNode);

    nodeItem.getChildren().sort(Comparator.comparing(TreeItem::getValue));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Reconciles the folder records with the disk. Walks the tree and, for each folder that is not in
   * use ({@link HyperPath#isInUse()}: no records associated with it or its subfolders, and not a
   * special folder) whose directory no longer exists, deletes its record tree and removes its node;
   * a node whose record is already gone is removed as well. A folder that is still in use keeps its
   * record and node even without its directory, so that no association is dropped silently;
   * {@code HDT_Folder.checkExists} reports such folders at load.
   * <p>
   * Apart from {@link HDT_Folder#delete()}, which handles a folder the File Manager deletes itself,
   * this is where the records of vanished directories are deleted. A directory removed outside the
   * application, by another instance, or through {@code FileDeletion} elsewhere in it reaches the
   * File Manager only as a prune-and-refresh request, and the check of {@code exists()} here does
   * not depend on which watcher events arrived, if any. That is also why the Refresh button runs a
   * prune, and why the File Manager's delete flow calls one itself while the watcher is stopped.
   * <p>
   * Runs on the FX thread. The watcher thread leaves folder-record deletion to this walk because
   * doing it there raced with it (see the deletion case in {@code FolderTreeWatcher}). While the
   * walk runs, {@code pruningOperationInProgress} tells the model's delete handler to leave the tree
   * items alone, since {@code removeIf} is iterating the children here; the handler still purges the
   * records' rows and their folder-history entries.
   */
  void prune()
  {
    treeModel.pruningOperationInProgress = true; // prevent ConcurrentModificationException

    pruneNode(getRoot());

    treeModel.pruningOperationInProgress = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pruneNode(TreeItem<FileRow> nodeItem)
  {
    nodeItem.getChildren().removeIf(childItem ->
    {
      FileRow fileRow = childItem.getValue();
      if (fileRow != null)
      {
        HyperPath hyperPath = fileRow.getHyperPath();
        if (hyperPath.isInUse() == false)
        {
          HDT_Folder folder = (HDT_Folder) hyperPath.getRecord();
          if (folder == null) return true;

          FilePath filePath = folder.filePath();

          if (FilePath.isEmpty(filePath) || (filePath.exists() == false))
          {
            HDT_Folder.deleteFolderRecordTree(folder);  // the delete handler purges the records' rows and history entries
            return true;
          }
        }
      }

      pruneNode(childItem);
      return false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDone()
  {
    fileTable.dragDone();
    dragReset();
  }

  @Override public void startDrag(FileRow row)
  {
    fileTable.startDragFromFolderTree(row);
  }

  @Override public void dragDroppedOnto(FileRow row)
  {
    fileTable.dragDroppedOnto(row);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isValidDragTarget(FileRow targetRow, DragEvent dragEvent, TreeItem<FileRow> treeItem)
  {
    scroll(dragEvent);

    if (((fileTable.draggingItems == null) && (dragEvent.getDragboard().hasFiles() == false)) || (targetRow == null) || (targetRow.isDirectory() == false)) return false;

    if (dragEvent.getDragboard().hasFiles())
      fileTable.draggingItems = dragEvent.getDragboard().getFiles().stream().map(EntityWithPath::new).toList();

    if (fileTable.draggingItems.size() == 1)
    {
      FilePath srcPath = fileTable.draggingItems.getFirst().getFilePath();
      if (srcPath.equals(targetRow.getFilePath()) || srcPath.getDirOnly().equals(targetRow.getFilePath()))
        return false;
    }

    expand(treeItem);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
