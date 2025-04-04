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

package org.hypernomicon.fileManager;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.Comparator;
import java.util.List;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.tree.AbstractTreeWrapper;
import org.hypernomicon.tree.TreeModel;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.filePath.FilePath;

import static org.hypernomicon.model.records.RecordType.*;

import com.google.common.collect.ImmutableList;

import javafx.scene.control.Control;
import javafx.scene.control.SelectionModel;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeItem;
import javafx.scene.control.TreeView;
import javafx.scene.image.ImageView;
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

    tv.setCellFactory(treeView ->
    {
      TreeCell<FileRow> row = new TreeCell<>();

      ImageView openImage = imgViewForRecordType(hdtFolder);

      row.itemProperty().addListener((ob, oldValue, newValue) ->
      {
        if (oldValue == newValue) return;

        if (newValue == null)
        {
          row.setText(null);
          row.setGraphic(null);
          row.setContextMenu(null);
          return;
        }

        if (newValue.getFilePath() == null) // happens right before a filerow is deleted sometimes
          return;

        String fileName = newValue.getFileName();
        row.setText(fileName);
        setToolTip(row, fileName.length() >= FILENAME_LENGTH_TO_SHOW_TOOLTIP ? fileName : null);

        if (row.getGraphic() == null)
          row.setGraphic(openImage);

        row.setContextMenu(createContextMenu(newValue, fileTable.getContextMenuSchemata()));
      });

      setupDragHandlers(row);

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

    tv.refresh();
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
            folderHistory.removeRecord(folder);
            HDT_Folder.deleteFolderRecordTree(folder);
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
      FilePath srcPath = fileTable.draggingItems.get(0).getFilePath();
      if (srcPath.equals(targetRow.getFilePath()) || srcPath.getDirOnly().equals(targetRow.getFilePath()))
        return false;
    }

    expand(treeItem);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
