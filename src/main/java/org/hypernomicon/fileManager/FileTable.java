/*
 * Copyright 2015-2021 Jason Winning
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

import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.App.*;

import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Set;

import org.hypernomicon.fileManager.FileManager.MarkedRowInfo;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.DragNDropContainer;
import org.hypernomicon.view.wrappers.HyperTable;

import com.google.common.collect.Lists;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeItem;
import javafx.scene.input.DragEvent;
import javafx.scene.text.Text;

public class FileTable extends DragNDropContainer<FileRow>
{

//---------------------------------------------------------------------------

  static class FileCellValue<Comp_T extends Comparable<Comp_T>> implements Comparable<FileCellValue<Comp_T>>
  {
    private final String text;
    private final Comparable<Comp_T> sortVal;

    FileCellValue(String text, Comparable<Comp_T> sortVal)
    {
      this.text = text;
      this.sortVal = sortVal;
    }

    @Override public String toString() { return text; }

    @SuppressWarnings("unchecked")
    @Override public int compareTo(FileCellValue<Comp_T> other)
    {
      return sortVal.compareTo((Comp_T) other.sortVal);
    }

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((sortVal == null) ? 0 : sortVal.hashCode());
      return result;
    }

    @SuppressWarnings("unchecked")
    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;
      if (sortVal.getClass() != FileCellValue.class.cast(obj).sortVal.getClass()) return false;

      FileCellValue<Comp_T> other = (FileCellValue<Comp_T>)obj;
      return sortVal.equals(other.sortVal);
    }
  }

//---------------------------------------------------------------------------

  private final TableView<FileRow> fileTV;
  private final ObservableList<FileRow> rows;
  List<MarkedRowInfo> draggingRows;

  void clear() { rows.clear(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean refreshing = false;

  @SuppressWarnings("unchecked") FileTable(TableView<FileRow> fileTV, String prefID)
  {
    super(fileTV);

    this.fileTV = fileTV;
    rows = FXCollections.observableArrayList();

    if (prefID.length() > 0)
      HyperTable.registerTable(fileTV, prefID, fileManagerDlg);

    fileTV.setItems(rows);
    fileTV.setPlaceholder(new Text("This folder is empty."));

    fileTV.setOnSort(event ->
    {
      if (refreshing) return;
      refreshing = true;
      fileManagerDlg.refresh();
      refreshing = false;
    });

    fileTV.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

    TableColumn<FileRow, FileRow>                nameCol    = (TableColumn<FileRow, FileRow>)                fileTV.getColumns().get(0);
    TableColumn<FileRow, FileCellValue<Instant>> modDateCol = (TableColumn<FileRow, FileCellValue<Instant>>) fileTV.getColumns().get(1);
    TableColumn<FileRow, String>                 typeCol    = (TableColumn<FileRow, String>)                 fileTV.getColumns().get(2);
    TableColumn<FileRow, FileCellValue<Long>>    sizeCol    = (TableColumn<FileRow, FileCellValue<Long>>)    fileTV.getColumns().get(3);
    TableColumn<FileRow, String>                 recordsCol = (TableColumn<FileRow, String>)                 fileTV.getColumns().get(4);

    nameCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue()));
    nameCol.setComparator((v1, v2) -> v1.getFileName().compareToIgnoreCase(v2.getFileName()));

    modDateCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getModifiedDateCellValue()));

    typeCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getTypeString()));
    sizeCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue().getSizeCellValue()));
    sizeCol.setStyle( "-fx-alignment: CENTER-RIGHT;");

    recordsCol.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().getHyperPath().getRecordsString()));

    nameCol.setCellFactory(col -> new TableCell<>()
    {
      @Override public void updateItem(FileRow item, boolean empty)
      {
        super.updateItem(item, empty);

        if (empty || (item == null))
        {
          setText(null);
          setGraphic(null);
        }
        else
        {
          setText(item.getFileName());
          setGraphic(item.getGraphic());
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void update(HDT_Folder folder, TreeItem<FileRow> parentTreeItem)
  {
    previewWindow.disablePreviewUpdating = true;
    clear();
    previewWindow.disablePreviewUpdating = false;

    try (DirectoryStream<Path> stream = Files.newDirectoryStream(folder.filePath().toPath(), "**"))
    {
      int nextDirNdx = 0;

      for (Path entry: stream)
      {
        FilePath filePath = new FilePath(entry);

        Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

        if (set.size() > 0)
        {
          FileRow row = new FileRow(set.iterator().next(), null);

          if (filePath.isDirectory())
          {
            rows.add(nextDirNdx++, row);

            for (TreeItem<FileRow> childTreeItem : parentTreeItem.getChildren())
            {
              FileRow fileRow = childTreeItem.getValue();
              if (fileRow.getRecord().getID() > 0)  // a deleted folder might still be in the tree at this point; the delete recordHandler gets called
                                                    // in a Platform.runLater call
              {
                if (fileRow.getFilePath().equals(filePath))
                {
                  row.setFolderTreeItem(childTreeItem);
                  break;
                }
              }
            }
          }
          else
            rows.add(row);
        }
        else
        {
          rows.add(new FileRow(new HyperPath(filePath), null));
        }
      }
    }
    catch (DirectoryIteratorException | IOException ex)
    {
      messageDialog("An error occurred while displaying the folder's contents: " + ex.getMessage(), mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void selectByFileName(FilePath fileName)
  {
    FilePath nameOnly = fileName.getNameOnly();

    nullSwitch(findFirst(rows, row -> row.getFilePath().getNameOnly().equals(nameOnly)), row ->
    {
      fileTV.getSelectionModel().select(row);
      HyperTable.scrollToSelection(fileTV, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void startDrag(FileRow fileRow)
  {
    draggingRows = fileManagerDlg.getMarkedRows(fileRow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void startDragFromFolderTree(FileRow fileRow)
  {
    draggingRows = Lists.newArrayList(new MarkedRowInfo(fileRow, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDone()
  {
    draggingRows = null;
    dragReset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isValidDragTarget(FileRow targetRow, DragEvent dragEvent, TreeItem<FileRow> treeItem)
  {
    scroll(dragEvent);

    if (draggingRows == null) return false;
    if ((targetRow == null) || (targetRow.isDirectory() == false))
      targetRow = fileManagerDlg.getFolderRow();

    if (targetRow == null) return false;

    if (draggingRows.size() == 1)
    {
      FilePath srcPath = draggingRows.get(0).row.getFilePath();
      if (srcPath.equals(targetRow.getFilePath()) || srcPath.getDirOnly().equals(targetRow.getFilePath()))
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDroppedOnto(FileRow targetRow)
  {
    DialogResult result = new PopupDialog("Move or copy?")

      .addButton("Move", mrMove)
      .addButton("Copy", mrCopy)
      .addButton("Cancel", mrCancel)

      .showModal();

    if (result == mrCancel) return;

    boolean copying = (result == mrCopy);

    if (!fileManagerDlg.moveCopy(draggingRows, copying, true)) return;

    fileManagerDlg.paste(targetRow, copying, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
