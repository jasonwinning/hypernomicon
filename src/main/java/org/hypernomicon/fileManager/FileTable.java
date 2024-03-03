/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;

import java.io.IOException;
import java.nio.file.DirectoryIteratorException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.PopupDialog;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.DragNDropContainer;
import org.hypernomicon.view.wrappers.HyperTable;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.Control;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeItem;
import javafx.scene.input.DragEvent;
import javafx.scene.input.TransferMode;
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
      if (sortVal.getClass() != ((FileCellValue<Comp_T>) obj).sortVal.getClass()) return false;

      FileCellValue<Comp_T> other = (FileCellValue<Comp_T>)obj;
      return sortVal.equals(other.sortVal);
    }
  }

//---------------------------------------------------------------------------

  private final FileManager dlg;
  private final TableView<FileRow> fileTV;
  private final ObservableList<FileRow> rows;
  List<? extends AbstractEntityWithPath> draggingItems;

  void clear() { rows.clear(); }

  @Override protected Control getControl() { return fileTV; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean refreshing = false;

  @SuppressWarnings("unchecked") FileTable(TableView<FileRow> fileTV, FileManager dlg)
  {
    this.dlg = dlg;
    this.fileTV = fileTV;
    rows = FXCollections.observableArrayList();

    HyperTable.registerTable(fileTV, PREF_KEY_HT_MGR_FILES, dlg);

    fileTV.setItems(rows);
    fileTV.setPlaceholder(new Text("This folder is empty."));

    fileTV.setOnSort(event ->
    {
      if (refreshing) return;
      refreshing = true;
      dlg.refresh();
      refreshing = false;
    });

    fileTV.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);

    TableColumn<FileRow, FileRow>                nameCol    = (TableColumn<FileRow, FileRow>)                fileTV.getColumns().get(0);
    TableColumn<FileRow, FileCellValue<Instant>> modDateCol = (TableColumn<FileRow, FileCellValue<Instant>>) fileTV.getColumns().get(1);
    TableColumn<FileRow, String>                 typeCol    = (TableColumn<FileRow, String>)                 fileTV.getColumns().get(2);
    TableColumn<FileRow, FileCellValue<Long>>    sizeCol    = (TableColumn<FileRow, FileCellValue<Long>>)    fileTV.getColumns().get(3);
    TableColumn<FileRow, String>                 recordsCol = (TableColumn<FileRow, String>)                 fileTV.getColumns().get(4);

    nameCol.setCellValueFactory(cellData -> new SimpleObjectProperty<>(cellData.getValue()));
    nameCol.setComparator(FileRow::compareTo);

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
          setTooltip(null);
        }
        else
        {
          String fileName = item.getFileName();
          setText(fileName);
          setToolTip(this, fileName.length() >= FILENAME_LENGTH_TO_SHOW_TOOLTIP ? fileName : null);
          setGraphic(item.getGraphic());
        }
      }
    });

    fileTV.setOnDragDone(event ->
    {
      try
      {
        dragDone();
      }
      catch (Throwable th)
      {
        th.printStackTrace();
      }

      event.consume();
    });

    fileTV.setOnDragDropped(event ->
    {
      try
      {
        dragDroppedOnto(dlg.getFolderRow());
      }
      catch (Throwable th)
      {
        th.printStackTrace();
      }

      event.consume();
    });

    fileTV.setOnDragOver(event ->
    {
      try
      {
        if (isValidDragTarget(dlg.getFolderRow(), event, null))
          event.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);
      }
      catch (Throwable th)
      {
        th.printStackTrace();
      }

      event.consume();
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
      for (Path entry: stream)
      {
        FilePath filePath = new FilePath(entry);

        Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

        if (set.size() > 0)
        {
          FileRow row = new FileRow(set.iterator().next(), null);

          if (filePath.isDirectory())
          {
            addToSortedList(rows, row);

            for (TreeItem<FileRow> childTreeItem : parentTreeItem.getChildren())
            {
              FileRow fileRow = childTreeItem.getValue();
              if (fileRow.getRecord().getID() > 0)  // a deleted folder might still be in the tree at this point; the delete recordHandler gets called
                                                    // in a Platform.runLater call
                if (fileRow.getFilePath().equals(filePath))
                {
                  row.setFolderTreeItem(childTreeItem);
                  break;
                }
            }
          }
          else
            addToSortedList(rows, row);
        }
        else
        {
          addToSortedList(rows, new FileRow(new HyperPath(filePath), null));
        }
      }
    }
    catch (DirectoryIteratorException | IOException ex)
    {
      messageDialog("An error occurred while displaying the folder's contents: " + getThrowableMessage(ex), mtError);
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
    draggingItems = dlg.getMarkedRows(fileRow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void startDragFromFolderTree(FileRow fileRow)
  {
    draggingItems = List.of(new EntityWithRow(fileRow));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void dragDone()
  {
    draggingItems = null;
    dragReset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isValidDragTarget(FileRow targetRow, DragEvent dragEvent, TreeItem<FileRow> treeItem)
  {
    scroll(dragEvent);

    if ((draggingItems == null) && (dragEvent.getDragboard().hasFiles() == false)) return false;
    if ((targetRow == null) || (targetRow.isDirectory() == false))
      targetRow = dlg.getFolderRow();

    if (targetRow == null) return false;

    if (dragEvent.getDragboard().hasFiles())
      draggingItems = dragEvent.getDragboard().getFiles().stream().map(EntityWithPath::new).collect(Collectors.toList());

    if ((draggingItems == null) || (draggingItems.size() != 1)) return true;

    FilePath srcPath = draggingItems.get(0).getFilePath();

    return (srcPath             .equals(targetRow.getFilePath()) == false) &&
           (srcPath.getDirOnly().equals(targetRow.getFilePath()) == false);
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

    if (dlg.moveCopy(draggingItems, copying, true) == false) return;

    if ((targetRow == null) || (targetRow.isDirectory() == false))
      targetRow = dlg.getFolderRow();

    dlg.paste(targetRow, copying, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
