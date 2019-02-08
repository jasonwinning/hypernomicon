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

package org.hypernomicon.view.fileManager;

import java.io.IOException;
import java.time.Instant;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.apache.tika.mime.MediaType;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.fileManager.FileTable.CondFileRowHandler;
import org.hypernomicon.view.fileManager.FileTable.FileCellValue;
import org.hypernomicon.view.fileManager.FileTable.FileRowHandler;
import org.hypernomicon.view.fileManager.FileTable.FileRowMenuItem;
import org.hypernomicon.view.fileManager.FileTable.FileRowMenuItemSchema;
import org.hypernomicon.view.wrappers.AbstractTreeRow;
import org.hypernomicon.view.wrappers.TreeModel;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class FileRow extends AbstractTreeRow<FileRow>
{
  private HyperPath hyperPath;
  private MediaType mimetype = null;

//---------------------------------------------------------------------------

  FileRow(HyperPath hyperPath, TreeModel<FileRow> treeModel)
  {
    this.hyperPath = hyperPath;
    this.treeModel = treeModel;

    if (treeModel != null)
      treeItem = new TreeItem<>(this);
  }

//---------------------------------------------------------------------------

  FilePath getFilePath()   { return hyperPath.getFilePath(); }
  boolean isDirectory()    { return hyperPath.getFilePath().isDirectory(); }
  HDT_Folder getFolder()   { return hyperPath.getParentFolder(); }
  String getFileName()     { return hyperPath.getNameStr(); }
  HyperPath getHyperPath() { return hyperPath; }
  private void determineType()    { if (mimetype == null) mimetype = getMediaType(hyperPath.getFilePath()); }

  void setFolderTreeItem(TreeItem<FileRow> treeItem) { this.treeItem  = treeItem; }

  @Override public HDT_RecordWithPath getRecord() { return hyperPath.getRecord(); }
  @Override public HDT_RecordType getRecordType() { return hyperPath.getRecordType(); }
  @Override public int getRecordID()              { return hyperPath.getRecordID(); }

  static FileRowMenuItemSchema addContextMenuItem(String caption, FileRowHandler handler, List<FileRowMenuItemSchema> contextMenuSchemata)
  {
    return addCondContextMenuItem(caption, fileRow -> true, handler, contextMenuSchemata);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FileCellValue<Instant> getModifiedDateCellValue()
  {
    Instant i = hyperPath.getFilePath().lastModified();

    return new FileCellValue<>(dateTimeToUserReadableStr(i), i);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FileCellValue<Long> getSizeCellValue()
  {
    long size;

    try                   { size = hyperPath.getFilePath().size(); }
    catch (IOException e) { return new FileCellValue<>("", Long.valueOf(-1)); }

    if (size >= 1000)
      return new FileCellValue<>(numberFormat.format(size / 1000) + " KB", Long.valueOf(size));

    return new FileCellValue<>(String.valueOf(size) + " bytes", Long.valueOf(size));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTypeString()
  {
    if (isDirectory()) return "File folder";

    determineType();

    if (mimetype == MediaType.OCTET_STREAM)
      return FilenameUtils.getExtension(hyperPath.getNameStr()).toUpperCase() + " File";

    return mimetype.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ImageView getGraphic()
  {
    if (graphic != null) return graphic;

    boolean isDir = isDirectory();

    if (!isDir)
      determineType();

    graphic = getImageViewForRelativePath(getImageRelPathForFilePath(hyperPath.getFilePath(), mimetype, isDir));
    return graphic;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static FileRowMenuItemSchema addCondContextMenuItem(String caption, CondFileRowHandler condHandler, FileRowHandler handler, List<FileRowMenuItemSchema> contextMenuSchemata)
  {
    FileRowMenuItemSchema mnu;

    mnu = new FileRowMenuItemSchema(caption);
    mnu.condHandler = condHandler;
    mnu.handler = handler;

    contextMenuSchemata.add(mnu);
    return mnu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static ContextMenu createContextMenu(FileRow fileRow, List<FileRowMenuItemSchema> contextMenuSchemata)
  {
    boolean noneVisible = true;
    ContextMenu rowMenu = new ContextMenu();

    for (FileRowMenuItemSchema schema : contextMenuSchemata)
    {
      if (schema.condHandler.handle(fileRow))
      {
        FileRowMenuItem newItem = new FileRowMenuItem(schema.caption, schema);

        newItem.setOnAction(event ->
        {
          rowMenu.hide();
          schema.handler.handle(fileRow);
        });
        rowMenu.getItems().add(newItem);

        noneVisible = false;
      }
    }

    rowMenu.setOnShowing(event ->
    {
      for (MenuItem menuItem : rowMenu.getItems())
      {
        FileRowMenuItem rowItem = (FileRowMenuItem)menuItem;
        rowItem.setVisible(rowItem.schema.visible);
        rowItem.setDisable(rowItem.schema.disabled);
      }
    });

    if (noneVisible) return null;
    return rowMenu;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean rename(String newName)
  {
    if (isDirectory())
    {
      HDT_Folder theFolder = (HDT_Folder)getRecord();
      if (theFolder.renameTo(newName) == false) return false;
    }

    TreeItem<FileRow> item = getTreeItem();
    item.setValue(this);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(FileRow o)
  {
    if (hyperPath == null) return 0;
    if (o == null) return 0;
    if (o.hyperPath == null) return 0;

    FilePath fileName = hyperPath.getFileName();
    if (FilePath.isEmpty(fileName)) return 0;

    FilePath oFileName = o.hyperPath.getFileName();
    if (FilePath.isEmpty(oFileName)) return 0;

    return fileName.toPath().compareTo(oFileName.toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
