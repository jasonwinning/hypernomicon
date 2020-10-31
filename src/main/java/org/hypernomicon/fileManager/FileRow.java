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

package org.hypernomicon.fileManager;

import java.io.IOException;
import java.time.Instant;

import org.apache.commons.io.FilenameUtils;
import org.apache.tika.mime.MediaType;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;

import org.hypernomicon.fileManager.FileTable.*;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.tree.AbstractTreeRow;
import org.hypernomicon.tree.TreeModel;
import org.hypernomicon.util.filePath.FilePath;

import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class FileRow extends AbstractTreeRow<HDT_RecordWithPath, FileRow>
{
  private final HyperPath hyperPath;
  private MediaType mimetype = null;

//---------------------------------------------------------------------------

  FileRow(HyperPath hyperPath, TreeModel<FileRow> treeModel)
  {
    super(treeModel);
    this.hyperPath = hyperPath;

    if (treeModel != null)
      treeItem = new TreeItem<>(this);
  }

//---------------------------------------------------------------------------

  public FilePath getFilePath() { return hyperPath.filePath(); }
  boolean isDirectory()         { return nullSwitch(hyperPath.filePath(), false, FilePath::isDirectory); }
  public HDT_Folder getFolder() { return hyperPath.parentFolder(); }
  String getFileName()          { return hyperPath.getNameStr(); }
  HyperPath getHyperPath()      { return hyperPath; }
  private void determineType()  { if (mimetype == null) mimetype = getMediaType(hyperPath.filePath()); }

  void setFolderTreeItem(TreeItem<FileRow> treeItem) { this.treeItem  = treeItem; }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_RecordWithPath> HDT_T getRecord() { return (HDT_T) hyperPath.getRecord(); }
  
  @Override public int hashCode() { return hyperPath == null ? 0 : nullSwitch(hyperPath.getFileName(), 0, FilePath::hashCode); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FileCellValue<Instant> getModifiedDateCellValue()
  {
    Instant i = hyperPath.filePath().lastModified();

    return new FileCellValue<>(dateTimeToUserReadableStr(i), i);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  FileCellValue<Long> getSizeCellValue()
  {
    long size = 0;
    FilePath filePath = hyperPath.filePath();

    if (FilePath.isEmpty(filePath) == false)
    {
      if (filePath.isDirectory()) return new FileCellValue<>("", Long.valueOf(0));

      try                   { size = filePath.size(); }
      catch (IOException e) { return new FileCellValue<>("", Long.valueOf(-1)); }

      if (size >= 1000)
        return new FileCellValue<>(numberFormat.format(size / 1000) + " KB", Long.valueOf(size));
    }

    return new FileCellValue<>(String.valueOf(size) + " bytes", Long.valueOf(size));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTypeString()
  {
    if (isDirectory()) return "File folder";

    determineType();

    return mimetype == MediaType.OCTET_STREAM ?
      (FilenameUtils.getExtension(hyperPath.getNameStr()).toUpperCase() + " File")
    :
      mimetype.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ImageView getGraphic()
  {
    if (graphic != null) return graphic;

    boolean isDir = isDirectory();

    if (!isDir)
      determineType();

    return graphic = imgViewFromFilePath(hyperPath.filePath(), mimetype, isDir);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean rename(String newName)
  {
    if (isDirectory() && (HDT_Folder.class.cast(getRecord()).renameTo(newName) == false))
      return false;

    getTreeItem().setValue(this);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if ((obj instanceof FileRow) == false) return false;

    return compareTo((FileRow)obj) == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(FileRow o)
  {
    if (o           == null) return 1;
    if (  hyperPath == null) return o.hyperPath == null ? 0 : -1;
    if (o.hyperPath == null) return 1;

    FilePath  fileName =   hyperPath.getFileName(),
             oFileName = o.hyperPath.getFileName();

    if (FilePath.isEmpty(fileName )) return FilePath.isEmpty(oFileName) ? 0 : -1;
    if (FilePath.isEmpty(oFileName)) return 1;

    return fileName.toPath().compareTo(oFileName.toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
