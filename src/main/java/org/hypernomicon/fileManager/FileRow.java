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

import java.io.IOException;
import java.nio.file.Path;
import java.time.Instant;

import org.apache.commons.io.FilenameUtils;
import org.apache.tika.mime.MediaType;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.MediaUtil.*;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_RecordWithPath;
import org.hypernomicon.tree.AbstractTreeRow;
import org.hypernomicon.tree.TreeModel;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.ObjectCellValue;

import javafx.scene.control.TreeItem;
import javafx.scene.image.ImageView;

//---------------------------------------------------------------------------

public class FileRow extends AbstractTreeRow<HDT_RecordWithPath, FileRow>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperPath hyperPath;
  private final boolean isDir;
  private MediaType mimetype = null;

//---------------------------------------------------------------------------

  FileRow(HyperPath hyperPath, boolean isDir)
  {
    this(hyperPath, isDir, null);
  }

  FileRow(HyperPath hyperPath, boolean isDir, TreeModel<FileRow> treeModel)
  {
    super(treeModel);
    this.hyperPath = hyperPath;
    this.isDir = isDir;

    if (treeModel != null)
      treeItem = new TreeItem<>(this);
  }

//---------------------------------------------------------------------------

  public FilePath getFilePath() { return hyperPath.filePath(); }
  boolean isDirectory()         { return isDir; }
  public HDT_Folder getFolder() { return hyperPath.parentFolder(); }
  String getFileName()          { return hyperPath.getNameStr(); }
  HyperPath getHyperPath()      { return hyperPath; }
  private void determineType()  { if (mimetype == null) mimetype = getMediaType(hyperPath.filePath()); }

  void setFolderTreeItem(TreeItem<FileRow> treeItem) { this.treeItem  = treeItem; }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_RecordWithPath> HDT_T getRecord() { return (HDT_T) hyperPath.getRecord(); }

  @Override public int hashCode()             { return hyperPath == null ? 0 : nullSwitch(hyperPath.getFileName(), 0, FilePath::hashCode); }
  @Override public boolean equals(Object obj) { return (this == obj) || ((obj instanceof FileRow fileRow) && (compareTo(fileRow) == 0)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ObjectCellValue<Instant> getModifiedDateCellValue()
  {
    Instant i = hyperPath.filePath().lastModified();

    return new ObjectCellValue<>(dateTimeToUserReadableStr(i), i);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ObjectCellValue<Long> getSizeCellValue()
  {
    long size = 0L;
    FilePath filePath = hyperPath.filePath();

    if (FilePath.isEmpty(filePath) == false)
    {
      if (filePath.isDirectory()) return new ObjectCellValue<>("", 0L);

      try                   { size = filePath.size(); }
      catch (IOException e) { return new ObjectCellValue<>("", (long) -1); }

      if (size >= 1000L)
        return new ObjectCellValue<>(numberFormat.format(size / 1000L) + " KB", size);
    }

    return new ObjectCellValue<>(size + " bytes", size);
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

  ImageView getGraphic()
  {
    if (graphic != null) return graphic;

    if (isDirectory()) return graphic = imgViewForRecordType(hdtFolder);

    determineType();

    return graphic = imgViewFromFilePath(hyperPath.filePath(), mimetype);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean rename(String newName)
  {
    if (isDirectory() && (((HDT_Folder) getRecord()).renameTo(newName) == false))
      return false;

    getTreeItem().setValue(this);
    return true;
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

    if (FilePath.isEmpty( fileName)) return FilePath.isEmpty(oFileName) ? 0 : -1;
    if (FilePath.isEmpty(oFileName)) return 1;

    if (  isDirectory() && (o.isDirectory() == false)) return -1;
    if (o.isDirectory() && (  isDirectory() == false)) return 1;

    String fileNameStr1 =  fileName.toString(),
           fileNameStr2 = oFileName.toString();

    if (Character.isDigit(fileNameStr1.charAt(fileNameStr1.length() - 1)) &&
        Character.isDigit(fileNameStr2.charAt(fileNameStr2.length() - 1)))
    {
      StringBuilder prefix1 = new StringBuilder(),
                    prefix2 = new StringBuilder();

      int num1 = splitIntoPrefixAndNumber(fileNameStr1, prefix1),
          num2 = splitIntoPrefixAndNumber(fileNameStr2, prefix2);

      if ((num1 >= 0) && (num2 >= 0))
      {
        prefix1.append('A');  // Adding 'A' to
        prefix2.append('A');  // avoid InvalidPathException

        if (Path.of(prefix1.toString()).equals(Path.of(prefix2.toString())))
          return num1 - num2;
      }
    }

    return fileName.toPath().compareTo(oFileName.toPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
