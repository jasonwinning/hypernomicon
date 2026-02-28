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

import java.io.IOException;
import java.time.Instant;
import java.util.Locale;

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
import org.hypernomicon.util.file.FilenameRules;
import org.hypernomicon.util.file.FilePath;
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

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * When the FileRow has an associated database record, the hash is based on that
   * record's identity. When there is no record (a file on disk not tracked by the
   * database), the hash is based on the filesystem-normalized filename alone. This
   * is sufficient because recordless FileRows are only created by
   * {@link FileTable#update}, which lists a single folder's contents; within one
   * folder, the filesystem guarantees filename uniqueness under its own normalization
   * rules, so the normalized name uniquely identifies the entry.
   */
  @Override public int hashCode()
  {
    HDT_RecordWithPath record = getRecord();

    if (record != null)
      return record.hashCode();

    // No record; use normalized filename (only compared within same folder)

    if (hyperPath == null) return 0;

    return FilenameRules.current().normalize(hyperPath.getNameStr()).hashCode();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * When both FileRows have associated database records, equality is determined by
   * record identity. When neither has a record, equality is determined by
   * filesystem-normalized filename alone. This is sufficient because recordless
   * FileRows are only created by {@link FileTable#update}, which lists a single
   * folder's contents; within one folder, the filesystem guarantees filename
   * uniqueness under its own normalization rules, so the normalized name uniquely
   * identifies the entry.
   */
  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if ((obj instanceof FileRow) == false) return false;

    FileRow other = (FileRow) obj;

    HDT_RecordWithPath thisRecord = getRecord(),
                       otherRecord = other.getRecord();

    // Both have records; compare by record identity

    if ((thisRecord != null) && (otherRecord != null))
      return thisRecord == otherRecord;

    // One has record, other doesn't; not equal

    if ((thisRecord != null) || (otherRecord != null))
      return false;

    // Neither has record; compare by normalized filename

    if (hyperPath == null) return other.hyperPath == null;
    if (other.hyperPath == null) return false;

    String fileNameStr = hyperPath.getNameStr(),
           otherFileNameStr = other.hyperPath.getNameStr();

    return FilenameRules.current().normalize(fileNameStr).equals(FilenameRules.current().normalize(otherFileNameStr));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ObjectCellValue<Instant> getModifiedDateCellValue()
  {
    FilePath filePath = hyperPath.filePath();

    if (FilePath.isEmpty(filePath))
      return new ObjectCellValue<>("", null);

    Instant i = filePath.lastModified();

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
      (FilenameUtils.getExtension(hyperPath.getNameStr()).toUpperCase(Locale.ROOT) + " File")
    :
      mimetype.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ImageView getGraphic()
  {
    if (graphic != null) return graphic;

    if (isDirectory()) return graphic = imgViewForRecord(getRecord(), hdtFolder);

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

  /**
   * Provides display ordering for the File Manager: directories sort before files,
   * filenames with numeric suffixes sort naturally (file2 before file10), and
   * everything else is ordered by {@link FilenameRules}-normalized filename. This
   * ensures the sort order respects the filesystem's comparison rules (such as
   * case-insensitive ordering on case-insensitive filesystems) and is consistent
   * with {@link #equals} and {@link #hashCode}.
   */
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

    FilenameRules rules = FilenameRules.current();

    if (Character.isDigit(fileNameStr1.charAt(fileNameStr1.length() - 1)) &&
        Character.isDigit(fileNameStr2.charAt(fileNameStr2.length() - 1)))
    {
      PrefixAndNumber pn1 = splitIntoPrefixAndNumber(fileNameStr1),
                      pn2 = splitIntoPrefixAndNumber(fileNameStr2);

      if ((pn1.number() >= 0) && (pn2.number() >= 0) && rules.normalize(pn1.prefix()).equals(rules.normalize(pn2.prefix())))
        return pn1.number() - pn2.number();
    }

    return rules.normalize(fileNameStr1).compareTo(rules.normalize(fileNameStr2));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
