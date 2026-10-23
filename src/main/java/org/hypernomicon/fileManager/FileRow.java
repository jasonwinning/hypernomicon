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

import java.nio.file.attribute.BasicFileAttributes;
import java.time.Instant;
import java.util.Locale;

import org.apache.commons.io.FilenameUtils;
import org.apache.tika.mime.MediaType;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
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
  private final ObjectCellValue<Instant> modifiedDateCellValue;
  private final ObjectCellValue<Long> sizeCellValue;
  private MediaType mimetype = null;

//---------------------------------------------------------------------------

  /**
   * A row for the file table. Its date and size come from {@code attrs}, read when the table listed
   * the folder, so that showing or sorting by the Modified and Size columns never goes back to the
   * disk; the table builds new rows each time it lists a folder. Null {@code attrs} (the entry could
   * not be read) make a file row with no date or size.
   */
  FileRow(HyperPath hyperPath, BasicFileAttributes attrs)
  {
    this(hyperPath, (attrs != null) && attrs.isDirectory(), attrs, null);
  }

  /** A row without a date or size. Only the file table shows those, and it builds its rows from attributes. */
  FileRow(HyperPath hyperPath, boolean isDir)
  {
    this(hyperPath, isDir, null, null);
  }

  FileRow(HyperPath hyperPath, boolean isDir, TreeModel<FileRow> treeModel)
  {
    this(hyperPath, isDir, null, treeModel);
  }

  private FileRow(HyperPath hyperPath, boolean isDir, BasicFileAttributes attrs, TreeModel<FileRow> treeModel)
  {
    super(treeModel);
    this.hyperPath = hyperPath;
    this.isDir = isDir;

    modifiedDateCellValue = modifiedDateCellValueFrom(attrs);
    sizeCellValue         = sizeCellValueFrom(attrs, isDir);

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

  ObjectCellValue<Instant> getModifiedDateCellValue() { return modifiedDateCellValue; }
  ObjectCellValue<Long> getSizeCellValue()            { return sizeCellValue; }

  void setFolderTreeItem(TreeItem<FileRow> treeItem)  { this.treeItem  = treeItem; }

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

  /** The Modified column's value, truncated to milliseconds as {@link FilePath#lastModified()}
   *  truncates it; blank without attributes. */
  private static ObjectCellValue<Instant> modifiedDateCellValueFrom(BasicFileAttributes attrs)
  {
    if (attrs == null)
      return new ObjectCellValue<>("", NO_DATE);

    Instant modified = Instant.ofEpochMilli(attrs.lastModifiedTime().toMillis());
    return new ObjectCellValue<>(dateTimeToUserReadableStr(modified), modified);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The Size column's value: blank for a folder, and blank with a value of -1 for a file whose
   *  attributes could not be read. */
  private static ObjectCellValue<Long> sizeCellValueFrom(BasicFileAttributes attrs, boolean isDir)
  {
    if (isDir)
      return new ObjectCellValue<>("", 0L);

    if (attrs == null)
      return new ObjectCellValue<>("", (long) -1);

    return new ObjectCellValue<>(formatFileSize(attrs.size(), false, 0), attrs.size());
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

    return FilePath.compareFileNames(fileName.toString(), oFileName.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
