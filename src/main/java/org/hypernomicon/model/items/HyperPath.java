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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Associates a database record with a location in the filesystem, expressed as a parent
 * {@link HDT_Folder} and a leaf filename.
 * <p>
 * Every {@link HDT_RecordWithPath} owns a HyperPath. The concrete record types that use it are:
 * <ul>
 *   <li>{@link HDT_Folder}: the folder itself; the parent folder pointer is the
 *       {@code rtParentFolderOfFolder} relation, and the filename is the directory name.</li>
 *   <li>{@link HDT_WorkFile}: a file attached to one or more works (via
 *       {@code rtFolderOfWorkFile}).</li>
 *   <li>{@link HDT_MiscFile}: a miscellaneous file record (via
 *       {@code rtFolderOfMiscFile}).</li>
 *   <li>{@link HDT_Person}: a person's picture file (via
 *       {@code rtPictureFolderOfPerson}).</li>
 * </ul>
 * {@link HDT_Note} and {@link HDT_Work} also implement {@code HDT_RecordWithPath} but delegate
 * to the path of a related folder or work-file record rather than owning a HyperPath directly.
 *
 * <h2>Path Resolution</h2>
 * The full filesystem path is never stored; instead, {@link #filePath()} dynamically resolves it
 * by walking the parent-folder chain: {@code parentFolder().filePath().resolve(fileName)}. This
 * means re-parenting a single folder record automatically fixes the resolved paths of all
 * descendants without updating any other records.
 *
 * <h2>Filename Map</h2>
 * Every non-empty HyperPath registers itself in
 * {@link org.hypernomicon.model.AbstractHyperDB#filenameMap db.filenameMap}, a map from leaf
 * filename strings to sets of HyperPath instances. This enables efficient reverse lookup from a
 * filesystem path to the record(s) that reference it (see {@link #getHyperPathSetForFilePath}).
 * Since multiple files in different directories can share the same filename, the map values are
 * sets.
 *
 * <h2>"In Use" Semantics</h2>
 * A file or folder is considered "in use" when its HyperPath has associated database records
 * (see {@link #isInUse()} and {@link #isInUseByRecords()}). This distinction drives file-management
 * decisions throughout the application: whether the FileManager requires deletion confirmation,
 * whether the FolderTreeWatcher warns about external renames, and whether missing folders are
 * reported.
 *
 * @see HDT_RecordWithPath
 * @see #filePath()
 * @see #getHyperPathSetForFilePath(FilePath)
 */
public class HyperPath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final HyperPath EmptyPath = new HyperPath(null);
  private final HyperObjPointer<? extends HDT_RecordWithPath, HDT_Folder> folderPtr;
  private final HDT_RecordWithPath record;
  private HDT_Folder folder = null;
  private FilePath fileName = null;

//---------------------------------------------------------------------------

  public HyperPath(HyperObjPointer<? extends HDT_RecordWithPath, HDT_Folder> folderPtr, HDT_RecordWithPath record)
  {
    this.folderPtr = folderPtr;
    this.record = record;
  }

//---------------------------------------------------------------------------

  public HyperPath(FilePath filePath)
  {
    folderPtr = null;
    record = null;

    if (FilePath.isEmpty(filePath))
      return;

    if (db.getRootPath().contains(filePath) == false)
    {
      internalErrorPopup(90177);
      return;
    }

    if (getHyperPathSetForFilePath(filePath).isEmpty() == false)
    {
      internalErrorPopup(90178);
      return;
    }

    folder = getFolderFromFilePath(filePath, false);
    assignNameInternal(filePath.getNameOnly());
  }

//---------------------------------------------------------------------------

  public HDT_RecordWithPath getRecord()     { return record; }
  public RecordType         getRecordType() { return HDT_Record.getTypeSafe(record); }
  public FilePath           getFileName()   { return fileName; }
  public HDT_Folder         parentFolder()  { return folderPtr == null ? folder : folderPtr.get(); }

  /** @see #isEmpty() */
  public boolean isNotEmpty()               { return isEmpty() == false; }

  @Override public String toString()        { return filePath().toString(); }

  public static final Collection<Tag> FOLDER_TAGS = EnumSet.of(tagFolder, tagParentFolder, tagPictureFolder);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return whether this path has no filename assigned. A HyperPath is empty when it has not
   * been associated with a file or folder on disk.
   * <p>
   * The root folder is a special case: it always returns {@code false} even though its
   * {@code fileName} field is empty, because the root folder's path is resolved via
   * {@link org.hypernomicon.model.AbstractHyperDB#getRootPath() db.getRootPath()} rather than
   * from a stored filename.
   *
   * @return True if this path has no filename and is not the root folder
   */
  public boolean isEmpty()
  {
    return (record != null) && (record.getType() == hdtFolder) && (record.getID() == ROOT_FOLDER_ID) ?
      false
    :
      FilePath.isEmpty(fileName);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final String ROOT_PATH_STR = "<Root>";

  /**
   * Return the display name for this path. For the root folder this is the constant
   * {@code "<Root>"}; for all other paths it is the leaf filename, or an empty string
   * if no filename has been assigned.
   *
   * @return The display name
   */
  public String getNameStr()
  {
    return (record != null) && (record.getType() == hdtFolder) && (record.getID() == ROOT_FOLDER_ID) ?
      ROOT_PATH_STR
    :
      (fileName == null ? "" : fileName.getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** @see #clear(boolean) */
  public void clear() { clear(true); }

  /**
   * Dissociate this path from its parent folder and filename, resetting the folder pointer to
   * {@code -1} and removing the filename from {@code filenameMap}.
   * <p>
   * If {@code deleteFile} is true and no other {@link HyperPath} still references the same
   * physical file, the database is notified that the file is no longer in use (which may
   * trigger deletion of the physical file). Pass {@code false} when the file on disk has
   * already been deleted or will be handled separately by the caller.
   *
   * @param deleteFile Whether to notify the database that the file may no longer be in use
   */
  public void clear(boolean deleteFile)
  {
    FilePath filePath = filePath();

    if (folderPtr != null)
      folderPtr.setID(-1);
    else
      folder = null;

    assignNameInternal(null);

    if (deleteFile)
      notifyIfNoLongerInUse(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void notifyIfNoLongerInUse(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)   ||
        (filePath.exists() == false) ||
        (filePath.isFile() == false))   return;

    if (getHyperPathSetForFilePath(filePath).isEmpty())
      db.fileNoLongerInUse(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Return the set of {@link HyperPath} instances whose resolved file path equals the given path.
   * <p>
   * Lookup is a two-step process: first, the leaf filename is used to find candidate entries in
   * {@code filenameMap} (a fast O(1) lookup); then each candidate's dynamically resolved
   * {@link #filePath()} is compared against the given path to filter out same-name files in
   * different directories.
   *
   * @param filePath The filesystem path to look up
   * @return A set of matching {@code HyperPath} instances, or an empty set if none match
   */
  public static Set<HyperPath> getHyperPathSetForFilePath(FilePath filePath)
  {
    return nullSwitch(db.filenameMap.get(filePath.getNameOnly().toString()), new HashSet<>(),
                      paths -> paths.stream().filter(path -> filePath.equals(path.filePath())).collect(Collectors.toSet()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath filePath()
  {
    if ((record != null) && (record.getType() == hdtFolder) && (record.getID() == ROOT_FOLDER_ID))
      return db.getRootPath();

    if (FilePath.isEmpty(fileName)) return null;

    return nullSwitch(parentFolder()    , fileName, pFolder  ->
           nullSwitch(pFolder.filePath(), fileName, parentFP -> parentFP.resolve(fileName)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Look up (and optionally create) the {@link HDT_Folder} record for a filesystem directory path.
   * <p>
   * The path must be inside the database root folder; if it is not, {@code null} is returned.
   * If a folder record already exists for the path (found via {@code filenameMap}), it is
   * returned immediately. Otherwise, if the directory exists on disk and {@code doCreateRecord}
   * is {@code true}, the method recursively ensures that all ancestor folders up to the database
   * root have records, then creates a new {@code HDT_Folder} record for this path. The new
   * record's creation/modified/view dates are taken from the directory's filesystem attributes.
   * <p>
   * When {@code doCreateRecord} is {@code false}, the method only returns a record if one already
   * exists in {@code filenameMap} for the exact path. No records are created and no side effects
   * occur.
   * <p>
   * This method is {@code synchronized} because it is called from both the FX thread (UI
   * operations, FileManager paste) and the FolderTreeWatcher thread ({@code registerTree}).
   * The check-then-create sequence (lookup in {@code filenameMap}, then
   * {@code createNewRecordFromState}) must be atomic to prevent duplicate records or stale
   * state when both threads attempt to create a folder record for the same path.
   *
   * @param dirFilePath    The filesystem path of the directory. If this points to a file rather
   *                       than a directory, it is normalized to the containing directory first.
   * @param doCreateRecord If {@code true}, create new folder records as needed for this path and
   *                       any ancestor paths that lack records. If {@code false}, only return an
   *                       existing record or {@code null}.
   * @return The {@code HDT_Folder} record for the given path, or {@code null} if the path is
   *         outside the database root, does not exist on disk, or {@code doCreateRecord} is
   *         {@code false} and no record exists.
   */
  public static synchronized HDT_Folder getFolderFromFilePath(FilePath dirFilePath, boolean doCreateRecord)
  {
    dirFilePath = dirFilePath.getDirOnly();

    if (db.getRootPath().contains(dirFilePath) == false)  // the path is not in the database folder tree
      return null;

    Set<HyperPath> set = getHyperPathSetForFilePath(dirFilePath);

    HDT_RecordWithPath folder = findFirst(set, hyperPath -> hyperPath.getRecordType() == hdtFolder, HyperPath::getRecord);
    if (folder != null) return (HDT_Folder) folder;

    if ((doCreateRecord == false) || (dirFilePath.exists() == false)) return null;

    HDT_Folder parentRecord = getFolderFromFilePath(dirFilePath.getParent(), true);

    if (parentRecord == null) return null;

    RecordState recordState = new RecordState(hdtFolder);

    try
    {
      BasicFileAttributes attribs = Files.readAttributes(dirFilePath.toPath(), BasicFileAttributes.class);

      recordState.creationDate = attribs.creationTime    ().toInstant();
      recordState.modifiedDate = attribs.lastModifiedTime().toInstant();
      recordState.viewDate     = attribs.lastAccessTime  ().toInstant();
    }
    catch (IOException e) { noOp(); }

    HDT_Folder newFolder;

    try
    {
      newFolder = db.createNewRecordFromState(recordState, true);
    }
    catch (DuplicateRecordException | RelationCycleException | SearchKeyException | RestoreException e)
    {
      throw newAssertionError(e);
    }
    catch (HDB_InternalError e)
    {
      errorPopup(e);
      return null;
    }

    newFolder.getPath().assignInternal(parentRecord, dirFilePath.getNameOnly());

    return newFolder;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_RecordWithPath createRecordAssignedToPath(RecordType type, FilePath filePath)
  {
    if ((type != hdtWorkFile) && (type != hdtMiscFile))
    {
      internalErrorPopup(42221);
      return null;
    }

    if (filePath.isDirectory())
    {
      internalErrorPopup(42231);
      return null;
    }

    HDT_Folder folder = getFolderFromFilePath(filePath.getDirOnly(), true);
    if (folder == null) return null;

    HDT_RecordWithPath file = db.createNewBlankRecord(type);
    file.getPath().assign(folder, filePath.getNameOnly());

    return file;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_RecordWithPath getRecordFromFilePath(FilePath filePath)
  {
    return findFirstHaving(getHyperPathSetForFilePath(filePath), HyperPath::getRecord, record ->
      (record.getType() == hdtMiscFile) || (record.getType() == hdtWorkFile) || (record.getType() == hdtPerson));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String alreadyInUseMessage(FilePath filePath, HDT_RecordWithPath existingRecord)
  {
    return switch (existingRecord.getType())
    {
      case hdtWorkFile ->
      {
        HDT_WorkFile workFile = (HDT_WorkFile) existingRecord;
        yield workFile.works.isEmpty() ?
          "The file: " + filePath + " is already in use as a work file, ID: " + workFile.getID()
        :
          "The file: " + filePath + " is already in use as a work file, work record ID: " + workFile.works.getFirst().getID();
      }

      case hdtMiscFile -> "The file: " + filePath + " is already in use as a miscellaneous file, record ID: " + existingRecord.getID();
      case hdtPerson   -> "The file: " + filePath + " is already in use as a picture, person record ID: " + existingRecord.getID();
      default          -> "The file: " + filePath + " is already in use. Record type: " + getTypeName(existingRecord.getType()) + " ID: " + existingRecord.getID();
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean moveToFolder(int folderID, boolean confirm, boolean changeFilename, String newName) throws IOException, HDB_InternalError
  {
    if ((folderID == -1) || (db.folders.getByID(folderID) == null))
      throw new HDB_InternalError(77392);

    FilePath srcFilePath = filePath();
    HDT_Folder newFolder = db.folders.getByID(folderID);

    if (srcFilePath.isDirectory())
      throw new HDB_InternalError(77393);

    Set<HyperPath> set = getHyperPathSetForFilePath(srcFilePath);

    FilePath destFilePath = newFolder.filePath().resolve(changeFilename ? new FilePath(newName) : srcFilePath.getNameOnly());

    if (srcFilePath.equals(destFilePath)) return true;

    if (srcFilePath.moveTo(destFilePath, confirm) == false) return false;

    db.unmapFilePath(srcFilePath);

    set.forEach(hyperPath -> hyperPath.assign(db.folders.getByID(folderID), destFilePath.getNameOnly()));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void assign(HDT_Folder parentFolder, FilePath nameOnly)
  {
    if (FilePath.isEmpty(fileName) && FilePath.isEmpty(nameOnly)) return;

    if ((FilePath.isEmpty(fileName) == false) && (FilePath.isEmpty(nameOnly) == false))
      if ((parentFolder() == parentFolder) && fileName.getNameOnly().equals(nameOnly.getNameOnly())) return;

    FilePath filePath = filePath();

    assignInternal(parentFolder, nameOnly);
    if (record != null) record.modifyNow();

    notifyIfNoLongerInUse(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void assignInternal(HDT_Folder parentFolder, FilePath nameOnly)
  {
    if (folderPtr == null)
    {
      if (record != null)
        internalErrorPopup(83902);

      folder = parentFolder;
    }
    else
      folderPtr.set(parentFolder);

    assignNameInternal(nameOnly);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void assignNameInternal(FilePath newFileName)
  {
    if (FilePath.isEmpty(fileName) == false)
      nullSwitch(db.filenameMap.get(fileName.toString()), set -> set.remove(this));

    if (FilePath.isEmpty(newFileName) == false)
    {
      newFileName = newFileName.getNameOnly();
      Set<HyperPath> set = db.filenameMap.computeIfAbsent(newFileName.toString(), _ -> ConcurrentHashMap.newKeySet());

      set.add(this);
    }

    fileName = newFileName;

    if (record != null)
      record.updateSortKey();

    if (FilePath.isEmpty(fileName))
      return;

    // now remove duplicates; for this to work, folder records have to be brought online first

    HDT_Folder parent = parentFolder();

    db.filenameMap.get(fileName.getNameOnly().toString()).removeIf(path ->
    {
      if ((path == this) || path.isEmpty()) return false;

      HDT_Folder otherParent = path.parentFolder();
      return (otherParent != null) && (parent == otherParent);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Build a human-readable summary of the database records associated with this path's file or
   * folder, formatted as semicolon-separated entries of the form {@code "Type: Name"}.
   * <p>
   * This string serves two purposes in the application:
   * <ol>
   *   <li><b>Display</b>: It populates the "Record(s)" column in the FileManager table,
   *       giving the user a quick view of which database records reference a given file or
   *       folder (e.g. {@code "Person: Smith, John; Work: Some Title"}).</li>
   *   <li><b>"In use" determination</b>: {@link #isInUseByRecords()} uses the same logic with
   *       a lower limit to short-circuit after finding the first association.</li>
   * </ol>
   * <p>
   * The string is assembled in two parts:
   * <ul>
   *   <li>For person and misc-file records, all same-type records sharing this exact file path
   *       are listed (since multiple persons or misc-file records can reference the same
   *       physical file).</li>
   *   <li>Then, related records (both subject-side and object-side relations) are appended via
   *       {@link org.hypernomicon.model.AbstractHyperDB#getRelatives getRelatives}, up to the
   *       specified limit.</li>
   * </ul>
   * For folders with no direct associations, if any child folder is in use by records, the string
   * {@code "(Subfolders have associated records)"} is returned instead. This check is indirectly
   * recursive: each child's {@link #isInUseByRecords()} calls {@code getRecordsString()}, which
   * in turn checks its own children, so the entire subtree is considered.
   *
   * @return A semicolon-separated description of associated records, or an empty string if none
   */
  public String getRecordsString()
  {
    return getRecordsString(10);
  }

//---------------------------------------------------------------------------

  private String getRecordsString(int maxRelatives)
  {
    if (getRecord() == null) return "";

    StringBuilder val = new StringBuilder();

    RecordType recordType = getRecordType();

    switch (recordType)
    {
      case hdtPerson : case hdtMiscFile :

        getHyperPathSetForFilePath(filePath()).forEach(hyperPath ->
        {
          if (hyperPath.getRecordType() != recordType) return;

          if (val.length() > 0) val.append("; ");
          val.append(getTypeName(recordType)).append(": ").append(hyperPath.getRecord().defaultChoiceText());
        });

        break;

      default :

        break;
    }

    LinkedHashSet<HDT_Record> set = new LinkedHashSet<>();
    db.getRelatives(getRecord(), set, maxRelatives, false);

    set.forEach(relative ->
    {
      if (val.length() > 0) val.append("; ");
      val.append(getTypeName(relative.getType())).append(": ").append(relative.defaultChoiceText());
    });

    if (val.isEmpty() && (getRecordType() == hdtFolder))
      if (((HDT_Folder) getRecord()).childFolders.stream().anyMatch(subFolder -> subFolder.getPath().isInUseByRecords()))
        return "(Subfolders have associated records)";

    return val.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if {@link #getRecordsString()} returns non-empty, or this is the path for a DB special folder.
   * @return The return value just described
   */
  public boolean isInUse()
  {
    return isInUseByRecords() || ((getRecordType() == hdtFolder) && ((HDT_Folder) getRecord()).isSpecial(true));
  }

  /**
   * Returns true if this path has associated database records. Uses
   * {@link #getRecordsString(int) getRecordsString(1)} to short-circuit
   * after finding the first relative.
   * @return The return value just described
   */
  public boolean isInUseByRecords()
  {
    return strNotNullOrEmpty(getRecordsString(1));
  }

  /**
   * Returns true if non-null and either {@link #getRecordsString()} returns non-empty, or this is the path for a DB special folder.
   * @return The return value just described
   */
  public static boolean isInUse(HyperPath hyperPath)
  {
    return (hyperPath != null) && hyperPath.isInUse();
  }

  /**
   * Returns true if non-null and {@link #isInUseByRecords()} returns true.
   * @return The return value just described
   */
  public static boolean isInUseByRecords(HyperPath hyperPath)
  {
    return (hyperPath != null) && hyperPath.isInUseByRecords();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean renameFile(FilePath filePath, String newNameStr) throws IOException, HDB_InternalError
  {
    Set<HyperPath> set = getHyperPathSetForFilePath(filePath);

    if (set.isEmpty())
      return filePath.renameTo(newNameStr);

    for (HyperPath hyperPath : set)
      if (hyperPath.moveToFolder(hyperPath.parentFolder().getID(), false, true, newNameStr) == false)
        return false;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
