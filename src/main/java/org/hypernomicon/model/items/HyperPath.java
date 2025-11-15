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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
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
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------

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

    if (db.getRootPath().isSubpath(filePath) == false)
    {
      internalErrorPopup(90177);
      return;
    }

    if (getHyperPathSetForFilePath(filePath).size() > 0)
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

  public boolean isNotEmpty()               { return isEmpty() == false; }

  @Override public String toString()        { return filePath().toString(); }

  public static final Collection<Tag> FOLDER_TAGS = EnumSet.of(tagFolder, tagParentFolder, tagPictureFolder);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

  public String getNameStr()
  {
    return (record != null) && (record.getType() == hdtFolder) && (record.getID() == ROOT_FOLDER_ID) ?
      ROOT_PATH_STR
    :
      (fileName == null ? "" : fileName.getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear() { clear(true); }

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

  public static HDT_Folder getFolderFromFilePath(FilePath dirFilePath, boolean doCreateRecord)
  {
    dirFilePath = dirFilePath.getDirOnly();

    if (db.getRootPath().isSubpath(dirFilePath) == false)  // the path is not in the database folder tree
      return null;

    Set<HyperPath> set = getHyperPathSetForFilePath(dirFilePath);

    HDT_RecordWithPath folder = findFirst(set, hyperPath -> hyperPath.getRecordType() == hdtFolder, HyperPath::getRecord);
    if (folder != null) return (HDT_Folder) folder;

    if (dirFilePath.exists() == false) return null;

    HDT_Folder parentRecord = getFolderFromFilePath(dirFilePath.getParent(), doCreateRecord);

    if ((parentRecord == null) || (doCreateRecord == false)) return null;

    RecordState recordState = new RecordState(hdtFolder);

    try
    {
      BasicFileAttributes attribs = Files.readAttributes(dirFilePath.toPath(), BasicFileAttributes.class);

      recordState.creationDate = attribs.creationTime    ().toInstant();
      recordState.modifiedDate = attribs.lastModifiedTime().toInstant();
      recordState.viewDate     = attribs.lastAccessTime  ().toInstant();
    }
    catch (IOException e) { noOp(); }

    HDT_Folder newFolder = null;

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

  public String getRecordsString()
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
          val.append(getTypeName(recordType)).append(": ").append(hyperPath.getRecord().listName());
        });

        break;

      default :

        break;
    }

    LinkedHashSet<HDT_Record> set = new LinkedHashSet<>();
    db.getRelatives(getRecord(), set, 10, false);

    set.forEach(relative ->
    {
      if (relative.getType() == hdtFolder) return;

      if (val.length() > 0) val.append("; ");
      val.append(getTypeName(relative.getType())).append(": ").append(relative.listName());
    });

    if (val.isEmpty() && (getRecordType() == hdtFolder))
    {
      if (((HDT_Folder) getRecord()).childFolders.stream().anyMatch(subFolder -> subFolder.getPath().getRecordsString().length() > 0))
        return "(Subfolders have associated records)";
    }

    return val.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isInUse()
  {
    if (getRecordsString().isEmpty() == false) return true;

    return (getRecordType() == hdtFolder) && db.isSpecialFolder(getRecord().getID(), true);
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
