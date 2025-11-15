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

package org.hypernomicon.model.records;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.HyperDB.*;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.InvalidPathException;
import java.util.List;

import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------

public class HDT_Folder extends HDT_RecordBase implements HDT_RecordWithPath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final List<HDT_Folder> childFolders;
  public final List<HDT_Note> notes;

  private final List<HDT_MiscFile> miscFiles;
  private final List<HDT_WorkFile> workFiles;
  private final List<HDT_Person> picturePeople;

  private final HyperPath path;
  private boolean checkedForExistence;

//---------------------------------------------------------------------------

  public HDT_Folder(RecordState xmlState, DatasetAccessor<HDT_Folder> dataset)
  {
    super(xmlState, dataset);

    checkedForExistence = false;

    childFolders  = getSubjList(rtParentFolderOfFolder );
    miscFiles     = getSubjList(rtFolderOfMiscFile     );
    workFiles     = getSubjList(rtFolderOfWorkFile     );
    notes         = getSubjList(rtFolderOfNote         );
    picturePeople = getSubjList(rtPictureFolderOfPerson);

    path = new HyperPath(getObjPointer(rtParentFolderOfFolder), this);
  }

//---------------------------------------------------------------------------

  public boolean isSpecial(boolean checkSubfolders) { return db.isSpecialFolder(getID(), checkSubfolders); }

  @Override public HyperPath getPath() { return path; }
  @Override public String name()       { return path.getNameStr(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean renameTo(String newName)
  {
    if (getID() == ROOT_FOLDER_ID)
      return falseWithErrorPopup("Unable to rename the folder: Root folder cannot be renamed.");

    if (this == db.getXmlFolder())
      return falseWithErrorPopup("Unable to rename the folder: XML folder cannot be renamed.");

    if (parentFolder() == null)
      return falseWithErrorPopup("Unable to rename the folder: parent folder record is null.");

    FilePath srcFilePath = filePath();

    if (srcFilePath.exists() == false)
      return falseWithErrorPopup("Unable to rename the folder: it does not exist.");

    FilePath parentFilePath = parentFolder().filePath();

    if (parentFilePath.exists() == false)
      return falseWithErrorPopup("Unable to rename the folder: parent folder does not exist.");

    FilePath destFilePath = parentFilePath.resolve(newName);

    if (destFilePath.exists())
      return falseWithErrorPopup("Unable to rename the folder: a file or folder already has that name.");

    folderTreeWatcher.stop();

    try
    {
      if (srcFilePath.anyOpenFilesInDir())
      {
        folderTreeWatcher.createNewWatcherAndStart();
        return false;
      }

      srcFilePath.renameDirectory(destFilePath);
    }
    catch (IOException e)
    {
      folderTreeWatcher.createNewWatcherAndStart();
      return falseWithErrorPopup("Unable to rename the folder: " + getThrowableMessage(e));
    }

    db.unmapFilePath(srcFilePath);
    setNameInternal(newName, true);
    path.assign(parentFolder(), new FilePath(newName));

    folderTreeWatcher.createNewWatcherAndStart();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Delete the folder on the file system and delete the folder record and its child folder records
   * @param singleCall If true, means that this function isn't being called repeatedly; just a one-off
   * @return True if the folder was deleted successfully
   */
  public boolean delete(boolean singleCall)
  {
    FilePath filePath = filePath();

    if (db.isProtectedRecord(this, true))
      return falseWithErrorPopup("The folder \"" + filePath + "\" is in use by the database and cannot be deleted.");

    if (parentFolder() == null)
      return falseWithErrorPopup("Unable to delete the folder \"" + filePath + "\": parent folder record is null.");

    if (filePath.exists() == false)
      return falseWithErrorPopup("Unable to delete the folder \"" + filePath + "\": it does not exist.");

    boolean restartWatcher = folderTreeWatcher.stop();

    try
    {
      if (filePath.anyOpenFilesInDir())
      {
        folderTreeWatcher.createNewWatcherAndStart();
        return false;
      }

      filePath.deleteDirectory(singleCall);
      db.unmapFilePath(filePath);
    }
    catch (IOException e)
    {
      folderTreeWatcher.createNewWatcherAndStart();
      return falseWithErrorPopup("An error occurred while attempting to delete the folder \"" + filePath + "\": " + getThrowableMessage(e));
    }

    deleteFolderRecordTree(this);

    if (restartWatcher)
      folderTreeWatcher.createNewWatcherAndStart();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void deleteFolderRecordTree(HDT_Folder folder)
  {
    FilePath filePath = folder.filePath();

    if (folder.childFolders.isEmpty() == false)
      folder.childFolders.forEach(HDT_Folder::deleteFolderRecordTree);

    if (folder.getID() > 0)
      db.deleteRecord(folder);

    db.unmapFilePath(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Show a warning to the user if this folder or any of its subfolders no longer exists on the file system.<br>
   * Shows a message for each folder.<br>
   * This is intended to be called during database load.
   */
  public void checkExists()
  {
    if (checkedForExistence) return;

    checkedForExistence = true;

    if (getID() != ROOT_FOLDER_ID)
      if (filePath().exists() == false)
        if (isSpecial(false) || (path.getRecordsString().length() > 0))
          warningPopup("The folder: \"" + filePath() + "\" is referred to by one or more database records but cannot be found." + System.lineSeparator() + System.lineSeparator() +
                       "Next time, only use the " + appTitle + " File Manager to make changes to move, rename, or delete database folders.");

    childFolders.forEach(HDT_Folder::checkExists);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean containsFilesThatAreInUse()
  {
    if ((workFiles.size() > 0) || (picturePeople.size() > 0) || (miscFiles.size() > 0) || isSpecial(true)) return true;

    return childFolders.stream().anyMatch(childFolder -> childFolder.path.getRecordsString().length() > 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean hasNoNonFolderRecordDependencies()
  {
    if ( ! (notes.isEmpty() && picturePeople.isEmpty() && workFiles.isEmpty() && miscFiles.isEmpty())) return false;

    if (db.isProtectedRecord(this, false)) return false;

    return childFolders.stream().allMatch(HDT_Folder::hasNoNonFolderRecordDependencies);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Note closestAncestorNote()
  {
    return notes.isEmpty() ? nullSwitch(parentFolder(), db.notes.getByID(1), HDT_Folder::closestAncestorNote) : notes.getFirst();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Folder createSubfolder(String folderName)
  {
    boolean restartWatcher = folderTreeWatcher.stop();

    FilePath childFilePath = filePath().resolve(folderName.strip());
    HDT_Folder childFolder = null;

    try
    {
      childFilePath.createDirectory();
      childFolder = HyperPath.getFolderFromFilePath(childFilePath, true);
    }
    catch (FileAlreadyExistsException e)
    {
      errorPopup("Unable to create the folder: A file with that name already exists.");
    }
    catch (IOException | InvalidPathException e)
    {
      errorPopup("Unable to create the folder: " + getThrowableMessage(e));
    }

    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();

    FileManager.setNeedRefresh();

    return childFolder;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
