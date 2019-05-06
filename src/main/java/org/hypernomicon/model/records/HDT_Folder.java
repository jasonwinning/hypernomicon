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

package org.hypernomicon.model.records;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;

import java.io.IOException;
import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.filePath.FilePath;

public class HDT_Folder extends HDT_RecordBase implements HDT_RecordWithPath
{
  public final List<HDT_Folder> childFolders;
  public final List<HDT_MiscFile> miscFiles;
  public final List<HDT_WorkFile> workFiles;
  public final List<HDT_Note> notes;

  private final HyperPath path;
  private boolean checkedForExistence;

  public HDT_Folder(HDT_RecordState xmlState, HyperDataset<HDT_Folder> dataset)
  {
    super(xmlState, dataset, tagName); // tagName is not actually used; name should always be blank

    checkedForExistence = false;

    childFolders = getSubjList(rtParentFolderOfFolder);
    miscFiles = getSubjList(rtFolderOfMiscFile);
    workFiles = getSubjList(rtFolderOfWorkFile);
    notes = getSubjList(rtFolderOfNote);

    path = new HyperPath(getObjPointer(rtParentFolderOfFolder), this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperPath getPath()      { return path; }
  @Override public String name()            { return path.getNameStr(); }
  @Override public String getCBText()       { return path.getNameStr(); }
  @Override public String listName()        { return path.getNameStr(); }
  @Override public void expire()            { path.clear(); super.expire(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean renameTo(String newName)
  {
    if (getID() == ROOT_FOLDER_ID)
      return falseWithErrorMessage("Unable to rename the folder: Root folder cannot be renamed.");

    if (getID() == XML_FOLDER_ID)
      return falseWithErrorMessage("Unable to rename the folder: XML folder cannot be renamed.");

    if (parentFolder() == null)
      return falseWithErrorMessage("Unable to rename the folder: parent folder record is null.");

    FilePath srcFilePath = filePath();

    if (srcFilePath.exists() == false)
      return falseWithErrorMessage("Unable to rename the folder: it does not exist.");

    FilePath parentFilePath = parentFolder().filePath();

    if (parentFilePath.exists() == false)
      return falseWithErrorMessage("Unable to rename the folder: parent folder does not exist.");

    FilePath destFilePath = parentFilePath.resolve(newName);

    if (destFilePath.exists())
      return falseWithErrorMessage("Unable to rename the folder: a file or folder already has that name.");

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
      return falseWithErrorMessage("Unable to rename the folder: " + e.getMessage());
    }

    db.unmapFilePath(srcFilePath);
    setNameInternal(newName, true);
    path.assign(parentFolder(), new FilePath(newName));

    switch (getID())
    {
      case BOOKS_FOLDER_ID     : db.prefs.put(PREF_KEY_BOOKS_PATH,      newName); break;
      case MISC_FOLDER_ID      : db.prefs.put(PREF_KEY_MISC_FILES_PATH, newName); break;
      case PAPERS_FOLDER_ID    : db.prefs.put(PREF_KEY_PAPERS_PATH,     newName); break;
      case PICTURES_FOLDER_ID  : db.prefs.put(PREF_KEY_PICTURES_PATH,   newName); break;
      case RESULTS_FOLDER_ID   : db.prefs.put(PREF_KEY_RESULTS_PATH,    newName); break;
      case TOPICAL_FOLDER_ID   : db.prefs.put(PREF_KEY_TOPICAL_PATH,    newName); break;
      case UNENTERED_FOLDER_ID : db.prefs.put(PREF_KEY_UNENTERED_PATH,  newName); break;
      default :                                                                   break;
    }

    folderTreeWatcher.createNewWatcherAndStart();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // singleCall means that this function isn't being called repeatedly; just a one-off

  public boolean delete(boolean singleCall)
  {
    FilePath filePath = filePath();

    if (isProtectedRecord(getID(), getType()))
      return falseWithErrorMessage("The folder \"" + filePath + "\" cannot be deleted.");

    if (parentFolder() == null)
      return falseWithErrorMessage("Unable to delete the folder \"" + filePath + "\": parent folder record is null.");

    if (filePath.exists() == false)
      return falseWithErrorMessage("Unable to rename the folder \"" + filePath + "\": it does not exist.");

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
      return falseWithErrorMessage("An error occurred while attempting to delete the folder \"" + filePath + "\": " + e.getMessage());
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

    int folderID = folder.getID();
    if (folderID > 0)
      db.deleteRecord(hdtFolder, folderID);

    db.unmapFilePath(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void checkExists()
  {
    if (checkedForExistence) return;

    checkedForExistence = true;

    if (getID() != ROOT_FOLDER_ID)
      if (filePath().exists() == false)
        if ((getID() < FIRST_USER_FOLDER_ID) || (path.getRecordsString().length() > 0))
          messageDialog("The folder: \"" + filePath() + "\" is referred to by one or more database records but cannot be found." + System.lineSeparator() + System.lineSeparator() +
                        "Next time, only use the Hypernomicon File Manager to make changes to move, rename, or delete database folders.", mtWarning);

    childFolders.forEach(HDT_Folder::checkExists);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean containsFilesThatAreInUse()
  {
    if ((workFiles.size() > 0) || (miscFiles.size() > 0)) return true;

    return childFolders.stream().anyMatch(childFolder -> childFolder.path.getRecordsString().length() > 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean hasNoNonFolderRecordDependencies()
  {
    if ( ! (notes.isEmpty() && workFiles.isEmpty() && miscFiles.isEmpty())) return false;

    return childFolders.stream().allMatch(HDT_Folder::hasNoNonFolderRecordDependencies);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
