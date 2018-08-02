/*
 * Copyright 2015-2018 Jason Winning
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

import org.hypernomicon.App;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.filePath.FilePath;

public class HDT_Folder extends HDT_Record implements HDT_RecordWithPath
{
  public List<HDT_Folder> childFolders;
  public List<HDT_MiscFile> miscFiles;
  public List<HDT_WorkFile> workFiles;
  public List<HDT_Note> notes;

  private HyperPath path;
  private boolean checkedForExistence;
  
  public HDT_Folder(HDT_RecordState xmlState, HyperDataset<HDT_Folder> dataset)
  {
    super(xmlState, dataset);
    
    checkedForExistence = false;
    
    nameTag = tagName; // this is not actually used; name should always be blank
    
    childFolders = getSubjList(rtParentFolderOfFolder);    
    miscFiles = getSubjList(rtFolderOfMiscFile);
    workFiles = getSubjList(rtFolderOfWorkFile);
    notes = getSubjList(rtFolderOfNote);
    
    HyperObjPointer<HDT_Folder, HDT_Folder> parentFolder = getObjPointer(rtParentFolderOfFolder);
    path = new HyperPath(parentFolder, this);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperPath getPath()      { return path; }    
  @Override public String name()            { return path.getNameStr(); }
  @Override public String getCBText()       { return path.getNameStr(); }
  @Override public String listName()        { return path.getNameStr(); }  
  @Override public void expire()            { path.clear(); super.expire(); }
  @Override public HDT_RecordType getType() { return hdtFolder; }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  public boolean renameTo(String newName)
  {
    if (getID() == HyperDB.ROOT_FOLDER_ID)
    {
      messageDialog("Unable to rename the folder: Root folder cannot be renamed.", mtError);
      return false;
    }     
    
    if (path.getParentFolder() == null)
    {
      messageDialog("Unable to rename the folder: parent folder record is null.", mtError);
      return false;
    }
    
    FilePath srcFilePath = path.getFilePath();
    
    if (srcFilePath.exists() == false)
    {
      messageDialog("Unable to rename the folder: it does not exist.", mtError);
      return false;
    }
    
    FilePath parentFilePath = path.getParentFolder().path.getFilePath();
    
    if (parentFilePath.exists() == false)
    {
      messageDialog("Unable to rename the folder: parent folder does not exist.", mtError);
      return false;
    }
        
    FilePath destFilePath = parentFilePath.resolve(new FilePath(newName));
    
    if (destFilePath.exists())
    {
      messageDialog("Unable to rename the folder: a file or folder already has that name.", mtError);
      return false;
    }
    
    folderTreeWatcher.stop();
    
    try
    {
      if (srcFilePath.anyOpenFilesInDir()) 
      {
        App.folderTreeWatcher.createNewWatcherAndStart();
        return false;
      }
      
      srcFilePath.renameDirectory(destFilePath);      
    } 
    catch (IOException e)
    {
      folderTreeWatcher.createNewWatcherAndStart();
      messageDialog("Unable to rename the folder: " + e.getMessage(), mtError); 
      
      return false;
    }
    
    db.unmapFilePath(srcFilePath);
    setNameInternal(newName, true);
    path.assign(path.getParentFolder(), new FilePath(newName));
    
    switch (getID())
    {
      case HyperDB.BOOKS_FOLDER_ID :     db.prefs.put(PREF_KEY_BOOKS_PATH,      newName); break;
      case HyperDB.MISC_FOLDER_ID :      db.prefs.put(PREF_KEY_MISC_FILES_PATH, newName); break;
      case HyperDB.PAPERS_FOLDER_ID :    db.prefs.put(PREF_KEY_PAPERS_PATH,     newName); break;
      case HyperDB.PICTURES_FOLDER_ID :  db.prefs.put(PREF_KEY_PICTURES_PATH,   newName); break;
      case HyperDB.RESULTS_FOLDER_ID :   db.prefs.put(PREF_KEY_RESULTS_PATH,    newName); break;
      case HyperDB.TOPICAL_FOLDER_ID :   db.prefs.put(PREF_KEY_TOPICAL_PATH,    newName); break;
      case HyperDB.UNENTERED_FOLDER_ID : db.prefs.put(PREF_KEY_UNENTERED_PATH,  newName); break;
      default :                                                                           break;
    }
    
    folderTreeWatcher.createNewWatcherAndStart();
    
    return true;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  // singleCall means that this function isn't being called repeatedly; just a one-off
  
  public boolean delete(boolean singleCall)
  {
    if (HyperDB.isProtectedRecord(getID(), getType()))
    {
      messageDialog("The folder \"" + path.getFilePath() + "\" cannot be deleted.", mtError);
      return false;
    }
    
    if (path.getParentFolder() == null)
    {
      messageDialog("Unable to delete the folder \"" + path.getFilePath() + "\": parent folder record is null.", mtError);
      return false;
    }
    
    FilePath filePath = path.getFilePath();
    
    if (filePath.exists() == false)
    {
      messageDialog("Unable to rename the folder \"" + path.getFilePath() + "\": it does not exist.", mtError);
      return false;
    }

    boolean restartWatcher = folderTreeWatcher.stop();
    
    try
    {
      if (filePath.anyOpenFilesInDir()) 
      {
        App.folderTreeWatcher.createNewWatcherAndStart();
        return false;
      }
      
      filePath.deleteDirectory(singleCall);
      db.unmapFilePath(filePath);
    } 
    catch (IOException e)
    {
      folderTreeWatcher.createNewWatcherAndStart();
      messageDialog("An error occurred while attempting to delete the folder \"" + path.getFilePath() + "\": " + e.getMessage(), mtError); 
      
      return false;
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
    FilePath filePath = folder.getPath().getFilePath();
    
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
    
    if (getID() != HyperDB.ROOT_FOLDER_ID)
      if (path.getFilePath().exists() == false)
        if ((getID() < HyperDB.FIRST_USER_FOLDER_ID) || (path.getRecordsString().length() > 0))
          messageDialog("The folder: \"" + path.getFilePath() + "\" is referred to by one or more database records but cannot be found." + System.lineSeparator() + System.lineSeparator() +
                        "Next time, only use the Hypernomicon File Manager to make changes to move, rename, or delete database folders.", mtWarning);
    
    childFolders.forEach(folder -> folder.checkExists());
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public boolean containsFilesThatAreInUse()
  {
    if (workFiles.size() > 0) return true;
    if (miscFiles.size() > 0) return true;
    
    for (HDT_Folder childFolder : childFolders)
      if (childFolder.path.getRecordsString().length() > 0)
        return true;
    
    return false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public boolean hasNoNonFolderRecordDependencies()
  {
    if (notes.isEmpty() == false) return false;
    if (workFiles.isEmpty() == false) return false;
    if (miscFiles.isEmpty() == false) return false;
    
    for (HDT_Folder child : childFolders)
      if (child.hasNoNonFolderRecordDependencies() == false) return false;
    
    return true;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

}
