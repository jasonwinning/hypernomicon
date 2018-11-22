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

package org.hypernomicon.model.items;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;

import com.google.common.collect.Sets;

import org.hypernomicon.model.Exceptions.DuplicateRecordException;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HubChangedException;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.filePath.FilePath;

public class HyperPath
{
  public static final HyperPath EmptyPath = new HyperPath(null);
  private HyperObjPointer<? extends HDT_RecordWithPath, HDT_Folder> folderPtr = null;
  private HDT_RecordWithPath record = null;
  private HDT_Folder folder = null;
  private FilePath fileName = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperPath(HyperObjPointer<? extends HDT_RecordWithPath, HDT_Folder> folderPtr, HDT_RecordWithPath record)
  {
    this.folderPtr = folderPtr;
    this.record = record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_RecordWithPath getRecord()       { return record; }
  public HDT_RecordType     getRecordType()   { return record == null ? hdtNone : record.getType(); }
  public int                getRecordID()     { return record == null ? -1 : record.getID(); }
  public FilePath           getFileName()     { return fileName; }
  public HDT_Folder         getParentFolder() { return folderPtr == null ? folder : folderPtr.get(); }
  
  @Override public String toString() { return getFilePath().toString(); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  public HyperPath(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) 
      return;
    
    if (db.getRootFilePath().isSubpath(filePath) == false)
    {
      messageDialog("Internal error: Hyperpath not in database folder tree", mtError);
      return;
    }
    
    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);
    if (set.size() > 0)
    {
      messageDialog("Internal error #90178", mtError);
      return;
    }
    
    folder = HyperPath.getFolderFromFilePath(filePath, false);
    assignNameInternal(filePath.getNameOnly());
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()                  
  { 
    if (record != null)
      if ((record.getType() == hdtFolder) && (record.getID() == HyperDB.ROOT_FOLDER_ID))
        return false;
    
    return FilePath.isEmpty(getFileName()); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public String getNameStr()          
  { 
    if (record != null)
      if (record.getID() == HyperDB.ROOT_FOLDER_ID)
        if (record.getType() == hdtFolder)
          return "Root";
    
    return nullSwitch(getFileName(), "", fn -> fn.getNameOnly().toString());
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear() { clear(true); }
  
  public void clear(boolean deleteFile)
  {
    FilePath filePath = getFilePath();
    
    if (folderPtr != null) 
      folderPtr.setID(-1);
    else
      folder = null;
    
    if (FilePath.isEmpty(filePath) == false)
      if (filePath.exists() && filePath.isFile())
      {
        Set<HyperPath> paths = getHyperPathSetForFilePath(filePath);
        
        if (deleteFile && paths.isEmpty())
          db.fileNoLongerInUse(filePath);
      }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Set<HyperPath> getHyperPathSetForFilePath(FilePath filePath)
  {
    String name = filePath.getNameOnly().toString();
    Set<HyperPath> paths = db.filenameMap.get(name);
    Set<HyperPath> matchedPaths = new HashSet<>();
    
    if (paths == null) return matchedPaths;
    
    for (HyperPath path : paths)
      if (path.getFilePath().equals(filePath))
        matchedPaths.add(path);
    
    return matchedPaths;    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePath getFilePath()
  {   
    if (record != null)
      if ((record.getType() == hdtFolder) && (record.getID() == HyperDB.ROOT_FOLDER_ID))
        return db.getRootFilePath();
    
    if (FilePath.isEmpty(getFileName())) return null;
    
    HDT_Folder folder = getParentFolder();
    if (folder == null) return getFileName();
    
    HyperPath hPath = folder.getPath();
    if (hPath == null) return getFileName();
    
    return nullSwitch(hPath.getFilePath(), getFileName(), folderFP -> folderFP.resolve(getFileName()));    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Folder getFolderFromFilePath(FilePath dirFilePath, boolean doCreate)
  {
    dirFilePath = dirFilePath.getDirOnly();
    
    if (db.getRootFilePath().isSubpath(dirFilePath) == false)  // the path is not in the database folder tree
      return null;
    
    Set<HyperPath> set = getHyperPathSetForFilePath(dirFilePath);
    
    for (HyperPath hyperPath : set)
      if (hyperPath.getRecordType() == hdtFolder)
        return (HDT_Folder)hyperPath.getRecord();
    
    if (dirFilePath.exists() == false) return null;
    
    HDT_Folder parentRecord = getFolderFromFilePath(dirFilePath.getParent(), doCreate);
    
    if ((parentRecord == null) || (doCreate == false)) return null;
       
    HDT_RecordState recordState = new HDT_RecordState(hdtFolder, -1, "", "", "", "");
    
    try
    {
      BasicFileAttributes attribs = Files.readAttributes(dirFilePath.toPath(), BasicFileAttributes.class);
      
      recordState.creationDate = attribs.creationTime().toInstant();
      recordState.modifiedDate = attribs.lastModifiedTime().toInstant();
      recordState.viewDate = attribs.lastAccessTime().toInstant();
    } 
    catch (IOException e) { noOp(); }
     
    HDT_Folder newFolder = null;
    
    try
    {
      newFolder = db.createNewRecordFromState(recordState, true);
    } 
    catch (DuplicateRecordException | RelationCycleException | SearchKeyException | HubChangedException e)
    {
      noOp();
    }
    catch (HDB_InternalError e)
    {
      messageDialog(e.getMessage(), mtError);
      return null;
    }
       
    newFolder.getPath().assignInternal(parentRecord, dirFilePath.getNameOnly());

    return newFolder;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public static HDT_RecordWithPath createRecordAssignedToPath(HDT_RecordType type, FilePath filePath)
  {
    if ((type != hdtWorkFile) && (type != hdtMiscFile))
    {
      messageDialog("Internal error #42221", mtError);
      return null;
    }
    
    if (filePath.isDirectory())
    {
      messageDialog("Internal error #42231", mtError);
      return null;
    }
    
    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath.getDirOnly(), true);
    if (folder == null) return null;
    
    HDT_RecordWithPath file = db.createNewBlankRecord(type);
    file.getPath().assign(folder, filePath.getNameOnly());
    
    return file;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_RecordWithPath getFileFromFilePath(FilePath filePath)
  {
    Set<HyperPath> set = getHyperPathSetForFilePath(filePath);
    
    for (HyperPath hyperPath : set)
    {
      if (hyperPath.record != null)
      {
        switch (hyperPath.record.getType())
        {
          case hdtMiscFile : case hdtWorkFile :
            return hyperPath.record;
          default:
            break;
        }
      }
    }
    
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean moveToFolder(int folderID, boolean confirm, boolean changeFilename, String newName) throws IOException
  {
    if (folderID == -1)
    {
      messageDialog("Internal error #77392", mtError);
      return false;
    }
    
    if (db.folders.getByID(folderID) == null)
    {
      messageDialog("Internal error #77392", mtError);
      return false;
    }
    
    FilePath srcFilePath = getFilePath(), destFilePath;
    HDT_Folder newFolder = db.folders.getByID(folderID);
    
    if (srcFilePath.isDirectory())
    {
      messageDialog("Internal error #77393", mtError);
      return false;      
    }
    
    if (getRecordType() == hdtPerson)
      newFolder = getParentFolder();
    
    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(srcFilePath);
    
    if (changeFilename)
      destFilePath = newFolder.getPath().getFilePath().resolve(new FilePath(newName));
    else
      destFilePath = newFolder.getPath().getFilePath().resolve(srcFilePath.getNameOnly());
    
    if (srcFilePath.equals(destFilePath)) return true;
    
    if (srcFilePath.moveTo(destFilePath, confirm))
    {
      db.unmapFilePath(srcFilePath);
      
      for (HyperPath hyperPath : set)
        hyperPath.assign(db.folders.getByID(folderID), destFilePath.getNameOnly());
      
      return true;
    }
    
    return false;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void assign(HDT_Folder parentFolder, FilePath nameOnly)
  {
    if (FilePath.isEmpty(getFileName()) && FilePath.isEmpty(nameOnly)) return;
    
    if ((FilePath.isEmpty(getFileName()) == false) && (FilePath.isEmpty(nameOnly) == false))
      if ((getParentFolder() == parentFolder) && (getFileName().getNameOnly().equals(nameOnly.getNameOnly()))) return;
    
    clear();
    assignInternal(parentFolder, nameOnly);
    if (record != null) record.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void assignInternal(HDT_Folder parentFolder, FilePath nameOnly)
  {    
    if (folderPtr == null)
    {
      if ((record != null) && (record.getType() != hdtPerson))
        messageDialog("Internal error #83902", mtError);
      
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
    {
      Set<HyperPath> set = db.filenameMap.get(fileName.toString());
      if (set != null)
        set.remove(this);
    }
    
    if (FilePath.isEmpty(newFileName) == false)
    {
      newFileName = newFileName.getNameOnly();
      Set<HyperPath> set = db.filenameMap.get(newFileName.toString());
      if (set == null)
      {
        set = Sets.newConcurrentHashSet();
        db.filenameMap.put(newFileName.toString(), set);
      }
      
      set.add(this);
    }
    
    fileName = newFileName;
    
    if (record != null)
      record.updateSortKey();    
    
    if (FilePath.isEmpty(fileName) == false)
    {
      Set<HyperPath> set = db.filenameMap.get(fileName.getNameOnly().toString());
      
      Iterator<HyperPath> it = set.iterator();
      
      while (it.hasNext())
      {
        HyperPath path = it.next();
        
        if (path.isEmpty() == false)
        {
          if (path.getFilePath().equals(getFilePath())) // for this to work, folder records have to be brought online first
            if (path != this)
              it.remove();
        }
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getRecordsString()
  {
    Set<HDT_Base> set = new LinkedHashSet<>();
    String val = "";
    String piece;
    HDT_Person person;
    
    if (getRecord() == null) return "";
    
    if (getParentFolder() == db.folders.getByID(HyperDB.PICTURES_FOLDER_ID))
    {
      Set<HyperPath> hyperPathSet = HyperPath.getHyperPathSetForFilePath(getFilePath());
      for (HyperPath hyperPath : hyperPathSet)
      {
        if (hyperPath.getRecordType() == hdtPerson)
        {
          person = (HDT_Person) hyperPath.getRecord();
          piece = db.getTypeName(hdtPerson) + ": " + person.listName();
          val = val.length() == 0 ? piece : val + "; " + piece;
        }
      }
    }
    
    db.getRelatives(getRecord(), set, 10);
    
    for (HDT_Base relative : set)
      if (relative.getType() != hdtFolder)
      {
        piece = db.getTypeName(relative.getType()) + ": " + relative.listName();
        val = val.length() == 0 ? piece : val + "; " + piece;
      }
    
    if ((val.length() == 0) && (getRecordType() == hdtFolder))
    {
      HDT_Folder theFolder = (HDT_Folder) getRecord();
      for (HDT_Folder subFolder : theFolder.childFolders)
      {
        String childStr = subFolder.getPath().getRecordsString();
        if (childStr.length() > 0)
          return "(Subfolders have associated records)";
      }
    }
    
    return val;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean renameFile(FilePath filePath, String newNameStr) throws IOException
  {
    Set<HyperPath> set = getHyperPathSetForFilePath(filePath);
    
    if (set.isEmpty())
      return filePath.renameTo(newNameStr);

    for (HyperPath hyperPath : set)
      if (hyperPath.moveToFolder(hyperPath.getParentFolder().getID(), false, true, newNameStr) == false)
        return false;
    
    return true;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
