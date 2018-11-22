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

package org.hypernomicon.model;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.PathInfo.FileKind.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Set;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;

public class PathInfo
{
  public static enum FileKind
  {
    fkFileRecord,
    fkFolderRecord,
    fkPicture,
    fkFile,
    fkFolder,
    fkUnknown
  }
  
//---------------------------------------------------------------------------

  private HDT_Folder parentFolder;
  private HyperPath hyperPath;
  private FileKind fileKind;
  private FilePath filePath;

  public HDT_Folder getParentFolder() { return parentFolder; }
  public HyperPath getHyperPath()     { return hyperPath; }
  public FilePath getFilePath()       { return filePath; }
  public FileKind getFileKind()       { return fileKind; }
  public boolean isDirectory()        { return (fileKind == fkFolder) || (fileKind == fkFolderRecord); }
  
  @Override public String toString()  { return filePath.toString(); }

//---------------------------------------------------------------------------
  
  public PathInfo(FilePath filePath)
  {
    this.filePath = filePath;
    
    if (filePath.equals(db.getRootFilePath()))
    {
      fileKind = fkFolderRecord;
      parentFolder = null;
      hyperPath = db.folders.getByID(HyperDB.ROOT_FOLDER_ID).getPath();
      return;
    }
    
    if (db.getRootFilePath().isSubpath(filePath))
    {      
      parentFolder = getParentFolderOfPath(filePath);
         
      Set<HyperPath> set = db.filenameMap.get(filePath.getNameOnly().toString());
      
      if (set != null)      
        for (HyperPath setHyperPath : set)
        {          
          if (parentFolder == setHyperPath.getParentFolder())
          {
            hyperPath = setHyperPath;
            
            switch (hyperPath.getRecordType())
            {
              case hdtPerson : fileKind = fkPicture; return;
              case hdtFolder : fileKind = fkFolderRecord; return;
              case hdtWorkFile : case hdtMiscFile : fileKind = fkFileRecord; return;
              case hdtNone : break;
                
              default : messageDialog("Internal error #68754", mtError);  return;
            }
          }
        }
    }
    
    if (filePath.exists() == false)
      fileKind = fkUnknown;
    else if (filePath.isDirectory())
      fileKind = fkFolder;
    else
      fileKind = fkFile;
    
    return;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Folder getParentFolderOfPath(FilePath filePath)
  {
    for (HyperPath hyperPath : HyperPath.getHyperPathSetForFilePath(filePath.getParent()))
      if (hyperPath.getRecordType() == hdtFolder)
        return (HDT_Folder) hyperPath.getRecord();
    
    return null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
