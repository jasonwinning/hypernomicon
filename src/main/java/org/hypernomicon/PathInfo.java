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

package org.hypernomicon;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.PathInfo.FileKind.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Set;

import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;

//---------------------------------------------------------------------------

class PathInfo
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum FileKind
  {
    fkFileRecord,
    fkFolderRecord,
    fkPicture,
    fkFile,
    fkFolder,
    fkUnknown
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Folder parentFolder;
  private final HyperPath hyperPath;
  private final FileKind fileKind;
  private final FilePath filePath;

//---------------------------------------------------------------------------

  HDT_Folder getParentFolder()        { return parentFolder; }
  HyperPath getHyperPath()            { return hyperPath; }
  FilePath getFilePath()              { return filePath; }
  FileKind getFileKind()              { return fileKind; }
  boolean isDirectory()               { return (fileKind == fkFolder) || (fileKind == fkFolderRecord); }

  @Override public String toString()  { return filePath.toString(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  PathInfo(FilePath filePath)
  {
    this.filePath = filePath;

    if (filePath.equals(db.getRootPath()))
    {
      fileKind = fkFolderRecord;
      parentFolder = null;
      hyperPath = db.getRootFolder().getPath();
      return;
    }

    if (db.getRootPath().isSubpath(filePath))
    {
      parentFolder = getParentFolderOfPath(filePath);

      Set<HyperPath> set = db.filenameMap.get(filePath.getNameOnly().toString());

      if (set != null) for (HyperPath setHyperPath : set)
      {
        if (parentFolder == setHyperPath.parentFolder())
        {
          switch (setHyperPath.getRecordType())
          {
            case hdtPerson :

              hyperPath = setHyperPath;
              fileKind = fkPicture;
              return;

            case hdtFolder :

              hyperPath = setHyperPath;
              fileKind = fkFolderRecord;
              return;

            case hdtWorkFile : case hdtMiscFile :

              hyperPath = setHyperPath;
              fileKind = fkFileRecord;
              return;

            case hdtNone :

              break;

            default :

              hyperPath = null;
              fileKind = fkUnknown;
              internalErrorPopup(68754);
              return;
          }
        }
      }
    }
    else
      parentFolder = null;

    hyperPath = null;

    if (filePath.exists() == false)  fileKind = fkUnknown;
    else if (filePath.isDirectory()) fileKind = fkFolder;
    else                             fileKind = fkFile;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Folder getParentFolderOfPath(FilePath filePath)
  {
    return (HDT_Folder) findFirst(HyperPath.getHyperPathSetForFilePath(filePath.getParent()),
                                  hp -> hp.getRecordType() == hdtFolder,
                                  HyperPath::getRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
