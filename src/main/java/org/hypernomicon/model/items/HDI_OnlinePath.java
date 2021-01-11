/*
 * Copyright 2015-2021 Jason Winning
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

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.util.filePath.FilePath;

public class HDI_OnlinePath extends HDI_OnlineBase<HDI_OfflinePath>
{
  private HyperPath hyperPath;
  private final RelationType relType;
  private final HDT_RecordWithPath recordWithPath;

//---------------------------------------------------------------------------

  public HDI_OnlinePath(HDI_Schema schema, HDT_RecordWithPath recordWithPath)
  {
    super(schema, recordWithPath);

    relType = schema.getRelType();
    this.recordWithPath = recordWithPath;
    initPath();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initPath()
  {
    hyperPath = recordWithPath.getPath();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflinePath val, Tag tag) throws RelationCycleException
  {
    switch (tag)
    {
      case tagParentFolder : case tagFolder : case tagPictureFolder :

        HyperObjList<HDT_Record, HDT_Record> objList = db.getObjectList(relType, recordWithPath, false);
        objList.clear();

        if (val.folderID > 0)
        {
          HDT_Folder folder = db.folders.getByID(val.folderID);
          if (folder != null)
          {
            objList.add(folder);
            objList.throwLastException();
          }
        }

        break;

      default :

        initPath();

        if (hyperPath == null) return;
        if (hyperPath.isEmpty() && val.fileName.isEmpty()) return;

        hyperPath.assignNameInternal(new FilePath(val.fileName));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers() throws HDB_InternalError
  {
    if (relType == rtNone) return;

    db.resolvePointersByRelation(relType, recordWithPath);

    // The remainder of this method is for backwards compatibility with records XML version 1.0

    if ((relType != RelationType.rtPictureFolderOfPerson) || hyperPath.isEmpty() || (hyperPath.parentFolder() != null)) return;

    // This has to be done after bringing records online because special folder IDs are loaded from the Settings file afterward

    HDI_OfflinePath offlinePath = new HDI_OfflinePath(getSchema(), recordWithPath.getRecordStateBackup());
    offlinePath.folderID = db.getPicturesFolder().getID();
    try { setFromOfflineValue(offlinePath, Tag.tagPictureFolder); } catch (RelationCycleException e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (hyperPath.isNotEmpty())
      list.add(hyperPath.getFileName().getNameOnly().toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    if (hyperPath.isEmpty()) return "";

    switch (tag)
    {
      case tagFolder :

        if (hyperPath.filePath().isDirectory())
        {
          return hyperPath.getRecord() == db.getRootFolder() ?
            HyperPath.ROOT_PATH_STR
          :
            db.getRootPath().relativize(hyperPath.filePath()).toString();
        }

        // now it should fall through to tagParentFolder case

      case tagParentFolder : case tagPictureFolder :

        if (hyperPath.parentFolder() == null)
          return "";

        return hyperPath.parentFolder().getID() == ROOT_FOLDER_ID ?
          HyperPath.ROOT_PATH_STR
        :
          db.getRootPath().relativize(hyperPath.parentFolder().filePath()).toString();

      default :

        return hyperPath.getNameStr();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflinePath val, Tag tag)
  {
    val.folderID = -1;
    val.fileName = "";

    if (hyperPath.isEmpty()) return;

    HDT_Folder parentFolder = hyperPath.parentFolder();

    if (parentFolder != null)
      val.folderID = parentFolder.getID();

    FilePath fileName = hyperPath.getFileName();

    if (fileName != null)
      val.fileName = fileName.getNameOnly().toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
