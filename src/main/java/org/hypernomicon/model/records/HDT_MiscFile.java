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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;
import java.util.Objects;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.authors.*;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

//---------------------------------------------------------------------------

public class HDT_MiscFile extends HDT_RecordWithMainText implements HDT_RecordWithPath, HDT_RecordWithAuthors<RecordAuthors>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperPath path;

  public final HyperObjPointer<HDT_MiscFile, HDT_Work> work;
  public final HyperObjPointer<HDT_MiscFile, HDT_FileType> fileType;

//---------------------------------------------------------------------------

  public HDT_MiscFile(RecordState xmlState, DatasetAccessor<HDT_MiscFile> dataset)
  {
    super(xmlState, dataset);

    work = getObjPointer(rtWorkOfMiscFile);
    fileType = getObjPointer(rtTypeOfFile);

    path = new HyperPath(getObjPointer(rtFolderOfMiscFile), this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean setAuthors(List<HDT_Person> list) { return updateObjectsFromList(rtAuthorOfFile, list); }

  public boolean getAnnotated()                  { return getTagBoolean(tagAnnotated); }
  public void setAnnotated(boolean val)          { updateTagBoolean(tagAnnotated, val); }

  @Override public HyperPath getPath()           { return path; }
  @Override public RecordAuthors getAuthors()    { return nullSwitch(work.get(), new FileAuthors(getObjList(rtAuthorOfFile), this), HDT_Work::getAuthors); }
  @Override public String listName()             { return name(); }
  @Override public String makeKeyWorkSearchKey() { return name().strip(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    nullSwitch(fileType.get(), oldFileType ->
    {
      fileType.setID(-1);
      if (oldFileType.miscFiles.isEmpty())
        db.deleteRecord(oldFileType);
    });

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public List<HDT_Person> authorRecords()
  {
    return getAuthors().stream().map(Author::getPerson)
                                .filter(Objects::nonNull)
                                .toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
