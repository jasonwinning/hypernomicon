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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;

public class HDT_Note extends HDT_RecordWithMainText implements HDT_RecordWithPath
{
  public final List<HDT_Note> parentNotes, subNotes;
  public final HyperObjPointer<HDT_Note, HDT_Folder> folder;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Note(RecordState xmlState, DatasetAccessor<HDT_Note> dataset)
  {
    super(xmlState, dataset);

    parentNotes = getObjList(rtParentNoteOfNote);
    subNotes = getSubjList(rtParentNoteOfNote);
    folder = getObjPointer(rtFolderOfNote);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean setParentNotes(List<HDT_Note> list) { return updateObjectsFromList(rtParentNoteOfNote, list); }
  public String getFolderStr(boolean absolute)       { return nullSwitch(filePath(), "", filePath -> absolute ? filePath.toString() : db.getRootPath().relativize(filePath).toString()); }
  public HDT_Note getAncestorWithFolder()            { return folder.isNotNull() ? this : findFirstHaving(parentNotes, HDT_Note::getAncestorWithFolder); }

  @Override public HyperPath getPath()               { return folder.isNull() ? null : folder.get().getPath(); }
  @Override public String listName()                 { return name(); }
  @Override public final boolean isUnitable()        { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
