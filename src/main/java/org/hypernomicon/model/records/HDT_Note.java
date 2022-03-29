/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.util.filePath.FilePath;

public class HDT_Note extends HDT_RecordWithMainText implements HDT_RecordWithPath
{
  public final List<HDT_Note> parentNotes, subNotes;
  public final HyperObjPointer<HDT_Note, HDT_Folder> folder;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Note(RecordState xmlState, HyperDataset<HDT_Note> dataset)
  {
    super(xmlState, dataset, tagName);

    parentNotes = getObjList(rtParentNoteOfNote);
    subNotes = getSubjList(rtParentNoteOfNote);
    folder = getObjPointer(rtFolderOfNote);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getFolderStr()          { return nullSwitch(filePath(), "", FilePath::toString); }
  public HDT_Folder getDefaultFolder()  { return folder.isNotNull() ? folder.get() : findFirstHaving(parentNotes, HDT_Note::getDefaultFolder); }

  public void setParentNotes(List<HDT_Note> list) { updateObjectsFromList(rtParentNoteOfNote, list); }

  @Override public HyperPath getPath()        { return folder.isNull() ? null : folder.get().getPath(); }
  @Override public String listName()          { return name(); }
  @Override public final boolean isUnitable() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
