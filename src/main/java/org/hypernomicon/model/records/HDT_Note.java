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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.Const.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.view.wrappers.HyperTable;

public class HDT_Note extends HDT_RecordWithConnector
{
  public List<HDT_Note> parentNotes;
  public List<HDT_Note> subNotes;
  public HyperObjPointer<HDT_Note, HDT_Folder> folder;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Note(HDT_RecordState xmlState, HyperDataset<HDT_Note> dataset)
  {
    super(xmlState, dataset);
    
    nameTag = tagName;
    
    parentNotes = getObjList(rtParentNoteOfNote);
    subNotes = getSubjList(rtParentNoteOfNote);
    folder = getObjPointer(rtFolderOfNote);
  }
    
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getFolderStr()         
  {  
    if (folder.isNull())
      return "";
    
    return db.getPath(PREF_KEY_TOPICAL_PATH, null).relativize(folder.get().getPath().getFilePath()).toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void setParentNotes(HyperTable ht) { updateObjectsFromHT(rtParentNoteOfNote, ht, 2); }
  
  @Override public String listName()        { return name(); }
  @Override public HDT_RecordType getType() { return hdtNote; }
  @Override public boolean isUnitable()     { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Folder getDefaultFolder()
  {
    if (folder.isNotNull())
      return folder.get();
    
    HDT_Folder defFolder;
    
    for (HDT_Note parent : parentNotes)
    {
      defFolder = parent.getDefaultFolder();
      if (defFolder != null) return defFolder;
    }
    
    return null;
  }
  
//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
}
