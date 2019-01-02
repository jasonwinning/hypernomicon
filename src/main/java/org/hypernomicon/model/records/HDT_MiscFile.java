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

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_FileType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.HyperObjPointer;

public class HDT_MiscFile extends HDT_RecordWithConnector implements HDT_RecordWithPath
{ 
  protected final HyperPath path;
  public final List<HDT_Person> authors;
  public final HyperObjList<HDT_MiscFile, HDT_WorkLabel> labels;
  
  public final HyperObjPointer<HDT_MiscFile, HDT_Work> work;
  public final HyperObjPointer<HDT_MiscFile, HDT_FileType> fileType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_MiscFile(HDT_RecordState xmlState, HyperDataset<HDT_MiscFile> dataset)
  {
    super(xmlState, dataset, tagName);
    
    authors = getObjList(rtAuthorOfFile);
    labels = getObjList(rtLabelOfFile);
    
    work = getObjPointer(rtWorkOfMiscFile);
    fileType = getObjPointer(rtTypeOfFile);
    
    path = new HyperPath(getObjPointer(rtFolderOfMiscFile), this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAuthors(List<HDT_Person> list)       { updateObjectsFromList(rtAuthorOfFile, list); }
  public void setWorkLabels(List<HDT_WorkLabel> list) { updateObjectsFromList(rtLabelOfFile, list); }
  
  public boolean getAnnotated()         { return getTagBoolean(tagAnnotated); }
  public void setAnnotated(boolean val) { updateTagBoolean(tagAnnotated, val); }
  
  @Override public HyperPath getPath()      { return path; }
  @Override public HDT_RecordType getType() { return hdtMiscFile; }
  @Override public String listName()        { return name(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getShortAuthorsStr(boolean fullNameIfSingleton)
  {
    ArrayList<Author> authList = new ArrayList<>();
    
    authors.forEach(authRecord -> authList.add(new Author(null, authRecord)));
    
    return Authors.getShortAuthorsStr(authList, false, fullNameIfSingleton);
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
  @Override public void expire()
  {
    path.clear();
    super.expire();
  }
    
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
