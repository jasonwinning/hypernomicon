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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.relations.HyperSubjList;

public class HDT_WorkLabel extends HDT_RecordWithConnector
{
  public List<HDT_WorkLabel> parentLabels;
  public List<HDT_WorkLabel> subLabels;
  public HyperSubjList<HDT_Work, HDT_WorkLabel> works;
  public HyperSubjList<HDT_MiscFile, HDT_WorkLabel> miscFiles;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_WorkLabel(HDT_RecordState xmlState, HyperDataset<HDT_WorkLabel> dataset)
  {
    super(xmlState, dataset);
    nameTag = tagText;
    
    parentLabels = getObjList(rtParentLabelOfLabel);
    subLabels = getSubjList(rtParentLabelOfLabel);
    works = getSubjList(rtLabelOfWork);
    miscFiles = getSubjList(rtLabelOfFile);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String listName()           { return name(); }
  @Override public HDT_RecordType getType()    { return hdtWorkLabel; }
  @Override public String getCBText()          { return getExtendedText(); }
  @Override public boolean isUnitable()        { return true; }
  
  public void setText(String newStr)           { setNameInternal(newStr, true); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getExtendedText()
  {
    String parentText;
  
    if (parentLabels.size() > 0)
    {
      if (parentLabels.get(0).getID() == 1) return name();
      parentText = parentLabels.get(0).getExtendedText();
      if (parentText.length() > 0)
        return parentText + "/" + name();
    }
  
    return name();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Changes subjects, leaves key works alone
  public void refreshSubjects()
  {    
    getMainText().getKeyWorks().forEach(keyWork ->
    {
      if (keyWork.getRecordType() == hdtWork)
      {
        if (works.contains(keyWork.getRecord()) == false)
          db.getObjectList(rtLabelOfWork, keyWork.getRecord(), true).add(this);
      }
      else
      {
        if (miscFiles.contains(keyWork.getRecord()) == false)
          db.getObjectList(rtLabelOfFile, keyWork.getRecord(), true).add(this);
      }
    });

    ArrayList<HDT_Work> worksCopy = new ArrayList<HDT_Work>();
    worksCopy.addAll(works);
    
    worksCopy.forEach(work ->
    {
      if (getMainText().getKeyWork(work) == null)
        db.getObjectList(rtLabelOfWork, work, true).remove(this);
    });
    
    ArrayList<HDT_MiscFile> filesCopy = new ArrayList<HDT_MiscFile>();
    filesCopy.addAll(miscFiles);
    
    filesCopy.forEach(file ->
    {
      if (getMainText().getKeyWork(file) == null)
        db.getObjectList(rtLabelOfFile, file, true).remove(this);
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
}
