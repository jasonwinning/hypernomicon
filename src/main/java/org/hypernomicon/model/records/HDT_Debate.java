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

import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.List;

import org.hypernomicon.model.HyperDataset;

public class HDT_Debate extends HDT_RecordWithConnector
{
  public final List<HDT_Debate> largerDebates;
  public final List<HDT_Debate> subDebates;
  public final List<HDT_Position> positions;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Debate(HDT_RecordState xmlState, HyperDataset<HDT_Debate> dataset)
  {
    super(xmlState, dataset, tagName);
    
    largerDebates = getObjList(rtParentDebateOfDebate);
    
    subDebates = getSubjList(rtParentDebateOfDebate);
    positions = getSubjList(rtDebateOfPosition);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
   
  @Override public String listName()        { return name(); }
  @Override public HDT_RecordType getType() { return hdtDebate; }
  @Override public boolean isUnitable()     { return true; }

  public void setLargerDebates(List<HDT_Debate> list) { updateObjectsFromList(rtParentDebateOfDebate, list); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

 
}
