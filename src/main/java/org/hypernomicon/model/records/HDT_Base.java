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

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HubChangedException;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.SearchKeys.SearchKeyword;

public interface HDT_Base
{
  public int getID();
  public HDT_RecordType getType();
  public void assignID(int newID);
  
  public void bringStoredCopyOnline() throws RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException;
  public boolean hasStoredState();
  public HDT_RecordState getRecordStateBackup();
  public void restoreTo(HDT_RecordState backupState) throws RelationCycleException, HDB_InternalError, SearchKeyException, HubChangedException;
  public void saveToStoredState() throws HDB_InternalError;
  public void writeStoredStateToXML(StringBuilder xml);
     
  public void modifyNow();
  public void viewNow();
  public Instant getModifiedDate();
  public Instant getViewDate();
  public Instant getCreationDate();
  
  public HDI_Schema getSchema(Tag tag);
  public String getResultTextForTag(Tag tag);
  public boolean getTagBoolean(Tag tag);
  public Set<Tag> getAllTags();
  public boolean isUnitable();
  public boolean hasDesc();     // this means the record has a description, but not necessarily that it is connected directly to a MainText object (true for HDT_Term)
  public boolean hasMainText(); // this means the record is directly connected to a MainText object (false for HDT_Term)
  public void expire();
  public boolean isExpired();
  public boolean isDummy();
  public boolean changeID(int newID);
  
  public void getAllStrings(ArrayList<String> list, boolean searchLinkedRecords);
  public String name();
  public Tag getNameTag();
  public void setName(String str);
  public String listName();
  public String getNameEngChar();
  public String getCBText();
  public String getXMLObjectName();
  public String getSortKey();
  public String makeSortKey();
  public String getSortKeyAttr();
  public String getSearchKey();
  public String getFirstActiveKeyWord();
  public void setSearchKey(String newKey) throws SearchKeyException;
  public void setSearchKey(String newKey, boolean noMod) throws SearchKeyException;

  public void resolvePointers() throws HDB_InternalError;
  public void updateSortKey();
  public List<SearchKeyword> getSearchKeys();
}
