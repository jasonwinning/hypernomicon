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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.view.tabs.WorkTabController;

public class KeyWork implements Comparable<KeyWork>
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private abstract class RecordPointer
  {
    abstract public int getID();
    abstract public HDT_RecordType getType();
    abstract public HDT_Base getRecord();
    
    public boolean isExpired() { return false; }
    
    //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + getID();
      result = prime * result + nullSwitch(getType(), 0, type -> type.hashCode());
      return result;
    }

    //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if ((obj instanceof RecordPointer) == false) return false;
      
      RecordPointer other = (RecordPointer) obj;

      if (getID() == other.getID())
        if (getType() == other.getType())
          return true;
      
      return false;
    }
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  private class OfflineRecordPointer extends RecordPointer
  {
    private final HDT_RecordType type;
    private final int id;

    //---------------------------------------------------------------------------

    public OfflineRecordPointer(HDT_RecordType type, int id)
    {
      this.type = type;
      this.id = id;
    }

    @Override public int getID()              { return id; }
    @Override public HDT_RecordType getType() { return type; }
    @Override public HDT_Base getRecord()     { return db.records(getType()).getByID(getID()); }
    
    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

  }
  
  private class OnlineRecordPointer extends RecordPointer
  {
    private final HDT_Base record;
    
    public OnlineRecordPointer(HDT_Base record)
    {
      this.record = record;
    }
    
    @Override public int getID()              { return record.getID(); }
    @Override public HDT_RecordType getType() { return record.getType(); }
    @Override public HDT_Base getRecord()     { return record; }
    
    @Override public boolean isExpired()      { return HDT_Record.isEmpty(record); }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  private final RecordPointer record;
  private String searchKey = "";
  private boolean searchKeyInitialized = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public KeyWork(HDT_RecordWithPath recordWithPath)
  {
    record = new OnlineRecordPointer(recordWithPath);    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public KeyWork(HDT_RecordType recordType, int recordID, String searchKey, boolean online)
  {
    if (online)
      record = new OnlineRecordPointer(db.records(recordType).getByID(recordID));
    else
      record = new OfflineRecordPointer(recordType, recordID);
    
    this.searchKey = searchKey;
    searchKeyInitialized = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public HDT_RecordType getRecordType() { return record.getType(); }
  public int getRecordID()              { return record.getID(); }
  public HDT_RecordWithPath getRecord() { return (HDT_RecordWithPath) record.getRecord(); }
  public boolean isExpired()            { return record == null ? true : record.isExpired(); }
  public KeyWork getOnlineCopy()        { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), true); }
  public KeyWork getOfflineCopy()       { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), false); }
  
  @Override public int hashCode()       { return record.hashCode(); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
 
  private String makeSearchKey()
  {
    String searchKey;

    if (record.getType() == hdtWork)
    {
      HDT_Work work = (HDT_Work) record.getRecord();
      searchKey = WorkTabController.makeWorkSearchKey(work.getAuthors(), work.getYear(), work);
      
      if (searchKey.length() == 0)
      {
        if (work.largerWork.isNotNull())
        {
          String lwSearchKey = new SplitString(work.largerWork.get().getSearchKey(), ';').next();
          
          if (lwSearchKey.length() == 0)
            lwSearchKey = WorkTabController.makeWorkSearchKey(work.largerWork.get().getAuthors(), work.largerWork.get().getYear(), work);
          
          if (lwSearchKey.length() == 0)
            lwSearchKey = (work.largerWork.get().getYear() + " " + work.largerWork.get().name()).trim();
            
          searchKey = new SplitString(work.name(), ':').next() + " in " + 
                      new SplitString(lwSearchKey, ':').next();
        }
        else
          searchKey = (work.getYear() + " " + work.name()).trim();
      }
        
      searchKey = new SplitString(searchKey, ':').next();
    }
    else
      searchKey = HDT_MiscFile.class.cast(record.getRecord()).name();
    
    return searchKey;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private String getSearchKey()
  {
    return getSearchKey(false);
  }
  
  public String getSearchKey(boolean updateFirst)
  {
    if (updateFirst || (searchKeyInitialized == false))
      updateSearchKeyAndCheckIfActive();
    
    return searchKey;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private boolean updateSearchKeyAndCheckIfActive()
  {
    searchKeyInitialized = true;
    
    if (searchKey.length() == 0)
      searchKey = makeSearchKey(); // Try using the author and year and see if that is a keyword match
    
    if (searchKey.length() > 0)
    {
      SearchKeyword hyperKey = db.getKeyByKeyword(searchKey);
  
      if (hyperKey != null)
      {
        if (hyperKey.record.getID() == record.getID())
          if (hyperKey.record.getType() == record.getType())
            return true;
        
        searchKey = "";
      }
    }

    String activeKeyWord = record.getRecord().getFirstActiveKeyWord();
    
    if (activeKeyWord.length() == 0)
    {
      if (searchKey.length() == 0)
        searchKey = makeSearchKey(); // Use the author and year; the only reason why it wasn't a keyword match is because there are no active keywords      
      
      return false;
    }
    
    searchKey = activeKeyWord;
    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public String getEditorText()
  {
    if (updateSearchKeyAndCheckIfActive())
      return searchKey;
    
    return "<a id=\"" + record.getID() + "\" type=\"" + db.getTypeTagStr(record.getType()) + "\">" + searchKey + "</a>";
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  private int getYear()
  {
    int year = 0;
    
    if (record.getType() == hdtWork)
      year = parseInt(HDT_Work.class.cast(record.getRecord()).getYear(), 0);
    else if (record.getType() == hdtMiscFile)
    {
      HDT_Work work = HDT_MiscFile.class.cast(record.getRecord()).work.get();
      if (work != null)
        year = parseInt(work.getYear(), 0);
    }
    
    return year;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    
    KeyWork other = (KeyWork) obj;

    if ((other.record != null) && (record != null))
      return record.equals(other.record);
    
    return false;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public int compareTo(KeyWork o)
  {
    int year1 = getYear(),
        year2 = o.getYear();
           
    if (year1 == year2)
      return getSearchKey().compareToIgnoreCase(o.getSearchKey());
      
    return year1 - year2;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}