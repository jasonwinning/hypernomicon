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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.apache.commons.lang3.mutable.MutableInt;

import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.view.tabs.WorkTabController;

public class KeyWork implements Comparable<KeyWork>
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private class RecordPointer
  {
    private boolean online = false;
    private HDT_RecordType type = hdtNone;
    private int id = -1;
    private HDT_Base record = null;

    //---------------------------------------------------------------------------

    public RecordPointer(HDT_RecordType type, int id, boolean online)
    {
      this.online = online;
      
      this.type = type;
      this.id = id;
      
      if (online)
        record = db.records(type).getByID(id);
    }

    //---------------------------------------------------------------------------

    public RecordPointer(HDT_Base record)
    {
      this.online = true;
      
      this.type = record.getType();
      this.id = record.getID();
      this.record = record;
    }

    //---------------------------------------------------------------------------
    
    public boolean isExpired() { return HDT_Record.isEmpty(record); }

    //---------------------------------------------------------------------------

    public int getID()
    {
      if (online)
        id = record.getID();

      return id;    
    }

    //---------------------------------------------------------------------------

    public HDT_RecordType getType()
    {
      if (online)
        type = record.getType();
      
      return type;
    }

    //---------------------------------------------------------------------------

    public HDT_Base getRecord()
    {
      if (record == null)
        record = db.records(getType()).getByID(getID()); 
      
      return record;
    }

    //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((record == null) ? 0 : record.hashCode());
      result = prime * result + id;
      result = prime * result + ((type == null) ? 0 : type.hashCode());
      return result;
    }

    //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;
      
      RecordPointer other = (RecordPointer) obj;

      if ((other.record != null) && (record != null))
        return record.equals(other.record);
      
      if (id != other.id) return false;
      if (type != other.type) return false;
      return true;
    }
    
    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  private RecordPointer record = null;
  private String searchKey = "";
  private boolean searchKeyInitialized = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public KeyWork(HDT_RecordWithPath recordWithPath)
  {
    record = new RecordPointer(recordWithPath);    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public KeyWork(HDT_RecordType recordType, int recordID, String searchKey, boolean online)
  {
    record = new RecordPointer(recordType, recordID, online);
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
          String lwSearchKey = nextSubString(work.largerWork.get().getSearchKey(), ";", new MutableInt(0));
          
          if (lwSearchKey.length() == 0)
            lwSearchKey = WorkTabController.makeWorkSearchKey(work.largerWork.get().getAuthors(), work.largerWork.get().getYear(), work);
          
          if (lwSearchKey.length() == 0)
            lwSearchKey = (work.largerWork.get().getYear() + " " + work.largerWork.get().name()).trim();
            
          searchKey = nextSubString(work.name(), ":", new MutableInt(0)) + " in " + 
                      nextSubString(lwSearchKey, ":", new MutableInt(0));
        }
        else
          searchKey = (work.getYear() + " " + work.name()).trim();
      }
        
      searchKey = nextSubString(searchKey, ":", new MutableInt(0));
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