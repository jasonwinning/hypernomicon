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

package org.hypernomicon.model.unities;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.view.tabs.WorkTabCtrlr;

public class KeyWork implements Comparable<KeyWork>
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private abstract static class RecordPointer
  {
    abstract int getID();
    abstract RecordType getType();
    abstract HDT_Record getRecord();

    boolean isExpired() { return false; }

    //---------------------------------------------------------------------------

    @Override public int hashCode()
    {
      final int prime = 31;
      int result = 1;
      result = prime * result + getID();
      result = prime * result + nullSwitch(getType(), 0, RecordType::hashCode);
      return result;
    }

    //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if ((obj instanceof RecordPointer) == false) return false;

      RecordPointer other = (RecordPointer) obj;

      return (getID() == other.getID()) && (getType() == other.getType());
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private static final class OfflineRecordPointer extends RecordPointer
  {
    private final RecordType type;
    private final int id;

    //---------------------------------------------------------------------------

    private OfflineRecordPointer(RecordType type, int id)
    {
      this.type = type;
      this.id = id;
    }

    @Override int getID()            { return id; }
    @Override RecordType getType()   { return type; }
    @Override HDT_Record getRecord() { return db.records(getType()).getByID(getID()); }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

  }

  private static final class OnlineRecordPointer extends RecordPointer
  {
    private final HDT_Record record;

    private OnlineRecordPointer(HDT_Record record)
    {
      this.record = record;
    }

    @Override int getID()            { return record.getID(); }
    @Override RecordType getType()   { return record.getType(); }
    @Override HDT_Record getRecord() { return record; }

    @Override boolean isExpired()    { return HDT_Record.isEmpty(record); }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final RecordPointer recordPtr;
  private String searchKey = "";
  private boolean searchKeyInitialized = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public KeyWork(HDT_RecordWithPath recordWithPath)
  {
    recordPtr = new OnlineRecordPointer(recordWithPath);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public KeyWork(RecordType recordType, int recordID, String searchKey, boolean online)
  {
    recordPtr = online ?
      new OnlineRecordPointer(db.records(recordType).getByID(recordID))
    :
      new OfflineRecordPointer(recordType, recordID);

    this.searchKey = searchKey;
    searchKeyInitialized = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public RecordType getRecordType()     { return recordPtr.getType(); }
  public int getRecordID()              { return recordPtr.getID(); }
  public HDT_RecordWithPath getRecord() { return (HDT_RecordWithPath) recordPtr.getRecord(); }
  boolean isExpired()                   { return (recordPtr == null) || recordPtr.isExpired(); }
  KeyWork getOnlineCopy()               { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), true); }
  KeyWork getOfflineCopy()              { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), false); }

  @Override public int hashCode()       { return recordPtr.hashCode(); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void makeSearchKey()
  {
    if (recordPtr.getType() == hdtWork)
    {
      HDT_Work work = (HDT_Work) recordPtr.getRecord();
      searchKey = WorkTabCtrlr.makeWorkSearchKey(work.getAuthors(), work.getYear(), work);

      if (searchKey.isEmpty())
      {
        if (work.largerWork.isNotNull())
        {
          String lwSearchKey = new SplitString(work.largerWork.get().getSearchKey(), ';').next();

          if (lwSearchKey.isEmpty())
            lwSearchKey = WorkTabCtrlr.makeWorkSearchKey(work.largerWork.get().getAuthors(), work.largerWork.get().getYear(), work);

          if (lwSearchKey.isEmpty())
            lwSearchKey = (work.largerWork.get().getYear() + ' ' + work.largerWork.get().name()).trim();

          searchKey = new SplitString(work.name(), ':').next() + " in " +
                      new SplitString(lwSearchKey, ':').next();
        }
        else
          searchKey = (work.getYear() + ' ' + work.name()).trim();
      }

      searchKey = new SplitString(searchKey, ':').next();
    }
    else
      searchKey = recordPtr.getRecord().name();
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

    if (searchKey.isEmpty())
      makeSearchKey(); // Try using the author and year and see if that is a keyword match

    if (searchKey.length() > 0)
    {
      SearchKeyword hyperKey = db.getKeyByKeyword(searchKey);

      if (hyperKey != null)
      {
        if (hyperKey.record.getID() == recordPtr.getID())
          if (hyperKey.record.getType() == recordPtr.getType())
            return true;

        searchKey = "";
      }
    }

    String activeKeyWord = recordPtr.getRecord().firstActiveKeyWord();

    if (activeKeyWord.isEmpty())
    {
      if (searchKey.isEmpty())
        makeSearchKey(); // Use the author and year; the only reason why it wasn't a keyword match is because there are no active keywords

      return false;
    }

    searchKey = activeKeyWord;
    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public String getEditorText()
  {
    return updateSearchKeyAndCheckIfActive() ?
      searchKey
    :
      "<a id=\"" + recordPtr.getID() + "\" type=\"" + Tag.getTypeTagStr(recordPtr.getType()) + "\">" + searchKey + "</a>";
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private int getYear()
  {
    if (recordPtr.getType() == hdtWork)
      return parseInt(((HDT_Work) recordPtr.getRecord()).getYear(), 0);
    if (recordPtr.getType() == hdtMiscFile)
      return nullSwitch(((HDT_MiscFile) recordPtr.getRecord()).work.get(), 0, work -> parseInt(work.getYear(), 0));

    return 0;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if ((obj == null) || (recordPtr == null)) return false;
    if (getClass() != obj.getClass()) return false;

    return recordPtr.equals(((KeyWork)obj).recordPtr);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public int compareTo(KeyWork o)
  {
    int diff = getYear() - o.getYear();
    return diff == 0 ? getSearchKey().compareToIgnoreCase(o.getSearchKey()) : diff;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
