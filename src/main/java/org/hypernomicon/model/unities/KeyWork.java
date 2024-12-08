/*
 * Copyright 2015-2024 Jason Winning
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

import java.util.List;

import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.Authors;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordWithAuthors;
import org.hypernomicon.model.records.HDT_MiscFile;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.settings.WorkSearchKeySettings;
import org.hypernomicon.settings.WorkSearchKeySettings.CitationParenthesesOption;
import org.hypernomicon.settings.WorkSearchKeySettings.WorkSearchKeyConfig;

//---------------------------------------------------------------------------

public class KeyWork implements Comparable<KeyWork>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private abstract static class RecordPointer
  {

//---------------------------------------------------------------------------

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

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class OfflineRecordPointer extends RecordPointer
  {

//---------------------------------------------------------------------------

    private final RecordType type;
    private final int id;

//---------------------------------------------------------------------------

    private OfflineRecordPointer(RecordType type, int id)
    {
      this.type = type;
      this.id = id;
    }

//---------------------------------------------------------------------------

    @Override int getID()            { return id; }
    @Override RecordType getType()   { return type; }
    @Override HDT_Record getRecord() { return db.records(getType()).getByID(getID()); }

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class OnlineRecordPointer extends RecordPointer
  {

//---------------------------------------------------------------------------

    private final HDT_Record record;

    private OnlineRecordPointer(HDT_Record record)
    {
      this.record = record;
    }

//---------------------------------------------------------------------------

    @Override int getID()            { return record.getID(); }
    @Override RecordType getType()   { return record.getType(); }
    @Override HDT_Record getRecord() { return record; }

    @Override boolean isExpired()    { return HDT_Record.isEmpty(record, false); }

//---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final RecordPointer recordPtr;
  private String searchKey = "";
  private boolean searchKeyInitialized = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public KeyWork(HDT_RecordWithAuthors<? extends Authors> recordWithAuthors)
  {
    recordPtr = new OnlineRecordPointer(recordWithAuthors);
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
  boolean isExpired()                   { return (recordPtr == null) || recordPtr.isExpired(); }
  KeyWork getOnlineCopy()               { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), true); }
  KeyWork getOfflineCopy()              { return new KeyWork(getRecordType(), getRecordID(), getSearchKey(), false); }

  @Override public int hashCode()       { return recordPtr.hashCode(); }

  @SuppressWarnings("unchecked")
  public HDT_RecordWithAuthors<? extends Authors> getRecord() { return (HDT_RecordWithAuthors<? extends Authors>) recordPtr.getRecord(); }

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

  public String getEditorText()
  {
    return updateSearchKeyAndCheckIfActive() ?
      searchKey
    :
      "<a id=\"" + recordPtr.getID() + "\" type=\"" + Tag.getTypeTagStr(recordPtr.getType()) + "\">" + htmlEscaper.escape(searchKey) + "</a>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Generate the search key string if it is currently empty and check whether that string would actually link to this record.
   * @return True if the search key string would link to this record; false otherwise
   */
  private boolean updateSearchKeyAndCheckIfActive()
  {
    searchKeyInitialized = true;

    HDT_RecordWithAuthors<? extends Authors> record = getRecord();

    if (searchKey.isEmpty())
      searchKey = record.makeKeyWorkSearchKey(); // Try using the author and year and see if that is a keyword match

    if (searchKey.length() > 0)
    {
      SearchKeyword hyperKey = db.getKeyByKeyword(searchKey);

      if (hyperKey != null)
      {
        if (hyperKey.record.getID() == record.getID())
          if (hyperKey.record.getType() == record.getType())
            return true; // Return true if the search key would link to this record

        searchKey = "";
      }
    }

    // If we got here, the value set to searchKey would *not* link to this record
    // so use first active keyword that matches one of the single author search key patterns

    if (record.getType() == hdtWork)
    {
      HDT_Work work = (HDT_Work)record;

      if ((work.getAuthors().isEmpty() == false) && (work.getYearStr().isEmpty() == false))
      {
        WorkSearchKeySettings settings = WorkSearchKeySettings.loadFromPrefNode();

        // Loop through active keywords

        for (SearchKeyword keyObj : work.getSearchKeys())
        {
          // Loop through single author search key patterns

          for (WorkSearchKeyConfig keyConfig : settings)
          {
            if (keyConfig.multipleAuthors)
              continue;

            String keyStr = keyObj.text;

            // Remove letter from active keyword if one was added

            int letterPos = keyConfig.parentheses == CitationParenthesesOption.none ? keyStr.length() - 1 : keyStr.length() - 2;

            if (safeSubstring(keyStr, letterPos - work.getYearStr().length(), letterPos).equals(work.getYearStr()))
              keyStr = keyStr.substring(0, letterPos) + safeSubstring(keyStr, letterPos + 1, keyStr.length());

            // Loop through authors and see if key generated by the pattern matches the active key

            for (Author author : work.getAuthors())
            {
              if (keyStr.equals(keyConfig.format(List.of(author.singleName()), work.getYearStr())))
              {
                searchKey = keyObj.text;
                return true;
              }
            }
          }
        }
      }
    }

    // Now try getting the first active searchKey for the record instead

    String activeKeyWord = record.firstActiveKeyWord();

    if (activeKeyWord.isEmpty())
    {
      if (searchKey.isEmpty())
        searchKey = record.makeKeyWorkSearchKey(); // Use the author and year; the only reason why it wasn't a keyword match is because there are no active keywords

      return false;
    }

    searchKey = activeKeyWord;
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int getYear()
  {
    if (recordPtr.getType() == hdtWork)
      return parseInt(((HDT_Work) recordPtr.getRecord()).getYearStr(), 0);
    if (recordPtr.getType() == hdtMiscFile)
      return nullSwitch(((HDT_MiscFile) recordPtr.getRecord()).work.get(), 0, work -> parseInt(work.getYearStr(), 0));

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
