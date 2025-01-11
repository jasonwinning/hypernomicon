/*
 * Copyright 2015-2025 Jason Winning
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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.UIUtil.*;

import java.time.Instant;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.model.unities.HDT_RecordWithDescription;
import org.hypernomicon.model.Tag;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public interface HDT_Record
{
  int keyNdx();
  int getID();
  RecordType getType();

  void bringStoredCopyOnline(boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError;
  boolean hasStoredState();
  RecordState getRecordStateBackup();
  void restoreTo(RecordState backupState, boolean rebuildMentions) throws RelationCycleException, SearchKeyException, RestoreException, HDB_InternalError;
  void saveToStoredState() throws HDB_InternalError;
  void writeStoredStateToXML(StringBuilder xml);
  void revertToXmlVersion() throws HyperDataException;

  void modifyNow();
  void viewNow();
  Instant getModifiedDate();
  Instant getViewDate();
  Instant getCreationDate();

  HDI_Schema getSchema(Tag tag);
  String resultTextForTag(Tag tag);
  int resultCount(Tag tag);
  boolean getTagBoolean(Tag tag);
  Set<Tag> getAllTags();
  boolean isUnitable();
  boolean hasDesc();     // this means the record has a description, but not necessarily that it is associated with a
                         // specific MainText object (true for HDT_Term)
  boolean hasMainText(); // this means the record is associated with a specific MainText object (false for HDT_Term)
  void expire();
  boolean isExpired();
  boolean isDummy();
  boolean updateObjectGroups(RelationType relType, List<ObjectGroup> newGroups, Collection<Tag> tags);

  void getAllStrings(List<String> list, boolean searchLinkedRecords);
  String name();
  Tag getNameTag();
  void setName(String str);
  String listName();
  String getNameEngChar();
  String getCBText();
  String getXMLObjectName();
  String getSortKey();
  String makeSortKey();
  String getSortKeyAttr();
  void setSortKeyAttr(String sortKeyAttr);
  String getSearchKey();
  String firstActiveKeyWord();
  void setSearchKey(String newKey) throws DuplicateSearchKeyException, SearchKeyTooShortException;
  void setSearchKey(String newKey, boolean noMod, boolean rebuildMentions) throws DuplicateSearchKeyException, SearchKeyTooShortException;

  void resolvePointers() throws HDB_InternalError;
  void updateSortKey();
  Iterable<SearchKeyword> getSearchKeys();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getDescHtml(HDT_Record record)
  {
    return (HDT_Record.isEmpty(record, false) == false) && record.hasDesc() ? ((HDT_RecordWithDescription) record).getDesc().getHtml() : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean isEmpty(HDT_Record record, boolean newerInstanceOK)
  {
    try { return isEmptyThrowsException(record, newerInstanceOK); }
    catch (HDB_InternalError e) { errorPopup(e); }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean isEmptyThrowsException(HDT_Record record, boolean newerInstanceOK) throws HDB_InternalError
  {
    if ((record == null) || record.isExpired()) return true;

    if (record.getID() < 1)
      throw new HDB_InternalError(28883);

    HDT_Record curInstance = getCurrentInstance(record);

    return newerInstanceOK ? (curInstance == null) : (curInstance != record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Get current live record instance having the same type and ID as the passed-in record.
   * @param record Possibly out of date record instance
   * @return Current live instance
   */
  @SuppressWarnings("unchecked")
  static <HDT_R extends HDT_Record> HDT_R getCurrentInstance(HDT_R record)
  {
    return record == null ? null : (HDT_R) db.records(record.getType()).getByID(record.getID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
