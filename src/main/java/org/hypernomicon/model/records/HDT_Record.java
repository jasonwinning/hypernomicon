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
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.UIUtil.*;

import java.time.Instant;
import java.util.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.model.unities.HDT_RecordWithDescription;
import org.hypernomicon.model.Tag;

//---------------------------------------------------------------------------

public interface HDT_Record
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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
  String resultTextForTag(Tag tag, boolean limitTo20Items, boolean engChar);
  int resultCount(Tag tag);
  boolean getTagBoolean(Tag tag);
  Ternary getTagTernary(Tag tag);
  Set<Tag> getAllTags();
  boolean isUnitable();
  boolean hasDesc();     // this means the record has a description, but not necessarily that it is associated with a
                         // specific MainText object (true for HDT_Term)
  boolean hasMainText(); // this means the record is associated with a specific MainText object (false for HDT_Term)
  void expire();
  boolean isExpired();
  boolean isDummy();
  boolean updateObjectGroups(RelationType relType, List<ObjectGroup> newGroups, Collection<Tag> tags);

  /**
   * Add strings associated with this record to the passed-in list for search and indexing purposes.
   * @param list The passed-in list where strings are being accumulated
   * @param searchLinkedRecords Whether to include the strings for this record's related records
   * <p>For example, if this is called on a debate record, the larger debate names would be added as well.</p>
   * <p>The "where any field contains" query passes true if and only if the search is constrained
   * to a certain record type.</p>
   * @param includeMainText Whether to include the main text. Main text is indexed separately when
   * building the mentions index.
   * @param engChar If true, add text converted to English characters
   */
  void getAllStrings(List<String> list, boolean searchLinkedRecords, boolean includeMainText, boolean engChar);
  String name();
  void setName(String str);

  /**
   * The original primary intended use case of this function is to return the text that should show in HyperTable
   * cells where the rows are a list of records, and the column is indicating to the user which record it is.
   * <p>Typically, this will be the same as what is returned for name() or an abbreviation, and assumes the
   * surrounding context of the table can help the user to know which record is being indicated, unlike defaultChoiceText().</p>
   * @see #defaultChoiceText()
   * @return The default text that should show in a table cell for this record
   */
  String defaultCellText();
  String getNameEngChar();

  /**
   * The original primary intended use case of this function is to return the text that should show in ComboBoxes,
   * when showing the selected item and the choices in the dropdown.
   * <p>In some cases, this will contain more context information than name() or defaultCellText(). For example, it
   * includes the glossary in the case of concept records.</p>
   * @see #defaultCellText()
   * @see ResultRow#defaultChoiceText
   * @see RecordPopulator#generateCellText(HyperTableRow row, HDT_Record record)
   * @return The choice text so the user knows which record is selected or which record they are choosing
   */
  String defaultChoiceText();

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

  /**
   * Returns the passed-in record's ID, or -1 if it is null
   * @param record The record
   * @return the passed-in record's ID, or -1 if it is null
   */
  static int getIDSafe(HDT_Record record)
  {
    return record == null ? -1 : record.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the passed-in record's type, or hdtNone if it is null
   * @param record The record
   * @return the passed-in record's type, or hdtNone if it is null
   */
  static RecordType getTypeSafe(HDT_Record record)
  {
    return record == null ? hdtNone : record.getType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getDescHtml(HDT_Record record)
  {
    return (isEmpty(record, false) == false) && record.hasDesc() ? ((HDT_RecordWithDescription) record).getDesc().getHtml() : "";
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
