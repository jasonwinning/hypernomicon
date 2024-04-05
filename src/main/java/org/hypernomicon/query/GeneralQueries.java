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

package org.hypernomicon.query;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.query.WorkQueries.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.query.Query.FilteredRecordQuery;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.concurrent.Worker.State;

public final class GeneralQueries
{

//---------------------------------------------------------------------------

  private GeneralQueries() { throw new UnsupportedOperationException(); }

  // Numeric IDs associated with queries should never be changed. Changing them could break user query favorite settings.

  public static final int  QUERY_WITH_NAME_CONTAINING    =  1,  // "with name containing"
                           QUERY_ANY_FIELD_CONTAINS      =  2,  // "where any field contains"
                           QUERY_LIST_ALL                =  3;  // "list all records"
  private static final int QUERY_WHERE_FIELD             =  4,  // "where field"
                           QUERY_WHERE_RELATIVE          =  5,  // "where set of records related by being"
                           QUERY_WHERE_KEY_WORKS         =  6,  // "where key works"
                           QUERY_RECORD_TYPE             =  7,  // "record type equals"
                           QUERY_RECORD_EQUALS           =  8,  // "show specified record"
                           QUERY_ASSOCIATED_WITH_PHRASE  =  9;  // "show the record this phrase would link to"
  public static final int  QUERY_LINKING_TO_RECORD       = 10,  // "with description linking to record"
                           QUERY_MATCHING_RECORD         = 11,  // "with any text that would link to this record"
                           QUERY_MATCHING_STRING         = 12;  // "with any text that would link to a record having this search key"
  private static final int QUERY_MENTIONED_BY            = 13,  // "that are mentioned by record"
                           QUERY_WHERE_DISPLAYED_RECORDS = 14;  // "where displayed records"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(List<Query<?>> allQueries)
  {
    allQueries.add(new RecordQuery(QUERY_WITH_NAME_CONTAINING, "with name containing")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, vp1.getRestricted(row) ? 1 : 2);
        vp1.setRestricted(row, false);

        return false;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        String str = getCellText(op1);
        return (str.isEmpty() == false) && record.listName().toUpperCase().contains(str.toUpperCase());
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }

      @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_ANY_FIELD_CONTAINS, "where any field contains")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, vp1.getRestricted(row) ? 1 : 2);
        vp1.setRestricted(row, false);

        return false;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        String val1 = getCellText(op1);
        if (val1.isBlank())
          return false;

        List<String> list = new ArrayList<>();
        record.getAllStrings(list, ui.queryHyperTab().getCurQueryCtrlr().getSearchLinkedRecords());

        String val1LC = val1.toLowerCase();
        return list.stream().anyMatch(str -> str.toLowerCase().contains(val1LC));
      }

      @Override public boolean autoShowDescription() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }

      @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_LIST_ALL, "list all records")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, 1);
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return true;
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return false; }

      @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new QueryWhereField(QUERY_WHERE_FIELD, "where field"));

    allQueries.add(new QueryWhereBibField(QUERY_WHERE_BIB_FIELD, "where bibliographic field"));

    allQueries.add(new QueryWhereRelative(QUERY_WHERE_RELATIVE, "where set of records related by being"));

    allQueries.add(new QueryWhereKeyWorks(QUERY_WHERE_KEY_WORKS, "where key works"));

    allQueries.add(new QueryWhereDisplayedRecords(QUERY_WHERE_DISPLAYED_RECORDS, "where displayed records"));

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_RECORD_TYPE, "record type equals")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setPopulator(row, new RecordTypePopulator(true));
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return record.getType() == getCellType(op1);
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new FilteredRecordQuery(QUERY_RECORD_EQUALS, "show specified record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOpChange(op1, row, vp2);
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return true;
      }

      @Override protected void runFilter(LinkedHashSet<HDT_Record> records, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        HDT_Record specifiedRecord = getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord) == false)
          records.add(specifiedRecord);
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum < 3; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new FilteredRecordQuery(QUERY_ASSOCIATED_WITH_PHRASE, "show the record this phrase would link to")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setRestricted(row, false);
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return true;
      }

      @Override protected void runFilter(LinkedHashSet<HDT_Record> records, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        List<KeywordLink> linkList = KeywordLinkList.generate(getCellText(op1));
        if (linkList.size() > 0)
          records.add(linkList.get(0).key().record);
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_LINKING_TO_RECORD, "with description linking to record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOpChange(op1, row, vp2);
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        HDT_Record specifiedRecord = getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord)) return false;

        boolean result = db.firstMentionsSecond(record, specifiedRecord, true, choseNotToWait);

        if (choseNotToWait.isTrue()) // Mentions index rebuild should never be running here
          throw new HDB_InternalError(54681);

        return result;
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean autoShowDescription() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum < 3; }

      @Override public boolean show(QueryType queryType, RecordType recordType)
      {
        return (queryType == qtAllRecords) || HDT_RecordWithDescription.class.isAssignableFrom(recordType.getRecordClass());
      }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new FilteredRecordQuery(QUERY_MATCHING_RECORD, "with any text that would link to this record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOpChange(op1, row, vp2);
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return true;
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override protected void runFilter(LinkedHashSet<HDT_Record> records, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        HDT_Record specifiedRecord = getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord)) return;

        records.addAll(db.getMentionerSet(specifiedRecord, false, choseNotToWait));
        records.remove(specifiedRecord);

        if (specifiedRecord.getType() == hdtWork)
          ((HDT_Work) specifiedRecord).workFiles.forEach(records::remove);

        if (choseNotToWait.isTrue())
          throw new HDB_InternalError(61187); // Mentions index rebuild should never be running here
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean autoShowDescription() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum < 3; }

      @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_MATCHING_STRING, "with any text that would link to a record having this search key")
    {
      private final SearchKeys dummySearchKeys = new SearchKeys();
      private HDT_Record searchDummy = null;

      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setRestricted(row, false);
        return true;
      }

      @Override public void init(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        dummySearchKeys.removeAll();

        searchDummy = db.createNewRecordFromState(new RecordState(hdtPerson, -1, "", "", "", "", true), true);

        dummySearchKeys.setSearchKey(searchDummy, getCellText(op1), true, false);
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        if (searchDummy == null) return false;

        List<String> list = new ArrayList<>();
        record.getAllStrings(list, true);
        boolean add = false;

        for (String str : list)
          if (KeywordLinkList.generate(str.toLowerCase(), dummySearchKeys::getKeywordsByPrefix).size() > 0)
            add = true;

        return add;
      }

      @Override public void cleanup(State state)
      {
        if (searchDummy != null)
        {
          db.deleteRecord(searchDummy);
          searchDummy = null;
          dummySearchKeys.removeAll();
        }
      }

      @Override public boolean autoShowDescription() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }

      @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    allQueries.add(new RecordQuery(QUERY_MENTIONED_BY, "that are mentioned by record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOpChange(op1, row, vp2);
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
      {
        HDT_Record specifiedRecord = getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord)) return false;

        boolean result = db.firstMentionsSecond(specifiedRecord, record, true, choseNotToWait);

        if (choseNotToWait.isTrue()) // Mentions index rebuild should never be running here
          throw new HDB_InternalError(54681);

        return result;
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum < 3; }

      @Override public boolean show(QueryType queryType, RecordType recordType)
      {
        return (queryType == qtAllRecords) || HDT_RecordWithDescription.class.isAssignableFrom(recordType.getRecordClass());
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
