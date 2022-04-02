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

package org.hypernomicon.query;

import static org.hypernomicon.App.curQV;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.query.ui.QueryTabCtrlr.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.getCellText;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.KeywordLink;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordState;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.query.sources.FilteredQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.ListMultimap;

public final class GeneralQueries
{

//---------------------------------------------------------------------------

  private GeneralQueries() { throw new UnsupportedOperationException(); }

  private static final int QUERY_RECORD_TYPE            = QUERY_FIRST_NDX + 1,
                           QUERY_RECORD_EQUALS          = QUERY_FIRST_NDX + 2,
                           QUERY_ASSOCIATED_WITH_PHRASE = QUERY_FIRST_NDX + 3;
  public static final int  QUERY_LINKING_TO_RECORD      = QUERY_FIRST_NDX + 4,
                           QUERY_MATCHING_RECORD        = QUERY_FIRST_NDX + 5,
                           QUERY_MATCHING_STRING        = QUERY_FIRST_NDX + 6;
  private static final int QUERY_MENTIONED_BY           = QUERY_FIRST_NDX + 7;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean recordByTypeInit(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2)
  {
    vp1.setPopulator(row, new RecordTypePopulator(true));
    vp2.setPopulator(row, new RecordByTypePopulator());
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean recordByTypeOp1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp2)
  {
    RecordByTypePopulator rtp = vp2.getPopulator(row);
    rtp.setRecordType(row, HyperTableCell.getCellType(op1));
    rtp.populate(row, false);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final List<QueryType> otherTypes = List.of(qtPersons  , qtWorks, qtInstitutions  , qtDebates, qtPositions, qtFolders,
                                                            qtArguments, qtNotes, qtInvestigations, qtFiles  , qtConcepts);

  private static void addQuery(boolean addToAll, ListMultimap<QueryType, Query<?>> queryTypeToQueries, RecordQuery query)
  {
    queryTypeToQueries.put(qtAllRecords, query);

    if (addToAll == false) return;

    otherTypes.forEach(queryType -> queryTypeToQueries.put(queryType, query));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(ListMultimap<QueryType, Query<?>> queryTypeToQueries)
  {
    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_WITH_NAME_CONTAINING, "with name containing")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, vp1.getRestricted(row) ? 1 : 2);
        vp1.setRestricted(row, false);

        return false;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return record.listName().toUpperCase().contains(getCellText(op1).toUpperCase());
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp)
      {
        return opNum == 1;
      }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_ANY_FIELD_CONTAINS, "where any field contains")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, vp1.getRestricted(row) ? 1 : 2);
        vp1.setRestricted(row, false);

        return false;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        List<String> list = new ArrayList<>();
        record.getAllStrings(list, curQV.getSearchLinkedRecords());

        String val1 = getCellText(op1).toLowerCase();

        return list.stream().anyMatch(str -> str.toLowerCase().contains(val1));
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp)
      {
        return opNum == 1;
      }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_LIST_ALL, "list all records")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, 1);
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return true;
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp)
      {
        return false;
      }
    });

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new QueryWhereField(QUERY_WHERE_FIELD, "where field"));

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new QueryWhereRelative(QUERY_WHERE_RELATIVE, "where set of records having this record as"));

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    addQuery(false, queryTypeToQueries, new RecordQuery(QUERY_RECORD_TYPE, "record type equals")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setPopulator(row, new RecordTypePopulator(true));
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return record.getType() == HyperTableCell.getCellType(op1);
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum == 1; }
    });

  //---------------------------------------------------------------------------

    addQuery(false, queryTypeToQueries, new RecordQuery(QUERY_RECORD_EQUALS, "show specified record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOp1Change(op1, row, vp2);
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return true;
      }

      @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return new FilteredQuerySource(origSource, op1, op2)
        {
          @Override protected void runFilter(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
          {
            HDT_Record specifiedRecord = HyperTableCell.getRecord(op2);
            if (HDT_Record.isEmpty(specifiedRecord) == false)
              list.add(specifiedRecord);
          }
        };
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum < 3; }
    });

  //---------------------------------------------------------------------------

    addQuery(false, queryTypeToQueries, new RecordQuery(QUERY_ASSOCIATED_WITH_PHRASE, "show the record this phrase would link to")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setRestricted(row, false);
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return true;
      }

      @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return new FilteredQuerySource(origSource, op1)
        {
          @Override protected void runFilter(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
          {
            List<KeywordLink> linkList = KeywordLinkList.generate(getCellText(op1));
            if (linkList.size() > 0)
              list.add(linkList.get(0).key.record);
          }
        };
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum == 1; }
    });

  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_LINKING_TO_RECORD, "with description linking to record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOp1Change(op1, row, vp2);
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall) throws HyperDataException
      {
        HDT_Record specifiedRecord = HyperTableCell.getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord)) return false;

        boolean result = db.firstMentionsSecond(record, specifiedRecord, true, choseNotToWait);

        if (choseNotToWait.isTrue()) // Mentions index rebuild should never be running here
          throw new HDB_InternalError(54681);

        return result;
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum < 3; }
    });

  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_MATCHING_RECORD, "with any text that would link to this record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOp1Change(op1, row, vp2);
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
      {
        return true;
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override public QuerySource getSource(QuerySource origSource, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return new FilteredQuerySource(origSource, op1, op2)
        {
          @Override protected void runFilter(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
          {
            HDT_Record specifiedRecord = HyperTableCell.getRecord(op2);
            if (HDT_Record.isEmpty(specifiedRecord)) return;

            list.addAll(db.getMentionerSet(specifiedRecord, false, choseNotToWait));

            list.removeIf(specifiedRecord::equals);

            if (specifiedRecord.getType() == hdtWork)
            {
              HDT_Work work = (HDT_Work) specifiedRecord;
              work.workFiles.forEach(workFile -> list.removeIf(workFile::equals));
            }

            if (choseNotToWait.isTrue())
              throw new HDB_InternalError(61187); // Mentions index rebuild should never be running here
          }
        };
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum < 3; }
    });

  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_MATCHING_STRING, "with any text that would link to a record having this search key")
    {
      private final SearchKeys dummySearchKeys = new SearchKeys();
      private HDT_Record searchDummy = null;

      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp1.setRestricted(row, false);
        return true;
      }

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall) throws HyperDataException
      {
        if (firstCall)
        {
          dummySearchKeys.removeAll();

          searchDummy = db.createNewRecordFromState(new RecordState(hdtPerson, -1, "", "", "", "", true), true);

          dummySearchKeys.setSearchKey(searchDummy, getCellText(op1), true, false);
        }

        if (searchDummy == null) return false;

        List<String> list = new ArrayList<>();
        record.getAllStrings(list, true);
        boolean add = false;

        for (String str : list)
          if (KeywordLinkList.generate(str.toLowerCase(), true, dummySearchKeys).size() > 0)
            add = true;

        return add;
      }

      @Override public void cleanup()
      {
        if (searchDummy != null)
        {
          db.deleteRecord(searchDummy);
          searchDummy = null;
          dummySearchKeys.removeAll();
        }
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum == 1; }
    });

  //---------------------------------------------------------------------------

    addQuery(true, queryTypeToQueries, new RecordQuery(QUERY_MENTIONED_BY, "that are mentioned by record")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeInit(row, vp1, vp2);
      }

      @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        return recordByTypeOp1Change(op1, row, vp2);
      }

      private final MutableBoolean choseNotToWait = new MutableBoolean();

      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall) throws HyperDataException
      {
        HDT_Record specifiedRecord = HyperTableCell.getRecord(op2);
        if (HDT_Record.isEmpty(specifiedRecord)) return false;

        boolean result = db.firstMentionsSecond(specifiedRecord, record, true, choseNotToWait);

        if (choseNotToWait.isTrue()) // Mentions index rebuild should never be running here
          throw new HDB_InternalError(54681);

        return result;
      }

      @Override public boolean needsMentionsIndex() { return true; }

      @Override public boolean hasOperand(int opNum, HyperTableCell prevOp) { return opNum < 3; }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
