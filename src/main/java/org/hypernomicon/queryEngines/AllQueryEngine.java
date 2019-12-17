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

package org.hypernomicon.queryEngines;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.QueryTabCtrlr.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.model.Exceptions.SearchKeyException;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.records.*;
import org.hypernomicon.querySources.AllQuerySource;
import org.hypernomicon.querySources.FilteredQuerySource;
import org.hypernomicon.querySources.QuerySource;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class AllQueryEngine extends QueryEngine<HDT_Record>
{
  private static final int QUERY_RECORD_TYPE            = QUERY_FIRST_NDX + 1,
                           QUERY_RECORD_EQUALS          = QUERY_FIRST_NDX + 2,
                           QUERY_ASSOCIATED_WITH_PHRASE = QUERY_FIRST_NDX + 3;
  public static final int  QUERY_LINKING_TO_RECORD      = QUERY_FIRST_NDX + 4,
                           QUERY_MATCHING_RECORD        = QUERY_FIRST_NDX + 5,
                           QUERY_MATCHING_STRING        = QUERY_FIRST_NDX + 6;
  private static final int QUERY_MENTIONED_BY           = QUERY_FIRST_NDX + 7,
                           QUERY_DUPLICATE_FOLDERS      = QUERY_FIRST_NDX + 8;

  public static final KeywordLinkList linkList = new KeywordLinkList();
  private static final SearchKeys dummySearchKeys = new SearchKeys();
  private static HDT_Record searchDummy = null;
  private static final MutableBoolean choseNotToWait = new MutableBoolean();

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    pop.addEntry(row, QUERY_RECORD_TYPE, "record type equals");
    pop.addEntry(row, QUERY_RECORD_EQUALS, "show specified record");
    pop.addEntry(row, QUERY_ASSOCIATED_WITH_PHRASE, "show the record this phrase would link to");
    pop.addEntry(row, QUERY_LINKING_TO_RECORD, "with description linking to record");
    pop.addEntry(row, QUERY_MATCHING_RECORD, "with any text that would link to this record");
    pop.addEntry(row, QUERY_MATCHING_STRING, "with any text that would link to a record having this search key");
    pop.addEntry(row, QUERY_MENTIONED_BY, "that are mentioned by record");

    if (app.debugging())
      pop.addEntry(row, QUERY_DUPLICATE_FOLDERS, "that are duplicate folders");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    switch (query)
    {
      case QUERY_RECORD_TYPE :

        vp1.setPopulator(row, new RecordTypePopulator());
        vp2.setPopulator(row, null);
        vp3.setPopulator(row, null);
        break;

      case QUERY_LINKING_TO_RECORD :
      case QUERY_MATCHING_RECORD :
      case QUERY_RECORD_EQUALS:
      case QUERY_MENTIONED_BY:

        vp1.setPopulator(row, new RecordTypePopulator());
        vp2.setPopulator(row, new RecordByTypePopulator());
        vp3.setPopulator(row, null);
        break;

      case QUERY_ASSOCIATED_WITH_PHRASE : case QUERY_MATCHING_STRING :

        vp1.setPopulator(row, null);
        vp1.setRestricted(row, false);
        vp2.setPopulator(row, null);
        vp3.setPopulator(row, null);
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void cancelled()
  {
    cleanupSearchDummy();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupSearchDummy()
  {
    if (searchDummy != null)
    {
      db.deleteRecord(hdtPerson, searchDummy.getID());
      searchDummy = null;
      dummySearchKeys.removeAll();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, boolean firstCall, boolean lastCall)
  {
    boolean add = false;

    switch (curQuery)
    {
      case QUERY_RECORD_TYPE :
        return record.getType() == HyperTableCell.getCellType(param1);

      case QUERY_RECORD_EQUALS :
      case QUERY_MATCHING_RECORD :
      case QUERY_ASSOCIATED_WITH_PHRASE :

        return true;

      case QUERY_MATCHING_STRING :

        if (firstCall)
        {
          dummySearchKeys.removeAll();

          HDT_RecordState recordState = new HDT_RecordState(hdtPerson, -1, "", "", "", "", true);

          try { searchDummy = db.createNewRecordFromState(recordState, true); } catch (Exception e) { noOp(); }

          try
          {
            dummySearchKeys.setSearchKey(searchDummy, getCellText(param1), true, true);
          }
          catch (SearchKeyException e)
          {
            messageDialog(e.getMessage(), mtError);
            cleanupSearchDummy();
            return false;
          }
        }

        if (searchDummy == null) return false;

        List<String> list = new ArrayList<>();
        record.getAllStrings(list, true);

        for (String str : list)
        {
          linkList.generate(str.toLowerCase(), true, dummySearchKeys);
          if (linkList.getLinks().size() > 0) add = true;
        }

        if (lastCall)
          cleanupSearchDummy();

        return add;

      case QUERY_LINKING_TO_RECORD : case QUERY_MENTIONED_BY :

        HDT_Record specifiedRecord = HyperTableCell.getRecord(param2);
        if (HDT_Record.isEmpty(specifiedRecord)) return false;

        boolean result;

        if (curQuery == QUERY_LINKING_TO_RECORD)
          result = db.firstMentionsSecond(record, specifiedRecord, true, choseNotToWait);
        else
          result = db.firstMentionsSecond(specifiedRecord, record, true, choseNotToWait);

        if (choseNotToWait.isTrue()) // Mentions index rebuild should never be running here
        {
          messageDialog("Internal error #54681", mtError);
          task.cancel();
        }

        return result;

      case QUERY_DUPLICATE_FOLDERS :

        return true;

    }

    return add;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void op1Change(int query, HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    switch (query)
    {
      case QUERY_LINKING_TO_RECORD :
      case QUERY_MATCHING_RECORD :
      case QUERY_RECORD_EQUALS :
      case QUERY_MENTIONED_BY :

        RecordByTypePopulator rtp = vp2.getPopulator(row);
        rtp.setRecordType(row, HyperTableCell.getCellType(op1));
        rtp.populate(row, false);
        break;

      default:
        break;

    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void op2Change(int query, HyperTableCell op1, HyperTableCell op2,
      HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2,
      VariablePopulator vp3)
  {

    switch (query)
    {

    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QueryType getQueryType()
  {
    return QueryType.qtAllRecords;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    switch (query)
    {
      case QUERY_MATCHING_RECORD :
        return new FilteredQuerySource(getQueryType(), query, op1, op2)
        {
          @Override protected void runFilter()
          {
            HDT_Record specifiedRecord = HyperTableCell.getRecord(op2);
            if (HDT_Record.isEmpty(specifiedRecord)) return;

            MutableBoolean choseNotToWait = new MutableBoolean();
            list.addAll(db.getMentionerSet(specifiedRecord, false, choseNotToWait));

            list.removeIf(specifiedRecord::equals);

            if (specifiedRecord.getType() == hdtWork)
            {
              HDT_Work work = (HDT_Work) specifiedRecord;
              work.workFiles.forEach(workFile -> list.removeIf(workFile::equals));
            }

            if (choseNotToWait.isTrue())
              messageDialog("Internal error #61187", mtError); // Mentions index rebuild should never be running here
          }
        };

      case QUERY_ASSOCIATED_WITH_PHRASE :
        return new FilteredQuerySource(getQueryType(), query, op1)
        {
          @Override protected void runFilter()
          {
            linkList.generate(getCellText(op1));
            if (linkList.getLinks().size() > 0)
              list.add(linkList.getLinks().get(0).key.record);
          }
        };

      case QUERY_RECORD_EQUALS :
        return new FilteredQuerySource(getQueryType(), query, op1, op2)
        {
          @Override protected void runFilter()
          {
            HDT_RecordType specifiedType = getCellType(op1);
            int specifiedID = getCellID(op2);
            if ((specifiedType == hdtNone) || (specifiedID == -1)) return;
            HDT_Record record = db.records(specifiedType).getByID(specifiedID);
            if (record != null)
              list.add(db.records(specifiedType).getByID(specifiedID));
          }
        };

      case QUERY_DUPLICATE_FOLDERS :
        return new FilteredQuerySource(getQueryType(), query)
        {
          @Override protected void runFilter()
          {
            Map<FilePath, HDT_Folder> map = new HashMap<>();
            Set<HDT_Folder> set = new HashSet<>();

            db.folders.forEach(folder ->
            {
              FilePath filePath = folder.filePath();

              if (map.containsKey(filePath))
              {
                if (set.contains(map.get(filePath)) == false)
                {
                  set.add(map.get(filePath));
                  list.add(map.get(filePath));
                }

                set.add(folder);
                list.add(folder);
              }
              else
                map.put(filePath, folder);
            });
          }
        };

      default :
        break;
    }

    return new AllQuerySource();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean needsMentionsIndex(int query)
  {
    switch (query)
    {
      case QUERY_LINKING_TO_RECORD :
      case QUERY_MENTIONED_BY :
      case QUERY_MATCHING_RECORD :
        return true;

      default :
        return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
