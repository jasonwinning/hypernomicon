/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.view;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.OmniFinder.TierEnum.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.queryEngines.AllQueryEngine;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import com.google.common.collect.ImmutableSet;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.text.Text;

public class OmniFinder
{
  private final HyperTable htFind;
  private final List<ObservableList<HyperTableCell>> cellLists = new ArrayList<>();
  private final List<HyperTableRow> rows = new ArrayList<>();
  private final EnumSet<TierEnum> tierSet;
  private final EnumMap<TierEnum, ImmutableSet<HDT_RecordType>> tierToTypeSet = new EnumMap<>(TierEnum.class);
  private final Set<HDT_Record> records = new HashSet<>();

  private String query = "";
  private Iterator<HDT_Record> source = null;
  private FinderThread finderThread = null;
  private boolean stopRequested = false, stopped = true, showingMore = false, incremental = true;
  private HDT_RecordType typeFilter;
  public Runnable doneHndlr = null;

  public boolean noResults() { return collEmpty(records); }

  protected enum TierEnum
  {
    tierExactName,
    tierKeywordStart,
    tierNameStartExact,
    tierAuthorExact,
    tierAuthorStartExact,
    tierKeyword,
    tierAuthorKeyword,
    tierNameContains,
    tierAuthorContains,
    tierKeywordContains
  }

  private static final int ROWS_TO_SHOW = 25;

  public OmniFinder(HyperTable htFind) { this(htFind, hdtNone, true); }

  public OmniFinder(HyperTable htFind, HDT_RecordType typeFilter, boolean incremental)
  {
    this.typeFilter = typeFilter;
    this.incremental = incremental;

    ImmutableSet<HDT_RecordType> typeSet, authoredSet = ImmutableSet.of(hdtWork, hdtMiscFile);

    if (typeFilter == hdtNone)
      typeSet = ImmutableSet.of
      (
        hdtTerm,      hdtPosition,    hdtDebate, hdtPerson,    hdtPersonGroup, hdtWork,
        hdtWorkLabel, hdtMiscFile,    hdtNote,   hdtGlossary,  hdtArgument,    hdtInstitution, hdtInvestigation
      );
    else
      typeSet = ImmutableSet.of(typeFilter);

    this.htFind = htFind;

    tierToTypeSet.put(tierExactName       , typeSet);
    tierToTypeSet.put(tierNameStartExact  , typeSet);
    tierToTypeSet.put(tierAuthorExact     , authoredSet);
    tierToTypeSet.put(tierAuthorStartExact, authoredSet);
    tierToTypeSet.put(tierKeyword         , typeSet);
    tierToTypeSet.put(tierAuthorKeyword   , authoredSet);
    tierToTypeSet.put(tierKeywordContains , typeSet);
    tierToTypeSet.put(tierNameContains    , typeSet);
    tierToTypeSet.put(tierAuthorContains  , authoredSet);

    tierSet = EnumSet.allOf(TierEnum.class);

    for (int ndx = 0; ndx < ROWS_TO_SHOW; ndx++)
    {
      ObservableList<HyperTableCell> oList = FXCollections.observableArrayList(new HyperTableCell(-1, "", hdtWork),
                                                                               new HyperTableCell(-1, "", hdtWork),
                                                                               new HyperTableCell(-1, "", hdtWork, hsmNumeric),
                                                                               new HyperTableCell(-1, "", hdtPerson, hsmTextSimple));
      cellLists.add(oList);
      rows.add(new HyperTableRow(oList, htFind));
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private class FinderThread extends Thread
  {
    private final HyperTable htFind;
    private final List<HDT_Record> buffer = new ArrayList<>();

    private TierEnum curTier;
    private boolean done = false, lastShowingMore, firstBuffer = true;
    private String lastQuery = "", queryLC;
    private Iterator<TierEnum> tierIt;
    private Iterator<? extends HDT_Record> recordIt;
    private Iterator<HDT_RecordType> typeIt;
    int rowNdx = 0, runLaters = 0;
    long startTime, nextInterval;

    FinderThread(HyperTable htFind)
    {
      super();

      setDaemon(true);
      this.htFind = htFind;

      start();
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void startOver()
    {
      lastQuery = query;
      queryLC = convertToEnglishChars(query).toLowerCase().trim();
      lastShowingMore = showingMore;
      buffer.clear();
      firstBuffer = true;

      AllQueryEngine.linkList.generate(query);

      tierIt = tierSet.iterator();
      curTier = tierIt.next();
      typeIt = tierToTypeSet.get(curTier).iterator();
      recordIt = db.records(typeIt.next()).iterator();
      records.clear();

      done = false;

      while (runLaters > 0)
        sleepForMillis(5);

      rowNdx = 0;
      startTime = System.currentTimeMillis();
      nextInterval = 250;
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private HDT_Record nextRecord()
    {
      if (source != null)
      {
        if (source.hasNext()) return source.next();
        done = true;
        return null;
      }

      if (curTier == tierKeywordStart)
      {
        curTier = tierIt.next();
        typeIt = tierToTypeSet.get(curTier).iterator();
        recordIt = db.records(typeIt.next()).iterator();
      }

      while (recordIt.hasNext() == false)
      {
        while (typeIt.hasNext() == false)
        {
          if (tierIt.hasNext() == false)
          {
            done = true;
            return null;
          }

          curTier = tierIt.next();

          if (curTier == tierKeywordStart)
          {
            List<SearchKeyword> keys = db.getKeysByPrefix(safeSubstring(query, 0, 3).toLowerCase());

            for (SearchKeyword key : keys)
            {
              if (key.endOnly)
              {
                if (key.text.equalsIgnoreCase(query))
                  return getResultRecord(key.record);
              }
              else
              {
                if (query.toLowerCase().startsWith(key.text.toLowerCase()))
                  return getResultRecord(key.record);
              }
            }

            curTier = tierIt.next();  // Start of query did not match a keyword
          }

          typeIt = tierToTypeSet.get(curTier).iterator();
        }

        recordIt = db.records(typeIt.next()).iterator();
      }

      return recordIt.next();
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private HDT_Record getResultRecord(HDT_Record input)
    {
      if (input.getType() != hdtHub)
        return input;

      StrongLink link = HDT_Hub.class.cast(input).getLink();

      if (link.getDebate  () != null) return link.getDebate  ();
      if (link.getPosition() != null) return link.getPosition();
      if (link.getNote    () != null) return link.getNote    ();
      return link.getConcept();
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean authorMatch(Author author, HDT_Person person, String queryLC, TierEnum tier)
    {
      String listName, fullName = "";

      if (author != null) listName = author.getNameLastFirst(true).toLowerCase().trim();
      else                listName = person.getNameLastFirst(true).toLowerCase().trim();

      switch (tier)
      {
        case tierAuthorContains:

          if (listName.contains(queryLC)) return true;
          break;

        case tierAuthorExact:

          if (listName.equals(queryLC)) return true;

          if (author != null) fullName = author.getFullName(true).toLowerCase().trim();
          else                fullName = person.getFullName(true).toLowerCase().trim();

          if (removeFirstParenthetical(fullName).equals(queryLC)) return true;

          break;

        case tierAuthorKeyword:

          if (AllQueryEngine.linkList.getLinks().size() > 0)
          {
            if (person == null)
              person = author.getPerson();

            if (person != null)
              for (KeywordLink keyLink : AllQueryEngine.linkList.getLinks())
                if (keyLink.key.record == person)
                  return true;
          }
          break;

        case tierAuthorStartExact:

          if (listName.startsWith(queryLC)) return true;

          if (author != null) fullName = author.getFullName(true).toLowerCase().trim();
          else                fullName = person.getFullName(true).toLowerCase().trim();

          if (fullName.startsWith(queryLC)) return true;

          break;

        default: break;
      }

      return false;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    // Similar to HyperCB.cbOnAction

    private boolean isMatch(HDT_Record record)
    {
      if (source != null) return true;
      if ((typeFilter != hdtNone) && (record.getType() != typeFilter)) return false;

      if (records.contains(record)) return false;

      switch (curTier)
      {
        case tierKeywordStart:

          return true;

        case tierAuthorContains: case tierAuthorExact: case tierAuthorKeyword: case tierAuthorStartExact:

          if (record.getType() == hdtWork)
          {
            for (Author author : HDT_Work.class.cast(record).getAuthors())
              if (authorMatch(author, null, queryLC, curTier)) return true;
          }
          else if (record.getType() == hdtMiscFile)
          {
            for (HDT_Person author : HDT_MiscFile.class.cast(record).authors)
              if (authorMatch(null, author, queryLC, curTier)) return true;
          }
          return false;

        case tierExactName:

          if (record.getType() == hdtPerson)
            return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorExact);

          return record.getNameEngChar().toLowerCase().equals(queryLC);

        case tierKeyword:

          if (AllQueryEngine.linkList.getLinks().size() > 0)
          {
            for (KeywordLink keyLink : AllQueryEngine.linkList.getLinks())
              if (keyLink.key.record == record)
                return true;
          }
          return false;

        case tierKeywordContains:

          return record.getSearchKey().toLowerCase().contains(queryLC);

        case tierNameContains:

          if (record.getType() == hdtPerson)
            return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorContains);

          return record.getNameEngChar().toLowerCase().contains(queryLC);

        case tierNameStartExact:

          if (record.getType() == hdtPerson)
            return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorStartExact);

          return record.getNameEngChar().toLowerCase().startsWith(queryLC);

        default: return false;
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean addRecord(HDT_Record record)
    {
      buffer.add(record);
      records.add(record);

      if (showingMore == false)
      {
        if ((buffer.size() + rowNdx) >= ROWS_TO_SHOW) // rowNdx should be the number of rows currently in the
          return true;                                // table if the buffer has already been purged at least once
      }

      return false;
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void purgeBuffer()
    {
      boolean add;

      if (buffer.isEmpty()) return;

      List<HyperTableRow> curRows = new ArrayList<>();
      ObservableList<HyperTableCell> cells;

      for (HDT_Record record : buffer)
      {
        if (showingMore)
          cells = FXCollections.observableArrayList(new HyperTableCell(-1, "", hdtWork),
                                                    new HyperTableCell(-1, "", hdtWork),
                                                    new HyperTableCell(-1, "", hdtWork, hsmNumeric),
                                                    new HyperTableCell(-1, "", hdtPerson, hsmTextSimple));
        else
          cells = cellLists.get(rowNdx);

        add = true;

        cells.set(0, new HyperTableCell(record.getID(), "", record.getType()));
        cells.set(1, new HyperTableCell(record.getID(), record.listName(), record.getType()));

        switch (record.getType())
        {
          case hdtWork :

            HDT_Work work = (HDT_Work) record;

            cells.set(2, new HyperTableCell(work.getID(), work.getYear(), hdtWork, hsmNumeric));

            if (work.authorRecords.isEmpty())
              cells.set(3, new HyperTableCell(work.getID(), work.getShortAuthorsStr(true), hdtWork, hsmTextSimple));
            else if ((work.getAuthors().size() == 1) && (work.authorRecords.size() == 1))
            {
              HDT_Person author = work.authorRecords.get(0);
              cells.set(3, new HyperTableCell(author.getID(), author.getCBText(), hdtPerson, hsmTextSimple));
            }
            else
              cells.set(3, new HyperTableCell(work.authorRecords.get(0).getID(), work.getShortAuthorsStr(true), hdtPerson, hsmTextSimple));

            break;

          case hdtMiscFile :

            HDT_MiscFile miscFile = (HDT_MiscFile) record;

            cells.set(2, new HyperTableCell(miscFile.getID(), "", hdtMiscFile, hsmNumeric));

            if (miscFile.authors.isEmpty())
              cells.set(3, new HyperTableCell(-1, "", hdtPerson, hsmTextSimple));
            else if (miscFile.authors.size() == 1)
            {
              HDT_Person author = miscFile.authors.get(0);
              cells.set(3, new HyperTableCell(author.getID(), author.getCBText(), hdtPerson, hsmTextSimple));
            }
            else
              cells.set(3, new HyperTableCell(miscFile.authors.get(0).getID(), miscFile.getShortAuthorsStr(true), hdtPerson, hsmTextSimple));

            break;

          case hdtWorkLabel :

            HDT_WorkLabel label = (HDT_WorkLabel) record;

            cells.set(2, new HyperTableCell(label.getID(), "", hdtWorkLabel, hsmNumeric));
            cells.set(3, new HyperTableCell(label.getID(), label.getExtendedText(), hdtWorkLabel));

            break;

          default :

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
        }

        if (add)
        {
          if (showingMore)
            curRows.add(new HyperTableRow(cells, htFind));
          else
          {
            if (rowNdx == (ROWS_TO_SHOW - 1))  // This will be the "show more" row
            {
              cells.set(0, new HyperTableCell(-1, "", hdtNone, hsmLast));
              cells.set(1, new HyperTableCell(-1, "", hdtAuxiliary, hsmLast));
              cells.set(2, new HyperTableCell(-1, "", hdtNone, hsmLast));
              cells.set(3, new HyperTableCell(-1, "", hdtNone, hsmLast));
            }

            curRows.add(rows.get(rowNdx));
          }

          rowNdx++;
        }
      }

      buffer.clear();

      runLaters++;

      final boolean finalShowingMore = showingMore,
                    finalFirstBuffer = firstBuffer;

      firstBuffer = false;

      Platform.runLater(() ->
      {
        if (finalFirstBuffer == false)
          htFind.addDataRows(curRows);
        else
          htFind.setDataRows(curRows);

        if (finalShowingMore && incremental)
        {
          htFind.selectRow(ROWS_TO_SHOW - 1);
          htFind.refresh();
          runDelayedInFXThread(1, 30, htFind::scrollToSelection);
        }
        else if (finalFirstBuffer)
          htFind.selectRow(0);

        runLaters--;
      });
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    @Override public void run()
    {
      HDT_Record record = null;

      startOver();

      while (true)
      {
        if (stopRequested)
        {
          stopRequested = false;
          return;
        }

        if ((query.equals(lastQuery) == false) || (showingMore != lastShowingMore))
          startOver();

        if (!done)
          record = nextRecord();

        if ((System.currentTimeMillis() - startTime) > nextInterval)
        {
          purgeBuffer();

          if (done)
          {
            Platform.runLater(() ->
            {
              if (doneHndlr != null)
                doneHndlr.run();
              else
                htFind.getTV().setPlaceholder(new Text("No results."));
            });

            return;
          }

          startTime = System.currentTimeMillis();
          nextInterval = 100;
        }

        if (!done && isMatch(record))
          done = addRecord(record);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setSourceAndStart(Iterator<HDT_Record> source, boolean showingMore)
  {
    if (finderThread != null)
      stop();

    this.query = "";
    this.source = source;
    this.showingMore = showingMore;

    finderThread = new FinderThread(htFind);

    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setQueryAndStart(String query, boolean showingMore)
  {
    boolean newThread = false;

    if (finderThread == null)
    {
      newThread = true;
    }
    else if (finderThread.isAlive() == false)
    {
      stop();
      newThread = true;
    }

    this.query = query;
    this.source = null;
    this.showingMore = showingMore;

    if (newThread)
      finderThread = new FinderThread(htFind);

    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean isRunning()
  {
    if (stopped == true) return false;
    return nullSwitch(finderThread, false, FinderThread::isAlive);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean stop()
  {
    boolean wasRunning = isRunning();

    if ((finderThread != null) && finderThread.isAlive())
    {
      stopRequested = true;

      try { finderThread.join(); } catch (InterruptedException e) { noOp(); }
    }

    finderThread = null;
    runInFXThread(htFind::clear);

    return wasRunning;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
