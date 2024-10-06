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

package org.hypernomicon.view;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.OmniFinder.TierEnum.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr.PersonForDupCheck;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_Hub;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.RecordHTC;

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
  private final EnumMap<TierEnum, ImmutableSet<RecordType>> tierToTypeSet = new EnumMap<>(TierEnum.class);
  private final Set<HDT_Record> records = new HashSet<>();
  private final RecordType typeFilter;
  private final boolean incremental;

  private volatile String query = "";
  private volatile Iterator<HDT_Record> source = null;
  private FinderThread finderThread = null;
  private volatile boolean stopRequested = false, showingMore = false;
  public Runnable doneHndlr = null;

  public boolean noResults()  { return collEmpty(records); }
  private boolean isRunning() { return HyperThread.isRunning(finderThread); }

  protected enum TierEnum
  {
    tierExactName,
    tierAuthorYear,
    tierPersonMatch,
    tierPersonMatchStart,
    tierAuthorMatch,
    tierAuthorMatchStart,
    tierNameStartExact,
    tierKeyword,
    tierKeywordStart,
    tierAuthorKeyword,
    tierNameContains,
    tierAuthorContains,
    tierKeywordContains
  }

  private static final int ROWS_TO_SHOW = 25;

  OmniFinder(HyperTable htFind) { this(htFind, hdtNone, true); }

  public OmniFinder(HyperTable htFind, RecordType typeFilter, boolean incremental)
  {
    this.typeFilter = typeFilter;
    this.incremental = incremental;

    ImmutableSet<RecordType> typeSet, authoredSet = ImmutableSet.of(hdtWork, hdtMiscFile);

    if (typeFilter == hdtNone)
      typeSet = ImmutableSet.of
      (
        hdtTerm, hdtInvestigation, hdtPosition, hdtDebate, hdtPerson,   hdtPersonGroup,
        hdtWork, hdtWorkLabel,     hdtMiscFile, hdtNote,   hdtGlossary, hdtArgument,    hdtInstitution
      );
    else
      typeSet = ImmutableSet.of(typeFilter);

    this.htFind = htFind;

    tierToTypeSet.put(tierExactName       , typeSet);
    tierToTypeSet.put(tierNameStartExact  , typeSet);
    tierToTypeSet.put(tierAuthorMatch     , authoredSet);
    tierToTypeSet.put(tierAuthorYear      , authoredSet);
    tierToTypeSet.put(tierAuthorMatchStart, authoredSet);
    tierToTypeSet.put(tierPersonMatch     , ImmutableSet.of(hdtPerson));
    tierToTypeSet.put(tierPersonMatchStart, ImmutableSet.of(hdtPerson));
    tierToTypeSet.put(tierKeyword         , typeSet);
    tierToTypeSet.put(tierAuthorKeyword   , authoredSet);
    tierToTypeSet.put(tierKeywordContains , typeSet);
    tierToTypeSet.put(tierNameContains    , typeSet);
    tierToTypeSet.put(tierAuthorContains  , authoredSet);

    tierSet = EnumSet.allOf(TierEnum.class);

    for (int ndx = 0; ndx < ROWS_TO_SHOW; ndx++)
    {
      ObservableList<HyperTableCell> oList = FXCollections.observableArrayList(new RecordHTC("", hdtWork),
                                                                               new RecordHTC("", hdtWork),
                                                                               new RecordHTC("", hdtWork),
                                                                               new RecordHTC("", hdtPerson));
      cellLists.add(oList);
      rows.add(new HyperTableRow(oList, htFind));
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final class FinderThread extends HyperThread
  {
    private final List<HDT_Record> buffer = new ArrayList<>();

    private TierEnum curTier;
    private boolean done = false, lastShowingMore, firstBuffer = true;
    private String lastQuery = "", queryLC;
    private Map<HDT_Record, List<PersonForDupCheck>> recordToPersonList;
    private PersonForDupCheck queryPerson;
    private List<KeywordLink> linkList;
    private Iterator<TierEnum> tierIt;
    private Iterator<? extends HDT_Record> recordIt;
    private Iterator<RecordType> typeIt;
    private int rowNdx = 0, runLaters = 0;
    private long startTime, nextInterval;

    private FinderThread()
    {
      super("OmniFinder");

      setDaemon(true);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void startOver()
    {
      lastQuery = query;
      queryLC = convertToEnglishChars(query).toLowerCase().trim();
      queryPerson = new PersonForDupCheck(new PersonName(queryLC).toLowerCase(), null);
      recordToPersonList = new HashMap<>();
      lastShowingMore = showingMore;
      buffer.clear();
      firstBuffer = true;

      linkList = KeywordLinkList.generate(query);

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
      nextInterval = 250L;
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
            for (SearchKeyword key : db.getKeysByPrefix(safeSubstring(query, 0, 3).toLowerCase()))
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

    private static HDT_Record getResultRecord(HDT_Record input)
    {
      return input.getType() != hdtHub ? input : ((HDT_Hub)input).mainSpoke(true);
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean authorMatch(PersonForDupCheck otherPerson, String year, TierEnum tier)
    {
      String listName = otherPerson.getAuthor().getNameLastFirst(true).toLowerCase().trim();

      switch (tier)
      {
        case tierAuthorContains:

          return listName.contains(queryLC);

        case tierAuthorMatch: case tierPersonMatch:

          return queryPerson.matches(otherPerson);

        case tierAuthorYear:

          if (year.isBlank()) return false;

          String singleName = otherPerson.getAuthor().singleName(true).toLowerCase().trim() + ' ' + year;

          if (removeFirstParenthetical(singleName).equals(queryLC)) return true;

          break;

        case tierAuthorKeyword:

          if (linkList.size() > 0)
          {
            HDT_Person otherPersonRecord = otherPerson.getPerson();

            if (otherPersonRecord != null)
              for (KeywordLink keyLink : linkList)
                if (keyLink.key().record == otherPersonRecord)
                  return true;
          }

          break;

        case tierAuthorMatchStart: case tierPersonMatchStart:

          return otherPerson.startsWith(queryPerson.getName().getFull());

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

      return switch (curTier)
      {
        case tierKeywordStart     -> true;
        case tierExactName        -> (record.getType() != hdtPerson) && record.getNameEngChar().toLowerCase().equals(queryLC);
        case tierNameStartExact   -> (record.getType() != hdtPerson) && record.getNameEngChar().toLowerCase().startsWith(queryLC);
        case tierKeyword          -> linkList.stream().anyMatch(keyLink -> keyLink.key().record == record);
        case tierKeywordContains  -> record.getSearchKey().toLowerCase().contains(queryLC);
        case tierPersonMatch,
             tierPersonMatchStart -> authorMatch(getPersonList(record).get(0), "", curTier);

        case tierNameContains ->
        {
          yield switch (record.getType())
          {
            case hdtPerson -> authorMatch(getPersonList(record).get(0), "", tierAuthorContains);
            default        -> record.getNameEngChar().toLowerCase().contains(queryLC);
          };
        }

        case tierAuthorContains, tierAuthorMatch, tierAuthorYear, tierAuthorKeyword, tierAuthorMatchStart ->
        {
          yield switch (record.getType())
          {
            case hdtWork     -> getPersonList(record).stream().anyMatch(otherPerson -> authorMatch(otherPerson, ((HDT_Work)record).getYearStr(), curTier));
            case hdtMiscFile -> getPersonList(record).stream().anyMatch(otherPerson -> authorMatch(otherPerson, ""                             , curTier));
            default          -> false;
          };
        }

        default -> false;
      };
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean addRecord(HDT_Record record)
    {
      buffer .add(record);
      records.add(record);

      return (showingMore == false) &&
             ((buffer.size() + rowNdx) >= ROWS_TO_SHOW); // rowNdx should be the number of rows currently in the
    }                                                    // table if the buffer has already been purged at least once

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private void purgeBuffer()
    {
      if (buffer.isEmpty()) return;

      List<HyperTableRow> curRows = new ArrayList<>();

      for (HDT_Record record : buffer)
      {
        ObservableList<HyperTableCell> cells = showingMore ?
          FXCollections.observableArrayList(new RecordHTC("", hdtWork),
                                            new RecordHTC("", hdtWork),
                                            new RecordHTC("", hdtWork),
                                            new RecordHTC("", hdtPerson))
        :
          cellLists.get(rowNdx);

        cells.set(0, new RecordHTC(record.getID(), ""               , record.getType()));
        cells.set(1, new RecordHTC(record.getID(), record.listName(), record.getType()));

        switch (record.getType())
        {
          case hdtWork :

            HDT_Work work = (HDT_Work) record;

            cells.set(2, new RecordHTC(work, work.getYearStr()));

            if (work.authorRecords.isEmpty())
              cells.set(3, new RecordHTC(work, work.getShortAuthorsStr(true)));
            else if ((work.getAuthors().size() == 1) && (work.authorRecords.size() == 1))
            {
              HDT_Person author = work.authorRecords.get(0);
              cells.set(3, new RecordHTC(author, author.getCBText()));
            }
            else
              cells.set(3, new RecordHTC(work.authorRecords.get(0), work.getShortAuthorsStr(true)));

            break;

          case hdtMiscFile :

            HDT_MiscFile miscFile = (HDT_MiscFile) record;

            cells.set(2, new RecordHTC(miscFile, ""));

            List<HDT_Person> authorRecords = miscFile.authorRecords();

            if (authorRecords.isEmpty())
              cells.set(3, new RecordHTC("", hdtPerson));
            else if (authorRecords.size() == 1)
            {
              HDT_Person author = authorRecords.get(0);
              cells.set(3, new RecordHTC(author, author.getCBText()));
            }
            else
              cells.set(3, new RecordHTC(authorRecords.get(0), miscFile.getShortAuthorsStr(true)));

            break;

          case hdtInvestigation :

            HDT_Investigation inv = (HDT_Investigation) record;
            HDT_Person person = inv.person.get();

            cells.set(2, new RecordHTC(inv, ""));
            cells.set(3, new RecordHTC(person, person.getCBText()));

            break;

          case hdtWorkLabel :

            HDT_WorkLabel label = (HDT_WorkLabel) record;

            cells.set(2, new RecordHTC(label, ""));
            cells.set(3, new RecordHTC(label, label.extendedText()));

            break;

          case hdtConcept :

            HDT_Concept concept = (HDT_Concept) record;
            cells.set(2, new RecordHTC(concept, ""));
            cells.set(3, new RecordHTC(concept, "Glossary: " + concept.glossary.get().listName()));

            break;

          default :

            cells.set(2, new RecordHTC(record, ""));
            cells.set(3, new RecordHTC(record, ""));

            break;
        }

        if (showingMore)
          curRows.add(new HyperTableRow(cells, htFind));
        else
        {
          if (rowNdx == (ROWS_TO_SHOW - 1))  // This will be the "show more" row
          {
            cells.set(0, new RecordHTC("", hdtNone     , true));
            cells.set(1, new RecordHTC("", hdtAuxiliary, true));
            cells.set(2, new RecordHTC("", hdtNone     , true));
            cells.set(3, new RecordHTC("", hdtNone     , true));
          }

          curRows.add(rows.get(rowNdx));
        }

        rowNdx++;
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

        if (finalShowingMore && incremental && (htFind.dataRowCount() >= ROWS_TO_SHOW))
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

        if (done == false)
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
          nextInterval = 100L;
        }

        if ((done == false) && isMatch(record))
          done = addRecord(record);
      }
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    private List<PersonForDupCheck> getPersonList(HDT_Record record)
    {
      switch (record.getType())
      {
        case hdtPerson :

          switch (curTier)
          {
            case tierPersonMatch: case tierPersonMatchStart: case tierNameContains:
              List<PersonForDupCheck> personList = recordToPersonList.get(record);
              if (personList == null)
                recordToPersonList.put(record, personList = List.of(new PersonForDupCheck((HDT_Person)record)));

              return personList;

            default : return null;
          }

        case hdtWork : case hdtMiscFile :

          switch (curTier)
          {
            case tierAuthorContains: case tierAuthorMatch: case tierAuthorYear: case tierAuthorKeyword: case tierAuthorMatchStart:
              List<PersonForDupCheck> personList = recordToPersonList.get(record);
              if (personList == null)
                recordToPersonList.put(record, personList = ((HDT_RecordWithAuthors<?>)record).getAuthors().stream().map(PersonForDupCheck::new).toList());

              return personList;

            default : return null;
          }

        default : return null;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void setSourceAndStart(Iterator<HDT_Record> source, boolean showingMore)
  {
    if (finderThread != null)
      stop();

    this.query = "";
    this.source = source;
    this.showingMore = showingMore;

    (finderThread = new FinderThread()).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void setQueryAndStart(String query, boolean showingMore)
  {
    boolean newThread = isRunning() == false;

    if ((finderThread != null) && newThread)
      stop();

    this.query = query;
    this.source = null;
    this.showingMore = showingMore;

    if (newThread)
      (finderThread = new FinderThread()).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  synchronized void stop()
  {
    if (isRunning())
    {
      stopRequested = true;

      try { finderThread.join(); } catch (InterruptedException e) { noOp(); }
    }

    finderThread = null;
    runInFXThread(htFind::clear);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
