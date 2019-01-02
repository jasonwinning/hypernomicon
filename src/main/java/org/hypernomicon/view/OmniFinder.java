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

package org.hypernomicon.view;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.OmniFinder.TierEnum.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;

import static java.util.Objects.*;

import java.util.ArrayList;
import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.*;
import org.hypernomicon.queryEngines.AllQueryEngine;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.text.Text;

public class OmniFinder
{ 
  private final HyperTable htFind;
  private final ArrayList<ObservableList<HyperTableCell>> cellLists;
  private final ArrayList<HyperTableRow> rows;
  private final EnumSet<TierEnum> tierSet;
  private final EnumMap<TierEnum, LinkedHashSet<HDT_RecordType>> tierToTypeSet;
  private final HashSet<HDT_Base> records;
 
  private String query = "";
  private FinderThread finderThread = null;
  public boolean stopRequested = false;
  private boolean stopped = true, showingMore = false;
  
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
  
  public static final int ROWS_TO_SHOW = 25;

  public OmniFinder(HyperTable htFind)
  {
    LinkedHashSet<HDT_RecordType> typeSet = new LinkedHashSet<>();
    
    typeSet.add(hdtTerm);          typeSet.add(hdtPosition);      typeSet.add(hdtDebate);
    typeSet.add(hdtPerson);        typeSet.add(hdtPersonGroup);   typeSet.add(hdtWork);
    typeSet.add(hdtWorkLabel);     typeSet.add(hdtMiscFile);      typeSet.add(hdtNote);
    typeSet.add(hdtGlossary);      typeSet.add(hdtArgument);      typeSet.add(hdtInstitution);
    typeSet.add(hdtInvestigation);
    
    LinkedHashSet<HDT_RecordType> authoredSet = new LinkedHashSet<>();
    
    authoredSet.add(hdtWork);      authoredSet.add(hdtMiscFile);
    
    this.htFind = htFind;
    
    cellLists = new ArrayList<>();
    rows = new ArrayList<>();

    tierToTypeSet = new EnumMap<TierEnum, LinkedHashSet<HDT_RecordType>>(TierEnum.class);
    records = new HashSet<>();
    
    tierToTypeSet.put(tierExactName,        typeSet);
    tierToTypeSet.put(tierNameStartExact,   typeSet);
    tierToTypeSet.put(tierAuthorExact,      authoredSet);
    tierToTypeSet.put(tierAuthorStartExact, authoredSet);
    tierToTypeSet.put(tierKeyword,          typeSet);
    tierToTypeSet.put(tierAuthorKeyword,    authoredSet);
    tierToTypeSet.put(tierKeywordContains,  typeSet);
    tierToTypeSet.put(tierNameContains,     typeSet);
    tierToTypeSet.put(tierAuthorContains,   authoredSet);
    
    tierSet = EnumSet.allOf(TierEnum.class);
        
    for (int ndx = 0; ndx < ROWS_TO_SHOW; ndx++)
    {
      ArrayList<HyperTableCell> cellList = new ArrayList<>();
      
      cellList.add(new HyperTableCell(-1, "", hdtWork));
      cellList.add(new HyperTableCell(-1, "", hdtWork));
      cellList.add(new HyperTableCell(-1, "", hdtWork, hsmNumeric));
      cellList.add(new HyperTableCell(-1, "", hdtPerson, hsmTextSimple));
      
      ObservableList<HyperTableCell> oList = FXCollections.observableList(cellList);
      
      cellLists.add(oList);
      
      rows.add(new HyperTableRow(oList, htFind));
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public class FinderThread extends Thread
  {
    private final HyperTable htFind;
    private final ArrayList<HDT_Base> buffer = new ArrayList<>();
    
    private TierEnum curTier;
    private boolean done = false, lastShowingMore, firstBuffer = true;
    private String lastQuery = "", queryLC;
    private Iterator<TierEnum> tierIt;
    private Iterator<? extends HDT_Base> recordIt; 
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

    private HDT_Base nextRecord()
    {
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

    private HDT_Base getResultRecord(HDT_Base input)
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
      
      if (nonNull(author))
        listName = author.getNameLastFirst(true).toLowerCase().trim();
      else
        listName = person.getNameLastFirst(true).toLowerCase().trim();
      
      switch (tier)
      {
      case tierAuthorContains:
      
        if (listName.contains(queryLC)) return true;
        break;
        
      case tierAuthorExact:
        
        if (listName.equals(queryLC)) return true;
        
        if (nonNull(author))
          fullName = author.getFullName(true).toLowerCase().trim();
        else
          fullName = person.getFullName(true).toLowerCase().trim();
        
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
              {
                return true;
              }
        }
        break;
        
      case tierAuthorStartExact:

        if (listName.startsWith(queryLC)) return true;
        
        if (nonNull(author))
          fullName = author.getFullName(true).toLowerCase().trim();
        else
          fullName = person.getFullName(true).toLowerCase().trim();
        
        if (fullName.startsWith(queryLC)) return true;

        break;
        
      default: break;
      }
      
      return false;
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    // Similar to HyperCB.cbOnAction
    
    private boolean isMatch(HDT_Base record)
    {
      if (records.contains(record)) return false;
           
      String listName;
      
      switch (curTier)
      {
      case tierKeywordStart:

        return true;
        
      case tierAuthorContains: case tierAuthorExact: case tierAuthorKeyword: case tierAuthorStartExact:
        
        if (record.getType() == hdtWork)
        {
          HDT_Work work = (HDT_Work) record;
          
          for (Author author : work.getAuthors())
            if (authorMatch(author, null, queryLC, curTier)) return true;
        }
        else if (record.getType() == hdtMiscFile)
        {
          HDT_MiscFile file = (HDT_MiscFile) record;
          
          for (HDT_Person author : file.authors)
            if (authorMatch(null, author, queryLC, curTier)) return true;
        }
        
        break;
        
      case tierExactName:
        
        if (record.getType() == hdtPerson)
          return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorExact);
        
        listName = record.getNameEngChar().toLowerCase();
        if (listName.equals(queryLC)) return true;
        break;
        
      case tierKeyword:
        
        if (AllQueryEngine.linkList.getLinks().size() > 0)
        {
          for (KeywordLink keyLink : AllQueryEngine.linkList.getLinks())
            if (keyLink.key.record == record)
              return true;
        }
        break;
        
      case tierKeywordContains:
        
        if (record.getSearchKey().toLowerCase().contains(queryLC)) return true;        
        break;
        
      case tierNameContains:

        if (record.getType() == hdtPerson)
          return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorContains);
        
        listName = record.getNameEngChar().toLowerCase();
        if (listName.contains(queryLC)) return true;
        break;
        
      case tierNameStartExact:
        
        if (record.getType() == hdtPerson)
          return authorMatch(null, HDT_Person.class.cast(record), queryLC, tierAuthorStartExact);      
        
        listName = record.getNameEngChar().toLowerCase();
        if (listName.startsWith(queryLC)) return true;
        break;
        
      default:
        break;
      }
      
      return false;
    }
    
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean addRecord(HDT_Base record)
    {
      buffer.add(record);
      records.add(record);
      
      if (showingMore == false)
      {
        if ((buffer.size() + rowNdx) >= ROWS_TO_SHOW) // rowNdx should be the number of rows currently in the table if the buffer has already been purged at least once
          return true;
      }
      
      return false;
    }
    
    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------
      
    private void purgeBuffer()
    {      
      boolean add;
      
      if (buffer.isEmpty()) return;
      
      ArrayList<HyperTableRow> curRows = new ArrayList<>();
      ObservableList<HyperTableCell> cells;
      
      for (HDT_Base record : buffer)
      {
        if (showingMore)
        {
          ArrayList<HyperTableCell> cellList = new ArrayList<>();
          
          cellList.add(new HyperTableCell(-1, "", hdtWork));
          cellList.add(new HyperTableCell(-1, "", hdtWork));
          cellList.add(new HyperTableCell(-1, "", hdtWork, hsmNumeric));
          cellList.add(new HyperTableCell(-1, "", hdtPerson, hsmTextSimple));
          
          cells = FXCollections.observableList(cellList);
        }
        else
        {
          cells = cellLists.get(rowNdx);
        }
        
        add = true;
        
        cells.set(0, new HyperTableCell(record.getID(), "", record.getType()));
        cells.set(1, new HyperTableCell(record.getID(), record.listName(), record.getType()));
               
        switch (record.getType())
        {
          case hdtPerson:
        
            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtPersonGroup:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtInstitution:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
                       
          case hdtInvestigation:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtDebate:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtArgument:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtPosition:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtTerm:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtWorkLabel:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtWork:

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
                       
          case hdtMiscFile:

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
            
          case hdtNote:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          case hdtGlossary:

            cells.set(2, new HyperTableCell(record.getID(), "", record.getType(), hsmNumeric));
            cells.set(3, new HyperTableCell(record.getID(), "", record.getType(), hsmTextSimple));

            break;
            
          default :
            
            add = false;
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
      
      final boolean finalShowingMore = showingMore;
      final boolean finalFirstBuffer = firstBuffer;
      
      firstBuffer = false;
      
      Platform.runLater(() ->
      {
        if (finalFirstBuffer == false)
          htFind.addDataRows(curRows);
        else
          htFind.setDataRows(curRows);
                
        if (finalShowingMore)
        {
          htFind.selectRow(ROWS_TO_SHOW - 1);
          htFind.getTV().refresh();
          runDelayedInFXThread(1, 30, (event) -> htFind.scrollToSelection());
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
      HDT_Base record = null;
      
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
            Platform.runLater(() -> ui.tvFind.setPlaceholder(new Text("No results.")));
            return;
          }
        
          startTime = System.currentTimeMillis();
          nextInterval = 100;
        }
        
        if (!done)
          if (isMatch(record))
            done = addRecord(record);
      }
    }
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
    this.showingMore = showingMore;
    
    if (newThread)
      finderThread = new FinderThread(htFind);
    
    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isRunning()
  { 
    if (stopped == true) return false;
    return finderThread == null ? false : finderThread.isAlive(); 
  } 

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean stop()
  {
    boolean wasRunning = isRunning();
    
    if (finderThread != null)
      if (finderThread.isAlive())
      {
        stopRequested = true;
        
        try { finderThread.join(); } catch (InterruptedException e) { noOp(); }
      }
          
    finderThread = null;
    Platform.runLater(htFind::clear);
    
    return wasRunning;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
