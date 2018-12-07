/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.populators;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------  

public class RecordByTypePopulator extends Populator
{
  private HashMap<HyperTableRow, HDT_RecordType> rowToRecordType;
  private HashMap<HyperTableRow, Boolean> rowToChanged;
  private HashMap<HyperTableRow, List<HyperTableCell>> rowToChoices;
  private PopulatorFilter filter = null;
  private boolean nameOnly;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public RecordByTypePopulator()                       { this(null, false); }
  public RecordByTypePopulator(boolean nameOnly)       { this(null, nameOnly); }
  public RecordByTypePopulator(PopulatorFilter filter) { this(filter, false); }
  
  public RecordByTypePopulator(PopulatorFilter filter, boolean nameOnly) 
  { 
    this.filter = filter;
    this.nameOnly = nameOnly;
    
    rowToChoices = new HashMap<HyperTableRow, List<HyperTableCell>>();
    rowToChanged = new HashMap<HyperTableRow, Boolean>();
    rowToRecordType = new HashMap<HyperTableRow, HDT_RecordType>();
  }

  @Override public CellValueType getValueType()        { return cvtRecord; }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  public void setRecordType(HyperTableRow row, HDT_RecordType newType)
  {
    if (row == null) row = dummyRow;
    
    if (rowToRecordType.put(row, newType) != newType)
      rowToChanged.put(row, true);
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  private HDT_Base getNextRecord(Iterator<? extends HDT_Base> it)
  {
    while (it.hasNext())
    {      
      HDT_Base record = it.next();
           
      if ((filter == null) || (filter.filter(record)))
        if (HyperDB.isUnstoredRecord(record.getID(), record.getType()) == false)
          return record;
    }
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  // If num = 5, returns a list of the 5 most recently viewed records
  
  private ArrayList<Integer> getRecent(HDT_RecordType recordType, int num)
  {
    Instant dates[] = new Instant[num];
    int ids[] = new int[num], pos[] = new int[num], revPos[] = new int[num];
    
    for (int ndx = 0; ndx < num; ndx++)
    {
      dates[ndx] = Instant.MIN;
      ids[ndx] = -1;
      pos[ndx] = ndx;
      revPos[ndx] = ndx;
    }
      
    Iterator<? extends HDT_Base> it = db.records(recordType).keyIterator();    
    HDT_Base record = getNextRecord(it);
    
    while (record != null)
    {
      Instant curDate = record.getViewDate();      
      int slotNdx = -1;
      
      for (int ndx = num - 1; ndx >= 0; ndx--)
      {
        if (dates[revPos[ndx]].compareTo(curDate) >= 0)  // Most of the time this happens on the first iteration so
          break;                                         // the containing loop moves quickly to its next iteration
        
        slotNdx = revPos[ndx];
      }
      
      if (slotNdx > -1)
      {
        int insertPos = pos[slotNdx];
            
        for (int ndx = 0; ndx < num; ndx++)
        {
          if (pos[ndx] == (num - 1))
          {
            pos[ndx] = insertPos;
            ids[ndx] = record.getID();
            dates[ndx] = curDate;
          }
          
          else if (pos[ndx] >= insertPos)
            pos[ndx]++;
          
          revPos[pos[ndx]] = ndx;
        }
      }
      
      record = getNextRecord(it);
    }
       
    ArrayList<Integer> recent = new ArrayList<Integer>();
    
    for (int ndx = 0; ndx < num; ndx++)
    {
      int slotNdx = revPos[ndx];
      if (ids[slotNdx] > 0)
        recent.add(ids[slotNdx]);
    }
    
    return recent;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if (row == null) row = dummyRow;
    
    HyperTableCell choice;
    HashMap<Integer, Boolean> map = new HashMap<Integer, Boolean>();
    boolean firstAdd = true;
    ArrayList<Integer> recent;
       
    if (rowToRecordType.containsKey(row) == false)
      rowToRecordType.put(row, hdtNone);
    
    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());
    
    List<HyperTableCell> choices = rowToChoices.get(row), recentChoices = new ArrayList<>();
    
    if ((hasChanged(row) == false) && (force == false))
      return choices;
    
    HDT_RecordType recordType = rowToRecordType.get(row);
    choices.clear();
    choices.add(new HyperTableCell(-1, "", hdtNone));
    
    if (recordType == hdtNone) return choices;
      
    if (recordType.getDisregardDates() == false)
    {
      recent = getRecent(recordType, 5);
            
      for (Integer id : recent)
      {
        HDT_Base record = db.records(recordType).getByID(id.intValue());
        
        if (firstAdd)
        {
          choices.clear();
          firstAdd = false;
        }
        
        recentChoices.add(new HyperTableCell(record.getID(), nameOnly ? record.name() : record.getCBText(), recordType));
        
        map.put(id, true);              
      }
    }
    
    for (HDT_Base record : db.records(recordType).keyIterable())
    {      
      if (((filter == null) || filter.filter(record)) && (map.containsKey(record.getID()) == false))
      {
        if (firstAdd)
        {
          choices.clear();
          firstAdd = false;
        }

        if (nameOnly)
          choice = new HyperTableCell(record.getID(), record.name(), recordType);
        else if (recordType == hdtWork)
          choice = new HyperTableCell(record.getID(), record.getCBText(), recordType, hsmWork);
        else
          choice = new HyperTableCell(record.getID(), record.getCBText(), recordType);
        
        int ndx = Collections.binarySearch(choices, choice);
        
        if (ndx < 0)
          ndx = (ndx + 1) * -1;
        
        choices.add(ndx, choice);
      }
    }
    
    if (db.records(recordType).size() == 0)
      choices.clear();
    else if (recentChoices.size() > 0)
    {
      for (int ndx = 0; ndx < recentChoices.size(); ndx++)
        choices.add(ndx, recentChoices.get(ndx));
    }
    
    choices.add(new HyperTableCell(-1, "", hdtNone));

    rowToChanged.put(row, false);
    return choices;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    return populate(nullSwitch(row, dummyRow), false).contains(cell) ? cell.clone() : null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  @Override public boolean hasChanged(HyperTableRow row)
  {
    if (row == null) row = dummyRow;
    
    rowToChanged.putIfAbsent(row, true);
    return rowToChanged.get(row);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  @Override public void setChanged(HyperTableRow row)
  {
    rowToChanged.put(nullSwitch(row, dummyRow), true);
  }
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  @Override public HDT_RecordType getRecordType(HyperTableRow row)
  {
    return rowToRecordType.getOrDefault(nullSwitch(row, dummyRow), hdtNone);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  @Override public void clear()
  {
    rowToChanged.clear(); 
    rowToChoices.clear();
    rowToRecordType.clear(); 
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value)
  {
    if (row == null) row = dummyRow;
    
    HDT_RecordType type = ((id > 0) || (safeStr(value).length() > 0)) ? rowToRecordType.getOrDefault(row, hdtNone) : hdtNone;
        
    HyperTableCell cell = new HyperTableCell(id, value, type);
    
    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());
    
    rowToChoices.get(row).add(cell);
    return cell;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

}
