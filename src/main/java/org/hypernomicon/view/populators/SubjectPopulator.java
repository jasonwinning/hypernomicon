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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod.*;

//---------------------------------------------------------------------------  

public class SubjectPopulator extends Populator
{
  private HashMap<HyperTableRow, Boolean> rowToChanged;
  private HashMap<HyperTableRow, List<HyperTableCell>> rowToChoices;
  private HashMap<HyperTableRow, HDT_Base> rowToObj = null;
  private HDT_Base obj = null;
  private RelationType relType;
  private boolean trackObjByRow, nameOnly;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public SubjectPopulator(RelationType relType, boolean trackObjByRow)                   { init(relType, trackObjByRow, false); }
  public SubjectPopulator(RelationType relType, boolean trackObjByRow, boolean nameOnly) { init(relType, trackObjByRow, nameOnly); }

  private void init(RelationType relType, boolean trackObjByRow, boolean nameOnly)
  {
    rowToChoices = new HashMap<HyperTableRow, List<HyperTableCell>>();
    rowToChanged = new HashMap<HyperTableRow, Boolean>();    
    
    this.relType = relType;
    this.trackObjByRow = trackObjByRow;
    this.nameOnly = nameOnly;
    
    if (trackObjByRow)
      rowToObj = new HashMap<HyperTableRow, HDT_Base>();
  }

//---------------------------------------------------------------------------
  
  @Override public CellValueType getValueType()                    { return cvtRecord; }
  @Override public HDT_RecordType getRecordType(HyperTableRow row) { return db.getSubjType(relType); }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  
  
  public HDT_Base getObj(HyperTableRow row)
  {
    if (row == null) row = dummyRow;
    
    if (trackObjByRow)
      return rowToObj.get(row);
    
    return obj;    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public void setObj(HyperTableRow row, HDT_Base newObj)
  {
    boolean changed; 
    HDT_Base oldObj;
    
    if (row == null) row = dummyRow;
    
    if (trackObjByRow)
      oldObj = rowToObj.put(row, newObj);
    else
    {
      oldObj = obj;
      obj = newObj;
    }
    
    changed = true;
    if (oldObj != null)
      if (oldObj == newObj)
        changed = hasChanged(row);
    
    rowToChanged.put(row, changed);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if (row == null) row = dummyRow;
    
    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());
    
    List<HyperTableCell> choices = rowToChoices.get(row);
    HyperTableCell choice;
    
    if ((hasChanged(row) == false) && (force == false))
      return choices;
    
    choices.clear();
    choices.add(new HyperTableCell(-2, "", db.getSubjType(relType)));
    
    HDT_Base curObj;
    
    if (trackObjByRow)
    {
      if (rowToObj.containsKey(row) == false) return choices;
      curObj = rowToObj.get(row);
    }
    else
    {
      if (obj == null) return choices;
      curObj = obj;
    }    
    
    boolean noneYet = true;
    for (HDT_Base subj : db.getSubjectList(relType, curObj))
    {
      if (noneYet)
      {
        choices.clear();
        noneYet = false;  
      }
      
      if (nameOnly)
        choice = new HyperTableCell(subj.getID(), subj.name(), subj.getType());
      else if (subj.getType() == hdtWork)
        choice = new HyperTableCell(subj.getID(), subj.getCBText(), subj.getType(), hsmWork);
      else
        choice = new HyperTableCell(subj.getID(), subj.getCBText(), subj.getType());
      
      int ndx = Collections.binarySearch(choices, choice);
      
      if (ndx < 0)
        ndx = (ndx + 1) * -1;

      choices.add(ndx, choice);     
    }
    
    if (noneYet) choices.clear();
    choices.add(new HyperTableCell(-2, "", db.getSubjType(relType))); // This is -2 instead of -1 to prevent an IndexOutOfBoundsException (I have no idea why the latter occurs)
  
    rowToChanged.put(row, false);
    return choices;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    if (row == null) row = dummyRow;
    
    List<HyperTableCell> choices = populate(row, false);
    
    if (choices.contains(cell)) return cell.clone();
    
    return null;
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
    if (row == null) row = dummyRow;
    
    rowToChanged.put(row, true);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public void clear()
  {
    rowToChanged.clear(); 
    rowToChoices.clear();
    
    if (trackObjByRow)
      rowToObj.clear();
    else
      obj = null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value)
  {
    if (row == null) row = dummyRow;
    
    HyperTableCell cell = new HyperTableCell(id, value, db.getSubjType(relType));
    
    if (rowToChoices.containsKey(row) == false)
      rowToChoices.put(row, new ArrayList<>());

    rowToChoices.get(row).add(cell);
    return cell;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
