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

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------  

public class VariablePopulator extends Populator
{
  private HashMap<HyperTableRow, Populator> rowToPop = new HashMap<>();
  private HashMap<HyperTableRow, Boolean> rowToRestricted = new HashMap<>();
  
//---------------------------------------------------------------------------  

  public void setPopulator(HyperTableRow row, Populator populator) { rowToPop.put(row, populator); rowToRestricted.put(row, true); }
  public void setRestricted(HyperTableRow row, boolean restrict)   { rowToRestricted.put(row, restrict); }
  public boolean getRestricted(HyperTableRow row)                  { return rowToRestricted.getOrDefault(row, true); }
  public Populator getPopulator(HyperTableRow row)                 { return rowToPop.get(row); }

  @Override public CellValueType getValueType()                                     { return cvtVaries; }
  @Override public void clear()                                                     { rowToPop.clear(); rowToRestricted.clear(); }
  @Override public HyperTableCell addEntry(HyperTableRow row, int id, String value) { return rowToPop.get(row).addEntry(row, id, value); }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    Populator pop = rowToPop.get(row);
    return pop == null ? new ArrayList<>() : pop.populate(row, force);
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    Populator pop = rowToPop.get(row);
    return pop == null ? null : pop.match(row, cell); 
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public boolean hasChanged(HyperTableRow row)
  {
    Populator pop = rowToPop.get(row);
    return pop == null ? true : pop.hasChanged(row); 
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public void setChanged(HyperTableRow row)                                                
  { 
    Populator pop = rowToPop.get(row);
    if (pop != null) pop.setChanged(row); 

  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @Override public HDT_RecordType getRecordType(HyperTableRow row)
  {
    Populator pop = rowToPop.get(row);
    return pop == null ? hdtNone : pop.getRecordType(row); 
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

}
