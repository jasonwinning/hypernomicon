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

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class CustomRecordPopulator extends Populator
{
  
//---------------------------------------------------------------------------
  
  @FunctionalInterface
  public static interface PopulateHandler { public List<HDT_Base> handle(HyperTableRow row, boolean force); }
  
//---------------------------------------------------------------------------
  
  private HDT_RecordType recordType;
  private PopulateHandler handler;

//---------------------------------------------------------------------------
  
  public CustomRecordPopulator(HDT_RecordType recordType, PopulateHandler handler)
  {
    this.recordType = recordType;
    this.handler = handler;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  @Override public CellValueType getValueType()                                 { return cvtRecord; }
  @Override public HDT_RecordType getRecordType(HyperTableRow row)              { return recordType; }
  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell) { return populate(row, false).contains(cell) ? cell.clone() : null; }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)  
  { 
    List<HDT_Base> recordList = handler.handle(row, force);
    List<HyperTableCell> cellList = new ArrayList<>();
    
    for (HDT_Base record : recordList)
      cellList.add(new HyperTableCell(record.getID(), record.getCBText(), recordType));

    return cellList;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
    
}
