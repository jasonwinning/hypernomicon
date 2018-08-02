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
import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class BooleanPopulator extends Populator
{
  public static final int TRUE_BOOLEAN_ID = 1;
  public static final int FALSE_BOOLEAN_ID = 2;

  @Override public CellValueType getValueType() { return cvtBoolean; }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    List<HyperTableCell> choices = new ArrayList<>();
    
    choices.add(new HyperTableCell(TRUE_BOOLEAN_ID, "True", hdtNone));
    choices.add(new HyperTableCell(FALSE_BOOLEAN_ID, "False", hdtNone));
    
    return choices;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    switch (HyperTableCell.getCellID(cell))
    {
      case TRUE_BOOLEAN_ID  : return new HyperTableCell(TRUE_BOOLEAN_ID, "True", hdtNone);
      case FALSE_BOOLEAN_ID : return new HyperTableCell(FALSE_BOOLEAN_ID, "False", hdtNone);
      default :               return null;
    }
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
