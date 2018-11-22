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

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class GenericOperandPopulator extends Populator
{
  public static final int EQUAL_TO_OPERAND_ID = 1;
  public static final int NOT_EQUAL_TO_OPERAND_ID = 2;
  public static final int CONTAINS_OPERAND_ID = 3;
  public static final int DOES_NOT_CONTAIN_OPERAND_ID = 4;
  public static final int IS_EMPTY_OPERAND_ID = 5;
  public static final int IS_NOT_EMPTY_OPERAND_ID = 6;

  @Override public CellValueType getValueType() { return cvtConnective; }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return Arrays.asList(new HyperTableCell(EQUAL_TO_OPERAND_ID, "Is or includes record", hdtNone),
                         new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID, "Excludes record", hdtNone),
                         new HyperTableCell(CONTAINS_OPERAND_ID, "Contains text", hdtNone),
                         new HyperTableCell(DOES_NOT_CONTAIN_OPERAND_ID, "Doesn't contain text", hdtNone),
                         new HyperTableCell(IS_EMPTY_OPERAND_ID, "Is empty", hdtNone),
                         new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID, "Is not empty", hdtNone));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    for (HyperTableCell choice : populate(dummyRow, false))
      if (HyperTableCell.getCellID(choice) == HyperTableCell.getCellID(cell))
        return choice;
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public HyperTableCell getChoice(int id)
  {
    for (HyperTableCell cell : populate(dummyRow, false))
      if (HyperTableCell.getCellID(cell) == id) 
        return cell;
    
    return new HyperTableCell(-1, "", hdtNone);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
