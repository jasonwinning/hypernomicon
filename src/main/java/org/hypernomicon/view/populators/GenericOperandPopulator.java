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

package org.hypernomicon.view.populators;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class GenericOperandPopulator extends Populator
{
  public static final int EQUAL_TO_OPERAND_ID         = 1,
                          NOT_EQUAL_TO_OPERAND_ID     = 2,
                          CONTAINS_OPERAND_ID         = 3,
                          DOES_NOT_CONTAIN_OPERAND_ID = 4,
                          IS_EMPTY_OPERAND_ID         = 5,
                          IS_NOT_EMPTY_OPERAND_ID     = 6;

  @Override public CellValueType getValueType() { return cvtConnective; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return Arrays.asList(new HyperTableCell(EQUAL_TO_OPERAND_ID        , "Is or includes record", hdtNone),
                         new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID    , "Excludes record"      , hdtNone),
                         new HyperTableCell(CONTAINS_OPERAND_ID        , "Contains text"        , hdtNone),
                         new HyperTableCell(DOES_NOT_CONTAIN_OPERAND_ID, "Doesn't contain text" , hdtNone),
                         new HyperTableCell(IS_EMPTY_OPERAND_ID        , "Is empty"             , hdtNone),
                         new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID    , "Is not empty"         , hdtNone));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    return nullSwitch(super.getChoiceByID(row, id), HyperTableCell.blankCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
