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

package org.hypernomicon.view.populators;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class TernaryPopulator extends Populator
{
  public static final int TRUE_TERNARY_ID  = 1,
                          FALSE_TERNARY_ID = 2,
                          UNSET_TERNARY_ID = 3;

  private static final HyperTableCell trueCell  = new HyperTableCell(TRUE_TERNARY_ID , "True" , hdtNone),
                                      falseCell = new HyperTableCell(FALSE_TERNARY_ID, "False", hdtNone),
                                      unsetCell = new HyperTableCell(UNSET_TERNARY_ID, "Unset", hdtNone);

  @Override public CellValueType getValueType() { return cvtTernary; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return Arrays.asList(unsetCell, trueCell, falseCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
