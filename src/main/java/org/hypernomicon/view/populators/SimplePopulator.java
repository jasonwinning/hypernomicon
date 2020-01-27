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

import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class SimplePopulator extends Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final CellValueType cellValueType;
  private final List<HyperTableCell> choices;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  SimplePopulator(CellValueType cellValueType, List<HyperTableCell> choices)
  {
    this.cellValueType = cellValueType;
    this.choices = choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force) { return choices; }
  @Override public CellValueType getValueType()                                    { return cellValueType; }
  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)    { return equalMatch(row, cell); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    return nullSwitch(super.getChoiceByID(row, id), HyperTableCell.blankCell);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
