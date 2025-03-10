/*
 * Copyright 2015-2025 Jason Winning
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

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.view.cellValues.BibEntryHTC;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class BibEntryPopulator extends Populator
{

//---------------------------------------------------------------------------

  @FunctionalInterface public interface PopulateHandler { List<? extends BibEntry<?, ?>> handle(HyperTableRow row, boolean force); }

//---------------------------------------------------------------------------

  private final PopulateHandler handler;

//---------------------------------------------------------------------------

  public BibEntryPopulator(PopulateHandler handler)
  {
    this.handler = handler;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public CellValueType getValueType() { return cvtBibEntry; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    List<HyperTableCell> choices = handler.handle(row, force).stream()
      .map(BibEntryHTC::new)
      .collect(Collectors.toCollection(ArrayList::new));

    choices.add(GenericNonRecordHTC.blankCell);
    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
