/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class TagItemPopulator extends Populator
{
  private final Set<Tag> tags;
  private final RecordType recordType;
  private final List<HyperTableCell> choices;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TagItemPopulator(RecordType recordType)
  {
    this.recordType = recordType;
    tags = db.getTagsByRecordType(recordType, true);

    removeAll(tags, tagDisplayRecord, tagKeyWork);

    choices = new ArrayList<>();
  }

//---------------------------------------------------------------------------

  @Override public CellValueType getValueType()                { return cvtTagItem; }
  @Override public RecordType getRecordType(HyperTableRow row) { return recordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if ((force == false) && (choices.isEmpty() == false)) return choices;

    choices.clear();

    tags.forEach(tag ->
    {
      HyperTableCell cell = new HyperTableCell(tag.num, tag.header, recordType);
      addToSortedList(choices, cell, Comparator.comparing(cel -> cel.text));
    });

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
