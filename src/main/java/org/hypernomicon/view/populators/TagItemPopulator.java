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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class TagItemPopulator extends Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class TagItemCell extends GenericNonRecordHTC
  {
    private TagItemCell(Tag tag, RecordType recordType)
    {
      super(tag.num, tag.header, recordType);
    }
  }

//---------------------------------------------------------------------------

  private final Set<Tag> tags;
  private final RecordType recordType;
  private final List<HyperTableCell> choices;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public TagItemPopulator(RecordType recordType)
  {
    this.recordType = recordType;
    tags = db.getTagsByRecordType(recordType, true);

    removeAll(tags, tagDisplayRecord, tagKeyWork, tagPictureCrop, tagSpokeRecord);

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
      HyperTableCell cell = new TagItemCell(tag, recordType);
      addToSortedList(choices, cell, Comparator.comparing(_cell -> HyperTableCell.getCellText(_cell).toLowerCase()));
    });

    return choices;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
