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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class TagItemPopulator extends Populator
{
  private final Set<Tag> tags;
  private final HDT_RecordType recordType;
  final List<HyperTableCell> choices;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public TagItemPopulator(HDT_RecordType recordType)
  {
    this.recordType = recordType;
    tags = db.getTagsByRecordType(recordType);
        
    tags.remove(Tag.tagHub);
    
    choices = new ArrayList<>();
  }

//---------------------------------------------------------------------------
  
  @Override public CellValueType getValueType()                    { return cvtTagItem; }
  @Override public HDT_RecordType getRecordType(HyperTableRow row) { return recordType; }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    if ((force == false) && (choices.isEmpty() == false)) return choices;

    choices.clear();
    
    tags.forEach(tag -> choices.add(new HyperTableCell(tag.getNum(), db.getTagHeader(tag), recordType)));

    choices.sort((c1, c2) -> c1.getText().compareTo(c2.getText()));
    
    return choices;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    if (choices.isEmpty()) populate(row, false);
    
    for (HyperTableCell choice : choices)
      if (HyperTableCell.getCellID(choice) == HyperTableCell.getCellID(cell))
        return choice;
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
