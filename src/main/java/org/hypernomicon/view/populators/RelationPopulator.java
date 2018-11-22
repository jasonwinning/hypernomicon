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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class RelationPopulator extends Populator
{
  private HDT_RecordType objType = hdtNone;

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------    
  
  public RelationPopulator(HDT_RecordType objType)                 { this.objType = objType; }
  
  @Override public CellValueType getValueType()                    { return cvtRelation; }
  @Override public HDT_RecordType getRecordType(HyperTableRow row) { return objType; }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    List<HyperTableCell> cells = new ArrayList<>();
    EnumSet<RelationType> relTypes;
    
    if (objType == hdtNone)
      relTypes = EnumSet.allOf(RelationType.class);
    else
      relTypes = getRelationsForObjType(objType);
    
    relTypes.remove(rtNone);
    relTypes.remove(rtUnited);
    
    for (RelationType relType : relTypes)
      cells.add(new HyperTableCell(relType.getCode(), relType.getTitle(), objType));

    return cells;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    for (HyperTableCell choice : populate(nullSwitch(row, dummyRow), false))
      if (HyperTableCell.getCellID(choice) == HyperTableCell.getCellID(cell))
        return choice;
    
    return null;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
