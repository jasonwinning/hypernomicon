/*
 * Copyright 2015-2026 Jason Winning
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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.model.relations.RelationSet.*;

import java.util.*;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class RelationPopulator extends Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class RelationCell extends GenericNonRecordHTC
  {
    private RelationCell(RelationType relType, RecordType objType)
    {
      super(relType.getCode(), relType.getTitle(), objType);
    }
  }

//---------------------------------------------------------------------------

  private final RecordType objType;

//---------------------------------------------------------------------------

  public RelationPopulator(RecordType objType)                 { this.objType = objType; }

  @Override public CellValueType getValueType()                { return cvtRelation; }
  @Override public RecordType getRecordType(HyperTableRow row) { return objType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    List<HyperTableCell> cells = new ArrayList<>();
    EnumSet<RelationType> relTypes = objType == hdtNone ? EnumSet.allOf(RelationType.class) : getRelationsForObjType(objType, false);

    removeAll(relTypes, rtNone, rtUnited, rtKeyWork);

    relTypes.forEach(relType ->
    {
      HyperTableCell cell = new RelationCell(relType, objType);
      addToSortedList(cells, cell, Comparator.comparing(HyperTableCell::getCellText));
    });

    return cells;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
