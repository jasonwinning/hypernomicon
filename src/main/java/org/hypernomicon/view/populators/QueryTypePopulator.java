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
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.queryEngines.QueryEngine.QueryType.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryTypePopulator extends Populator
{
  @Override public CellValueType getValueType() { return CellValueType.cvtQueryType; }

  //---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return Arrays.asList(new HyperTableCell(qtAllRecords.getCode(), "Any records", hdtNone),
                         new HyperTableCell(qtPersons.getCode(), "Person records", hdtPerson),
                         new HyperTableCell(qtInstitutions.getCode(), "Institution records", hdtInstitution),
                         new HyperTableCell(qtWorks.getCode(), "Work records", hdtWork),
                         new HyperTableCell(qtFiles.getCode(), "File records", hdtMiscFile),
                         new HyperTableCell(qtDebates.getCode(), "Problem/debate records", hdtDebate),
                         new HyperTableCell(qtPositions.getCode(), "Position records", hdtPosition),
                         new HyperTableCell(qtArguments.getCode(), "Argument records", hdtArgument),
                         new HyperTableCell(qtNotes.getCode(), "Note records", hdtNote),
                         new HyperTableCell(qtConcepts.getCode(), "Concept records", hdtConcept),
                         new HyperTableCell(qtInvestigations.getCode(), "Investigation records", hdtInvestigation),
                         new HyperTableCell(qtReport.getCode(), "Report", hdtNone));
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
