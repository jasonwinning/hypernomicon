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

import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class CustomPopulator extends RecordPopulator
{

//---------------------------------------------------------------------------

  @FunctionalInterface public interface PopulateHandler { Stream<HyperTableCell> handle(HyperTableRow row, boolean force); }

  @FunctionalInterface public interface RecordPopulateHandler { Stream<? extends HDT_Record> handle(HyperTableRow row, boolean force); }

//---------------------------------------------------------------------------

  private final RecordType recordType;
  private final PopulateHandler handler;
  private final CellValueType cellValueType;

//---------------------------------------------------------------------------

  public CustomPopulator(CellValueType cellValueType, PopulateHandler handler)
  {
    super(null, DisplayKind.cbText);

    this.recordType = RecordType.hdtNone;
    this.handler = handler;
    this.cellValueType = cellValueType;
  }

  public CustomPopulator(RecordType recordType, RecordPopulateHandler handler)
  {
    super(null, DisplayKind.cbText);

    this.recordType = recordType;
    this.handler = (row, force) -> handler.handle(row, force).map(this::generateCell);
    this.cellValueType = cvtRecord;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public CellValueType getValueType()                                 { return cellValueType; }
  @Override public RecordType getRecordType(HyperTableRow row)                  { return recordType; }
  @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell) { return matchFromList(row, cell); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force)
  {
    return handler.handle(row, force).collect(Collectors.toList());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
