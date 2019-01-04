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
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.List;

import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public abstract class Populator
{
  public static final HyperTableRow dummyRow = new HyperTableRow(null, null);

  public static enum CellValueType
  {
    cvtVaries,       cvtQuery,        cvtQueryType,      cvtRecordType,
    cvtRecord,       cvtConnective,   cvtBoolean,        cvtTernary,
    cvtOperand,      cvtTagItem,      cvtRelation
  }

  @FunctionalInterface public interface PopulatorFilter
  {
    public boolean filter(HDT_Base record);
  }

  public abstract HyperTableCell match(HyperTableRow row, HyperTableCell cell);
  public abstract List<HyperTableCell> populate(HyperTableRow row, boolean force);
  public abstract CellValueType getValueType();

  @SuppressWarnings("unused")
  public boolean hasChanged(HyperTableRow row)                            { return true; }

  @SuppressWarnings("unused")
  public void setChanged(HyperTableRow row)                               { return; }

  public void clear()                                                     { return; }

  @SuppressWarnings("unused")
  public HDT_RecordType getRecordType(HyperTableRow row)                  { return hdtNone; }

  @SuppressWarnings("unused")
  public HyperTableCell addEntry(HyperTableRow row, int id, String value) { messageDialog("Internal error #90129", mtError); return null; }
}
