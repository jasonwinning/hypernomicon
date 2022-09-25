/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public abstract class Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class SimplePopulator extends Populator
  {
  //---------------------------------------------------------------------------

    private final CellValueType cellValueType;
    private final List<HyperTableCell> choices;

  //---------------------------------------------------------------------------

    private SimplePopulator(CellValueType cellValueType, List<HyperTableCell> choices)
    {
      this.cellValueType = cellValueType;
      this.choices = choices;
    }

  //---------------------------------------------------------------------------

    @Override public List<HyperTableCell> populate(HyperTableRow row, boolean force) { return choices; }
    @Override public CellValueType getValueType()                                    { return cellValueType; }
    @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)    { return equalMatch(row, cell); }

  //---------------------------------------------------------------------------

    @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
    {
      return nullSwitch(super.getChoiceByID(row, id), HyperTableCell.blankCell);
    }

  //---------------------------------------------------------------------------

  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static final HyperTableRow dummyRow = new HyperTableRow(null, null);

  public enum DisplayKind
  {
    name, listName, cbText
  }

  public enum CellValueType
  {
    cvtVaries,   cvtQuery,       cvtQueryType,  cvtRecordType,
    cvtRecord,   cvtConnective,  cvtBoolean,    cvtTernary,
    cvtOperand,  cvtTagItem,     cvtRelation,   cvtBibEntry,
    cvtBibField, cvtSrchBtnPreset
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract List<HyperTableCell> populate(HyperTableRow row, boolean force);
  public abstract CellValueType getValueType();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  public boolean hasChanged(HyperTableRow row)                           { return true; }

  @SuppressWarnings("unused")
  public void setChanged(HyperTableRow row)                              { return; }

  public void clear()                                                    { return; }

  @SuppressWarnings("unused")
  public RecordType getRecordType(HyperTableRow row)                     { return hdtNone; }

  @SuppressWarnings("unused")
  public HyperTableCell addEntry(HyperTableRow row, int id, String text) { messageDialog("Internal error #90129", mtError); return null; }

  public HyperTableCell addEntry(HyperTableRow row, String text)         { return addEntry(row, -1, text); }

  @SuppressWarnings("unused")
  public CellValueType getValueType(HyperTableRow row)                   { return getValueType(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    return getChoiceByID(row, HyperTableCell.getCellID(cell));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  HyperTableCell equalMatch(HyperTableRow row, HyperTableCell cell)
  {
    return populate(nullSwitch(row, dummyRow), false).contains(cell) ? cell : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    return findFirst(populate(nullSwitch(row, dummyRow), false), cell -> HyperTableCell.getCellID(cell) == id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Populator create(CellValueType cellValueType, HyperTableCell... cells)
  {
    return new SimplePopulator(cellValueType, Arrays.asList(cells));
  }

  public static Populator create(CellValueType cellValueType, List<HyperTableCell> cells)
  {
    return new SimplePopulator(cellValueType, cells);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
