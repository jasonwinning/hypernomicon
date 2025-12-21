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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Arrays;
import java.util.List;

import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public abstract class Populator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class SimplePopulator extends Populator
  {
  //---------------------------------------------------------------------------

    private final CellValueType cellValueType;
    private final List<? extends HyperTableCell> choices;
    private final boolean idMatching;

  //---------------------------------------------------------------------------

    private SimplePopulator(CellValueType cellValueType, boolean idMatching, List<? extends HyperTableCell> choices)
    {
      this.cellValueType = cellValueType;
      this.idMatching = idMatching;
      this.choices = choices;
    }

  //---------------------------------------------------------------------------

    @Override public List<? extends HyperTableCell> populate(HyperTableRow row, boolean force) { return choices; }
    @Override public CellValueType getValueType()                                              { return cellValueType; }

  //---------------------------------------------------------------------------

    @Override public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
    {
      return idMatching ? getChoiceByID(row, HyperTableCell.getCellID(cell)) : matchFromList(row, cell);
    }

  //---------------------------------------------------------------------------

    @Override public HyperTableCell getChoiceByID(HyperTableRow row, int id)
    {
      return nullSwitch(super.getChoiceByID(row, id), GenericNonRecordHTC.blankCell);
    }

  //---------------------------------------------------------------------------

  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static final HyperTableRow dummyRow = new HyperTableRow(null, null);

  public enum DisplayKind
  {
    name, defaultCellText, defaultChoiceText, custom
  }

  public enum CellValueType
  {
    cvtVaries,  cvtQuery,   cvtQueryType, cvtRecordType, cvtRecord,    cvtFileNameComponent, cvtPageRange,
    cvtOperand, cvtTagItem, cvtRelation,  cvtBibEntry,   cvtBibField,  cvtSrchBtnPreset
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract List<? extends HyperTableCell> populate(HyperTableRow row, boolean force);
  public abstract CellValueType getValueType();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  protected boolean hasChanged(HyperTableRow row)                        { return true; }

  @SuppressWarnings("unused")
  public void setChanged(HyperTableRow row)                              { }

  public void clear()                                                    { }

  @SuppressWarnings("unused")
  public RecordType getRecordType(HyperTableRow row)                     { return hdtNone; }

  @SuppressWarnings("unused")
  public HyperTableCell addEntry(HyperTableRow row, int id, String text) { internalErrorPopup(90129); return null; }

  @SuppressWarnings("unused")
  public CellValueType getValueType(HyperTableRow row)                   { return getValueType(); }

  public final List<? extends HyperTableCell> populate(boolean force)  { return populate(dummyRow, force); }
  public final void setChanged()                                       { setChanged(dummyRow); }
  public final RecordType getRecordType()                              { return getRecordType(dummyRow); }
  public final HyperTableCell addEntry(int id, String text)            { return addEntry(dummyRow, id, text); }
  public final HyperTableCell addEntry(String text)                    { return addEntry(dummyRow, text); }
  public final HyperTableCell addEntry(HyperTableRow row, String text) { return addEntry(row, -1, text); }
  public final HyperTableCell getChoiceByID(int id)                    { return getChoiceByID(dummyRow, id); }

  protected final boolean hasChanged()                                 { return hasChanged(dummyRow); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Find or generate a choice that matches (in relevant respects for this populator) the input cell
   * @param row The table row
   * @param cell The input cell
   * @return The choice in this populator matching the input
   */
  public HyperTableCell match(HyperTableRow row, HyperTableCell cell)
  {
    return getChoiceByID(row, HyperTableCell.getCellID(cell));
  }

  /**
   * Populate the list of choices and find a choice that exactly matches (in terms of the HyperTableCell "equals" method)
   * the input cell from the list of choices
   * @param row The table row
   * @param cell The input cell
   * @return The choice in this populator matching the input
   */
  final HyperTableCell matchFromList(HyperTableRow row, HyperTableCell cell)
  {
    List<? extends HyperTableCell> list = populate(row, false);
    int ndx = list.indexOf(cell);
    return (ndx >= 0) ? list.get(ndx) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTableCell getChoiceByID(HyperTableRow row, int id)
  {
    return findFirst(populate(row, false), cell -> HyperTableCell.getCellID(cell) == id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create simple populator that matches cells based on ID, type, and text.
   * @param cellValueType Value type
   * @param cells Fixed array of cells returned by {@link #populate(HyperTableRow, boolean)}
   * @return The newly created populator
   */
  public static Populator createWithFullMatching(CellValueType cellValueType, HyperTableCell... cells)
  {
    return new SimplePopulator(cellValueType, false, Arrays.asList(cells));
  }

  /**
   * Create simple populator that matches cells based on ID, type, and text.
   * @param cellValueType Value type
   * @param cells Fixed list of cells returned by {@link #populate(HyperTableRow, boolean)}
   * @return The newly created populator
   */
  public static Populator createWithFullMatching(CellValueType cellValueType, List<? extends HyperTableCell> cells)
  {
    return new SimplePopulator(cellValueType, false, cells);
  }

  /**
   * Create simple populator that matches cells based on ID only.
   * @param cellValueType Value type
   * @param cells Fixed array of cells returned by {@link #populate(HyperTableRow, boolean)}
   * @return The newly created populator
   */
  public static Populator createWithIDMatching(CellValueType cellValueType, HyperTableCell... cells)
  {
    return new SimplePopulator(cellValueType, true, Arrays.asList(cells));
  }

  /**
   * Create simple populator that matches cells based on ID only.
   * @param cellValueType Value type
   * @param cells Fixed list of cells returned by {@link #populate(HyperTableRow, boolean)}
   * @return The newly created populator
   */
  public static Populator createWithIDMatching(CellValueType cellValueType, List<? extends HyperTableCell> cells)
  {
    return new SimplePopulator(cellValueType, true, cells);
  }

  /**
   * Creates a {@code Populator} that never supplies any values.
   * <p>
   * The {@link #populate(HyperTableRow, boolean)} method of the returned
   * instance always yields an empty list.
   * </p>
   *
   * @param cellValueType the value type associated with the populator
   * @return a populator whose {@code populate} method always returns an empty list
   */
  public static Populator emptyPopulator(CellValueType cellValueType)
  {
    return createWithIDMatching(cellValueType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static HyperTableCell addEntryToList(List<HyperTableCell> list, HyperTableCell cell)
  {
    if ((HyperTableCell.isEmpty(cell) == false) && (list.size() > 0) && HyperTableCell.isEmpty(list.getLast()))
      list.add(list.size() - 1, cell);
    else
      list.add(cell);

    return cell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
