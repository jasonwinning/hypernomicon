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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

import java.util.Objects;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public abstract class AbstractHTC implements HyperTableCell
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final boolean sortToBottom;

//---------------------------------------------------------------------------

  AbstractHTC(boolean sortToBottom)
  {
    this.sortToBottom = sortToBottom;
  }

//---------------------------------------------------------------------------

  @Override public boolean getSortToBottom() { return sortToBottom; }

  @Override public AbstractHTC clone()
  { try { return (AbstractHTC) super.clone(); } catch (CloneNotSupportedException e) { throw newAssertionError(e); }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord()
  {
    int id = getID();
    if (id < 1) return null;

    RecordType type = getRecordType();
    return type == hdtNone ? null : (HDT_T)db.records(type).getByID(id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(HyperTableCell otherCell)
  {
    if (sortToBottom)
      return Integer.MAX_VALUE;

    if (otherCell.getSortToBottom())
      return Integer.MIN_VALUE + 1;

    return compareCells(this, otherCell, smStandard);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return Objects.hash(getID(), safeStr(getText()), getRecordType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if ((obj instanceof AbstractHTC) == false) return false;

    HyperTableCell other = (HyperTableCell) obj;

    if (getCellType(this) != getCellType(other))
    {
      if ((getCellType(this) == hdtAuxiliary) || (getCellType(other) == hdtAuxiliary))
        return false;

      if ((getID() < 0) && (other.getID() < 0))
        return strNullOrEmpty(getText()) && strNullOrEmpty(other.getText());

      return false;
    }

    if (((getID() >= 0) || (other.getID() >= 0)) && (getID() != other.getID())) return false;

    return safeStr(getText()).equals(safeStr(other.getText()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
