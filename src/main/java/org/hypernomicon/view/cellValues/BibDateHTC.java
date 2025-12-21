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

package org.hypernomicon.view.cellValues;

import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;

//---------------------------------------------------------------------------

public class BibDateHTC extends AbstractHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int id;
  private final RecordType recordType;
  private final BibliographicDate bibDate;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibDateHTC(HDT_Record record, BibliographicDate bibDate)
  {
    super(false);

    boolean isEmpty = HDT_Record.isEmpty(record, false);

    this.id = isEmpty ? -1 : record.getID();
    this.recordType = isEmpty ? hdtNone : record.getType();
    this.bibDate = bibDate;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getID()                { return id; }
  @Override public String getText()           { return bibDate.displayToUser(); }
  @Override public RecordType getRecordType() { return recordType; }
  @Override public BibDateHTC clone()         { return (BibDateHTC) super.clone(); }
  @Override public boolean isEmpty()          { return (id == -1) && (recordType == hdtNone) && BibliographicDate.isEmpty(bibDate); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibDateHTC getCopyWithID(int newID)
  {
    BibDateHTC newCell = clone();
    newCell.id = newID;
    return newCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + id;
    result = prime * result + (BibliographicDate.isEmpty(bibDate) ? 0 : bibDate.hashCode());
    result = prime * result + (recordType == null ? 0 : recordType.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    BibDateHTC other = (BibDateHTC) obj;

    if (getCellType(this) != getCellType(other))
    {
      if ((getCellType(this) == hdtAuxiliary) || (getCellType(other) == hdtAuxiliary))
        return false;

      if ((id < 0) && (other.getID() < 0))
        return BibliographicDate.isEmpty(bibDate) && BibliographicDate.isEmpty(other.bibDate);

      return false;
    }

    if (((id >= 0) || (other.id >= 0)) && (id != other.getID())) return false;

    if (BibliographicDate.isEmpty(bibDate) && BibliographicDate.isEmpty(other.bibDate))
      return true;

    return bibDate.equals(other.bibDate);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Assumes at least one of the cells is a BibDateHTC
   * @param cell1 First cell to be compared
   * @param cell2 Second cell to be compared
   * @return Usual return value for compare function
   */
  static int compareCells(HyperTableCell cell1, HyperTableCell cell2)
  {
    return cell1 instanceof BibDateHTC bibDateHTC1 ?
      (cell2 instanceof BibDateHTC bibDateHTC2 ?
         bibDateHTC1.bibDate.compareTo(bibDateHTC2.bibDate)
       : 1)
    : -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
