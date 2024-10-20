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

package org.hypernomicon.view.cellValues;

import static org.hypernomicon.model.records.RecordType.*;

import java.util.Objects;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.PageRange;

//---------------------------------------------------------------------------

public class PageRangeHTC extends HyperTableCell
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int id;
  private final RecordType recordType;
  final PageRange pageRange;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PageRangeHTC(HDT_Record record, String pages)
  {
    super(false);

    boolean isEmpty = HDT_Record.isEmpty(record);

    this.id = isEmpty ? -1 : record.getID();
    this.recordType = isEmpty ? hdtNone : record.getType();
    this.pageRange = new PageRange(pages);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getID()                { return id; }
  @Override public String getText()           { return pageRange.toString(); }
  @Override public RecordType getRecordType() { return recordType; }
  @Override public String getImgRelPath()     { return ""; }
  @Override public PageRangeHTC clone()       { return (PageRangeHTC) super.clone(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public PageRangeHTC getCopyWithID(int newID)
  {
    PageRangeHTC newCell = clone();
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
    result = prime * result + pageRange.hashCode();
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

    PageRangeHTC other = (PageRangeHTC) obj;

    if (getCellType(this) != getCellType(other))
    {
      if ((getCellType(this) == hdtAuxiliary) || (getCellType(other) == hdtAuxiliary))
        return false;

      if ((id < 0) && (other.getID() < 0))
        return Objects.equals(pageRange, other.pageRange);

      return false;
    }

    if (((id >= 0) || (other.id >= 0)) && (id != other.getID())) return false;

    return Objects.equals(pageRange, other.pageRange);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
