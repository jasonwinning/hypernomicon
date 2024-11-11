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

package org.hypernomicon.query.ui;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.AbstractRow;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.CellSortMethod.*;

//---------------------------------------------------------------------------

public final class ResultRow extends AbstractRow<HDT_Record, ResultRow> implements HyperTableCell
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Record record;
  private final String cbText;

//---------------------------------------------------------------------------

  ResultRow(HDT_Record record)    { this.record = record; this.cbText = "";     }
  public ResultRow(String cbText) { this.record = null;   this.cbText = cbText; }

//---------------------------------------------------------------------------

  String getRecordIDStr()   { return record == null ? "" : String.valueOf(record.getID()); }
  String getRecordName()    { return record == null ? "" : (record.getType() == hdtPerson ? record.listName() : record.name()); }
  String getSearchKey()     { return record == null ? "" : record.getSearchKey(); }
  String getSortKey()       { return record == null ? "" : record.getSortKey(); }
  public String getCBText() { return record == null ? cbText : record.listName(); }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) record; }

  @Override public int compareTo(HyperTableCell otherCell) { return HyperTableCell.compareCells(this, otherCell, smStandard); }
  @Override public int getID()                             { return getRecordID(); }
  @Override public String getText()                        { return getCBText(); }
  @Override public String getImgRelPath()                  { return null; }
  @Override public HyperTableCell getCopyWithID(int newID) { throw new UnsupportedOperationException("copy"); }
  @Override public boolean getSortToBottom()               { return false; }

  @Override public ResultRow clone()
  { try { return (ResultRow) super.clone(); } catch (CloneNotSupportedException e) { throw new AssertionError(e); }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getRecordTypeStr()
  {
    RecordType type = getRecordType();
    return type == hdtNone ? "" : getTypeName(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
