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

import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class RecordHTC extends AbstractHTC
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int id;

  private final String text;
  private final RecordType recordType;

  @Override public int getID()                { return id; }
  @Override public String getText()           { return text; }
  @Override public RecordType getRecordType() { return recordType; }
  @Override public boolean isEmpty()          { return GenericNonRecordHTC.blankCell.equals(this); }
  @Override public RecordHTC clone()          { return (RecordHTC) super.clone(); }

//---------------------------------------------------------------------------

  public RecordHTC(int id           , String text, RecordType recordType)    { this(id            , text, recordType, false); }
  public RecordHTC(HDT_Record record, String text                       )    { this(record.getID(), text, record.getType()); }

  protected RecordHTC(int id, String text, RecordType recordType, boolean sortToBottom)
  {
    super(sortToBottom);

    this.id = id < 1 ? -1 : id;
    this.text = safeStr(text);
    this.recordType = recordType == null ? RecordType.hdtNone : recordType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordHTC getCopyWithID(int newID)
  {
    RecordHTC newCell = clone();
    newCell.id = newID;
    return newCell;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
