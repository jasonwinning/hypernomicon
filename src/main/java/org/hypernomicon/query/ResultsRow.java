/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.query;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.query.ResultsTable.ResultCellValue;
import org.hypernomicon.view.wrappers.AbstractRow;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.time.Instant;

public final class ResultsRow extends AbstractRow<HDT_Record, ResultsRow>
{
  private final HDT_Record record;
  private final String cbText;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ResultsRow(HDT_Record record)    { this.record = record; cbText = ""; }
  public ResultsRow(String cbText) { record = null;   this.cbText = cbText; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getRecordIDStr()   { return record == null ? "" : String.valueOf(record.getID()); }
  String getRecordName()    { return record == null ? "" : (record.getType() == hdtPerson ? record.listName() : record.name()); }
  String getSearchKey()     { return record == null ? "" : record.getSearchKey(); }
  String getSortKey()       { return record == null ? "" : record.getSortKey(); }
  public String getCBText() { return record == null ? cbText : record.listName(); }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) record; }

  String getRecordTypeStr()
  {
    HDT_RecordType type = getRecordType();
    return type == hdtNone ? "" : db.getTypeName(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ResultCellValue<Instant> getDateCellValue(Tag dateTag)
  {
    Instant i = null;

    if ((record != null) && (record.getType() != hdtNone)) switch (dateTag)
    {
      case tagCreationDate : i = record.getCreationDate(); break;
      case tagModifiedDate : i = record.getModifiedDate(); break;
      case tagViewDate     : i = record.getViewDate    (); break;
      default              :                               break;
    }

    return i == null ? new ResultCellValue<>("", Instant.MIN) : new ResultCellValue<>(dateTimeToUserReadableStr(i), i);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
