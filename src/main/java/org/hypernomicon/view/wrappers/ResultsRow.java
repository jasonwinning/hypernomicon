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

package org.hypernomicon.view.wrappers;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordBase.HDT_DateType;
import org.hypernomicon.model.records.HDT_RecordType;
import org.hypernomicon.view.wrappers.ResultsTable.ResultCellValue;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordBase.HDT_DateType.*;

import java.time.Instant;

public final class ResultsRow extends AbstractRow<HDT_Record, ResultsRow>
{
  private final HDT_Record record;
  private final String cbText;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ResultsRow(HDT_Record record) { this.record = record; cbText = ""; }
  public ResultsRow(String cbText)     { record = null;   this.cbText = cbText; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getTagText(Tag tag) { return record == null ? "" : record.getResultTextForTag(tag); }
  String getRecordIDStr()    { return record == null ? "" : String.valueOf(record.getID()); }
  String getRecordName()     { return record == null ? "" : record.listName(); }
  String getSearchKey()      { return record == null ? "" : record.getSearchKey(); }
  String getSortKey()        { return record == null ? "" : record.getSortKey(); }
  public String getCBText()  { return record == null ? cbText : record.listName(); }

  @SuppressWarnings("unchecked")
  @Override public <HDT_T extends HDT_Record> HDT_T getRecord() { return (HDT_T) record; }

  String getRecordTypeStr()
  {
    HDT_RecordType type = getRecordType();
    return type == hdtNone ? "" : db.getTypeName(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ResultCellValue<Instant> getCreationDateCellValue() { return getDateCellValue(dateTypeCreation); }
  ResultCellValue<Instant> getModifiedDateCellValue() { return getDateCellValue(dateTypeModified); }
  ResultCellValue<Instant> getViewDateCellValue()     { return getDateCellValue(dateTypeView); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ResultCellValue<Instant> getDateCellValue(HDT_DateType dateType)
  {
    if ((record == null) || (record.getType() == hdtNone))
      return new ResultCellValue<>("", Instant.MIN);

    Instant i = null;

    switch (dateType)
    {
      case dateTypeCreation: i = record.getCreationDate(); break;
      case dateTypeModified: i = record.getModifiedDate(); break;
      case dateTypeView:     i = record.getViewDate();     break;

      default: break;
    }

    return nullSwitch(i, new ResultCellValue<>("", Instant.MIN), j -> new ResultCellValue<>(dateTimeToUserReadableStr(j), j));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
