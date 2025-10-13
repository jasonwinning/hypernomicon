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

package org.hypernomicon.query;

import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText.DisplayItemType;
import org.hypernomicon.view.mainText.MainTextCtrlr;

//---------------------------------------------------------------------------

public class QueryWhereDisplayedRecords extends QueryWhereKeyWorks
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryWhereDisplayedRecords(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------

  @Override protected Stream<RecordType> operandRecordTypesStream() { return MainTextCtrlr.displayedTypesStream(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean evalInclude(HDT_RecordWithMainText recordWMT, HDT_Record specifiedRecord)
  {
    if (HDT_Record.isEmpty(specifiedRecord, false) || (specifiedRecord.hasMainText() == false))
      return false;

    HDT_RecordWithMainText specifiedRecordWMT = (HDT_RecordWithMainText)specifiedRecord;

    return recordWMT.getMainText().getDisplayItemsUnmod().stream()

      .filter(displayItem-> displayItem.type == DisplayItemType.diRecord)
      .anyMatch(displayItem -> displayItem.record.getMainText() == specifiedRecordWMT.getMainText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean evalExclude(HDT_RecordWithMainText recordWMT, HDT_Record specifiedRecord)
  {
    if (HDT_Record.isEmpty(specifiedRecord, false) || (specifiedRecord.hasMainText() == false))
      return false;

    HDT_RecordWithMainText specifiedRecordWMT = (HDT_RecordWithMainText)specifiedRecord;

    return recordWMT.getMainText().getDisplayItemsUnmod().stream()

      .filter(displayItem-> displayItem.type == DisplayItemType.diRecord)
      .noneMatch(displayItem -> displayItem.record.getMainText() == specifiedRecordWMT.getMainText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean evalEmpty(HDT_RecordWithMainText recordWMT)
  {
    return recordWMT.getMainText().getDisplayItemsUnmod().stream().noneMatch(displayItem -> displayItem.type == DisplayItemType.diRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean evalNotEmpty(HDT_RecordWithMainText recordWMT)
  {
    return recordWMT.getMainText().getDisplayItemsUnmod().stream().anyMatch(displayItem -> displayItem.type == DisplayItemType.diRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
