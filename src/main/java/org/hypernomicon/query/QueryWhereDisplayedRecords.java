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

  @Override protected boolean evalEmptyNotEmpty(HDT_RecordWithMainText rec, boolean trueMeansEmpty)
  {
    return trueMeansEmpty == rec.displayItemsStream().noneMatch(dispItem -> dispItem.type == DisplayItemType.diRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean evalIncludesExcludes(HDT_RecordWithMainText recordWMT, HDT_Record specifiedRecord, boolean includes)
  {
    if (specifiedRecord.hasMainText() == false)
      return false;

    HDT_RecordWithMainText specifiedRecordWMT = (HDT_RecordWithMainText) specifiedRecord;

    return includes == recordWMT.displayItemsStream()

      .filter(displayItem -> displayItem.type == DisplayItemType.diRecord)
      .anyMatch(displayItem -> displayItem.record.getMainText() == specifiedRecordWMT.getMainText());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
