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

package org.hypernomicon.query;

import static org.hypernomicon.view.cellValues.HyperTableCell.*;

import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.model.unities.MainText.DisplayItemType;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextCtrlr;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryWhereDisplayedRecords extends QueryWhereKeyWorks
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryWhereDisplayedRecords(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------

  @Override protected Stream<RecordType> operandRecordTypesStream() { return MainTextCtrlr.displayedTypesStream(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    if (record.hasMainText() == false)
      return false;

    HDT_RecordWithMainText recordWMT = (HDT_RecordWithMainText)record;
    int operandID = getCellID(op1);

    switch (operandID)
    {
      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :

        return recordWMT.getMainText().getDisplayItemsUnmod().stream().noneMatch(displayItem -> displayItem.type == DisplayItemType.diRecord)
          == (operandID == IS_EMPTY_OPERAND_ID);

      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

        HDT_Record specifiedRecord = HyperTableCell.getRecord(op3);
        if (HDT_Record.isEmpty(specifiedRecord) || (specifiedRecord.hasMainText() == false))
          return false;

        HDT_RecordWithMainText specifiedRecordWMT = (HDT_RecordWithMainText)specifiedRecord;

        return recordWMT.getMainText().getDisplayItemsUnmod().stream()

          .filter(displayItem-> displayItem.type == DisplayItemType.diRecord)
          .anyMatch(displayItem -> displayItem.record.getMainText() == specifiedRecordWMT.getMainText()) == (operandID == EQUAL_TO_OPERAND_ID);
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
