/*
 * Copyright 2015-2022 Jason Winning
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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.cvtOperand;
import static org.hypernomicon.view.wrappers.HyperTableCell.getCellID;

import java.util.EnumSet;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.RecordByTypePopulator;
import org.hypernomicon.view.populators.RecordTypePopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryWhereKeyWorks extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryWhereKeyWorks(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    vp1.setPopulator(row, Populator.create(cvtOperand,

        new HyperTableCell(EQUAL_TO_OPERAND_ID    , "Include record", hdtNone),
        new HyperTableCell(NOT_EQUAL_TO_OPERAND_ID, "Exclude record", hdtNone),
        new HyperTableCell(IS_EMPTY_OPERAND_ID    , "Empty"         , hdtNone),
        new HyperTableCell(IS_NOT_EMPTY_OPERAND_ID, "Not empty"     , hdtNone)));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected Set<RecordType> operandRecordTypes() { return EnumSet.of(hdtWork, hdtMiscFile); }

//---------------------------------------------------------------------------

  @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    clearOperands(row, 2);

    if ((op1.getID() != EQUAL_TO_OPERAND_ID) && (op1.getID() != NOT_EQUAL_TO_OPERAND_ID))
      vp2.setRestricted(row, false);
    else
    {
      vp2.setPopulator(row, new RecordTypePopulator(operandRecordTypes()));
      vp3.setPopulator(row, new RecordByTypePopulator());
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    if ((op1.getID() == EQUAL_TO_OPERAND_ID) || (op1.getID() == NOT_EQUAL_TO_OPERAND_ID))
      return recordByTypeOpChange(op2, row, vp3);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
  {
    if (record.hasMainText() == false)
      return false;

    HDT_RecordWithMainText recordWMT = (HDT_RecordWithMainText)record;
    int operandID = getCellID(op1);

    switch (operandID)
    {
      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :

        return recordWMT.getMainText().getKeyWorksUnmod().isEmpty() == (operandID == IS_EMPTY_OPERAND_ID);

      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

        HDT_Record specifiedRecord = HyperTableCell.getRecord(op3);
        if (HDT_Record.isEmpty(specifiedRecord))
          return false;

        return recordWMT.getMainText().getKeyWorksUnmod().stream().anyMatch(keyWork -> keyWork.getRecord() == specifiedRecord) ==
            (operandID == EQUAL_TO_OPERAND_ID);
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    if (opNum < 2)
      return true;

    switch (op1.getID())
    {
      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :
        return false;

      default :
        return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean show(QueryType queryType, RecordType recordType)
  {
    return (queryType == qtAllRecords) || HDT_RecordWithMainText.class.isAssignableFrom(recordType.getRecordClass());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
