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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.Query.ItemOperator.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.stream.Stream;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class QueryWhereKeyWorks extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryWhereKeyWorks(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalInclude(HDT_RecordWithMainText recordWMT, HDT_Record specifiedRecord)
  {
    return (HDT_Record.isEmpty(specifiedRecord, false) == false) &&
           recordWMT.getMainText().getKeyWorksUnmod().stream().anyMatch(keyWork -> keyWork.getRecord() == specifiedRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalExclude(HDT_RecordWithMainText recordWMT, HDT_Record specifiedRecord)
  {
    return (HDT_Record.isEmpty(specifiedRecord, false) == false) &&
           recordWMT.getMainText().getKeyWorksUnmod().stream().noneMatch(keyWork -> keyWork.getRecord() == specifiedRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalEmpty(HDT_RecordWithMainText recordWMT)
  {
    return recordWMT.getMainText().getKeyWorksUnmod().isEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalNotEmpty(HDT_RecordWithMainText recordWMT)
  {
    return recordWMT.getMainText().getKeyWorksUnmod().isEmpty() == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    vp1.setPopulator(row, Populator.createWithIDMatching(cvtOperand,

      new ItemOperatorHTC(itemOpEqualTo, "Include record", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return evalInclude((HDT_RecordWithMainText) record, HyperTableCell.getRecord(op3));
        }

        @Override public boolean op1Change(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp2.setPopulator(row, new RecordTypePopulator(operandRecordTypesStream()));
          vp3.setPopulator(row, new RecordByTypePopulator());

          return true;
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          return recordByTypeOpChange(op2, row, vp3);
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEqualTo, "Exclude record", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return evalExclude((HDT_RecordWithMainText) record, HyperTableCell.getRecord(op3));
        }

        @Override public boolean op1Change(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp2.setPopulator(row, new RecordTypePopulator(operandRecordTypesStream()));
          vp3.setPopulator(row, new RecordByTypePopulator());

          return true;
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          return recordByTypeOpChange(op2, row, vp3);
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpEmpty, "Empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return evalEmpty((HDT_RecordWithMainText) record);
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEmpty, "Not empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return evalNotEmpty((HDT_RecordWithMainText) record);
        }
      }));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected Stream<RecordType> operandRecordTypesStream() { return Stream.of(hdtWork, hdtMiscFile); }

//---------------------------------------------------------------------------

  @Override public final boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    clearOperands(row, 2);

    return operatorOp1Change(op1, row, vp1, vp2, vp3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    clearOperands(row, 3);

    return operatorOp2Change(op1, op1, op2, row, vp1, vp2, vp3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    return record.hasMainText() && evaluateOperator(op1, record, row, op1, op2, op3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    return (opNum < 2) || ItemOperatorHTC.hasOperand(op1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final boolean show(QueryType queryType, RecordType recordType)
  {
    return switch (queryType)
    {
      case qtWorks, qtFiles, qtArguments -> false;
      case qtAllRecords                  -> true;
      default                            -> HDT_RecordWithMainText.class.isAssignableFrom(recordType.getRecordClass());
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
