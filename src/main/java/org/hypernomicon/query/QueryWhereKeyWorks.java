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

  @Override public final boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    vp1.setPopulator(row, Populator.createWithIDMatching(cvtOperand,

      createIncludeExcludeOperatorCell(itemOpEqualTo   ),
      createIncludeExcludeOperatorCell(itemOpNotEqualTo),

      createMainTextEmptyOperatorCell(itemOpEmpty   ),
      createMainTextEmptyOperatorCell(itemOpNotEmpty)));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createIncludeExcludeOperatorCell(ItemOperator operator)
  {
    String caption = switch (operator)
    {
      case itemOpEqualTo    -> "Include record";
      case itemOpNotEqualTo -> "Exclude record";
      default               -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, true)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        HDT_Record target = HyperTableCell.getRecord(op3);

        return (HDT_Record.isEmpty(target, false) == false) && evalIncludesExcludes((HDT_RecordWithMainText) record, target, operator == itemOpEqualTo);
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
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createMainTextEmptyOperatorCell(ItemOperator operator)
  {
    String caption = switch (operator)
    {
      case itemOpEmpty    -> "Empty";
      case itemOpNotEmpty -> "Not empty";
      default             -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, true)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        HDT_RecordWithMainText rec = (HDT_RecordWithMainText) record;

        return evalEmptyNotEmpty(rec, operator == itemOpEmpty);
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalEmptyNotEmpty(HDT_RecordWithMainText rec, boolean trueMeansEmpty)
  {
    return trueMeansEmpty == rec.keyWorksStream().findAny().isEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected boolean evalIncludesExcludes(HDT_RecordWithMainText rec, HDT_Record target, boolean includes)
  {
    return includes == rec.keyWorksStream().anyMatch(kw -> kw.getRecord() == target);
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
