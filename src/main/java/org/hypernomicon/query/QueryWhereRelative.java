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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.query.Query.ItemOperator.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.Collection;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.query.ui.QueryCtrlr;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class QueryWhereRelative extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryWhereRelative(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean show(QueryType queryType, RecordType recordType) { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    CellValueType valueType = vp1.getValueType(row);

    if ((valueType != cvtRelation) || (vp1.getPopulator(row).getRecordType(row) != row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX)))
    {
      clearOperands(row, 1);
      vp1.setPopulator(row, new RelationPopulator(row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX)));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    if (getCellID(op1) < 1)
    {
      vp1.setPopulator(row, Populator.emptyPopulator(cvtOperand));
      return true;
    }

    vp2.setPopulator(row, Populator.createWithIDMatching(cvtOperand,

      new ItemOperatorHTC(itemOpEqualTo, "Is or includes record", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          int targetID = getCellID(op3);
          if (targetID < 1) return false;

          return nullSwitch(getSubjList(record, op1), false, subjList -> subjList.stream().anyMatch(subjRecord -> subjRecord.getID() == targetID));
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setPopulator(row, new StandardPopulator(db.getSubjType(RelationType.codeToVal(getCellID(op1)))));
          return true;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEqualTo, "Excludes record", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          int targetID = getCellID(op3);
          if (targetID < 1) return false;

          return nullSwitch(getSubjList(record, op1), false, subjList -> subjList.stream().noneMatch(subjRecord -> subjRecord.getID() == targetID));
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setPopulator(row, new StandardPopulator(db.getSubjType(RelationType.codeToVal(getCellID(op1)))));
          return true;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpContain, "Contains text", false)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String targetText = getCellText(op3).toLowerCase();
          if (strNullOrEmpty(targetText)) return false;

          return nullSwitch(getSubjList(record, op1), false, subjList -> subjList.stream().anyMatch(subjRecord -> subjRecord.listName().toLowerCase().contains(targetText)));
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setRestricted(row, false);

          return true;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotContain, "Doesn't contain text", false)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String targetText = getCellText(op3).toLowerCase();
          if (strNullOrEmpty(targetText)) return false;

          return nullSwitch(getSubjList(record, op1), false, subjList -> subjList.stream().noneMatch(subjRecord -> subjRecord.listName().toLowerCase().contains(targetText)));
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setRestricted(row, false);

          return true;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpEmpty, "Is empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return nullSwitch(getSubjList(record, op1), false, Collection::isEmpty);
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEmpty, "Is not empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          return nullSwitch(getSubjList(record, op1), false, subjList -> subjList.isEmpty() == false);
        }
      }));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    clearOperands(row, 3);

    return operatorOp2Change(op2, op1, op2, row, vp1, vp2, vp3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HyperSubjList<HDT_Record, HDT_Record> getSubjList(HDT_Record record, HyperTableCell op1)
  {
    RelationType relType = RelationType.codeToVal(getCellID(op1));
    if (record.getType() != db.getObjType(relType)) return null;

    return db.getSubjectList(relType, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    return evaluateOperator(op2, record, row, op1, op2, op3);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    return (opNum < 3) || ItemOperatorHTC.hasOperand(op2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
