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

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.query.ui.QueryCtrlr;
import org.hypernomicon.view.populators.RelationPopulator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryWhereRelative extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryWhereRelative(int queryID, String description)
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
    vp2.setPopulator(row, operandPopulator());

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    RelationType relType = RelationType.codeToVal(getCellID(op1));

    switch (getCellID(op2))
    {
      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

        vp3.setPopulator(row, new StandardPopulator(db.getSubjType(relType)));
        return true;

      default :
        clearOperands(row, 3);
        vp3.setRestricted(row, false);
        return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    RelationType relType = RelationType.codeToVal(getCellID(op1));
    if (record.getType() != db.getObjType(relType)) return false;

    HyperSubjList<HDT_Record, HDT_Record> subjList = db.getSubjectList(relType, record);
    int subjCount = subjList.size(), opID = getCellID(op2);

    if ((opID == IS_EMPTY_OPERAND_ID) || (opID == IS_NOT_EMPTY_OPERAND_ID))
      return (subjCount == 0) == (opID == IS_EMPTY_OPERAND_ID);

    for (HDT_Record subjRecord : subjList)
    {
      switch (opID)
      {
        case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

          if (subjRecord.getID() == getCellID(op3))
            return opID == EQUAL_TO_OPERAND_ID;

          break;

        case CONTAINS_OPERAND_ID : case DOES_NOT_CONTAIN_OPERAND_ID :

          if (subjRecord.listName().toLowerCase().contains(getCellText(op3).toLowerCase()))
            return opID == CONTAINS_OPERAND_ID;

          break;

        default :
          break;
      }
    }

    switch (opID)
    {
      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :
        return opID == NOT_EQUAL_TO_OPERAND_ID;

      case CONTAINS_OPERAND_ID : case DOES_NOT_CONTAIN_OPERAND_ID :
        return opID == DOES_NOT_CONTAIN_OPERAND_ID;

      default :
        return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2)
  {
    if (opNum < 3)
      return true;

    switch (op2.getID())
    {
      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :
        return false;

      default :
        return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
