/*
 * Copyright 2015-2023 Jason Winning
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
import static org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.query.ui.QueriesTabCtrlr.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.records.HDT_RecordBase.HyperDataCategory;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.query.ui.QueryCtrlr;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.StandardPopulator;
import org.hypernomicon.view.populators.TagItemPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryWhereField extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryWhereField(int queryID, String description)
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

    if ((valueType != cvtTagItem) || (vp1.getPopulator(row).getRecordType(null) != row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX)))
    {
      clearOperands(row, 1);
      vp1.setPopulator(row, new TagItemPopulator(row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX)));
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
    RecordType recordType = row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX), objType = hdtNone;
    HyperDataCategory cat = hdcString;
    boolean catSet = false;

    for (HDI_Schema schema : db.getSchemasByTag(Tag.getTag(getCellID(op1))))
    {
      RelationType relType = schema.getRelType();

      RecordType subjType = relType == rtNone ? hdtNone : db.getSubjType(relType);

      if ((recordType == hdtNone) || (recordType == subjType))
      {
        if (catSet == false)
        {
          cat = schema.getCategory();
          catSet = true;

          if ((cat == hdcPointerMulti) || (cat == hdcPointerSingle) || (cat == hdcAuthors))
            objType = db.getObjType(relType);
        }
        else
        {
          if ((cat == hdcPointerMulti) || (cat == hdcPointerSingle) || (cat == hdcAuthors))
          {
            if ((schema.getCategory() != hdcPointerMulti) && (schema.getCategory() != hdcPointerSingle) && (schema.getCategory() != hdcAuthors))
              cat = hdcString;
            else
            {
              if (objType != db.getObjType(relType))
                cat = hdcString;
            }
          }
          else if (cat != schema.getCategory())
            cat = hdcString;
        }
      }
    }

    if ((getCellID(op2) != EQUAL_TO_OPERAND_ID) && (getCellID(op2) != NOT_EQUAL_TO_OPERAND_ID))
      cat = hdcString;

    switch (cat)
    {
      case hdcString : case hdcPersonName : case hdcBibEntryKey : case hdcMainTextAndHub :

        clearOperands(row, 3);
        vp3.setRestricted(row, false);
        break;

      case hdcBoolean :

        vp3.setPopulator(row, Populator.create(cvtBoolean, trueCell, falseCell));
        break;

      case hdcTernary :

        vp3.setPopulator(row, Populator.create(cvtTernary, unsetCell, trueCell, falseCell));
        break;

      case hdcPath          : case hdcAuthors       : case hdcHubSpokes     :
      case hdcPointerSingle : case hdcPointerMulti  : case hdcNestedPointer :

        vp3.setPopulator(row, new StandardPopulator(objType));
        break;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    Tag tag = Tag.getTag(getCellID(op1));
    HDI_Schema schema = record.getSchema(tag);

    if (schema == null) return false;

    CellValueType valueType = row.getPopulator(QueryCtrlr.OPERAND_3_COL_NDX).getValueType(row);

    switch (getCellID(op2))
    {
      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

        switch (valueType)
        {
          case cvtRecord :

            for (HDT_Record objRecord : db.getObjectList(schema.getRelType(), record, true))
            {
              if ((objRecord.getID() == getCellID(op3)) && (objRecord.getType() == getCellType(op3)))
                return getCellID(op2) == EQUAL_TO_OPERAND_ID;
            }

            return getCellID(op2) == NOT_EQUAL_TO_OPERAND_ID;

          case cvtBoolean :

            if ((getCellID(op3) != TRUE_ID) && (getCellID(op3) != FALSE_ID)) return false;

            return (record.getTagBoolean(tag) == (getCellID(op3) == TRUE_ID)) == (getCellID(op2) == EQUAL_TO_OPERAND_ID);

          default :

            String tagStrVal = record.resultTextForTag(tag);
            if (tagStrVal.isEmpty()) return false;

            return tagStrVal.trim().equalsIgnoreCase(getCellText(op3).trim()) == (getCellID(op2) == EQUAL_TO_OPERAND_ID);
        }

      case CONTAINS_OPERAND_ID : case DOES_NOT_CONTAIN_OPERAND_ID :

        String val3 = getCellText(op3).trim();
        if (val3.isEmpty()) return false;

        String tagStrVal = record.resultTextForTag(tag).toLowerCase().trim();

        return tagStrVal.contains(val3.toLowerCase()) == (getCellID(op2) == CONTAINS_OPERAND_ID);

      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :

        switch (valueType)
        {
          case cvtRecord :

            return (db.getObjectList(schema.getRelType(), record, true).size() > 0) == (getCellID(op2) == IS_NOT_EMPTY_OPERAND_ID);

          case cvtBoolean :

            return getCellID(op2) == IS_EMPTY_OPERAND_ID;

          default :

            return (record.resultTextForTag(tag).length() > 0) == (getCellID(op2) == IS_NOT_EMPTY_OPERAND_ID);
        }

      default : return false;
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
