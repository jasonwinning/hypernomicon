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

import static org.hypernomicon.model.HDI_Schema.HyperDataCategory.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.query.Query.ItemOperator.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.*;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HDI_Schema.HyperDataCategory;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.model.relations.RelationSet.RelationType;
import org.hypernomicon.query.Query.RecordQuery;
import org.hypernomicon.query.ui.QueryCtrlr;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.*;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class QueryWhereField extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryWhereField(int queryID, String description)
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

    if ((valueType != cvtTagItem) || (vp1.getPopulator(row).getRecordType() != row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX)))
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
    if (getCellID(op1) < 1)
    {
      vp1.setPopulator(row, Populator.emptyPopulator(cvtOperand));
      return true;
    }

    boolean restrict = true,     // If false, use unrestricted text entry and no record select
            hasBoolean = false,  // Whether to show boolean operators
            hasEmpty = false;    // Whether to show empty/not empty operators

    RecordType recordType = row.getRecordType(QueryCtrlr.QUERY_TYPE_COL_NDX), objType = hdtNone;

    Tag tag = Tag.getTag(getCellID(op1));

    if (tag == Tag.tagMainText)
    {
      restrict = false;
      hasEmpty = true;
    }
    else for (HDI_Schema schema : db.getSchemasByTag(tag))
    {
      RelationType relType = schema.relType();

      HyperDataCategory cat = schema.category();

      if (cat == hdcPath)
      {
        if (HyperPath.FOLDER_TAGS.contains(tag))
        {
          cat = hdcPointerSingle;
        }
        else
        {
          relType = rtNone;
          cat = hdcString;  // This is the file name tag for the path, not the folder
        }
      }

      RecordType subjType = relType == rtNone ? hdtNone : db.getSubjType(relType);

      if ((relType == rtNone) || (recordType == hdtNone) || (recordType == subjType))
      {
        switch (cat)
        {
          case hdcString : case hdcPersonName : case hdcBibEntryKey : case hdcMainTextAndHub : case hdcBibDate :

            restrict = false;
            hasEmpty = true;
            break;

          case hdcBoolean : case hdcTernary :

            hasBoolean = true;
            if (cat == hdcTernary)
              hasEmpty = true;

            break;

          case hdcPointerMulti : case hdcPointerSingle : case hdcAuthors :

            if ((objType != hdtNone) && (objType != db.getObjType(relType)))
            {
              restrict = false;
              objType = hdtNone;
            }
            else
              objType = db.getObjType(relType);

            hasEmpty = true;
            break;

          default:
            break;

        }
      }
    }

//---------------------------------------------------------------------------

    List<ItemOperatorHTC> operatorCells = new ArrayList<>();

    if (hasBoolean)
    {
      operatorCells.add(new ItemOperatorHTC(itemOpTrue, "Is true", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          HyperDataCategory cat = nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category);

          return (cat != null) && switch (cat)
          {
            case hdcBoolean -> record.getTagBoolean(tag);
            case hdcTernary -> record.getTagTernary(tag).isTrue();
            default -> false;
          };
        }
      });

//---------------------------------------------------------------------------

      operatorCells.add(new ItemOperatorHTC(itemOpFalse, "Is false", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          HyperDataCategory cat = nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category);

          return (cat != null) && switch (cat)
          {
            case hdcBoolean -> record.getTagBoolean(tag) == false;
            case hdcTernary -> record.getTagTernary(tag).isFalse();
            default -> false;
          };
        }
      });
    }

//---------------------------------------------------------------------------

    RecordType finalObjType = objType;

    if ((objType != hdtNone) || (restrict == false))
    {
      operatorCells.add(new ItemOperatorHTC(itemOpEqualTo, objType != hdtNone ? "Is or includes record" : "Is exactly", restrict)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          if (finalObjType != hdtNone)
            return evalIncludesExcludesRecord(record, tag, op3, true);

          String tagStrVal = record.resultTextForTag(tag, false);

          return strNotNullOrEmpty(tagStrVal) && tagStrVal.strip().equalsIgnoreCase(getCellText(op3).strip());
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          if (finalObjType != hdtNone)
            vp3.setPopulator(row, new StandardPopulator(finalObjType));
          else
            vp3.setRestricted(row, false);

          return true;
        }
      });

//---------------------------------------------------------------------------

      operatorCells.add(new ItemOperatorHTC(itemOpNotEqualTo, objType != hdtNone ? "Excludes record" : "Is not", restrict)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          if (finalObjType != hdtNone)
            return evalIncludesExcludesRecord(record, tag, op3, false);

          String tagStrVal = record.resultTextForTag(tag, false);

          return (tagStrVal != null) && (tagStrVal.strip().equalsIgnoreCase(getCellText(op3).strip()) == false);
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          if (finalObjType != hdtNone)
            vp3.setPopulator(row, new StandardPopulator(finalObjType));
          else
            vp3.setRestricted(row, false);

          return true;
        }
      });

    //---------------------------------------------------------------------------

      operatorCells.add(new ItemOperatorHTC(itemOpContain, "Contains text", false)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String val3 = getCellText(op3).strip();

          return strNotNullOrEmpty(val3) && record.resultTextForTag(tag, false).toLowerCase().strip().contains(val3.toLowerCase());
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setRestricted(row, false);

          return true;
        }
      });

//---------------------------------------------------------------------------

      operatorCells.add(new ItemOperatorHTC(itemOpNotContain, "Doesn't contain text", false)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String val3 = getCellText(op3).strip();

          return record.resultTextForTag(tag, false).toLowerCase().strip().contains(val3.toLowerCase()) == false;
        }

        @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
        {
          vp3.setRestricted(row, false);

          return true;
        }
      });
    }

//---------------------------------------------------------------------------

    if (hasEmpty)
    {
      operatorCells.add(new ItemOperatorHTC(itemOpEmpty, "Is empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          Tag tag = Tag.getTag(getCellID(op1));

          if (finalObjType != hdtNone)
            return evalWhetherPointerEmpty(record, tag, true);

          return nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category) == hdcTernary ?
            record.getTagTernary(tag).isUnset()
          :
            record.resultTextForTag(tag, true).isEmpty();
        }
      });

//---------------------------------------------------------------------------

      operatorCells.add(new ItemOperatorHTC(itemOpNotEmpty, "Is not empty", true)
      {
        @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          Tag tag = Tag.getTag(getCellID(op1));

          if (finalObjType != hdtNone)
            return evalWhetherPointerEmpty(record, tag, false);

          return nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category) == hdcTernary ?
            record.getTagTernary(tag).isUnset() == false
          :
            record.resultTextForTag(tag, true).isEmpty() == false;
        }
      });
    }

    vp2.setPopulator(row, Populator.createWithIDMatching(cvtOperand, operatorCells));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean evalWhetherPointerEmpty(HDT_Record record, Tag tag, boolean trueMeansEmpty)
  {
    HDI_Schema schema = record.getSchema(tag);

    return (schema != null) && (db.getObjectList(schema.relType(), record, true).isEmpty() == trueMeansEmpty);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean evalIncludesExcludesRecord(HDT_Record record, Tag tag, HyperTableCell op3, boolean isIncludes)
  {
    HDI_Schema schema = record.getSchema(tag);

    if (schema == null) return false;

    for (HDT_Record objRecord : db.getObjectList(schema.relType(), record, true))
      if ((objRecord.getID() == getCellID(op3)) && (objRecord.getType() == getCellType(op3)))
        return isIncludes;

    return isIncludes == false;
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
