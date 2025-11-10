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

class QueryWhereField extends RecordQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String queryText;

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

  @Override public void init(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    queryText = convertToEnglishChars(getCellText(op3)).strip().toLowerCase();
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
      operatorCells.add(createBooleanOperatorCell(itemOpTrue , tag));
      operatorCells.add(createBooleanOperatorCell(itemOpFalse, tag));
    }

//---------------------------------------------------------------------------

    if ((objType != hdtNone) || (restrict == false))
    {
      operatorCells.add(createEqualityOperatorCell(itemOpEqualTo   , tag, objType, restrict));
      operatorCells.add(createEqualityOperatorCell(itemOpNotEqualTo, tag, objType, restrict));

      operatorCells.add(createContainsOperatorCell(itemOpContain   , tag));
      operatorCells.add(createContainsOperatorCell(itemOpNotContain, tag));
    }

//---------------------------------------------------------------------------

    if (hasEmpty)
    {
      operatorCells.add(createEmptyOperatorCell(itemOpEmpty   , objType));
      operatorCells.add(createEmptyOperatorCell(itemOpNotEmpty, objType));
    }

    vp2.setPopulator(row, Populator.createWithIDMatching(cvtOperand, operatorCells));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createBooleanOperatorCell(ItemOperator operator, Tag tag)
  {
    String caption = switch (operator)
    {
      case itemOpTrue  -> "Is true";
      case itemOpFalse -> "Is false";
      default          -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, true)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        HyperDataCategory cat = nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category);

        return (cat != null) && switch (cat)
        {
          case hdcBoolean -> (operator == itemOpTrue) == record.getTagBoolean(tag);
          case hdcTernary -> (operator == itemOpTrue) ? record.getTagTernary(tag).isTrue() : record.getTagTernary(tag).isFalse();
          default -> false;
        };
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createEqualityOperatorCell(ItemOperator operator, Tag tag, RecordType objType, boolean restrict)
  {
    String caption = switch (operator)
    {
      case itemOpEqualTo    -> (objType != hdtNone ? "Is or includes record" : "Is exactly");
      case itemOpNotEqualTo -> (objType != hdtNone ? "Excludes record" : "Is not");
      default               -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, restrict)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        if (objType != hdtNone)
        {
          HDI_Schema schema = record.getSchema(tag);
          if (schema == null) return false;

          for (HDT_Record objRecord : db.getObjectList(schema.relType(), record, true))
            if ((objRecord.getID() == getCellID(op3)) && (objRecord.getType() == getCellType(op3)))
              return operator == itemOpEqualTo;

          return operator == itemOpNotEqualTo;
        }

        String tagStrVal = record.resultTextForTag(tag, false, true);

        return operator == itemOpEqualTo ?
          (strNotNullOrEmpty(tagStrVal) && tagStrVal.strip().equalsIgnoreCase(queryText))
        :
          ((tagStrVal != null) && (tagStrVal.strip().equalsIgnoreCase(queryText) == false));
      }

      @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        if (objType != hdtNone)
          vp3.setPopulator(row, new StandardPopulator(objType));
        else
          vp3.setRestricted(row, false);

        return true;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createContainsOperatorCell(ItemOperator operator, Tag tag)
  {
    String caption = switch (operator)
    {
      case itemOpContain    -> "Contains text";
      case itemOpNotContain -> "Doesn't contain text";
      default               -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, false)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        if (strNullOrEmpty(queryText)) return false;

        return nullSwitch(record.resultTextForTag(tag, false, true), false, tagStrVal ->
          (operator == itemOpContain) == tagStrVal.strip().toLowerCase().contains(queryText));
      }

      @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        vp3.setRestricted(row, false);
        return true;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ItemOperatorHTC createEmptyOperatorCell(ItemOperator operator, RecordType objType)
  {
    String caption = switch (operator)
    {
      case itemOpEmpty    -> "Is empty";
      case itemOpNotEmpty -> "Is not empty";
      default             -> throw new IllegalArgumentException("Unsupported operator");
    };

    return new ItemOperatorHTC(operator, caption, true)
    {
      @Override public boolean evaluate(HDT_Record record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        Tag tag = Tag.getTag(getCellID(op1));

        if (objType != hdtNone)
        {
          HDI_Schema schema = record.getSchema(tag);

          return (schema != null) && (db.getObjectList(schema.relType(), record, true).isEmpty() == (operator == itemOpEmpty));
        }

        HyperDataCategory cat = nullSwitch(db.getSchema(record.getType(), tag), null, HDI_Schema::category);

        return (operator == itemOpEmpty) == (cat == hdcTernary ?
          record.getTagTernary(tag).isUnset()
        :
          record.resultTextForTag(tag, true, false).isEmpty());
      }
    };
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
