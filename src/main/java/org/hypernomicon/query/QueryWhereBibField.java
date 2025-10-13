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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.Query.ItemOperator.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;

import java.util.*;
import java.util.stream.Collectors;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.Query.WorkQuery;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableRow;

//---------------------------------------------------------------------------

public class QueryWhereBibField extends WorkQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  QueryWhereBibField(int queryID, String description)
  {
    super(queryID, description);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean show(QueryType queryType, RecordType recordType)
  {
    return db.bibLibraryIsLinked() && (queryType == qtWorks);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    CellValueType valueType = vp1.getValueType(row);

    if (valueType != cvtBibField)
    {
      clearOperands(row, 1);

      List<? extends HyperTableCell> cells = EnumSet.allOf(BibFieldEnum.class).stream()

        .filter(field -> field != bfWorkType)
        .map(field -> new GenericNonRecordHTC(field.ordinal(), field.getUserFriendlyName(), hdtWork))
        .collect(Collectors.toCollection(ArrayList::new));

      // cells needed to be initialized before the next line of code, separately from it,
      // in order to avoid a Maven false-positive build error. Don't ask me why.

      vp1.setPopulator(row, Populator.createWithIDMatching(cvtBibField, cells));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    vp2.setPopulator(row, Populator.createWithIDMatching(cvtOperand,

      new ItemOperatorHTC(itemOpEqualTo, "Is exactly", false)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          if (strNullOrEmpty(str)) return false;

          return str.strip().equalsIgnoreCase(getCellText(op3).strip());
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEqualTo, "Is not", false)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          if (strNullOrEmpty(str)) return false;

          return str.strip().equalsIgnoreCase(getCellText(op3).strip()) == false;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpContain, "Contains text", false)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          if (str == null) return false;

          String val3 = getCellText(op3).strip();
          if (val3.isEmpty()) return false;

          return str.contains(val3.toLowerCase());
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotContain, "Doesn't contain text", false)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          if (str == null) return false;

          String val3 = getCellText(op3).strip();
          if (val3.isEmpty()) return false;

          return str.contains(val3.toLowerCase()) == false;
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpEmpty, "Is empty", true)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          return (str != null) && str.isEmpty();
        }
      },

//---------------------------------------------------------------------------

      new ItemOperatorHTC(itemOpNotEmpty, "Is not empty", true)
      {
        @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
        {
          String str = getBibStr(work, op1);
          return strNotNullOrEmpty(str);
        }
      }));

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op2Change(HyperTableCell op1, HyperTableCell op2, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    clearOperands(row, 3);
    vp3.setRestricted(row, false);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getBibStr(HDT_Work record, HyperTableCell cell)
  {
    return nullSwitch(getEnumVal(getCellID(cell), BibFieldEnum.class), null, field -> record.getBibData().getStr(field).toLowerCase().strip());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Work record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
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
