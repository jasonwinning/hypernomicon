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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.populators.Populator.CellValueType.*;
import static org.hypernomicon.view.wrappers.HyperTableCell.*;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.stream.Collectors;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.Query.WorkQuery;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.populators.Populator;
import org.hypernomicon.view.populators.Populator.CellValueType;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class QueryWhereBibField extends WorkQuery
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public QueryWhereBibField(int queryID, String description)
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

      vp1.setPopulator(row, Populator.create(cvtBibField,

        EnumSet.allOf(BibFieldEnum.class).stream().filter(field -> field != bfWorkType)
                                                  .map(field -> new HyperTableCell(field.ordinal(), field.getUserFriendlyName(), hdtWork))
                                                  .collect(Collectors.toCollection(ArrayList::new))));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean op1Change(HyperTableCell op1, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    vp2.setPopulator(row, operandPopulator(true));

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

  @Override public boolean evaluate(HDT_Work record, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException
  {
    BibFieldEnum field = getEnumVal(getCellID(op1), BibFieldEnum.class);
    if (field == null)
      return false;

    String str = record.getBibData().getStr(field).toLowerCase().trim();

    switch (getCellID(op2))
    {
      case EQUAL_TO_OPERAND_ID : case NOT_EQUAL_TO_OPERAND_ID :

        if (str.isEmpty()) return false;

        return str.trim().equalsIgnoreCase(getCellText(op3).trim()) == (getCellID(op2) == EQUAL_TO_OPERAND_ID);

      case CONTAINS_OPERAND_ID : case DOES_NOT_CONTAIN_OPERAND_ID :

        String val3 = getCellText(op3).trim();
        if (val3.isEmpty()) return false;

        return str.contains(val3.toLowerCase()) == (getCellID(op2) == CONTAINS_OPERAND_ID);

      case IS_EMPTY_OPERAND_ID : case IS_NOT_EMPTY_OPERAND_ID :

        return (str.length() > 0) == (getCellID(op2) == IS_NOT_EMPTY_OPERAND_ID);

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
