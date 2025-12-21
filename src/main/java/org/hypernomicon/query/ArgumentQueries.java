/*
 * Copyright 2015-2026 Jason Winning
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

import org.hypernomicon.model.records.HDT_Argument;
import org.hypernomicon.query.Query.ArgumentQuery;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.App.app;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.cellValues.HyperTableCell.*;

import java.util.List;

//---------------------------------------------------------------------------

public final class ArgumentQueries
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ArgumentQueries() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  private static final int QUERY_SOURCES_AT_LEAST = 4001;  // "with at least ___ sources"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(List<Query<?>> allQueries)
  {
    if (app.debugging && Boolean.FALSE) allQueries.add(new ArgumentQuery(QUERY_SOURCES_AT_LEAST, "with at least ___ sources")
    {
      @Override public boolean initRow(HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
      {
        clearOperands(row, vp1.getRestricted(row) ? 1 : 2);
        vp1.setRestricted(row, false);

        return false;
      }

      @Override public boolean evaluate(HDT_Argument arg, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return arg.works.size() >= parseInt(getCellText(op1), Integer.MAX_VALUE);
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return opNum == 1; }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
