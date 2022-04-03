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

package org.hypernomicon.query.reports;

import static org.hypernomicon.query.GeneralQueries.*;

import java.util.List;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

public abstract class ReportEngine
{
  private static final int QUERY_DUPLICATE_AUTHORS  = QUERY_FIRST_NDX + 1;

  public static final int QUERY_LICENSE_AND_NOTICE = QUERY_FIRST_NDX + 2;

  protected TableView<HyperTableRow> tv;

  public abstract void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws TerminateTaskException;
  public abstract List<HyperTableRow> getRows();
  abstract HyperTable prepTable(TableView<HyperTableRow> tv);
  abstract String getHtml(HyperTableRow row);
  public abstract boolean alwaysShowDescription();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    pop.addEntry(row, QUERY_DUPLICATE_AUTHORS, "Duplicate authors");
    pop.addEntry(row, QUERY_LICENSE_AND_NOTICE, "Application license and notices");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ReportEngine createEngine(int report)
  {
    switch (report)
    {
      case QUERY_DUPLICATE_AUTHORS  : return new DupAuthorsReportEngine();
      case QUERY_LICENSE_AND_NOTICE : return new LicenseReportEngine();
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void addCol(String caption, int prefWidth)
  {
    TableColumn<HyperTableRow, HyperTableCell> col = new TableColumn<>(caption);
    col.setPrefWidth(prefWidth);
    tv.getColumns().add(col);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
