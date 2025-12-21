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

package org.hypernomicon.query.reports;

import java.util.List;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;

import static org.hypernomicon.util.UIUtil.*;

//---------------------------------------------------------------------------

public abstract class ReportEngine
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int QUERY_DUPLICATE_AUTHORS  = 10001;
  public  static final int QUERY_LICENSE_AND_NOTICE = 10002;
  private static final int QUERY_DANGLING_LABELS    = 10003;

  protected TableView<HyperTableRow> tv;

  public abstract void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) throws HyperDataException, CancelledTaskException;
  public abstract List<HyperTableRow> getRows();
  abstract HyperTable prepTable(TableView<HyperTableRow> tv);

  @SuppressWarnings("unused")
  String getHtml(HyperTableRow row)    { return ""; }
  public boolean autoShowDescription() { return false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    pop.addEntry(row, QUERY_DUPLICATE_AUTHORS, "Duplicate authors");
    pop.addEntry(row, QUERY_LICENSE_AND_NOTICE, "Application license and notices");
    pop.addEntry(row, QUERY_DANGLING_LABELS, "Dangling labels united to records with parent/child relationship");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ReportEngine createEngine(int report) { return switch (report)
  {
    case QUERY_DUPLICATE_AUTHORS  -> new DupAuthorsReportEngine();
    case QUERY_LICENSE_AND_NOTICE -> new LicenseReportEngine();
    case QUERY_DANGLING_LABELS    -> new DanglingLabelsReportEngine();

    default                       -> null;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected TableColumn<HyperTableRow, HyperTableCell> addCol(String caption, int prefWidth)
  {
    TableColumn<HyperTableRow, HyperTableCell> col = new TableColumn<>(caption);
    col.setPrefWidth(scalePropertyValueForDPI(prefWidth));
    tv.getColumns().add(col);
    return col;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
