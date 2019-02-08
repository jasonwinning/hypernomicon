/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.reports;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.scene.control.TableView;

public class LicenseReportEngine extends ReportEngine
{
  private HyperTable ht;
  private static StringBuilder license = null, notice = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean alwaysShowDescription() { return true; }

  @Override public void generate(HyperTask task, HyperTableCell param1, HyperTableCell param2, HyperTableCell param3) throws TerminateTaskException { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableRow> getRows()
  {
    ArrayList<HyperTableRow> rows = new ArrayList<>();

    ObservableList<HyperTableCell> cells = FXCollections.observableArrayList();

    cells.add(new HyperTableCell(-1, "LICENSE.html", hdtNone));

    rows.add(new HyperTableRow(cells, ht));

    cells = FXCollections.observableArrayList();

    cells.add(new HyperTableCell(-1, "NOTICE.html", hdtNone));

    rows.add(new HyperTableRow(cells, ht));

    if (license == null)
    {
      license = new StringBuilder();
      notice = new StringBuilder();

      try
      {
        readResourceTextFile("/LICENSE.html", license, true);
        processHtml(license);

        readResourceTextFile("/NOTICE.html", notice, true);
        processHtml(notice);
      }
      catch (IOException e)
      {
        messageDialog("An error occurred while trying to load the LICENSE and NOTICE files.", mtError);
      }
    }

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void processHtml(StringBuilder html)
  {
    Document doc = MainTextWrapper.makeDocLinksExternal(Jsoup.parse(html.toString()));

    assignSB(html, doc.html().replace("</head>", MainTextWrapper.getHeadContent()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override HyperTable prepTable(TableView<HyperTableRow> tv)
  {
    this.tv = tv;

    addCol("Document name", 600);

    ht = new HyperTable(tv, -1, false, "");

    ht.addCol(hdtNone, ctNone);

    return ht;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override String getHtml(HyperTableRow row)
  {
    if (row == null) return "";

    if (row.getCell(0).getText().contains("LICENSE"))     return license.toString();
    else if (row.getCell(0).getText().contains("NOTICE")) return notice.toString();

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
