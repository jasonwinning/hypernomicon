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

package org.hypernomicon.query.reports;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.List;

import org.hypernomicon.HyperTask;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.cellValues.GenericNonRecordHTC;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import com.google.common.collect.Lists;

import javafx.collections.FXCollections;
import javafx.scene.control.TableView;

public class LicenseReportEngine extends ReportEngine
{
  private HyperTable ht;
  private static StringBuilder license = null, notice = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean autoShowDescription() { return true; }

  @Override public void generate(HyperTask task, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HyperTableRow> getRows()
  {
    List<HyperTableRow> rows = Lists.newArrayList(
      new HyperTableRow(FXCollections.observableArrayList(new GenericNonRecordHTC("LICENSE.html", hdtNone)), ht),
      new HyperTableRow(FXCollections.observableArrayList(new GenericNonRecordHTC("NOTICE.html" , hdtNone)), ht));

    synchronized (LicenseReportEngine.class)
    {
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
          errorPopup("An error occurred while trying to load the LICENSE and NOTICE files.");
        }
      }
    }

    return rows;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void processHtml(StringBuilder html)
  {
    Document doc = MainTextUtil.makeDocLinksExternal(Jsoup.parse(html.toString()));

    assignSB(html, doc.html().replace("</head>", MainTextUtil.headContent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override HyperTable prepTable(TableView<HyperTableRow> tv)
  {
    this.tv = tv;

    addCol("Document name", 600);

    ht = new HyperTable(tv, -1, false, "");

    ht.addLabelCol(hdtNone);

    return ht;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override String getHtml(HyperTableRow row)
  {
    String str = nullSwitch(row, "", r -> nullSwitch(r.getCell(0), "", HyperTableCell::getCellText));

    if (str.contains("LICENSE")) return license.toString();
    if (str.contains("NOTICE" )) return notice .toString();

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
