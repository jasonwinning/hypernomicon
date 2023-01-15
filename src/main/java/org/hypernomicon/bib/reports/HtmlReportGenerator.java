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

package org.hypernomicon.bib.reports;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.view.mainText.MainTextUtil;

public final class HtmlReportGenerator extends ReportGenerator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HtmlReportGenerator(BibData bd)
  {
    super(bd);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String generate(BibData bd)
  {
    return bd == null ? "" : new HtmlReportGenerator(bd).generate();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String makeRow(String fieldName, String value)
  {
    return "<tr><td class=\"fieldName\">" + fieldName + "</td><td>" + value + "</td></tr>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String makeRows(String fieldName, Stream<String> stream)
  {
    List<String> list = stream.filter(Objects::nonNull).collect(Collectors.toList());

    if (list.isEmpty()) return "";

    StringBuilder html = new StringBuilder("<tr><td class=\"fieldName\">" + fieldName + "</td><td>");

    for (int ndx = 0; ndx < list.size(); ndx++)
    {
      html.append(list.get(ndx));
      if (ndx < (list.size() - 1))
        html.append("<br>");
    }

    return html.append("</td></tr>").toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getUrlContent(String url)
  {
    return "<a href=\"\" onclick=\"openURL('" + url + "'); return false;\">" + url + "</a>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override StringBuilder getStart()
  {
    return new StringBuilder()

      .append("<html><head>").append(MainTextUtil.scriptContent).append("<style>")
      .append("td.fieldName { vertical-align: text-top; text-align: right; padding-right:10px; }</style></head><body>")
      .append("<table style=\"font-size:9pt; font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen-Sans,Ubuntu,Cantarell,sans-serif; line-height:10pt;\">");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String lineSeparator() { return ""; }

  @Override String getEnd()               { return "</table></body></html>"; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
