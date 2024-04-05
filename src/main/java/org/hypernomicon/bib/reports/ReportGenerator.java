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

package org.hypernomicon.bib.reports;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

public abstract class ReportGenerator
{
  ReportGenerator() { }

  public static ReportGenerator create(boolean html)
  {
    return html ? new HtmlReportGenerator() : new PlainTextReportGenerator();
  }

  private final List<ReportField> fieldList = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record ReportField(String name, String content) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addField(String fieldName, String text)
  {
    if (safeStr(text).isBlank()) return;

    fieldList.add(new ReportField(fieldName, text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String render(Iterable<String> fieldOrder)
  {
    StringBuilder report = getStart();

    boolean foundAny = false;

    if (fieldOrder != null)
    {
      for (String fieldName : fieldOrder)
      {
        Iterator<ReportField> it = fieldList.iterator();

        while (it.hasNext())
        {
          ReportField field = it.next();

          if (field.name.equalsIgnoreCase(fieldName))
          {
            if (foundAny)
              report.append(lineSeparator());
            else
              foundAny = true;

            report.append(field.content);
            it.remove();
          }
        }
      }
    }

    for (ReportField field : fieldList)
    {
      if (foundAny)
        report.append(lineSeparator());
      else
        foundAny = true;

      report.append(field.content);
    }

    return report.append(getEnd()).toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract String makeRow(String fieldName, String value);
  public abstract String makeRows(String fieldName, Stream<String> stream);
  public abstract String getUrlContent(String url);
  public abstract String lineSeparator();
  abstract StringBuilder getStart();
  abstract String getEnd();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
