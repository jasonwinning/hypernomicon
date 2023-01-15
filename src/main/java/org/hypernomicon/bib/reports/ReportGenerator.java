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

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Stream;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.data.BibData;

public abstract class ReportGenerator
{
  private final BibData bd;
  private final List<ReportField> fieldList;

  ReportGenerator(BibData bd)
  {
    this.bd = bd;
    fieldList = new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class ReportField
  {
    private final String name, content;

    private ReportField(String name, String content)
    {
      this.name = name;
      this.content = content;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addField(String fieldName, String text)
  {
    if (safeStr(text).isBlank()) return;

    fieldList.add(new ReportField(fieldName, text));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String generate()
  {
    fieldList.clear();
    bd.createReport(this);

    StringBuilder report = getStart();

    boolean foundAny = false;

    if (bd instanceof BibEntry)
    {
      BibEntry bibEntry = (BibEntry)bd;
      List<String> fieldOrder = bibEntry.getReportFieldOrder();

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
