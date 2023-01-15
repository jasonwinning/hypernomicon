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

import java.util.Objects;
import java.util.stream.Stream;

import org.hypernomicon.bib.data.BibData;

public final class PlainTextReportGenerator extends ReportGenerator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PlainTextReportGenerator(BibData bd)
  {
    super(bd);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String generate(BibData bd)
  {
    return bd == null ? "" : new PlainTextReportGenerator(bd).generate();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String makeRow(String fieldName, String value)
  {
    return value.trim().isEmpty() ? "" : fieldName + ": " + value;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String makeRows(String fieldName, Stream<String> stream)
  {
    String line = stream.filter(Objects::nonNull)
                        .reduce((s1, s2) -> s1 + "; " + s2).orElse("");

    return makeRow(fieldName, line);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getUrlContent(String url) { return url; }
  @Override public String lineSeparator()           { return "\n"; }

  @Override StringBuilder getStart()                { return new StringBuilder(); }
  @Override String getEnd()                         { return ""; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
