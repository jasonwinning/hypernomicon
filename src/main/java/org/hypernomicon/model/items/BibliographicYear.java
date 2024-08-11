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

package org.hypernomicon.model.items;

import static org.hypernomicon.util.Util.*;

import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

//---------------------------------------------------------------------------

public class BibliographicYear implements Comparable<BibliographicYear>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Pattern yearPattern = Pattern.compile("^(?:[^0-9]*?)(-?[0-9]{1,6}) ?(B\\.? ?C\\.?(?: ?E\\.?)?)?\\b(.*?)$", Pattern.CASE_INSENSITIVE);

  public final int parsedVal;  // Zero means unentered. Mendeley year must be a numeric value between -999999 and 999999; cannot be 0.
                               // Zotero cannot handle negative year yet.

  public final String rawVal;  // This is the raw text as entered by the user.

//---------------------------------------------------------------------------

  public BibliographicYear(int parsedVal)
  {
    this(parsedVal, String.valueOf(parsedVal));
  }

//---------------------------------------------------------------------------

  public BibliographicYear(String rawVal)
  {
    if (rawVal == null) rawVal = "";

    Matcher m = yearPattern.matcher(rawVal);
    int tempParsedVal = 0;

    if (m.find())
    {
      tempParsedVal = parseInt(m.group(1), 0);

      if (safeStr(m.group(2)).length() > 0)  // If it ends with BC, B.C.E., etc.
        tempParsedVal = Math.abs(tempParsedVal) * -1;
    }

    parsedVal = tempParsedVal;
    this.rawVal = rawVal;
  }

//---------------------------------------------------------------------------

  public BibliographicYear(int parsedVal, String rawVal)
  {
    this.parsedVal = parsedVal;
    this.rawVal = rawVal == null ? "" : rawVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()
  {
    return (parsedVal == 0) && safeStr(rawVal).isBlank();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(BibliographicYear o)
  {
    if (o == null) return 1;

    if ((parsedVal == 0) && (o.parsedVal == 0)) return safeStr(rawVal).compareTo(safeStr(o.rawVal));

    if (parsedVal == 0) return -1;
    if (o.parsedVal == 0) return 1;

    return parsedVal - o.parsedVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return Objects.hash(parsedVal, rawVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (!(obj instanceof BibliographicYear)) return false;

    BibliographicYear other = (BibliographicYear) obj;

    return (parsedVal == other.parsedVal) && Objects.equals(rawVal, other.rawVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
