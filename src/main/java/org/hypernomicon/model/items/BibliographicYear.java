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

  private final int numericValue;  // There is no such thing as year zero. It goes from 1 BC to 1 AD.
                                   // For 1 AD, this will equal 1. For 1 BC, this will equal -1.
                                   // A zero value, then, represents that there is no numeric value for this year (rawValue might not be empty, however).

                                   // Mendeley year must be a numeric value between -999999 and 999999; cannot be 0. -1 means 1 BC.
                                   // Zotero cannot handle negative year yet.

  public final String rawValue;    // This is the raw text as entered by the user.

//---------------------------------------------------------------------------

  private BibliographicYear(int numericValue)
  {
    this(numericValue, numericValWhereMinusOneEqualsOneBCtoStr(numericValue));
  }

//---------------------------------------------------------------------------

  BibliographicYear(String rawValue, boolean yearZeroIsOneBC)
  {
    if (rawValue == null) rawValue = "";

    Matcher m = yearPattern.matcher(rawValue);
    int tempNumericValue = 0;

    if (m.find())
    {
      tempNumericValue = parseInt(m.group(1), 0);

      if (safeStr(m.group(2)).length() > 0)  // If it ends with BC, B.C.E., etc.
        tempNumericValue = Math.abs(tempNumericValue) * -1;

      if (yearZeroIsOneBC && (tempNumericValue < 1))
        tempNumericValue--;
    }

    numericValue = tempNumericValue;
    this.rawValue = rawValue;
  }

//---------------------------------------------------------------------------

  private BibliographicYear(int numericValue, String rawValue)
  {
    this.numericValue = numericValue;
    this.rawValue = rawValue == null ? "" : rawValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int numericValueWhereMinusOneEqualsOneBC()
  {
    return numericValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicYear fromNumberWhereMinusOneEqualsOneBC(int numericValue)
  {
    return new BibliographicYear(numericValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicYear fromRawStrWhereMinusOneEqualsOneBC(String rawValue)
  {
    return new BibliographicYear(rawValue, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicYear fromRawStrAndNumberWhereMinusOneEqualsOneBC(int numericValue, String rawValue)
  {
    return new BibliographicYear(numericValue, rawValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String numericValWhereMinusOneEqualsOneBCtoStr(int numericValue)
  {
    if (numericValue == 0) return "";

    String yearStr = String.valueOf(Math.abs(numericValue));

    return numericValue < 0 ?
      yearStr + " B.C.E."
    :
      yearStr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isEmpty()
  {
    return (numericValue == 0) && safeStr(rawValue).isBlank();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(BibliographicYear o)
  {
    if (o == null) return 1;

    if ((numericValue == 0) && (o.numericValue == 0)) return safeStr(rawValue).compareTo(safeStr(o.rawValue));

    if (numericValue == 0) return -1;
    if (o.numericValue == 0) return 1;

    return numericValue - o.numericValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return Objects.hash(numericValue, rawValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (!(obj instanceof BibliographicYear)) return false;

    BibliographicYear other = (BibliographicYear) obj;

    return (numericValue == other.numericValue) && Objects.equals(safeStr(rawValue), safeStr(other.rawValue));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
