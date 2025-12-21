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

package org.hypernomicon.util;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class PageRange implements Comparable<PageRange>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final int romanInt, decimalInt;
  private final String str;

//---------------------------------------------------------------------------

  public PageRange(String input)
  {
    input = stripSafe(input);
    String romanStr = match(input, romanNumeralPattern);

    int tempRomanInt = 0, tempDecimalInt = 0;

    if (romanStr.length() > 0)
    {
      tempRomanInt = romanToInt(romanStr);
    }
    else
    {
      String decimalStr = match(input, leadingNumberPattern);

      if (decimalStr.length() > 0)
        tempDecimalInt = parseInt(decimalStr, 0);
    }

    romanInt = tempRomanInt;
    decimalInt = tempDecimalInt;

    str = input;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String match(String input, Pattern pattern)
  {
    if (strNullOrBlank(input)) return "";

    Matcher matcher = pattern.matcher(input);

    return matcher.find() ? matcher.group(1) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Pattern for matching a valid Roman numeral at the start of the string
  private static final Pattern romanNumeralPattern = Pattern.compile("^(M{0,3}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3}))([^A-Za-z]|$)", Pattern.CASE_INSENSITIVE);

  //Pattern for matching a leading positive integer
  private static final Pattern leadingNumberPattern = Pattern.compile("^(\\d+)");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()
  {
    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object o)
  {
    if (this == o) return true;
    if ((o == null) || (getClass() != o.getClass())) return false;

    PageRange pageRange = (PageRange) o;

    if ((romanInt > 0) && (pageRange.romanInt > 0) && (romanInt != pageRange.romanInt))
      return false;

    if ((decimalInt > 0) && (pageRange.decimalInt > 0) && (decimalInt != pageRange.decimalInt))
      return false;

    return Objects.equals(str, pageRange.str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    if (romanInt > 0)
      return Objects.hash(romanInt, str);

    if (decimalInt > 0)
      return Objects.hash(decimalInt, str);

    return Objects.hash(str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int numericalValue()
  {
    return romanInt > 0 ? romanInt : Math.max(decimalInt, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<Character, Integer> romanMap = new HashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int romanToInt(String s)
  {
    if (strNullOrBlank(s)) return 0;

    s = s.strip();

    if (romanMap.isEmpty())
    {
      romanMap.put('I', 1);    romanMap.put('i', 1);
      romanMap.put('V', 5);    romanMap.put('v', 5);
      romanMap.put('X', 10);   romanMap.put('x', 10);
      romanMap.put('L', 50);   romanMap.put('l', 50);
      romanMap.put('C', 100);  romanMap.put('c', 100);
      romanMap.put('D', 500);  romanMap.put('d', 500);
      romanMap.put('M', 1000); romanMap.put('m', 1000);
    }

    int result = 0;

    for (int i = 0; i < s.length(); i++)
    {
      int current = romanMap.getOrDefault(s.charAt(i), -1),
          next = (i < s.length() - 1) ? romanMap.getOrDefault(s.charAt(i + 1), -1) : 0;

      if ((current < 0) || (next < 0)) return 0;

      if (current < next) result -= current;
      else                result += current;
    }

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(PageRange other)
  {
    if (other == null) return -1; // Consider this instance to be "less than" a null value

    if ((romanInt != 0) && (other.romanInt != 0)) return (romanInt == other.romanInt) ? str.compareTo(other.str) : Integer.compare(romanInt, other.romanInt);

    if (romanInt       != 0)  return -1;
    if (other.romanInt != 0)  return 1;

    if ((decimalInt != 0) && (other.decimalInt != 0)) return (decimalInt == other.decimalInt) ? str.compareTo(other.str) : Integer.compare(decimalInt, other.decimalInt);

    if (decimalInt       != 0) return -1;
    if (other.decimalInt != 0) return 1;

    return str.compareTo(other.str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
