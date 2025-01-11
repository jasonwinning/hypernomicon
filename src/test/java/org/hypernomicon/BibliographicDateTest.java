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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import org.apache.commons.lang3.compare.ComparableUtils;

import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicYear;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class BibliographicDateTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void removeLeadingZerosTest()
  {
    String actual = BibliographicDate.removeLeadingZerosFromDateStr(null);
    assertEquals("", actual, "Should return empty String for null input string");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("");
    assertEquals("", actual, "Should return empty String for empty input string");

    actual = BibliographicDate.removeLeadingZerosFromDateStr(" ");
    assertEquals(" ", actual, "For input string with just whitespace, should be unaltered");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("   ");
    assertEquals("   ", actual, "Should return empty String for blank input string");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("07");
    assertEquals("7", actual, "Should strip single number with leading zero");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("0007");
    assertEquals("7", actual, "Should strip single number with multiple leading zeros");

    actual = BibliographicDate.removeLeadingZerosFromDateStr(" 0007");
    assertEquals(" 7", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("0007 ");
    assertEquals("7 ", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("   0007");
    assertEquals("   7", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("0007   ");
    assertEquals("7   ", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr(" 0007 ");
    assertEquals(" 7 ", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("   0007   ");
    assertEquals("   7   ", actual, "Should strip zeros but not space");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("0007, November 01st");
    assertEquals("7, November 1st", actual, "Should strip leading zeros in multiple places, including start");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("November 07, 0800");
    assertEquals("November 7, 800", actual, "Should strip leading zeros in multiple places, including ending number");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("November 07, 0800 BC");
    assertEquals("November 7, 800 BC", actual, "Should strip leading zeros in multiple places, including ending number");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("November 10, 1986");
    assertEquals("November 10, 1986", actual, "Date with no leading zeros should be unaltered");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("November 0000000000000000000000000000010, 0000000000000000000000000000001986");
    assertEquals("November 10, 1986", actual, "Should strip large number of leading zeros");

    actual = BibliographicDate.removeLeadingZerosFromDateStr("November0000000000000000000000000000010,0000000000000000000000000000001986");
    assertEquals("November10,1986", actual, "Should not require leading zeros to be preceded by space");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void dateEqualityTest()
  {
    BibliographicDate date1 = BibliographicDate.EMPTY_DATE,
                      date2 = null;

    assertEquals(date1, date2, "Empty date should equal null");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, null));
    assertEquals(date1, null, "Empty date should equal null");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, null, "Empty date should equal null");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, BibliographicDate.EMPTY_DATE, "Empty dates should be equal");

    assertTrue(BibliographicDate.isEmpty(date1), "Empty date should be empty");
    assertTrue(BibliographicDate.isEmpty(BibliographicDate.EMPTY_DATE), "Empty date should be empty");
    assertTrue(BibliographicDate.isEmpty(null), "Null date should be empty");

    date1 = new BibliographicDate(1, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertNotEquals(date1, BibliographicDate.EMPTY_DATE, "Date with only a day should not equal empty");

    assertFalse(BibliographicDate.isEmpty(date1), "Date with only a day should not be empty");

    date1 = new BibliographicDate(0, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertNotEquals(date1, BibliographicDate.EMPTY_DATE, "Date with only a month should not equal empty");

    assertFalse(BibliographicDate.isEmpty(date1), "Date with only a month should not be empty");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, ""));
    assertNotEquals(date1, BibliographicDate.EMPTY_DATE, "Date with only a year should not equal empty");

    assertFalse(BibliographicDate.isEmpty(date1), "Date with only a year should not be empty");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, ""));
    assertNotEquals(date1, BibliographicDate.EMPTY_DATE, "Date with only a negative year should not equal empty");

    assertFalse(BibliographicDate.isEmpty(date1), "Date with only a negative year should not be empty");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "Q"));
    assertNotEquals(date1, BibliographicDate.EMPTY_DATE, "Date with only an alphabetic year should not equal empty");

    assertFalse(BibliographicDate.isEmpty(date1), "Date with only an alphabetic year should not be empty");

    date1 = new BibliographicDate(-1, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, BibliographicDate.EMPTY_DATE, "Date with invalid day and nothing else should be equal to empty date");

    date1 = new BibliographicDate(32, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, BibliographicDate.EMPTY_DATE, "Date with invalid day and nothing else should be equal to empty date");

    date1 = new BibliographicDate(0, -1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, BibliographicDate.EMPTY_DATE, "Date with invalid month and nothing else should be equal to empty date");

    date1 = new BibliographicDate(0, 13, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertEquals(date1, BibliographicDate.EMPTY_DATE, "Date with invalid month and nothing else should be equal to empty date");

    date1 = new BibliographicDate(1, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    date2 = new BibliographicDate(2, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertNotEquals(date1, date2, "Dates with only different days should not be equal");

    date1 = new BibliographicDate(0, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    date2 = new BibliographicDate(0, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertNotEquals(date1, date2, "Dates with only different months should not be equal");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, ""));
    date2 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, ""));
    assertNotEquals(date1, date2, "Dates with only different years should not be equal");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void dateComparisonTest()
  {
    BibliographicDate date1 = new BibliographicDate(1, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));

    assertGreaterThan(date1, null, "Date with only day should be greater than null");
    assertGreaterThan(date1, BibliographicDate.EMPTY_DATE, "Date with only day should be greater than empty date");

    BibliographicDate date2 = new BibliographicDate(2, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertLessThan(date1, date2, "If dates only have days and the day is lesser then the date should be lesser");

    date2 = new BibliographicDate(0, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertLessThan(date1, date2, "Date with only day is less than date with only month");

    date1 = new BibliographicDate(1, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertGreaterThan(date1, date2, "Date with day and month is greater than date with only month");

    date2 = new BibliographicDate(1, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertGreaterThan(date1, date2, "Date with day and month is greater than date with only day");

    date1 = new BibliographicDate(0, 0, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, ""));
    assertGreaterThan(date1, date2, "Date with only year is greater than date with only day");

    date2 = new BibliographicDate(0, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertGreaterThan(date1, date2, "Date with only year is greater than date with only month");

    date2 = new BibliographicDate(1, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertGreaterThan(date1, date2, "Date with only year is greater than date with only month and day");

    date1 = new BibliographicDate(2, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    date2 = new BibliographicDate(1, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""));
    assertLessThan(date1, date2, "Date with greater month is greater than date with greater day");

    date1 = new BibliographicDate(2, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, ""));
    date2 = new BibliographicDate(1, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, ""));
    assertLessThan(date1, date2, "Date with greater month is greater than date with greater day");

    date1 = new BibliographicDate(2, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "1"));
    date2 = new BibliographicDate(1, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "1"));
    assertLessThan(date1, date2, "Date with greater month is greater than date with greater day");

    date1 = new BibliographicDate(2, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "1"));
    date2 = new BibliographicDate(1, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "1"));
    assertLessThan(date1, date2, "Date with greater month is greater than date with greater day");

    date1 = new BibliographicDate(2, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "1"));
    date2 = new BibliographicDate(1, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "1"));
    assertLessThan(date1, date2, "Date with greater year is greater than date with greater day");

    date1 = new BibliographicDate(1, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "1"));
    date2 = new BibliographicDate(1, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "1"));
    assertLessThan(date1, date2, "Date with greater year is greater than date with greater month");

    date1 = new BibliographicDate(2, 2, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "1"));
    date2 = new BibliographicDate(1, 1, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "1"));
    assertLessThan(date1, date2, "Date with greater year is greater than date with greater month and day");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertLessThan(BibliographicDate date1, BibliographicDate date2, String message)
  {
    assertTrue(ComparableUtils.is(date1).lessThan(date2), message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertGreaterThan(BibliographicDate date1, BibliographicDate date2, String message)
  {
    assertTrue(ComparableUtils.is(date1).greaterThan(date2), message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertLessThan(BibliographicYear year1, BibliographicYear year2, String message)
  {
    assertTrue(ComparableUtils.is(year1).lessThan(year2), message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertGreaterThan(BibliographicYear year1, BibliographicYear year2, String message)
  {
    assertTrue(ComparableUtils.is(year1).greaterThan(year2), message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void yearEqualityTest()
  {
    BibliographicYear year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, ""),
                      year2 = null;

    assertEquals(year1, year2, "Empty year should equal null");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, null);
    assertEquals(year1, null, "Empty year should equal null");

    assertTrue(BibliographicYear.isEmpty(year1), "Empty date should be empty");
    assertTrue(BibliographicYear.isEmpty(BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "")), "Empty date should be empty");
    assertTrue(BibliographicYear.isEmpty(null), "Null date should be empty");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "");
    assertNotEquals(year1, year2, "Alphabetic year should not equal empty year");

    assertFalse(BibliographicYear.isEmpty(year1), "Alphabetic year should not be empty");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "");
    assertNotEquals(year1, year2, "Year with only numeric portion should not equal empty year");

    assertFalse(BibliographicYear.isEmpty(year1), "Year with only numeric portion  should not be empty");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, "");
    assertNotEquals(year1, year2, "Year with only negative numeric portion should not equal empty year");

    assertFalse(BibliographicYear.isEmpty(year1), "Year with only negative numeric portion  should not be empty");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "A");
    assertNotEquals(year1, year2, "Year with same raw portion but different numeric portion should not be equal");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "B");
    assertNotEquals(year1, year2, "Year with same numeric portion but different raw portion should not be equal");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "B");
    assertNotEquals(year1, year2, "Year with same numeric portion but different raw portion should not be equal");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, null);
    assertEquals(year1, year2, "Blank string portion should be considered equal to null string portion");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "  ");
    assertEquals(year1, year2, "Empty string portion should be considered equal to blank string portion");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, " A ");
    assertEquals(year1, year2, "Space-padded string portion should be considered equal to non-space-padded string portion");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void yearComparisonTest()
  {
    BibliographicYear year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "");
    assertGreaterThan(year1, null, "Year with only numeric portion should be greater than null");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, "");
    assertGreaterThan(year1, null, "Year with only negative numeric portion should be greater than null");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "");
    BibliographicYear year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "");
    assertGreaterThan(year1, year2, "Year with only numeric portion should be greater than empty year");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, "");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "");
    assertGreaterThan(year1, year2, "Year with only negative numeric portion should be greater than empty year");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-2, "");
    assertGreaterThan(year1, year2, "If numeric portion is greater and raw portions are blank, year is greater");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, "");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "2");
    assertGreaterThan(year1, year2, "Numeric comparision trumps raw comparison");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(2, null);
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(1, "2");
    assertGreaterThan(year1, year2, "Numeric comparision trumps raw comparison");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "B");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(0, "A");
    assertGreaterThan(year1, year2, "If numeric portions are empty, compare raw values");

    year1 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, "A");
    year2 = BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(-1, "B");
    assertLessThan(year1, year2, "If numeric portions are equal, compare raw values");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void yearParseTest()
  {
    String msg = "Should parse BC, AD, etc.";

    BibliographicYear bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 BC");
    assertEquals(-300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 BC", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("250 B.C.");
    assertEquals(-250, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("250 B.C.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("26 BCE");
    assertEquals(-26, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("26 BCE", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 B.C.E.");
    assertEquals(-955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 B.C.E.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 AD");
    assertEquals(300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 AD", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 A.D.");
    assertEquals(955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 A.D.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 CE");
    assertEquals(300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 CE", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 C.E.");
    assertEquals(955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 C.E.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 bc");
    assertEquals(-300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 bc", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("250 b.c.");
    assertEquals(-250, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("250 b.c.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("26 bce");
    assertEquals(-26, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("26 bce", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 b.c.e.");
    assertEquals(-955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 b.c.e.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 ad");
    assertEquals(300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 ad", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 a.d.");
    assertEquals(955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 a.d.", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("300 ce");
    assertEquals(300, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 ce", bibYear.rawValue, msg);

    bibYear = BibliographicYear.fromRawStrWhereMinusOneEqualsOneBC("955 c.e.");
    assertEquals(955, bibYear.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 c.e.", bibYear.rawValue, msg);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
