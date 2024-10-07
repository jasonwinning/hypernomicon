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

package org.hypernomicon;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.bib.zotero.ZoteroDate;
import org.hypernomicon.model.items.BibliographicDate;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class ZoteroDateTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void fromUserStrTest()
  {

  //---------------------------------------------------------------------------

    BibliographicDate bibDate = BibliographicDate.fromUserStr(null);

    assertEquals(BibliographicDate.EMPTY_DATE, bibDate, "Should return empty date object for null input string");

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("");

    assertEquals(BibliographicDate.EMPTY_DATE, bibDate, "Should return empty date object for empty input string");

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr(" ");

    assertEquals(BibliographicDate.EMPTY_DATE, bibDate, "Should return empty date object for blank input string");

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("June 26, 2010");
    String msg = "Should parse English month before date";

    assertEquals(6, bibDate.month, msg);
    assertEquals(26, bibDate.day, msg);
    assertEquals(2010, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("2010", bibDate.year.rawValue, msg);

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("26 June 2010");
    msg = "Should parse English month after date";

    assertEquals(6, bibDate.month, msg);
    assertEquals(26, bibDate.day, msg);
    assertEquals(2010, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("2010", bibDate.year.rawValue, msg);

  //---------------------------------------------------------------------------

    msg = "Should parse two- and three-digit dates with leading zeros";

    bibDate = BibliographicDate.fromUserStr("001");
    assertEquals(1, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("1", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("0001");
    assertEquals(1, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("1", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("012");
    assertEquals(12, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("12", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("0012");
    assertEquals(12, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("12", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("0123");
    assertEquals(123, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("123", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("01/01/08");
    assertEquals(2008, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("2008", bibDate.year.rawValue, msg);   // Always convert raw numeric value to match full parsed value

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("1/1/68");

    assertEquals(1968, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), "Should parse two-digit year greater than current year as previous century");

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("1/1/19");

    assertEquals(2019, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), "Should parse two-digit year less than or equal to current year as current century");

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("8/2020");

    msg = "Should parse one-digit month and four-digit year";

    assertEquals(8, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(2020, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("08/2020");

    msg = "Should parse two-digit month with leading zero and four-digit year";

    assertEquals(8, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(2020, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("1");

    msg = "Should parse string with just month number";

    assertEquals(1, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(0, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    bibDate = BibliographicDate.fromUserStr("25");

    msg = "Should parse string with just day number";

    assertEquals(0, bibDate.month, msg);
    assertEquals(25, bibDate.day, msg);
    assertEquals(0, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    ZoteroDate zoteroDate = ZoteroDate.fromUserStr("\u56db\u6708 26, 2010", "zh-CN");
    bibDate = new BibliographicDate(zoteroDate.getDay(), zoteroDate.getMonth(), zoteroDate.getRawYear(), false);

    msg = "Should parse Chinese month";

    assertEquals(4, bibDate.month, msg);
    assertEquals(26, bibDate.day, msg);
    assertEquals(2010, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    msg = "Should parse date with day suffix";

    bibDate = BibliographicDate.fromUserStr("Jun 5th, 2012");
    assertEquals(6, bibDate.month, msg);
    assertEquals(5, bibDate.day, msg);
    assertEquals(2012, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

    bibDate = BibliographicDate.fromUserStr("August 3rd, 2012");
    assertEquals(8, bibDate.month, msg);
    assertEquals(3, bibDate.day, msg);
    assertEquals(2012, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

    bibDate = BibliographicDate.fromUserStr("Sept. 22nd");
    assertEquals(9, bibDate.month, msg);
    assertEquals(22, bibDate.day, msg);
    assertEquals(0, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

    bibDate = BibliographicDate.fromUserStr("2024, Dec 31st");
    assertEquals(12, bibDate.month, msg);
    assertEquals(31, bibDate.day, msg);
    assertEquals(2024, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);

  //---------------------------------------------------------------------------

    msg = "Should parse BC, AD, etc.";

    bibDate = BibliographicDate.fromUserStr("300 BC");
    assertEquals(0, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(-300, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 BC", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("250 B.C.");
    assertEquals(0, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(-250, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("250 B.C.", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("26 BCE");
    assertEquals(0, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(-26, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("26 BCE", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("Jun 19th, 955 B.C.E.");
    assertEquals(6, bibDate.month, msg);
    assertEquals(19, bibDate.day, msg);
    assertEquals(-955, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 B.C.E.", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("300 AD");
    assertEquals(0, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(300, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 AD", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("955 A.D., Jun 19th");
    assertEquals(6, bibDate.month, msg);
    assertEquals(19, bibDate.day, msg);
    assertEquals(955, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 A.D.", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("300 CE");
    assertEquals(0, bibDate.month, msg);
    assertEquals(0, bibDate.day, msg);
    assertEquals(300, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("300 CE", bibDate.year.rawValue, msg);

    bibDate = BibliographicDate.fromUserStr("955 C.E., Jun 19th");
    assertEquals(6, bibDate.month, msg);
    assertEquals(19, bibDate.day, msg);
    assertEquals(955, bibDate.year.numericValueWhereMinusOneEqualsOneBC(), msg);
    assertEquals("955 C.E.", bibDate.year.rawValue, msg);

  //---------------------------------------------------------------------------

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
