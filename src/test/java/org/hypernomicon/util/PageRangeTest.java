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

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

import java.util.*;

//---------------------------------------------------------------------------

class PageRangeTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSorting()
  {
    List<PageRange> list = new ArrayList<>();
    list.add(new PageRange("XIV"));
    list.add(new PageRange("3"));
    list.add(new PageRange("12"));
    list.add(new PageRange("II"));
    list.add(new PageRange("Chapter One"));
    list.add(new PageRange("Appendix A"));

    Collections.sort(list);

    assertEquals("II", list.get(0).toString());
    assertEquals("XIV", list.get(1).toString());
    assertEquals("3", list.get(2).toString());
    assertEquals("12", list.get(3).toString());
    assertEquals("Appendix A", list.get(4).toString());
    assertEquals("Chapter One", list.get(5).toString());

    PageRange pr1 = new PageRange(null);
    PageRange pr2 = new PageRange(null);

    assertEquals(pr1, pr2);

    pr1 = new PageRange("");
    pr2 = new PageRange(null);

    assertEquals(pr1, pr2);

    pr1 = new PageRange(null);
    pr2 = new PageRange("");

    assertEquals(pr1, pr2);

    pr1 = new PageRange("");
    pr2 = new PageRange("");

    assertEquals(pr1, pr2);

    assertTrue(new PageRange("").compareTo(new PageRange("hi")) < 0);

    assertTrue(new PageRange(null).compareTo(new PageRange("hi")) < 0);
  }

  @Test
  void testRomanNumeralsFirst()
  {
    PageRange pr1 = new PageRange("X");
    PageRange pr2 = new PageRange("3");

    assertTrue(pr1.compareTo(pr2) < 0);
  }

  @Test
  void testPositiveIntegerSorting()
  {
    PageRange pr1 = new PageRange("5");
    PageRange pr2 = new PageRange("15");

    assertTrue(pr1.compareTo(pr2) < 0);
  }

  @Test
  void testStringSorting()
  {
    PageRange pr1 = new PageRange("Alpha");
    PageRange pr2 = new PageRange("Beta");

    assertTrue(pr1.compareTo(pr2) < 0);
  }

  @Test
  void testRomanNumeralOrder()
  {
    PageRange pr1 = new PageRange("I");
    PageRange pr2 = new PageRange("V");
    PageRange pr3 = new PageRange("X");
    PageRange pr4 = new PageRange("L");
    PageRange pr5 = new PageRange("C");
    PageRange pr6 = new PageRange("D");
    PageRange pr7 = new PageRange("M");

    List<PageRange> list = Arrays.asList(pr5, pr3, pr7, pr2, pr4, pr6, pr1);
    Collections.sort(list);

    assertEquals("I", list.get(0).toString());
    assertEquals("V", list.get(1).toString());
    assertEquals("X", list.get(2).toString());
    assertEquals("L", list.get(3).toString());
    assertEquals("C", list.get(4).toString());
    assertEquals("D", list.get(5).toString());
    assertEquals("M", list.get(6).toString());
  }

  @Test
  void testRomanNumeralCaseInsensitive()
  {
    PageRange pr1 = new PageRange("xiv"),
              pr2 = new PageRange("III");

    assertTrue(pr2.compareTo(pr1) < 0);
  }

  @Test
  void testNumericalValueForRomanNumerals()
  {
    assertEquals(1, new PageRange("I").numericalValue());
    assertEquals(4, new PageRange("IV-XI").numericalValue());
    assertEquals(9, new PageRange("IX").numericalValue());
    assertEquals(40, new PageRange("XL-C1").numericalValue());
    assertEquals(90, new PageRange("XC").numericalValue());
    assertEquals(400, new PageRange("CD-D'").numericalValue());
    assertEquals(900, new PageRange("CM").numericalValue());
    assertEquals(3999, new PageRange("MMMCMXCIX").numericalValue());
    assertEquals(21, new PageRange("XXI").numericalValue());
    assertEquals(58, new PageRange("LVIII").numericalValue());
    assertEquals(1994, new PageRange("MCMXCIV").numericalValue());
    assertEquals(444, new PageRange("CDXLIV").numericalValue());
    assertEquals(6, new PageRange("VI").numericalValue());
    assertEquals(10, new PageRange("X").numericalValue());
    assertEquals(50, new PageRange("L").numericalValue());
    assertEquals(100, new PageRange("C").numericalValue());
    assertEquals(500, new PageRange("D").numericalValue());
    assertEquals(1000, new PageRange("M").numericalValue());
    assertEquals(1987, new PageRange("MCMLXXXVII").numericalValue());
  }

  @Test
  void testNumericalValueForRomanNumeralsCaseInsensitive()
  {
    assertEquals(1, new PageRange("i").numericalValue());
    assertEquals(4, new PageRange("iv-xi").numericalValue());
    assertEquals(9, new PageRange("ix").numericalValue());
    assertEquals(90, new PageRange("xc").numericalValue());
  }

  @Test
  void testNumericalValueForInvalidRomanNumerals()
  {
    assertEquals(0, new PageRange("IIII").numericalValue());
    assertEquals(0, new PageRange("VV").numericalValue());
    assertEquals(0, new PageRange("XXXX").numericalValue());
    assertEquals(0, new PageRange("MMMM").numericalValue());
    assertEquals(0, new PageRange("ABC").numericalValue());
    assertEquals(0, new PageRange("IC").numericalValue());
    assertEquals(0, new PageRange("MCMC").numericalValue());
    assertEquals(0, new PageRange("A123").numericalValue());
    assertEquals(0, new PageRange("MMMMMMMMMM").numericalValue());
  }

  @Test
  void testNumericalValueForDecimalNumbers()
  {
    assertEquals(3, new PageRange("3").numericalValue());
    assertEquals(15, new PageRange("15-").numericalValue());
    assertEquals(42, new PageRange("42").numericalValue());
    assertEquals(1, new PageRange("1").numericalValue());
    assertEquals(123, new PageRange("123").numericalValue());
  }

  @Test
  void testNumericalValueForMixedInput()
  {
    assertEquals(1, new PageRange("1V").numericalValue());
    assertEquals(50, new PageRange("L3").numericalValue());
  }

  @Test
  void testStrings()
  {
    assertEquals(0, new PageRange("Chapter One").numericalValue());
    assertEquals(0, new PageRange("Introduction").numericalValue());
  }

  @Test
  void testZeroAndNegative()
  {
    assertEquals(0, new PageRange("0").numericalValue());
    assertEquals(0, new PageRange("-3").numericalValue());
  }

  @Test
  void testMixedValidInvalidRomanNumerals()
  {
    assertEquals(0, new PageRange("IVX").numericalValue());
    assertEquals(0, new PageRange("XLX").numericalValue());
    assertEquals(0, new PageRange("xM").numericalValue());
    assertEquals(0, new PageRange("IC").numericalValue());
    assertEquals(0, new PageRange("vX").numericalValue());
    assertEquals(0, new PageRange("ld").numericalValue());
    assertEquals(0, new PageRange("Dm").numericalValue());
    assertEquals(0, new PageRange("iVI").numericalValue());
    assertEquals(0, new PageRange("IxX").numericalValue());
    assertEquals(0, new PageRange("iXi").numericalValue());
    assertEquals(0, new PageRange("CcM").numericalValue());
    assertEquals(0, new PageRange("iXL").numericalValue());
    assertEquals(0, new PageRange("XVX").numericalValue());
    assertEquals(0, new PageRange("IiV").numericalValue());
    assertEquals(0, new PageRange("VIIV").numericalValue());
  }

  @Test
  void testComparisonWithNegativeNumbers()
  {
    PageRange pr1 = new PageRange("-1");
    PageRange pr2 = new PageRange("1");
    PageRange pr3 = new PageRange("-1");

    List<PageRange> list = Arrays.asList(pr2, pr1, pr3);
    Collections.sort(list);

    assertEquals("1", list.get(0).toString());
    assertEquals("-1", list.get(1).toString());
    assertEquals("-1", list.get(2).toString());
  }

  @Test
  void testComparisonWithMixedInvalidRomanNumerals()
  {
    PageRange pr1 = new PageRange("XIV");
    PageRange pr2 = new PageRange("IIIX");
    PageRange pr3 = new PageRange("3");
    PageRange pr4 = new PageRange("X");
    PageRange pr5 = new PageRange("IV");

    List<PageRange> list = Arrays.asList(pr1, pr2, pr3, pr4, pr5);
    Collections.sort(list);

    assertEquals("IV", list.get(0).toString());
    assertEquals("X", list.get(1).toString());
    assertEquals("XIV", list.get(2).toString());
    assertEquals("3", list.get(3).toString());
    assertEquals("IIIX", list.get(4).toString());
  }

  @Test
  void testComparisonWithMixedNumbersAndLetters()
  {
    PageRange pr1 = new PageRange("1A");
    PageRange pr2 = new PageRange("10B");
    PageRange pr3 = new PageRange("A1");
    PageRange pr4 = new PageRange("B10");

    List<PageRange> list = Arrays.asList(pr4, pr3, pr2, pr1);
    Collections.sort(list);

    assertEquals("1A", list.get(0).toString());
    assertEquals("10B", list.get(1).toString());
    assertEquals("A1", list.get(2).toString());
    assertEquals("B10", list.get(3).toString());
  }

  @Test
  void testComparisonWithPunctuation()
  {
    PageRange pr1 = new PageRange("XIV!");
    PageRange pr2 = new PageRange("!XIV");
    PageRange pr3 = new PageRange("!");
    PageRange pr4 = new PageRange("XIV");
    PageRange pr5 = new PageRange("3!");

    List<PageRange> list = Arrays.asList(pr1, pr2, pr3, pr4, pr5);
    Collections.sort(list);

    assertEquals("XIV", list.get(0).toString());
    assertEquals("XIV!", list.get(1).toString());
    assertEquals("3!", list.get(2).toString());
    assertEquals("!", list.get(3).toString());
    assertEquals("!XIV", list.get(4).toString());
  }

  @Test
  void testComparisonWithStartingTrailingSpaces()
  {
    PageRange pr1 = new PageRange(" XIV ");
    PageRange pr2 = new PageRange("  XIV");
    PageRange pr3 = new PageRange("XIV  ");
    PageRange pr4 = new PageRange(" XIV");

    List<PageRange> list = Arrays.asList(pr1, pr2, pr3, pr4);
    Collections.sort(list);

    assertEquals("XIV", list.get(0).toString());
    assertEquals("XIV", list.get(1).toString());
    assertEquals("XIV", list.get(2).toString());
    assertEquals("XIV", list.get(3).toString());
  }

  @Test
  void testMixedCases()
  {
    PageRange pr1 = new PageRange("IV");
    PageRange pr2 = new PageRange("iv");
    PageRange pr3 = new PageRange("X");
    PageRange pr4 = new PageRange("x");

    List<PageRange> list = Arrays.asList(pr4, pr3, pr2, pr1);
    Collections.sort(list);

    assertEquals("IV", list.get(0).toString().toUpperCase());
    assertEquals("IV", list.get(1).toString().toUpperCase());
    assertEquals("X", list.get(2).toString().toUpperCase());
    assertEquals("X", list.get(3).toString().toUpperCase());
  }

  @Test
  void testSpecialUnicodeCharacters()
  {
    PageRange pr1 = new PageRange("IV\u00A9"); // Roman numeral with copyright symbol
    assertEquals(4, pr1.numericalValue());
    PageRange pr2 = new PageRange("\u2160"); // Unicode Roman numeral for "I"
    assertEquals(0, pr2.numericalValue());
  }

  @Test
  void testOnlyNumbersOrLetters()
  {
    PageRange pr1 = new PageRange("123456");
    assertEquals(123456, pr1.numericalValue());
    PageRange pr2 = new PageRange("ABCDEF");
    assertEquals(0, pr2.numericalValue());
  }

  @Test
  void testMixedRomanAndDecimalWithoutSpace()
  {
    PageRange pr = new PageRange("V10");
    assertEquals(5, pr.numericalValue()); // Expected to match only the Roman numeral part
  }

  @Test
  void testEmbeddedNumbers()
  {
    PageRange pr1 = new PageRange("Part3");
    assertEquals(0, pr1.numericalValue()); // Assuming your class doesn't support embedded numbers without space
    PageRange pr2 = new PageRange("Vol4Issue2");
    assertEquals(0, pr2.numericalValue());
  }

  @Test
  void testMultipleValidRomanNumerals()
  {
    assertEquals(0, new PageRange("VIIII").numericalValue());
    assertEquals(0, new PageRange("IIIIX").numericalValue());
  }

  @Test
  void testMixedInputsWithSpacesAndPunctuation()
  {
    assertEquals(14, new PageRange("XIV 3, ABC").numericalValue());
    assertEquals(14, new PageRange("XIV,").numericalValue());
    assertEquals(3, new PageRange("3, XIV").numericalValue());
  }

  @Test
  void testNonStandardRomanNumerals()
  {
    assertEquals(0, new PageRange("IVXII").numericalValue());
    assertEquals(0, new PageRange("XLIVX").numericalValue());
  }

  @Test
  void testCombosWithLeadingAndTrailingDigits()
  {
    assertEquals(123, new PageRange("123XIV").numericalValue());
    assertEquals(14, new PageRange("XIV123").numericalValue());
  }

  @Test
  void testVeryShortStrings()
  {
    assertEquals(0, new PageRange("").numericalValue());
    assertEquals(0, new PageRange(" ").numericalValue());
    assertEquals(0, new PageRange("A").numericalValue());
    assertEquals(1, new PageRange("I").numericalValue());
    assertEquals(1, new PageRange("000000001").numericalValue());
  }

  @Test
  void testWithLeadingAndTrailingSpaces()
  {
    assertEquals(123, new PageRange("  123 ").numericalValue());
    assertEquals(123, new PageRange("  123.5 ").numericalValue());
    assertEquals(123, new PageRange("123 ").numericalValue());

    assertEquals("XIV", new PageRange(" XIV ").toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
