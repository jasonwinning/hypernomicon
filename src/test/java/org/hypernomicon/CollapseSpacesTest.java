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

import static org.hypernomicon.util.StringUtil.*;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

//---------------------------------------------------------------------------

class CollapseSpacesTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @BeforeEach
  void clearThreadLocalBuffer()
  {
    // ensure each test starts with a fresh buffer
    clearCollapseBufferForTest();
  }

  @Test
  void testEmptyString()
  {
    String in = "";
    String out = collapseSpaces(in);
    assertSame(in, out, "Empty string should be returned as-is");
  }

  @Test
  void testNoSpaces()
  {
    String in = "abcdef";
    String out = collapseSpaces(in);
    assertSame(in, out, "String with no spaces should be returned as-is");
  }

  @Test
  void testOnlySingleSpaces()
  {
    String in = "a b c d";
    String out = collapseSpaces(in);
    assertSame(in, out, "String with only single spaces should be returned as-is");
  }

  @Test
  void testExactlyTwoSpaces()
  {
    String in = "a  b";
    String expected = "a b";
    String out = collapseSpaces(in);

    assertEquals(expected, out);
    assertNotSame(in, out, "Collapsed string must be a new instance when spaces are removed");
  }

  @Test
  void testThreeSpaces()
  {
    String in = "a   b";
    String out = collapseSpaces(in);
    assertEquals("a b", out);
  }

  @Test
  void testMultipleRunsOfSpaces()
  {
    String in = "foo    bar     baz";
    String out = collapseSpaces(in);
    assertEquals("foo bar baz", out);
  }

  @Test
  void testLeadingSpaces()
  {
    String in = "   hello";
    String out = collapseSpaces(in);
    assertEquals(" hello", out, "Leading runs of spaces should collapse to single space");
  }

  @Test
  void testTrailingSpaces()
  {
    String in = "world   ";
    String out = collapseSpaces(in);
    assertEquals("world ", out, "Trailing runs of spaces should collapse to single space");
  }

  @Test
  void testAllSpaces()
  {
    String in = "     ";
    String out = collapseSpaces(in);
    assertEquals(" ", out, "String of only spaces should collapse to one space");
  }

  @Test
  void testLongStringTriggersBufferGrowth()
  {
    // Build a 200-char string with a run of two spaces in the middle

    String in = "x".repeat(90) + "  " + "y".repeat(108);  // two spaces to collapse

    String out = collapseSpaces(in);

    assertEquals(199, out.length(), "Length should shrink by exactly one");
    assertTrue(out.startsWith("x"), "Prefix preserved");
    assertTrue(out.endsWith("y"), "Suffix preserved");
    assertNotSame(in, out, "Should return new String on collapse");
  }

  @Test
  void testDoesNotTouchTabsOrOtherWhitespace()
  {
    // Only spaces should be collapsed; tabs remain
    String in = "A\t \t  \tB";
    String out = collapseSpaces(in);
    // The two spaces between the two tabs should collapse to one
    assertEquals("A\t \t \tB", out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
