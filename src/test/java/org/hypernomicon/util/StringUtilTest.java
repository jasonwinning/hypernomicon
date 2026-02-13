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

import static org.hypernomicon.util.StringUtil.*;

import java.util.List;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Unit test class for the utility functions in the {@code StringUtil} class.
 *
 * @see org.hypernomicon.util.StringUtil
 */
class StringUtilTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testConvertMultiLineStrToStrList_EmptyString()
  {
    List<String> expected = List.of(),
                 result   = convertMultiLineStrToStrList("", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_SingleLine()
  {
    List<String> expected = List.of("apple"),
                 result   = convertMultiLineStrToStrList("apple", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_MultipleLines()
  {
    List<String> expected = List.of("apple", "banana", "orange"),
                 result   = convertMultiLineStrToStrList("apple\nbanana\norange", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_LeadingTrailingBlanks()
  {
    List<String> expected = List.of("apple", "banana", "orange"),
                 result   = convertMultiLineStrToStrList(" \napple\nbanana\norange\n ", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_IntermediateEmptiesNotOK()
  {
    List<String> expected = List.of("apple", "banana", "orange"),
                 result   = convertMultiLineStrToStrList(" \napple\n\nbanana\norange\n ", false);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_IntermediateEmptiesOK()
  {
    List<String> expected = List.of("apple", "", "banana", "orange"),
                 result   = convertMultiLineStrToStrList(" \napple\n\nbanana\norange\n ", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_AllEmptyLines()
  {
    List<String> expected = List.of(),
                 result   = convertMultiLineStrToStrList("\n\n\n", false);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_AllEmptyLinesOK()
  {
    List<String> expected = List.of(),
                 result   = convertMultiLineStrToStrList("\n\n\n", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_OnlyBlanks()
  {
    List<String> expected = List.of(),
                 result   = convertMultiLineStrToStrList(" \n \n \n ", true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_TrailingNewline()
  {
    List<String> expected = List.of("apple", "banana", "orange"),
                 result   = convertMultiLineStrToStrList("apple\nbanana\norange\n", true);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testStrListToStr_NullList()
  {
    String expected = "";

    assertEquals(expected, strListToStr(null, true , false));
    assertEquals(expected, strListToStr(null, true , true ));
    assertEquals(expected, strListToStr(null, false, false));
    assertEquals(expected, strListToStr(null, false, true ));
  }

  @Test
  void testStrListToStr_EmptyList()
  {
    List<String> list = List.of();
    String expected = "",
           result   = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_SingleLine()
  {
    List<String> list = List.of("apple");
    String expected = "apple",
           result   = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_MultipleLines()
  {
    List<String> list = List.of("apple", "banana", "orange");
    String expected = "apple\nbanana\norange",
           result   = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_IncludeEmptyLines()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple\n\nbanana\norange\n",
           result   = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_ExcludeEmptyLines()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple\nbanana\norange",
           result   = strListToStr(list, false, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_UseSystemNewLineChar()
  {
    List<String> list = List.of("apple", "banana", "orange");
    String expected = "apple" + System.lineSeparator() + "banana" + System.lineSeparator() + "orange",
           result   = strListToStr(list, true, true);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_IncludeEmptyLinesWithSystemNewLine()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple" + System.lineSeparator() + System.lineSeparator() + "banana" + System.lineSeparator() + "orange" + System.lineSeparator(),
           result   = strListToStr(list, true, true);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_ExcludeEmptyLinesWithSystemNewLine()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple" + System.lineSeparator() + "banana" + System.lineSeparator() + "orange",
           result   = strListToStr(list, false, true);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testConvertToSingleLine_SimpleNewlines()
  {
    String input    = "This is a line.\nThis is another line.\nThis is yet another line.",
           expected = "This is a line. This is another line. This is yet another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_MultipleNewlines()
  {
    String input    = "This is a line.\n\n\nThis is another line.\r\n\r\nThis is yet another line.",
           expected = "This is a line. This is another line. This is yet another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_VerticalSpaces()
  {
    String input    = "This is a line.\u000B\u000BThis is another line.",
           expected = "This is a line. This is another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_MixedSpaces()
  {
    String input    = "This is a line.\n\u000B\nThis is another line.",
           expected = "This is a line. This is another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_UnknownCharacter()
  {
    String input    = "This is a line.\ufffd\nThis is another line.",
           expected = "This is a line. This is another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_HorizontalSpaces()
  {
    String input    = "This is a line.\u0020\u0020This is another line.",
           expected = "This is a line.\u0020\u0020This is another line.",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_EmptyString()
  {
    String input    = "",
           expected = "",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_OnlyNewlines()
  {
    String input    = "\n\n\n",
           expected = " ",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_OnlyVerticalSpaces()
  {
    String input    = "\u000B\u000B\u000B",
           expected = " ",
           result   = convertToSingleLine(input);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testTrimLines_EmptyString()
  {
    String input    = "",
           expected = "",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_NullString()
  {
    assertEquals("", trimLines(null));
  }

  @Test
  void testTrimLines_SingleLine()
  {
    String input    = "   apple   ",
           expected = "apple",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MultipleLines()
  {
    String input    = "   apple   \n   banana   \n   orange   ",
           expected = "apple\nbanana\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MultipleLinesWithEmptyLines()
  {
    String input    = "   apple   \n   \n   banana   \n   orange   ",
           expected = "apple\n\nbanana\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_LeadingAndTrailingWhitespace()
  {
    String input    = " \t \n   apple   \n   banana   \n   orange   \n \t ",
           expected = "apple\nbanana\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_WhitespaceOnlyLines()
  {
    String input    = "   \n   \n   \n",
           expected = "",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_UnknownCharacter()
  {
    String input    = "apple \ufffd\nbanana \ufffd\norange \ufffd",
           expected = "apple\nbanana\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MixedNewlinesAndVerticalSpaces()
  {
    String input    = "apple\n\nbanana\u000B\u000Borange",
           expected = "apple\n\nbanana\n\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_OnlyNewlines()
  {
    String input    = "\n\n\n",
           expected = "",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_OnlyVerticalSpaces()
  {
    String input    = "\u000B\u000B\u000B",
           expected = "",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_TrailingNewline()
  {
    String input    = "apple\nbanana\norange\n",
           expected = "apple\nbanana\norange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_IntermediateSpaces()
  {
    String input    = "apple  banana  orange",
           expected = "apple  banana  orange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_TabSpaces()
  {
    String input    = "   apple\tbanana\torange   ",
           expected = "apple\tbanana\torange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_CombinationWhitespaces()
  {
    String input    = "   apple\nbanana\torange\u000B",
           expected = "apple\nbanana\torange",
           result   = trimLines(input);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
