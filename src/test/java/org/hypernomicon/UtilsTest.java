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

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

/**
 * Unit test class for the utility functions in the {@code Util} class.
 *
 * @see org.hypernomicon.util.Util
 */
class UtilsTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRemoveDuplicatesInPlace_WithDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "banana", "apple", "orange", "banana"));
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should have duplicates removed and maintain the order of first occurrence.");
  }

  @Test
  void testRemoveDuplicatesInPlace_NoDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "banana", "orange"));
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should remain unchanged as there are no duplicates.");
  }

  @Test
  void testRemoveDuplicatesInPlace_EmptyList()
  {
    List<String> list = new ArrayList<>();
    List<String> expected = new ArrayList<>();

    List<String> result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "An empty list should remain empty.");
  }

  @Test
  void testRemoveDuplicatesInPlace_SingleElementList()
  {
    List<String> list = new ArrayList<>(List.of("apple"));
    List<String> expected = List.of("apple");

    List<String> result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "A single-element list should remain unchanged.");
  }

  @Test
  void testRemoveDuplicatesInPlace_AllDuplicates()
  {
    List<String> list = new ArrayList<>(List.of("apple", "apple", "apple"));
    List<String> expected = List.of("apple");

    List<String> result = removeDuplicatesInPlace(list);

    assertEquals(expected, result, "The list should have only one element after removing all duplicates.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testConvertMultiLineStrToStrList_EmptyString()
  {
    String str = "";
    List<String> expected = List.of();

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_SingleLine()
  {
    String str = "apple";
    List<String> expected = List.of("apple");

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_MultipleLines()
  {
    String str = "apple\nbanana\norange";
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_LeadingTrailingBlanks()
  {
    String str = " \napple\nbanana\norange\n ";
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_IntermediateEmptiesNotOK()
  {
    String str = " \napple\n\nbanana\norange\n ";
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = convertMultiLineStrToStrList(str, false);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_IntermediateEmptiesOK()
  {
    String str = " \napple\n\nbanana\norange\n ";
    List<String> expected = List.of("apple", "", "banana", "orange");

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_AllEmptyLines()
  {
    String str = "\n\n\n";
    List<String> expected = List.of();

    List<String> result = convertMultiLineStrToStrList(str, false);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_AllEmptyLinesOK()
  {
    String str = "\n\n\n";
    List<String> expected = List.of();

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_OnlyBlanks()
  {
    String str = " \n \n \n ";
    List<String> expected = List.of();

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

  @Test
  void testConvertMultiLineStrToStrList_TrailingNewline()
  {
    String str = "apple\nbanana\norange\n";
    List<String> expected = List.of("apple", "banana", "orange");

    List<String> result = convertMultiLineStrToStrList(str, true);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testStrListToStr_NullList()
  {
    List<String> list = null;
    String expected = "";

    String result = strListToStr(list, true, false);

    assertEquals(expected, result);

    result = strListToStr(list, true, true);

    assertEquals(expected, result);

    result = strListToStr(list, false, false);

    assertEquals(expected, result);

    result = strListToStr(list, false, true);

    assertEquals(expected, result);

  }

  @Test
  void testStrListToStr_EmptyList()
  {
    List<String> list = List.of();
    String expected = "";

    String result = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_SingleLine()
  {
    List<String> list = List.of("apple");
    String expected = "apple";

    String result = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_MultipleLines()
  {
    List<String> list = List.of("apple", "banana", "orange");
    String expected = "apple\nbanana\norange";

    String result = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_IncludeEmptyLines()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple\n\nbanana\norange\n";

    String result = strListToStr(list, true, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_ExcludeEmptyLines()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple\nbanana\norange";

    String result = strListToStr(list, false, false);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_UseSystemNewLineChar()
  {
    List<String> list = List.of("apple", "banana", "orange");
    String expected = "apple" + System.lineSeparator() + "banana" + System.lineSeparator() + "orange";

    String result = strListToStr(list, true, true);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_IncludeEmptyLinesWithSystemNewLine()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple" + System.lineSeparator() + System.lineSeparator() + "banana" + System.lineSeparator() + "orange" + System.lineSeparator();

    String result = strListToStr(list, true, true);

    assertEquals(expected, result);
  }

  @Test
  void testStrListToStr_ExcludeEmptyLinesWithSystemNewLine()
  {
    List<String> list = List.of("apple", "", "banana", "orange", "");
    String expected = "apple" + System.lineSeparator() + "banana" + System.lineSeparator() + "orange";

    String result = strListToStr(list, false, true);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testConvertToSingleLine_SimpleNewlines()
  {
    String input = "This is a line.\nThis is another line.\nThis is yet another line.";
    String expected = "This is a line. This is another line. This is yet another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_MultipleNewlines()
  {
    String input = "This is a line.\n\n\nThis is another line.\r\n\r\nThis is yet another line.";
    String expected = "This is a line. This is another line. This is yet another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_VerticalSpaces()
  {
    String input = "This is a line.\u000B\u000BThis is another line.";
    String expected = "This is a line. This is another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_MixedSpaces()
  {
    String input = "This is a line.\n\u000B\nThis is another line.";
    String expected = "This is a line. This is another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_UnknownCharacter()
  {
    String input = "This is a line.\ufffd\nThis is another line.";
    String expected = "This is a line. This is another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_HorizontalSpaces()
  {
    String input = "This is a line.\u0020\u0020This is another line.";
    String expected = "This is a line.\u0020\u0020This is another line.";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_EmptyString()
  {
    String input = "";
    String expected = "";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_OnlyNewlines()
  {
    String input = "\n\n\n";
    String expected = " ";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

  @Test
  void testConvertToSingleLine_OnlyVerticalSpaces()
  {
    String input = "\u000B\u000B\u000B";
    String expected = " ";

    String result = convertToSingleLine(input);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testTrimLines_EmptyString()
  {
    String input = "";
    String expected = "";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_NullString()
  {
    String input = null;
    String expected = "";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_SingleLine()
  {
    String input = "   apple   ";
    String expected = "apple";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MultipleLines()
  {
    String input = "   apple   \n   banana   \n   orange   ";
    String expected = "apple\nbanana\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MultipleLinesWithEmptyLines()
  {
    String input = "   apple   \n   \n   banana   \n   orange   ";
    String expected = "apple\n\nbanana\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_LeadingAndTrailingWhitespace()
  {
    String input = " \t \n   apple   \n   banana   \n   orange   \n \t ";
    String expected = "apple\nbanana\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_WhitespaceOnlyLines()
  {
    String input = "   \n   \n   \n";
    String expected = "";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_UnknownCharacter()
  {
    String input = "apple \ufffd\nbanana \ufffd\norange \ufffd";
    String expected = "apple\nbanana\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_MixedNewlinesAndVerticalSpaces()
  {
    String input = "apple\n\nbanana\u000B\u000Borange";
    String expected = "apple\n\nbanana\n\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_OnlyNewlines()
  {
    String input = "\n\n\n";
    String expected = "";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_OnlyVerticalSpaces()
  {
    String input = "\u000B\u000B\u000B";
    String expected = "";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_TrailingNewline()
  {
    String input = "apple\nbanana\norange\n";
    String expected = "apple\nbanana\norange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_IntermediateSpaces()
  {
    String input = "apple  banana  orange";
    String expected = "apple  banana  orange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_TabSpaces()
  {
    String input = "   apple\tbanana\torange   ";
    String expected = "apple\tbanana\torange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

  @Test
  void testTrimLines_CombinationWhitespaces()
  {
    String input = "   apple\nbanana\torange\u000B";
    String expected = "apple\nbanana\torange";

    String result = trimLines(input);

    assertEquals(expected, result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
