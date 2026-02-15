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

package org.hypernomicon.testTools.xmlDiff;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

class XmlDiffCtrlrTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void plainXmlLinesPassThrough()
  {
    List<String> input = List.of
    (
      "  <record>",
      "    <name>Test</name>",
      "  </record>"
    );

    assertEquals(input, XmlDiffCtrlr.processLines(input));
  }

//---------------------------------------------------------------------------

  @Test
  void splitsInlineTagsOntoSeparateLines()
  {
    List<String> input = List.of("&lt;p&gt;&lt;br&gt;&lt;/p&gt;");

    assertEquals(List.of("&lt;p&gt;", "&lt;br&gt;", "&lt;/p&gt;"),
                 XmlDiffCtrlr.processLines(input));
  }

//---------------------------------------------------------------------------

  @Test
  void splitsDescriptionLineIntoComponents()
  {
    List<String> input = List.of("  <description>&lt;p&gt;text&lt;/p&gt;</description>");

    assertEquals(List.of("  <description>", "&lt;p&gt;", "text", "&lt;/p&gt;", "</description>"),
                 XmlDiffCtrlr.processLines(input));
  }

//---------------------------------------------------------------------------

  @Test
  void inlineAndMultiLineNormalizeIdentically()
  {
    List<String> inline = List.of("  <description>&lt;p&gt;&lt;br&gt;&lt;/p&gt;</description>");

    List<String> multiLine = List.of
    (
      "  <description>&lt;p&gt;",
      "  &lt;br&gt;",
      " &lt;/p&gt;</description>"
    );

    assertEquals(XmlDiffCtrlr.processLines(inline),
                 XmlDiffCtrlr.processLines(multiLine));
  }

//---------------------------------------------------------------------------

  @Test
  void whitespaceBetweenTagsIsNormalized()
  {
    List<String> withSpaces    = List.of("&lt;p&gt;   &lt;br&gt;   &lt;/p&gt;");
    List<String> withoutSpaces = List.of("&lt;p&gt;&lt;br&gt;&lt;/p&gt;");

    assertEquals(XmlDiffCtrlr.processLines(withoutSpaces),
                 XmlDiffCtrlr.processLines(withSpaces));
  }

//---------------------------------------------------------------------------

  @Test
  void textContentDifferencesArePreserved()
  {
    List<String> version1 = XmlDiffCtrlr.processLines(List.of("&lt;p&gt;Hello&lt;/p&gt;"));
    List<String> version2 = XmlDiffCtrlr.processLines(List.of("&lt;p&gt;Goodbye&lt;/p&gt;"));

    assertNotEquals(version1, version2);
  }

//---------------------------------------------------------------------------

  @Test
  void tagDifferencesArePreserved()
  {
    List<String> version1 = XmlDiffCtrlr.processLines(List.of("&lt;p&gt;text&lt;/p&gt;"));
    List<String> version2 = XmlDiffCtrlr.processLines(List.of("&lt;p&gt;&lt;b&gt;text&lt;/b&gt;&lt;/p&gt;"));

    assertNotEquals(version1, version2);
  }

//---------------------------------------------------------------------------

  @Test
  void attributeDifferencesArePreserved()
  {
    List<String> version1 = XmlDiffCtrlr.processLines(List.of("&lt;p style=\"color:red\"&gt;text&lt;/p&gt;"));
    List<String> version2 = XmlDiffCtrlr.processLines(List.of("&lt;p style=\"color:blue\"&gt;text&lt;/p&gt;"));

    assertNotEquals(version1, version2);
  }

//---------------------------------------------------------------------------

  @Test
  void stripsIndentationAfterEscapedClosingTag()
  {
    List<String> input = List.of
    (
      "&lt;/p&gt;",
      "    &lt;p&gt;next&lt;/p&gt;"
    );

    List<String> result = XmlDiffCtrlr.processLines(input);

    assertEquals("&lt;p&gt;", result.get(1),
                 "Line after &gt; should have indentation stripped");
  }

//---------------------------------------------------------------------------

  @Test
  void structuralXmlIndentationIsPreserved()
  {
    List<String> input = List.of
    (
      "  <description>&lt;p&gt;text&lt;/p&gt;</description>",
      "  <search_key>value</search_key>"
    );

    List<String> result = XmlDiffCtrlr.processLines(input);

    assertEquals("  <search_key>value</search_key>", result.getLast(),
                 "Non-entity XML lines should keep indentation");
  }

//---------------------------------------------------------------------------

  @Test
  void leadingWhitespaceAfterTagIsStripped()
  {
    // Documents a known limitation: leading whitespace after an escaped tag
    // (e.g. inside a <pre> block) is stripped during splitting

    List<String> input = List.of("&lt;pre&gt;  indented code&lt;/pre&gt;");

    List<String> result = XmlDiffCtrlr.processLines(input);

    assertEquals("indented code", result.get(1));
  }

//---------------------------------------------------------------------------

  @Test
  void trailingWhitespaceIsStripped()
  {
    List<String> input = List.of("&lt;p&gt;text   ");

    assertEquals(List.of("&lt;p&gt;", "text"),
                 XmlDiffCtrlr.processLines(input));
  }

//---------------------------------------------------------------------------

  @Test
  void emptyLinesArePreserved()
  {
    List<String> input = List.of("line one", "", "line three");

    assertEquals(input, XmlDiffCtrlr.processLines(input));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
