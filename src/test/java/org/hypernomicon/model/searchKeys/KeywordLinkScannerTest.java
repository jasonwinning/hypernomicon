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

import org.hypernomicon.model.KeywordLinkList;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.KeywordLinkList.KeywordLink;
import org.hypernomicon.model.SearchKeys.SearchKeyword;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.function.Function;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

//---------------------------------------------------------------------------

/**
 * Unit tests for the KeywordLinkList.generate function covering various edge
 * cases. This class also (indirectly) provides effective unit testing for
 * convertToEnglishCharsWithMap, and contains a couple of direct unit tests
 * for it.
 */
class KeywordLinkListTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static TestHyperDB db;

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    // This code will run once before all tests

    db = TestHyperDB.instance();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Helper to create a Keyword with a dummy Record.
   */
  private static SearchKeyword createKeyword(String text, boolean startOnly, boolean endOnly)
  {
    if (startOnly)
      text = '^' + text;

    if (endOnly)
      text = text + '$';

    return new SearchKeyword(text, db.createNewBlankRecord(hdtWork));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Helper to build a prefix Multimap from Keywords using their first 3 letters.
   */
  private static Function<String, Iterable<SearchKeyword>> mapOf(SearchKeyword... keywords)
  {
    Multimap<String, SearchKeyword> map = ArrayListMultimap.create();

    for (SearchKeyword kw : keywords)
    {
      String key = kw.text.substring(0, Math.min(3, kw.text.length())).toLowerCase();
      map.put(key, kw);
    }

    return prefix -> map.get(prefix.toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSimpleAsciiMatch()
  {
    String text = "This is a test string.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should find one link");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset(), "Offset should point to the start of 'test'");
    assertEquals("test".length(), link.length(), "Length should match keyword length");

    assertSame(kw, link.key(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testCaseInsensitiveMatching()
  {
    String text = "This TEST is important.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());
    assertEquals(text.toLowerCase().indexOf("test"), links.getFirst().offset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkAtStartOfInput()
  {
    String text = "test-case example here.";
    SearchKeyword kw = createKeyword("test-case", true, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match keyword at very start");

    KeywordLink link = links.getFirst();

    assertEquals(0, link.offset());
    assertEquals("test-case".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkAtEndOfInput()
  {
    String text = "Here is an example test";
    SearchKeyword kw = createKeyword("test", false, true);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match keyword at end of input");

    KeywordLink link = links.getFirst();

    assertEquals(text.lastIndexOf("test"), link.offset());
    assertEquals("test".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testEndOnlyMatchesAtWordEnd()
  {
    String text = "We will protest later.";
    SearchKeyword kw = createKeyword("test", false, true);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match 'test' at the end of 'protest' when endOnly=true");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset());
    assertEquals("test".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testExactWordMatchWithStartAndEndOnly()
  {
    String text = "A test-case example.";
    SearchKeyword kw = createKeyword("test-case", true, true);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test-case"), link.offset());
    assertEquals("test-case".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testEmDash()
  {
    String text = "A test\u2014case example.";
    SearchKeyword kw = createKeyword("test-case", true, true);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(0, links.size(), "Em-dash is preserved by convertToEnglishChars");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationOffset()
  {
    String text = "Here is 你好 world.";
    SearchKeyword kw = createKeyword("nihao", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf('你'), link.offset(), "Offset should use original unicode index");
    assertEquals(2, link.length(), "Length should match original unicode length");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String ELEVE_NFD   =
      "\u0065\u0301"    // e + combining acute
    + "\u006C"          // l
    + "\u0065\u0300"    // e + combining grave
    + "\u0076\u0065";   // v, e

  private static final String MANANA_NFD =
      "\u006D\u0061\u006E" // m, a, n
    + "\u0303"             // combining tilde
    + "\u0061\u006E\u0061"; // a, n, a

  @Test
  void testUnicodeTransliterationLengths()
  {
    String text = "Hello " + ELEVE_NFD + "r " + MANANA_NFD + " 你好 goodbye.";
    SearchKeyword kw1 = createKeyword("nihao" , false, false),
                  kw2 = createKeyword("eleve" , false, false),
                  kw3 = createKeyword("manana", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw1, kw2, kw3));

    assertEquals(3, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(6, link.offset());
    assertEquals(8, link.length());
    assertSame(kw2, link.key());

    link = links.get(1);

    assertEquals(15, link.offset());
    assertEquals(7, link.length());
    assertSame(kw3, link.key());

    link = links.get(2);

    assertEquals(23, link.offset());
    assertEquals(2, link.length());
    assertSame(kw1, link.key());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSupplementaryCodePointTransliteration()
  {
    // Mathematical script capital A (U+1D49C) → "A"
    assertEquals("A", convertToEnglishChars(fromCodePoint(0x1D49C)));

    // Fraktur capital A (U+1D504) → "A"
    assertEquals("A", convertToEnglishChars(fromCodePoint(0x1D504)));

    // Bold capital A (U+1D400) → "A"
    assertEquals("A", convertToEnglishChars(fromCodePoint(0x1D400)));

    // Emoji face (U+1F600) → ""
    assertEquals("", convertToEnglishChars(fromCodePoint(0x1F600)));

    // Rocket emoji (U+1F680) → ""
    assertEquals("", convertToEnglishChars(fromCodePoint(0x1F680)));

    // Deseret capital letter E (U+10400) → may vary by ICU version
    String deseret = convertToEnglishChars(fromCodePoint(0x10400));
    assertTrue(deseret.isEmpty() || deseret.matches("[Ee]"));

    String input =
      new String(Character.toChars(0x1D49C)) + // 𝒜 (U+1D49C)
      ' ' +
      ELEVE_NFD +   // élève
      ' ' +
      "\u5317" + "\u4EAC" +                    // 北京
      ' ' +
      new String(Character.toChars(0x1F604)); // 😄 (U+1F604)

    String expected = "A eleve beijing ";

    assertEquals(expected, convertToEnglishChars(input), "Transliteration failed for mixed input");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSkipExistingHtmlLink()
  {
    String text = "Click <a href=\"http://example.com\">example</a> test.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should only find link outside existing anchor");
    assertEquals(text.lastIndexOf("test"), links.getFirst().offset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkImmediatelyBeforeAnchor()
  {
    String text = "Check this test<a href='#'>link</a> rest.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link word immediately before an anchor");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkImmediatelyAfterAnchor()
  {
    String text = "Start <a href='#'>link</a>test-end.";
    SearchKeyword kw = createKeyword("test-end", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link word immediately after an anchor");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test-end"), link.offset());
    assertEquals("test-end".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSkipWebUrls()
  {
    String text = "Visit http://domain.com and test here.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationLongerInternalLinkAtStart()
  {
    String text = "你好test here.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset());
    assertEquals("test".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationLongerInternalLinkAtEnd()
  {
    String text = "some text test你好";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.offset());
    assertEquals("test你好".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleKeywordsSamePrefixPickLongestMatch()
  {
    String text = "This is testing code.";

    SearchKeyword kwShort = createKeyword("test"   , false, false),
                  kwLong  = createKeyword("testing", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kwShort, kwLong));

    assertEquals(1, links.size(), "Should pick the longest matching keyword 'testing'");

    KeywordLink link = links.getFirst();

    assertSame(kwLong, link.key());
    assertEquals(text.indexOf("testing"), link.offset());
    assertEquals("testing".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleKeywordsSamePrefixExactMatchAndLongerPresent()
  {
    String text = "This is test code.";

    SearchKeyword kwShort = createKeyword("test"   , false, false),
                  kwLong  = createKeyword("testing", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kwShort, kwLong));

    assertEquals(1, links.size(), "Should pick 'test' when only exact match fits");

    KeywordLink link = links.getFirst();

    assertSame(kwShort, link.key());
    assertEquals(text.indexOf("test"), link.offset());
    assertEquals("test".length(), link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testOverlappingDifferentKeywordsSharePrefix()
  {
    String text = "abcd";

    SearchKeyword k1 = createKeyword("abc", false, false),
                  k2 = createKeyword("bcd", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(k1, k2));

    assertEquals(1, links.size(), "Should pick one non-overlapping link");

    KeywordLink link = links.getFirst();

    assertEquals(0, link.offset());
    assertEquals(4, link.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMiddleOfWordExpandsEndOfWord()
  {
    String text = "Many protesters arrived.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'testers' when keyword in middle of word");

    KeywordLink link = links.getFirst();

    int start = text.indexOf("testers");

    assertEquals(start, link.offset(), "Offset should point to start of 'testers'");
    assertEquals("testers".length(), link.length(), "Length should cover 'testers'");
    assertSame(kw, link.key(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testAdjacentLinks()
  {
    String text = "foofoo bar";
    SearchKeyword kw = createKeyword("foo", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should one link for 'foofoo'");

    KeywordLink first = links.getFirst();

    assertEquals(0, first.offset());
    assertEquals("foofoo".length(), first.length());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testReEnterKeywordWithAccentsExpandsToEndOfWord()
  {
    String text = "The user's password was pré-éntéréd into the field.";
    SearchKeyword kw = createKeyword("Re-enter", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'ré-éntéréd' when keyword 'Re-enter' appears in accented form");
    KeywordLink link = links.getFirst();

    int start = text.indexOf("ré-éntéréd");

    assertEquals(start, link.offset(), "Offset should point to start of 'ré-éntéréd'");
    assertEquals("ré-éntéréd".length(), link.length(), "Length should cover remainder of accented word");
    assertSame(kw, link.key(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testNoSpaceAfterPeriod()
  {
    String text = "Check.Test here.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'Test' when no space after period");

    KeywordLink link = links.getFirst();

    int start = text.toLowerCase().indexOf("test");

    assertEquals(start, link.offset(), "Offset should account for no space after period");
    assertEquals("Test".length(), link.length(), "Length should match keyword length regardless of spacing");
    assertSame(kw, link.key(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleSpacesAfterPeriod()
  {
    String text = "Check.   Test here.";
    SearchKeyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'Test' when multiple spaces after period");

    KeywordLink link = links.getFirst();

    int start = text.toLowerCase().indexOf("test");

    assertEquals(start, link.offset(), "Offset should account for multiple spaces after period");
    assertEquals("Test".length(), link.length(), "Length should match keyword length regardless of spacing");
    assertSame(kw, link.key(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testInitialsAndLastNameNoSpaceBetweenInitials()
  {
    String text = "I love the works of J.R. Tolkien.";
    SearchKeyword kw = createKeyword("J. R. Tolkien", false, false);

    List<KeywordLink> links = KeywordLinkList.generate(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'J.R. Tolkien' without space between initials");

    KeywordLink link = links.getFirst();

    int start = text.indexOf("J.R. Tolkien");

    assertEquals(start, link.offset());
    assertEquals("J.R. Tolkien".length(), link.length());
    assertSame(kw, link.key());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
