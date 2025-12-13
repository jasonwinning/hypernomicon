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

package org.hypernomicon.model.searchKeys;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Record;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

//---------------------------------------------------------------------------

/**
 * Unit tests for the KeywordLinkScanner.scan function covering various edge
 * cases. This class also (indirectly) provides effective unit testing for
 * convertToEnglishCharsWithMap, and contains a couple of direct unit tests
 * for it.
 */
class KeywordLinkScannerTest
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
  private static Keyword createKeyword(String text, boolean startOnly, boolean endOnly)
  {
    if (startOnly)
      text = '^' + text;

    if (endOnly)
      text = text + '$';

    return new Keyword(new KeywordBinding(text, db.createNewBlankRecord(hdtWork)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Helper to build a prefix Multimap from Keywords using their first 3 letters.
   */
  private static Function<String, Iterable<Keyword>> mapOf(Keyword... keywords)
  {
    Multimap<String, Keyword> map = ArrayListMultimap.create();

    Arrays.stream(keywords).forEach(kw -> map.put(safeSubstring(kw.normalizedText, 0, 3), kw));

    return prefix -> map.get(prefix.toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void assertSameBindings(Collection<KeywordBinding> expected, Collection<KeywordBinding> actual)
  {
    assertSameBindings(expected, actual, (String) null);
  }

  private static void assertSameBindings(Collection<KeywordBinding> expected, Collection<KeywordBinding> actual, String message)
  {
    Map<HDT_Record, KeywordBinding> expectedMap = expected.stream().collect(Collectors.toMap(KeywordBinding::getRecord, Function.identity())),
                                    actualMap   = actual  .stream().collect(Collectors.toMap(KeywordBinding::getRecord, Function.identity()));

    assertEquals(expectedMap.keySet(), actualMap.keySet());

    for (HDT_Record record : expectedMap.keySet())
      assertSame(expectedMap.get(record), actualMap.get(record), message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSimpleAsciiMatch()
  {
    String text = "This is a test string.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should find one link");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset(), "Offset should point to the start of 'test'");
    assertEquals("test".length(), link.getLength(), "Length should match keyword length");

    assertSameBindings(kw.getAllBindings(), link.getAllBindings(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testCaseInsensitiveMatching()
  {
    String text = "This TEST is important.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());
    assertEquals(text.toLowerCase().indexOf("test"), links.getFirst().getOffset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkAtStartOfInput()
  {
    String text = "test-case example here.";
    Keyword kw = createKeyword("test-case", true, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match keyword at very start");

    KeywordLink link = links.getFirst();

    assertEquals(0, link.getOffset());
    assertEquals("test-case".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkAtEndOfInput()
  {
    String text = "Here is an example test";
    Keyword kw = createKeyword("test", false, true);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match keyword at end of input");

    KeywordLink link = links.getFirst();

    assertEquals(text.lastIndexOf("test"), link.getOffset());
    assertEquals("test".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testEndOnlyMatchesAtWordEnd()
  {
    String text = "We will protest later.";
    Keyword kw = createKeyword("test", false, true);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should match 'test' at the end of 'protest' when endOnly=true");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset());
    assertEquals("test".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testExactWordMatchWithStartAndEndOnly()
  {
    String text = "A test-case example.";
    Keyword kw = createKeyword("test-case", true, true);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test-case"), link.getOffset());
    assertEquals("test-case".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testEmDash()
  {
    String text = "A test\u2014case example.";
    Keyword kw = createKeyword("test-case", true, true);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(0, links.size(), "Em-dash is preserved by convertToEnglishChars");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationOffset()
  {
    String text = "Here is 你好 world.";
    Keyword kw = createKeyword("nihao", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf('你'), link.getOffset(), "Offset should use original unicode index");
    assertEquals(2, link.getLength(), "Length should match original unicode length");
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
    Keyword kw1 = createKeyword("nihao" , false, false),
            kw2 = createKeyword("eleve" , false, false),
            kw3 = createKeyword("manana", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw1, kw2, kw3));

    assertEquals(3, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(6, link.getOffset());
    assertEquals(8, link.getLength());
    assertSameBindings(kw2.getAllBindings(), link.getAllBindings());

    link = links.get(1);

    assertEquals(15, link.getOffset());
    assertEquals(7, link.getLength());
    assertSameBindings(kw3.getAllBindings(), link.getAllBindings());

    link = links.get(2);

    assertEquals(23, link.getOffset());
    assertEquals(2, link.getLength());
    assertSameBindings(kw1.getAllBindings(), link.getAllBindings());
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
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should only find link outside existing anchor");
    assertEquals(text.lastIndexOf("test"), links.getFirst().getOffset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkImmediatelyBeforeAnchor()
  {
    String text = "Check this test<a href='#'>link</a> rest.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link word immediately before an anchor");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testLinkImmediatelyAfterAnchor()
  {
    String text = "Start <a href='#'>link</a>test-end.";
    Keyword kw = createKeyword("test-end", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link word immediately after an anchor");

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test-end"), link.getOffset());
    assertEquals("test-end".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSkipWebUrls()
  {
    String text = "Visit http://domain.com and test here.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationLongerInternalLinkAtStart()
  {
    String text = "你好test here.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset());
    assertEquals("test".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testUnicodeTransliterationLongerInternalLinkAtEnd()
  {
    String text = "some text test你好";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size());

    KeywordLink link = links.getFirst();

    assertEquals(text.indexOf("test"), link.getOffset());
    assertEquals("test你好".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleKeywordsSamePrefixPickLongestMatch()
  {
    String text = "This is testing code.";

    Keyword kwShort = createKeyword("test"   , false, false),
            kwLong  = createKeyword("testing", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kwShort, kwLong));

    assertEquals(1, links.size(), "Should pick the longest matching keyword 'testing'");

    KeywordLink link = links.getFirst();

    assertSameBindings(kwLong.getAllBindings(), link.getAllBindings());
    assertEquals(text.indexOf("testing"), link.getOffset());
    assertEquals("testing".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleKeywordsSamePrefixExactMatchAndLongerPresent()
  {
    String text = "This is test code.";

    Keyword kwShort = createKeyword("test"   , false, false),
            kwLong  = createKeyword("testing", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kwShort, kwLong));

    assertEquals(1, links.size(), "Should pick 'test' when only exact match fits");

    KeywordLink link = links.getFirst();

    assertSameBindings(kwShort.getAllBindings(), link.getAllBindings());
    assertEquals(text.indexOf("test"), link.getOffset());
    assertEquals("test".length(), link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testOverlappingDifferentKeywordsSharePrefix()
  {
    String text = "abcd";

    Keyword k1 = createKeyword("abc", false, false),
            k2 = createKeyword("bcd", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(k1, k2));

    assertEquals(1, links.size(), "Should pick one non-overlapping link");

    KeywordLink link = links.getFirst();

    assertEquals(0, link.getOffset());
    assertEquals(4, link.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMiddleOfWordExpandsEndOfWord()
  {
    String text = "Many protesters arrived.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'testers' when keyword in middle of word");

    KeywordLink link = links.getFirst();

    int start = text.indexOf("testers");

    assertEquals(start, link.getOffset(), "Offset should point to start of 'testers'");
    assertEquals("testers".length(), link.getLength(), "Length should cover 'testers'");
    assertSameBindings(kw.getAllBindings(), link.getAllBindings(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testAdjacentLinks()
  {
    String text = "foofoo bar";
    Keyword kw = createKeyword("foo", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should have one link for 'foofoo'");

    KeywordLink first = links.getFirst();

    assertEquals(0, first.getOffset());
    assertEquals("foofoo".length(), first.getLength());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testReEnterKeywordWithAccentsExpandsToEndOfWord()
  {
    String text = "The user's password was pré-éntéréd into the field.";
    Keyword kw = createKeyword("Re-enter", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'ré-éntéréd' when keyword 'Re-enter' appears in accented form");
    KeywordLink link = links.getFirst();

    int start = text.indexOf("ré-éntéréd");

    assertEquals(start, link.getOffset(), "Offset should point to start of 'ré-éntéréd'");
    assertEquals("ré-éntéréd".length(), link.getLength(), "Length should cover remainder of accented word");
    assertSameBindings(kw.getAllBindings(), link.getAllBindings(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testNoSpaceAfterPeriod()
  {
    String text = "Check.Test here.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'Test' when no space after period");

    KeywordLink link = links.getFirst();

    int start = text.toLowerCase().indexOf("test");

    assertEquals(start, link.getOffset(), "Offset should account for no space after period");
    assertEquals("Test".length(), link.getLength(), "Length should match keyword length regardless of spacing");
    assertSameBindings(kw.getAllBindings(), link.getAllBindings(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultipleSpacesAfterPeriod()
  {
    String text = "Check.   Test here.";
    Keyword kw = createKeyword("test", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'Test' when multiple spaces after period");

    KeywordLink link = links.getFirst();

    int start = text.toLowerCase().indexOf("test");

    assertEquals(start, link.getOffset(), "Offset should account for multiple spaces after period");
    assertEquals("Test".length(), link.getLength(), "Length should match keyword length regardless of spacing");
    assertSameBindings(kw.getAllBindings(), link.getAllBindings(), "Linked Keyword should match");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testInitialsAndLastNameNoSpaceBetweenInitials()
  {
    String text = "I love the works of J.R. Tolkien.";
    Keyword kw = createKeyword("J. R. Tolkien", false, false);

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "Should link 'J.R. Tolkien' without space between initials");

    KeywordLink link = links.getFirst();

    int start = text.indexOf("J.R. Tolkien");

    assertEquals(start, link.getOffset());
    assertEquals("J.R. Tolkien".length(), link.getLength());
    assertSameBindings(kw.getAllBindings(), link.getAllBindings());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMultiBinding()
  {
    String text = "I love the works of J.R. Tolkien.";

    HDT_Person person1 = db.createNewBlankRecord(hdtPerson),
               person2 = db.createNewBlankRecord(hdtPerson),
               person3 = db.createNewBlankRecord(hdtPerson);

    Keyword kw = new Keyword(new KeywordBinding("J. R. Tolkien", person1));

    kw.addBinding(new KeywordBinding("J.R. Tolkien", person2));
    kw.addBinding(new KeywordBinding("J. R. Tolkien", person3));

    List<KeywordLink> links = KeywordLinkScanner.scan(text, mapOf(kw));

    assertEquals(1, links.size(), "All 3 records should link 'J.R. Tolkien' without space between initials");
    assertEquals(3, links.getFirst().getAllBindings().size(), "All 3 records should link 'J.R. Tolkien' without space between initials");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
