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

import org.junit.jupiter.api.*;

//---------------------------------------------------------------------------

class TitleCaseTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testSimpleSentence()
  {
    String input    = "the quick brown fox jumps over the lazy dog",
           expected = "The Quick Brown Fox Jumps Over the Lazy Dog";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testSentenceWithPunctuation()
  {
    String input    = "a quick, brown-fox jumps; over the.lazy dog: and it's fast!",
           expected = "A Quick, Brown-Fox Jumps; Over the.Lazy Dog: And it's Fast!";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testSentenceWithParentheses()
  {
    String input    = "this (is a test) to check (title case)",
           expected = "This (is a Test) to Check (Title Case)";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testSentenceWithApostrophes()
  {
    String input    = "it's a dog's life",
           expected = "It's a Dog's Life";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testAllCapsSentence()
  {
    String input    = "THIS IS AN ALL CAPS SENTENCE",
           expected = "This is an All Caps Sentence";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testLowerCaseSentence()
  {
    String input    = "this is a completely lower case sentence",
           expected = "This is a Completely Lower Case Sentence";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testSingleWord()
  {
    String input    = "example",
           expected = "Example";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testEmptyString()
  {
    String input    = "",
           expected = "";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testWhitespaceString()
  {
    String input    = "    ",
           expected = "    ";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testMixedCaseSentence()
  {
    String input    = "thIs IS a MIXEd CaSE sentENCE",
           expected = "This is a Mixed Case Sentence";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testUnicodeCharacters()
  {
    String input    = "tête-à-tête and naïve café",
           expected = "Tête-à-Tête and Naïve Café";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testHyphenatedWords()
  {
    String input    = "well-being is important in self-care",
           expected = "Well-Being is Important in Self-Care";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testApostropheFollowedBySpace()
  {
    String input    = "it's not about the size, it's about the 'attitude' ",
           expected = "It's Not About the Size, it's About the 'Attitude' ";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testApostropheNotFollowedBySpace()
  {
    String input    = "don't let your dreams be dreams",
           expected = "Don't Let Your Dreams Be Dreams";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testSingleQuotesAroundWord()
  {
    String input    = "'hello' isn't just a greeting, it's an attitude",
           expected = "'Hello' isn't Just a Greeting, it's an Attitude";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testMultipleApostrophes()
  {
    String input    = "he's the friend who'll always be there",
           expected = "He's the Friend Who'll Always Be There";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testApostropheWithQuotes()
  {
    String input    = "the word 'friend's' meaning has changed",
           expected = "The Word 'Friend's' Meaning Has Changed";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testWordAtDifferentPositions()
  {
    String input    = "it's a nice day. don't you agree?",
           expected = "It's a Nice Day. Don't You Agree?";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testWordAfterColon()
  {
    String input    = "Remember: don't forget your keys.",
           expected = "Remember: Don't Forget Your Keys.";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testWordWithContractions()
  {
    String input    = "Y'all ain't seen nothin' yet.",
           expected = "Y'all Ain't Seen Nothin' yet.";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testLatinPhraseWithSmallLetterAWithGraveAccent()
  {
    String input    = "vis-à-vis",
           expected = "Vis-à-vis";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testLatinPhraseAlone()
  {
    String input    = "à la carte",
           expected = "À la Carte";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testLatinPhraseInSentence()
  {
    String input    = "we were dining à la carte",
           expected = "We Were Dining à la Carte";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testMultipleLatinPhrases()
  {
    String input    = "à la carte and vis-à-vis",
           expected = "À la Carte and vis-à-vis";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testLatinPhraseWithOtherText()
  {
    String input    = "The term à la carte is used in many contexts.",
           expected = "The Term à la Carte is Used in Many Contexts.";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testInitialAndAbbreviationInLastName()
  {
    String input    = "John b. st. james",
           expected = "John B. St. James";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameMcDonald()
  {
    String input    = "McDonald?",
           expected = "McDonald?";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameMacArthur()
  {
    String input    = "MacArthur's story",
           expected = "MacArthur's Story";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameDeMarco()
  {
    String input    = "DeMarco",
           expected = "DeMarco";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameVanDerbilt()
  {
    String input    = "VanDerbilt",
           expected = "VanDerbilt";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameOConnor()
  {
    String input    = "O'Connor",
           expected = "O'Connor";
    assertEquals(expected, titleCase(input));
  }

  @Test
  void testCommonNameFitzGerald()
  {
    String input    = "FitzGerald",
           expected = "FitzGerald";
    assertEquals(expected, titleCase(input));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
