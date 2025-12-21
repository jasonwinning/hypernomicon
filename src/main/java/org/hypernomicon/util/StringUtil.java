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

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.lang3.mutable.MutableInt;

import com.google.common.collect.Lists;

import com.ibm.icu.text.Transliterator;

//---------------------------------------------------------------------------

public final class StringUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private StringUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // at class level, initialize a small reusable buffer
  private static final ThreadLocal<char[]> COLLAPSE_BUF = ThreadLocal.withInitial(() -> new char[128]);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Use this in functions that are only supposed to run in a unit test.
   */
  public static void assertThatThisIsUnitTestThread()
  {
    for (StackTraceElement element : Thread.currentThread().getStackTrace())
      if (element.getClassName().startsWith("org.junit.") || element.getClassName().startsWith("junit."))
        return;

    throw new AssertionError("Can only run in unit test.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearCollapseBufferForTest()
  {
    assertThatThisIsUnitTestThread();

    COLLAPSE_BUF.remove();
  }

  /**
   * Collapse runs of two-or-more spaces into one. If no runs found, returns
   * the original String (no GC pressure); otherwise reuses a ThreadLocal
   * char[] and emits exactly one new String
   */
  public static String collapseSpaces(CharSequence cs)
  {
    int len = cs.length();

    // detect whether we even need to do work

    for (int i = 1; i < len; i++)
    {
      if ((cs.charAt(i) == ' ') && (cs.charAt(i - 1) == ' '))
      {
        // found >1 space, go to slow path

        return collapseWithBuffer(cs, len);
      }
    }

    // no runs of spaces, return original

    return cs.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String collapseWithBuffer(CharSequence cs, int len)
  {
    // grab or grow our shared buffer

    char[] buf = COLLAPSE_BUF.get();

    if (buf.length < len)
    {
      buf = new char[len];
      COLLAPSE_BUF.set(buf);
    }

    // emit chars, skipping extra spaces

    int out = 0;
    char prev = cs.charAt(0);
    buf[out++] = prev;

    for (int i = 1; i < len; i++)
    {
      char c = cs.charAt(i);

      if ((c == ' ') && (prev == ' '))
      {
        // skip this space
        continue;
      }

      buf[out++] = c;
      prev = c;
    }

    // wrap in a String exactly once

    return new String(buf, 0, out);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts multiline text into a single line by replacing various newline and vertical
   * whitespace characters with spaces. This method also removes a specific Unicode character
   * (\ufffd) that is often appended when copying text from Acrobat.
   * <p>
   * The function performs the following transformations:
   * <ul>
   *   <li>Removes the specific Unicode character \ufffd.</li>
   *   <li>Collapses consecutive newline characters into a single space.</li>
   *   <li>Collapses consecutive vertical whitespace characters into a single space.</li>
   *   <li>Collapses consecutive horizontal whitespace characters into a single space.</li>
   *   <li>Replaces any remaining newline, vertical, or horizontal whitespace characters with spaces.</li>
   * </ul>
   *
   * @param text the multiline text to convert
   * @return the single-line text with newline and vertical spaces replaced by spaces
   */
  public static String convertToSingleLine(String text)
  {
    return text.replace("\ufffd", "")  // I don't know what this is but it is usually appended at the end when copying text from Acrobat

      .replaceAll("\\R+(\\R)", "$1")
      .replaceAll("(\\R)\\R+", "$1")

      .replaceAll("(\\v)\\v+", "$1")
      .replaceAll("\\v+(\\v)", "$1")

      .replaceAll("\\R+(\\h)", "$1")
      .replaceAll("(\\h)\\R+", "$1")

      .replaceAll("\\v+(\\h)", "$1")
      .replaceAll("(\\h)\\v+", "$1")

      .replaceAll("\\R+", " ")
      .replaceAll("\\v+", " ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Trims each line of the input string, also removing leading and trailing
   * blank lines.
   * <p>
   * This function performs the following transformations:
   * <ul>
   *   <li>Ensures the input string is non-null by using {@code safeStr}.</li>
   *   <li>Splits the input string into lines based on newline and vertical tab characters.</li>
   *   <li>Trims each line to remove leading and trailing whitespace.</li>
   *   <li>Removes leading and trailing blank lines.</li>
   *   <li>Joins the trimmed lines back into a single string with newline characters.</li>
   * </ul>
   *
   * @param input the multiline input string to process
   * @return the processed string with each line trimmed and unnecessary whitespace removed
   */
  public static String trimLines(String input)
  {
    input = safeStr(input).replace("\ufffd", "");  // I don't know what this is but it is usually appended at the end when copying text from Acrobat

    if (convertToSingleLine(input).isBlank()) return "";

    List<String> list = convertMultiLineStrToStrList(input, true);

    list.replaceAll(String::strip);

    return strListToStr(list, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts a multiline string into a list of strings, with an option to preserve intermediate empty lines.
   * <p>
   * This method will remove any leading and trailing empty lines from the resulting list. The parameter
   * {@code preserveIntermediateEmpties} determines whether intermediate empty lines should be preserved.
   *
   * @param str the multiline string to convert
   * @param preserveIntermediateEmpties if true, includes intermediate empty lines in the result; otherwise, excludes them
   * @return a list of strings, each representing a line from the input string
   */
  public static List<String> convertMultiLineStrToStrList(String str, boolean preserveIntermediateEmpties)
  {
    // Normalize newlines to ensure \r\n is treated as a single newline
    str = str.replace("\r\n", "\n").replace("\r", "\n");

    // Split the input string by newline and vertical tab characters
    List<String> list = Lists.newArrayList(str.split("[\\n\\v]"));

    if (list.isEmpty()) return list;

    while (list.getFirst().isBlank())
    {
      list.removeFirst();
      if (list.isEmpty()) return list;
    }

    while (list.getLast().isBlank())
    {
      list.removeLast();
      if (list.isEmpty()) return list;
    }

    if (preserveIntermediateEmpties == false)
      list.removeIf(String::isBlank);

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts a list of strings into a single string, with an option to include or exclude empty lines.
   * This method uses the default newline character ('\n').
   *
   * @param list the list of strings to convert
   * @param includeEmptyLines if true, includes empty lines in the result; otherwise, excludes them
   * @return the concatenated string with lines separated by the default newline character
   */
  public static String strListToStr(List<String> list, boolean includeEmptyLines)
  {
    return strListToStr(list, includeEmptyLines, false);
  }

  /**
   * Converts a list of strings into a single string, with options to include or exclude empty lines
   * and to use the system's newline character or a default newline character ('\n').
   *
   * @param list the list of strings to convert
   * @param includeEmptyLines if true, includes empty lines in the result; otherwise, excludes them
   * @param useSystemNewLineChar if true, uses the system's newline character; otherwise, uses '\n'
   * @return the concatenated string with lines separated by the chosen newline character
   */
  public static String strListToStr(List<String> list, boolean includeEmptyLines, boolean useSystemNewLineChar)
  {
    if (list == null) return "";

    return list.stream().filter(one -> includeEmptyLines || strNotNullOrEmpty(one))
                        .map(StringUtil::safeStr)
                        .collect(Collectors.joining(useSystemNewLineChar ? System.lineSeparator() : "\n"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String strListToSpaceDelimitedStr(List<String> list)
  {
    return list.stream().map(String::strip).collect(Collectors.joining(" ")).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Compares two lists of strings to determine if they are equal.<br>
   * The comparison can be case-sensitive or case-insensitive based on the {@code ignoreCase} parameter.<br>
   * Null values within a list are treated as equal to an empty String.
   *
   * @param list1      The first list of strings to compare. Can be {@code null}.
   * @param list2      The second list of strings to compare. Can be {@code null}.
   * @param ignoreCase If {@code true}, the comparison is case-insensitive; otherwise, it is case-sensitive.
   * @return {@code true} if both lists are equal according to the specified criteria; {@code false} otherwise.
   *         Returns {@code false} if one list is {@code null} and the other is not.
   *         If both lists are {@code null}, returns {@code true}.
   */
  public static boolean strListsEqual(List<String> list1, List<String> list2, boolean ignoreCase)
  {
    if ((list1 == null) != (list2 == null)) return false;
    if (list1 == null) return true;
    if (list1.size() != list2.size()) return false;

    for (int ndx = 0; ndx < list1.size(); ndx++)
    {
      String str1 = stripSafe(list1.get(ndx)),
             str2 = stripSafe(list2.get(ndx));

      if ((ignoreCase ? str1.equalsIgnoreCase(str2) : str1.equals(str2)) == false)
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes all parenthetical expressions (i.e., text enclosed within parentheses)
   * from the given string. Strips any leading or trailing whitespace from the resulting string.
   * <p>
   * This method repeatedly removes parenthetical content using the {@link #removeFirstParenthetical(String)} method
   * until no opening parenthesis '(' is found in the string.
   *
   * @param str The input string from which all parenthetical expressions will be removed.
   *            If the string contains no opening parentheses, it is returned as is.
   * @return A new string with all parenthetical expressions removed and excess whitespace trimmed.
   * @throws NullPointerException If the input string is null. Ensure the provided string is properly initialized.
   * @see #removeFirstParenthetical(String) For details on how individual parentheticals are removed.
   */
  public static String removeAllParentheticals(String str)
  {
    while (str.contains("("))
      str = removeFirstParenthetical(str);

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes the first parenthetical expression (i.e., text enclosed within parentheses)
   * from the given string. Strips any leading or trailing whitespace from the resulting string.
   *
   * @param str The input string from which the first parenthetical expression will be removed.
   *            If no opening parenthesis '(' is found, the original string is returned.
   * @return A new string with the first parenthetical expression removed and excess whitespace trimmed.
   *         If the closing parenthesis ')' is missing, only text preceding the opening parenthesis is retained.
   * @throws NullPointerException If the input string is null. Ensure the provided string is properly initialized.
   */
  public static String removeFirstParenthetical(String str)
  {
    int pos1 = str.indexOf('(');
    if (pos1 < 0)
      return str;

    int pos2 = str.indexOf(')');
    String result = str.substring(0, pos1).strip();
    if (pos2 > pos1)
      result = (result + ' ' + safeSubstring(str, pos2 + 1, str.length())).strip();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

/**
 * Returns a substring of the given string, handling out-of-bound indices or
 * null input gracefully without throwing exceptions.
 *
 * @param str the original string
 * @param start the start index (inclusive)
 * @param end the end index (exclusive)
 * @return the substring from the start index to the end index, or an empty string if
 * the input is null
 */
  public static String safeSubstring(String str, int start, int end)
  {
    if (start < 0) start = 0;

    if ((end < start) || (str == null) || (start >= str.length())) return "";

    return end > str.length() ? str.substring(start) : str.substring(start, end);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static char toLowerAscii(char c)
  {
    return (c >= 'A') && (c <= 'Z') ? (char)(c + 32) : c;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if the string is entirely composed of either all uppercase or
   * all lowercase letters, determined by the case of the first character.
   * If the first character is not a letter, returns false.
   *
   * @param s The input string to evaluate.
   * @return true if all characters are letters and share the same case as the first letter.
   */
  public static boolean isUniformLetterCase(String s)
  {
    if (s.isEmpty()) return false;

    char first = s.charAt(0);
    if (Character.isLetter(first) == false) return false;

    boolean upper = Character.isUpperCase(first);

    for (char c : s.toCharArray())
      if ((Character.isLetter(c) == false) || (Character.isUpperCase(c) != upper))
        return false;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean lacksMixedCase(CharSequence s)
  {
    return s.chars().noneMatch(Character::isUpperCase)  ||
           s.chars().noneMatch(Character::isLowerCase);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String camelToTitle(CharSequence in)
  {
    String out = "";
    int len = in.length();

    char currentChar = len > 0 ? Character.toUpperCase(in.charAt(0)) : ' ',
         nextChar = ' ';

    for (int ndx = 0; ndx < len; ndx++)
    {
      boolean nextIsUpper = false,
              nextIsLower = false;

      if (ndx < (len - 1))
      {
        nextChar = in.charAt(ndx + 1);
        nextIsUpper =  Character.isUpperCase(nextChar);
        nextIsLower =  Character.isLowerCase(nextChar);
      }

      boolean isUpper = Character.isUpperCase(currentChar),
              isLower = Character.isLowerCase(currentChar);

      if (isUpper && nextIsLower)
        out = out + ' ' + currentChar;
      else if (isLower && nextIsUpper)
        out = out + currentChar + ' ';
      else
        out = out + currentChar;

      currentChar = nextChar;
    }

    return collapseSpaces(out).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String sentenceCase(String str)
  {
    if (strNullOrEmpty(str)) return "";

    str = str.toLowerCase();

    Pattern p = Pattern.compile("([.:?!]\\h)(\\p{IsAlphabetic})");

    str = p.matcher(str).replaceAll(match -> match.group(1) + match.group(2).toUpperCase());

    return str.substring(0, 1).toUpperCase() + str.substring(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String titleCase(String str)
  {
    MutableInt pos = new MutableInt(0);

    while (str.matches(".*\\h[.,:;)].*"))
      str = str.replaceFirst("\\h([.,:;)])", "$1");   // remove space before character

    while (str.matches(".*[(]\\h.*"))
      str = str.replaceFirst("([(])\\h", "$1");   // remove space after character

    outerLoop:
    for (String word = getNextWord(str, pos); word.length() > 0; word = getNextWord(str, pos))
    {
      int end = pos.intValue(), start = end - word.length();

      String pre = "", post = "";
      char lastChar = ' ';
      boolean noCaps = false, endsWithDot = false;

      if (start > 0)
      {
        pre = str.substring(0, start).strip();
        if (pre.length() > 0)
        {
          lastChar = pre.charAt(pre.length() - 1);

          if ("'".equals(convertToEnglishChars(String.valueOf(lastChar))))
            if (pre.length() > 1)
              if (pre.charAt(pre.length() - 2) != ' ') // don't capitalize letter immediately after apostrophe
                if (str.charAt(start - 1) != ' ')      // do capitalize letter after an apostrophe plus a space
                  noCaps = true;
        }

        pre = str.substring(0, start);
      }

      if (end < str.length())
      {
        post = str.substring(end);
        if (post.charAt(0) == '.')
          endsWithDot = true;
      }

      for (String prefix : COMMON_PREFIXES)
        if (word.startsWith(prefix) && isCorrectlyCapitalized(word, prefix))
        {
          str = pre + word + post;
          continue outerLoop;
        }

      word = word.toLowerCase();

      if (noCaps == false)
      {
        if ((lastChar == ':') || (lastChar == '?') || (lastChar == '/') || (start == 0))
          word = word.substring(0, 1).toUpperCase() + safeSubstring(word, 1, word.length()).toLowerCase();
        else if ((word.length() == 1) && endsWithDot)
          word = word.substring(0, 1).toUpperCase();
        else
        {
          int ndx = word.indexOf('\'');

          switch (ndx == -1 ? word : word.substring(0, ndx))
          {
            case "\u00e0": // Latin small letter a with grave accent, as in 'vis a vis' or 'a la'

            case "isn":    // Need 'isn't' to be lowercase if 'is' is lowercase

            case "a"    : case "also" : case "amid" : case "an"   : case "and"  :
            case "as"   : case "at"   : case "atop" : case "but"  : case "by"   :
            case "for"  : case "from" : case "if"   : case "in"   : case "into" :
            case "is"   : case "it"   : case "la"   : case "lieu" : case "nor"  :
            case "of"   : case "off"  : case "on"   : case "onto" : case "or"   :
            case "out"  : case "per"  : case "qua"  : case "sans" : case "so"   :
            case "than" : case "that" : case "the"  : case "then" : case "til"  :
            case "till" : case "to"   : case "unto" : case "upon" : case "via"  :
            case "vis"  : case "with" : case "yet"  :
              break;

            default :
              word = word.substring(0, 1).toUpperCase() + safeSubstring(word, 1, word.length()).toLowerCase();
          }
        }
      }

      str = pre + word + post;
    }

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final List<String> COMMON_PREFIXES = Arrays.asList("Mc", "Mac", "De", "Van", "O'", "Fitz");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean isCorrectlyCapitalized(String word, CharSequence prefix)
  {
    int prefixLength = prefix.length();
    if (word.length() <= (prefixLength + 1))
      return false;

    // Check the capitalization after the prefix
    char nextChar = word.charAt(prefixLength);
    return Character.isUpperCase(nextChar) && word.substring(prefixLength + 1).equals(word.substring(prefixLength + 1).toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getNextWord(String str, MutableInt posObj)
  {
    int start = posObj.intValue(), end;
    boolean gotStart = false;

    while ((start < str.length()) && (gotStart == false))
    {
      if (Character.isAlphabetic(str.charAt(start)))
        gotStart = true;
      else
        start++;
    }

    if (gotStart == false) return "";

    for (end = start + 1; end < str.length(); end++)
    {
      char c = str.charAt(end);

      if ((c == '\'') && (end < (str.length() - 1)) && Character.isAlphabetic(str.charAt(end + 1)))
        continue;  // Treat O'Connor and ain't as a word

      if (Character.isAlphabetic(str.charAt(end)) == false)
      {
        posObj.setValue(end);
        return str.substring(start, end);
      }
    }

    posObj.setValue(end);
    return str.substring(start);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void assignSB(StringBuilder sb, String s)
  {
    sb.replace(0, sb.length(), s);
  }

  public static void assignSB(StringBuffer sb, String s)
  {
    sb.replace(0, sb.length(), s);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if the input non-null and contains characters other than horizontal or vertical whitespace.
   * @param s Input string
   * @return boolean result
   */
  public static boolean strNotNullOrBlank(String s)   { return strNullOrBlank(s) == false; }

  /**
   * Returns true if the input non-null and not zero-length.
   * @param s Input string
   * @return boolean result
   */
  public static boolean strNotNullOrEmpty(CharSequence s)   { return strNullOrEmpty(s) == false; }

  /**
   * Returns true if the input is null, zero-length, or only contains horizontal or vertical whitespace.
   * Equivalent to s.isBlank() if s is non-null.
   * @param s Input string
   * @return boolean result
   */
  public static boolean strNullOrBlank(String s)   { return (s == null) || s.isBlank(); }

  /**
   * Returns true if the input is null or zero-length.
   * @param s Input string
   * @return boolean result
   */
  public static boolean strNullOrEmpty(CharSequence s)   { return (s == null) || s.isEmpty(); }

  /**
   * Safely trims leading and trailing whitespace from a string.
   * <p>
   * If the input string is {@code null}, this method returns an empty string
   * ({@code ""}). Otherwise, it uses {@code String.strip()} to remove all leading
   * and trailing whitespace, including Unicode whitespace characters.
   * </p>
   *
   * @param s the string to be stripped; may be {@code null}
   * @return the stripped string, or an empty string if the input is {@code null}
   */
  public static String stripSafe(String s)         { return s == null ? "" : s.strip(); }

  /**
   * Safely converts a potentially null string to a non-null value.
   * <p>
   * If the input string is {@code null}, this method returns an empty string
   * ({@code ""}). Otherwise, it simply returns the original string.
   * This ensures that null values are safely handled and do not cause errors
   * in subsequent processing.
   * </p>
   *
   * @param s the input string; may be {@code null}
   * @return the original string if it is non-null, or an empty string if it is {@code null}
   */
  public static String safeStr(String s)           { return s == null ? "" : s; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Transliterator TRANS = Transliterator.createFromRules
  (
    "CustomSupplementary",
    "::NFKC;"                         // Normalize to compatibility form (e.g. 𝒜 → A, ligatures → separate letters)
    + "[\\U00010000-\\U0001FFFF] > ;" // Remove remaining supplementary characters (U+10000 to U+1FFFF) after normalization
    + "::Any-Latin;"                  // Transliterate non-Latin scripts (e.g. Hanzi, Cyrillic, Greek) to Latin equivalents
    + "::Latin-ASCII;"                // Convert Latin letters with diacritics to plain ASCII (e.g. é → e)
    + "::NFD;"                        // Decompose characters (e.g. é → e + ́) to expose diacritics
    + "[:Nonspacing Mark:] > ;"       // Remove combining marks (e.g. ́) to strip diacritics
    + "::NFC;",                       // Recompose characters to canonical form (e.g. e + ́ → é if not removed)
    Transliterator.FORWARD
  );

  private static final Map<Integer, String> codePointCache = new ConcurrentHashMap<>();

//---------------------------------------------------------------------------

  public static String convertToEnglishChars(CharSequence input)
  {
    return convertToEnglishCharsWithMap(input, null);
  }

//---------------------------------------------------------------------------

  public static String convertToEnglishCharsWithMap(CharSequence input, ArrayList<Integer> posMap)
  {
    boolean asciiOnly = true;

    for (int i = 0; i < input.length(); i++)
    {
      if (input.charAt(i) > 0x7F)
      {
        asciiOnly = false;
        break;
      }
    }

    if (asciiOnly)
    {
      // Fast path: all ASCII

      if (posMap != null)
      {
        posMap.ensureCapacity(input.length());

        for (int i = 0; i < input.length(); i++)
          posMap.add(i); // each output char maps to its own offset
      }

      return input.toString();
    }

    StringBuilder output = new StringBuilder(input.length());

    List<Integer> codePoints = input.codePoints().boxed().toList(),
                  codePointOffsets = new ArrayList<>(codePoints.size());

    // Track original offsets of each code point
    for (int i = 0, offset = 0; i < codePoints.size(); i++)
    {
      codePointOffsets.add(offset);
      offset += Character.charCount(codePoints.get(i));
    }

    for (int i = 0; i < codePoints.size(); i++)
    {
      int cp = codePoints.get(i),
          offset = codePointOffsets.get(i);

      String s = cp == 0x2014 ?            // Preserve em dash
        new String(Character.toChars(cp))
      :
        codePointCache.computeIfAbsent(cp, _cp -> TRANS.transliterate(new String(Character.toChars(_cp))));

      output.append(s);

      if (posMap != null)
        for (int j = 0; j < s.length(); j++)
          posMap.add(offset); // map each output char to original code point offset
    }

    return output.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern UNICODE_NUMERALS_PATTERN = Pattern.compile
  (
    "[\\u2070\\u2080\\uFF10\\u24EA\\u2460\\u00B9\\u2081\\uFF11\\u00B2\\u2082\\u2072\\uFF12\\u00B3\\u2083\\u2073\\uFF13" +
    "\\u2074\\u2084\\uFF14\\u2075\\u2085\\uFF15\\u2076\\u2086\\uFF16\\u2077\\u2087\\uFF17\\u2078\\u2088\\uFF18\\u2079" +
    "\\u2089\\uFF19]"
  );

  /**
   * Converts Unicode numerals to their ASCII equivalents.
   *
   * <p>This method scans the input string for Unicode numerals, including superscript,
   * subscript, full-width, and circled numerals, and replaces them with their ASCII
   * counterparts using {@link #unicodeToAsciiNumeral(char)}.</p>
   *
   * @param input The string potentially containing Unicode numerals. Must not be {@code null}.
   * @return A string where Unicode numerals are replaced with their ASCII equivalents.
   * @throws NullPointerException if {@code input} is {@code null}.
   */
  public static String convertUnicodeNumeralsToAscii(CharSequence input)
  {
    Matcher matcher = UNICODE_NUMERALS_PATTERN.matcher(input);
    StringBuilder result = new StringBuilder();

    while (matcher.find())
    {
      char unicodeChar = matcher.group().charAt(0);
      char asciiChar = unicodeToAsciiNumeral(unicodeChar);
      matcher.appendReplacement(result, String.valueOf(asciiChar));
    }

    matcher.appendTail(result);
    return result.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static char unicodeToAsciiNumeral(char unicodeChar)
  {
    return switch (unicodeChar)
    {
      case '\u2070', '\u2080', '\uFF10', '\u24EA' -> '0';  // Superscript, subscript, fullwidth, circled zero
      case '\u00B9', '\u2081', '\uFF11', '\u2460' -> '1';  // Superscript, subscript, fullwidth, circled one
      case '\u00B2', '\u2082', '\uFF12', '\u2461' -> '2';  // Superscript, subscript, fullwidth, circled two
      case '\u00B3', '\u2083', '\uFF13', '\u2462' -> '3';  // Superscript, subscript, fullwidth, circled three
      case '\u2074', '\u2084', '\uFF14', '\u2463' -> '4';  // Superscript, subscript, fullwidth, circled four
      case '\u2075', '\u2085', '\uFF15', '\u2464' -> '5';  // Superscript, subscript, fullwidth, circled five
      case '\u2076', '\u2086', '\uFF16', '\u2465' -> '6';  // Superscript, subscript, fullwidth, circled six
      case '\u2077', '\u2087', '\uFF17', '\u2466' -> '7';  // Superscript, subscript, fullwidth, circled seven
      case '\u2078', '\u2088', '\uFF18', '\u2467' -> '8';  // Superscript, subscript, fullwidth, circled eight
      case '\u2079', '\u2089', '\uFF19', '\u2468' -> '9';  // Superscript, subscript, fullwidth, circled nine

      default -> unicodeChar;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String fromCodePoint(int cp)
  {
    return new String(Character.toChars(cp));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern domainPattern = Pattern.compile("^[A-Za-z\\-]+(\\.[A-Za-z\\-]+)+/?$"),
                               pathPattern = Pattern.compile(".*/\\w.*"),
                               fileExtensionPattern = Pattern.compile(".*\\.[a-zA-Z].*"),
                               whitespaceExtensionPattern = Pattern.compile(".*\\.\\h.*");

  public static boolean isStringUrl(String selText)
  {
    selText = selText.strip();

    return selText.contains("www." ) || selText.contains("http" ) ||
           selText.contains(".com" ) || selText.contains(".htm" ) ||
           selText.contains(".org" ) || selText.contains(".net" ) ||
           selText.contains(".us"  ) || selText.contains(".uk"  ) ||
           selText.contains(".ca"  ) || selText.contains(".au"  ) ||
           selText.contains(".edu" ) || selText.contains(".de"  ) ||
           selText.contains(".me"  ) || selText.contains(".info") ||
           selText.contains(".site") || selText.contains(".gov" ) ||
           selText.contains("://"  ) ||

           domainPattern.matcher(selText).matches() ||

           (pathPattern.matcher(selText).matches() && fileExtensionPattern.matcher(selText).matches() && !whitespaceExtensionPattern.matcher(selText).matches());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean charIsPartOfWebLink(CharSequence text, int ndx)
  {
    char c = text.charAt(ndx);

    return (c != '\n') && (c != ' ') && (c != ',') && (c != ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This should never be changed. It is the same algorithm as String.hashCode() as of Java 8u112

  public static int stringHash(String value)
  {
    int h = 0, len = value.length();

    if (len == 0) return 0;

    char[] val = value.toCharArray();

    for (int i = 0; i < len; i++)
      h = 31 * h + val[i];

    return h;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Generates a random hexadecimal string of the specified number of characters
   * <br>Example: 981579dd7bbb22edbcd44a8b21458bd3
   * @param size the length of the random string to generate
   * @return the generated string
   * @throws IllegalArgumentException if size is negative
   */
  public static String randomHexStr(int size)          { return randomStr(size, "0123456789abcdef"); }

  /**
   * Generates a random alphanumeric string of the specified number of characters
   * <br>Example: t3oVGLLzqT1fPeaAWHTZo5LQNxlWLsMt
   * @param size the length of the random string to generate
   * @return the generated string
   * @throws IllegalArgumentException if size is negative
   */
  public static String randomAlphanumericStr(int size) { return randomStr(size, "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHILJLMNOPQRSTUVWXYZ"); }

  /**
   * Generates a random string of the specified size using the characters from the given string.
   *
   * @param size the length of the random string to generate
   * @param charsStr the string containing characters to use for generating the random string
   * @return a random string of the specified size
   * @throws IllegalArgumentException if size is negative or charsStr is null/empty
   */
  private static String randomStr(int size, String charsStr)
  {
    if (size < 0)
      throw new IllegalArgumentException("Size must be non-negative");

    if (strNullOrEmpty(charsStr))
      throw new IllegalArgumentException("charsStr must not be null or empty");

    if (size == 0) return "";

    char[] chars  = charsStr.toCharArray(),
           result = new char[size];

    Random random = new Random();

    for (int i = 0; i < size; i++)
      result[i] = chars[random.nextInt(chars.length)];

    return new String(result);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean containsNoLetters(CharSequence input)
  {
    if (input == null) return true;

    for (int i = 0; i < input.length(); i++)
    {
      char c = input.charAt(i);

      if (((c >= 'A') && (c <= 'Z')) || ((c >= 'a') && (c <= 'z')))
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the given character with an added combining underline.
   *
   * <p>The method appends the Unicode combining low line character (U+0332)
   * to the input character, visually underlining it in supported environments.</p>
   *
   * @param c The character to underline.
   * @return A string containing the input character followed by a combining underline.
   */
  public static String underlinedChar(char c)
  {
    return c + "\u0332";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
