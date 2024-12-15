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

package org.hypernomicon.util;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.filePath.FilePath;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import static java.nio.charset.StandardCharsets.*;

import java.net.URLDecoder;
import java.net.URLEncoder;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.nio.file.AccessDeniedException;
import java.nio.file.FileSystemException;
import java.nio.file.Files;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.text.NumberFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.TemporalAccessor;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Random;
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.DataFormat;
import javafx.scene.input.KeyEvent;
import javafx.util.Duration;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.client.HttpResponseException;

import com.google.common.collect.Lists;
import com.google.common.escape.Escaper;
import com.google.common.html.HtmlEscapers;
import com.google.common.xml.XmlEscapers;

import com.ibm.icu.text.CharsetDetector;
import com.ibm.icu.text.Transliterator;

//---------------------------------------------------------------------------

public final class Util
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final ReentrantLock globalLock = new ReentrantLock();

  public static final StopWatch stopWatch1 = new StopWatch(), stopWatch2 = new StopWatch(), stopWatch3 = new StopWatch(),
                                stopWatch4 = new StopWatch(), stopWatch5 = new StopWatch(), stopWatch6 = new StopWatch();

  public static final Escaper htmlEscaper         = HtmlEscapers.htmlEscaper(),
                              xmlContentEscaper   = XmlEscapers.xmlContentEscaper(),
                              xmlAttributeEscaper = XmlEscapers.xmlAttributeEscaper();

  private Util() { throw new UnsupportedOperationException(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void copyToClipboard(String str)
  {
    ClipboardContent content = new ClipboardContent();
    content.putString(str);
    Clipboard.getSystemClipboard().setContent(content);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getClipboardText(boolean noCarriageReturns)
  {
    String text = safeStr((String) Clipboard.getSystemClipboard().getContent(DataFormat.PLAIN_TEXT));
    if (text.isEmpty()) return "";

    text = text.replace("\ufffd", ""); // I don't know what this is but it is usually appended at the end when copying text from Acrobat

    if (noCarriageReturns)
      text = convertToSingleLine(text);

    return text;
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

    if (ultraTrim(convertToSingleLine(input)).isEmpty()) return "";

    List<String> list = convertMultiLineStrToStrList(input, true);

    list.replaceAll(Util::ultraTrim);

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
    //List<String> list = Lists.newArrayList(str.split("\\r?\\n"));

    // Normalize newlines to ensure \r\n is treated as a single newline
    str = str.replace("\r\n", "\n").replace("\r", "\n");

    // Split the input string by newline and vertical tab characters
    List<String> list = Lists.newArrayList(str.split("[\\n\\v]"));


    if (list.isEmpty()) return list;

    while (list.get(0).isBlank())
    {
      list.remove(0);
      if (list.isEmpty()) return list;
    }

    while (list.get(list.size() - 1).isBlank())
    {
      list.remove(list.size() - 1);
      if (list.isEmpty()) return list;
    }

    if (preserveIntermediateEmpties == false)
      list.removeIf(s -> ultraTrim(s).isBlank());

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

    return list.stream().filter(one -> includeEmptyLines || (safeStr(one).length() > 0))
                        .collect(Collectors.joining(useSystemNewLineChar ? System.lineSeparator() : "\n"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String strListToSpaceDelimitedStr(List<String> list)
  {
    return ultraTrim(list.stream().map(Util::ultraTrim).collect(Collectors.joining(" ")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int parseInt(String value, int def)
  {
    try { return Integer.parseInt(value); }
    catch (NumberFormatException e) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int parseHex(String value, int def)
  {
    if (value == null) return def;

    if ((value.length() > 2) && ((value.startsWith("0x") || value.startsWith("0X"))))
      value = value.substring(2);

    try { return Integer.parseInt(value, 16); }
    catch (NumberFormatException e) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static long parseLong(String value, long def)
  {
    try { return Long.parseLong(value); }
    catch (NumberFormatException e) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes duplicate elements from the provided list while preserving the original order.
   * This method modifies the original list in place.
   *
   * @param <T> the type of elements in the list
   * @param list the list from which duplicates should be removed
   * @return the modified list with duplicates removed
   */
  public static <T> List<T> removeDuplicatesInPlace(List<T> list)
  {
    Set<T> set = new HashSet<>();
    Iterator<T> it = list.iterator();

    while (it.hasNext())
      if (set.add(it.next()) == false)
        it.remove();

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean strListsEqual(List<String> list1, List<String> list2, boolean ignoreCase)
  {
    if ((list1 == null) != (list2 == null)) return false;
    if (list1 == null) return true;
    if (list1.size() != list2.size()) return false;

    for (int ndx = 0; ndx < list1.size(); ndx++)
    {
      String str1 = ultraTrim(list1.get(ndx)),
             str2 = ultraTrim(list2.get(ndx));

      if ((ignoreCase ? str1.equalsIgnoreCase(str2) : str1.equals(str2)) == false)
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String removeAllParentheticals(String str)
  {
    while (str.contains("("))
      str = removeFirstParenthetical(str);

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String removeFirstParenthetical(String str)
  {
    int pos1 = str.indexOf('(');
    if (pos1 < 0)
      return str;

    int pos2 = str.indexOf(')');
    String result = str.substring(0, pos1).trim();
    if (pos2 > pos1)
      result = (result + ' ' + safeSubstring(str, pos2 + 1, str.length())).trim();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String escapeURL(String url, boolean removeParen)
  {
    if (removeParen)
      url = removeFirstParenthetical(url);

    return URLEncoder.encode(url.trim(), UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String unescapeURL(String url)
  {
    try
    {
      return URLDecoder.decode(safeStr(url), UTF_8);
    }
    catch (IllegalArgumentException e)
    {
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean parseBoolean(String s)
  {
    if (Boolean.parseBoolean(s)) return true;

    s = s.trim().toLowerCase();

    if (s.equalsIgnoreCase(Boolean.TRUE .toString().trim())) return true;
    if (s.equalsIgnoreCase(Boolean.FALSE.toString().trim())) return false;

    return (s.indexOf("yes") == 0) || (s.indexOf("tru") == 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final DateTimeFormatter

   //Formatter to display date and time in a user-readable format, using locale and time zone of the system
   userReadableDateTimeFormatter = DateTimeFormatter.ofPattern("M/d/yyyy h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   // Formatter to display only time in a user-readable format, using locale and time zone of the system
   userReadableTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   // Formatter for HTTP dates following the RFC 1123 standard, using locale and time zone of the system
   httpDate = DateTimeFormatter.RFC_1123_DATE_TIME.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   // Formatter for ISO 8601 instant dates, using locale and time zone of the system
   iso8601Format = DateTimeFormatter.ISO_INSTANT.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   // Formatter for ISO 8601 dates with offset, using locale and time zone of the system
   iso8601FormatOffset = DateTimeFormatter.ISO_OFFSET_DATE_TIME.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault());

  public static final NumberFormat numberFormat = NumberFormat.getInstance();

  public static final Instant APP_GENESIS_INSTANT = parseIso8601("2012-08-03T06:00:00Z");

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String dateTimeToIso8601(TemporalAccessor t) { return iso8601Format.format(t); }
  public static Instant parseIso8601(String s)               { return Instant.from(iso8601Format.parse(s)); }

  public static String timeToUserReadableStr(TemporalAccessor t)     { return userReadableTimeFormatter.format(t); }
  public static String dateTimeToUserReadableStr(TemporalAccessor t) { return userReadableDateTimeFormatter.format(t); }

  public static String dateTimeToIso8601offset(TemporalAccessor t) { return iso8601FormatOffset.format(t); }
  public static Instant parseIso8601offset(String s)               { return Instant.from(iso8601FormatOffset.parse(s)); }

  public static String dateTimeToHttpDate(TemporalAccessor t) { return httpDate.format(t); }
  public static Instant parseHttpDate(String s)               { return Instant.from(httpDate.parse(s)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getLocaleStr(Locale locale)
  {
    String language = locale.getLanguage();
    String country = locale.getCountry();

    return safeStr(country).isBlank() ?
      language
    :
      language + '-' + country;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String saveStringBuilderToFile(StringBuilder sb, FilePath filePath) throws IOException
  {
    int bufLen = 65536;
    char[] charArray = new char[bufLen];

    MessageDigest md = newMessageDigest();

    try (OutputStream os = Files.newOutputStream(filePath.toPath());
         DigestOutputStream dos = new DigestOutputStream(os, md);
         OutputStreamWriter writer = new OutputStreamWriter(dos, UTF_8))
    {
      for (int offsetIntoSB = 0; offsetIntoSB < sb.length(); offsetIntoSB += bufLen)
      {
        bufLen = Math.min(bufLen, sb.length() - offsetIntoSB);
        sb.getChars(offsetIntoSB, offsetIntoSB + bufLen, charArray, 0);
        writer.write(charArray, 0, bufLen);
      }
    }

    return digestHexStr(md);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String digestHexStr(MessageDigest md)
  {
    return Hex.encodeHexString(md.digest());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MessageDigest newMessageDigest()
  {
    try
    {
      return MessageDigest.getInstance("MD5");
    }
    catch (NoSuchAlgorithmException e)
    {
      throw new AssertionError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String safeSubstring(String str, int start, int end)
  {
    if (start < 0    ) start = 0;
    if (end   < start) end   = start;

    if ((str == null) || (start >= str.length())) return "";

    return end > str.length() ? str.substring(start) : str.substring(start, end);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Removes all horizontal whitespace characters [ \t\xA0\u1680\u180e\u2000-\u200a\u202f\u205f\u3000] at the beginning and end of the string

  public static String ultraTrim(String text)
  {
    return text.trim().replaceAll("(^\\h+)|(\\h+$)", "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static double round(double n)
  {
    return Math.round(n);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String camelToTitle(String in)
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

    return out.replace("  ", " ").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String sentenceCase(String str)
  {
    if (safeStr(str).isEmpty()) return "";

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
        pre = str.substring(0, start).trim();
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

  private static boolean isCorrectlyCapitalized(String word, String prefix)
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

  public static String safeStr(String s)           { return s == null ? "" : s; }

  public static boolean collEmpty(Collection<?> c) { return (c == null) || c.isEmpty(); }  // See ObjectUtils.isEmpty
  public static boolean collEmpty(Map<?, ?> m)     { return (m == null) || m.isEmpty(); }  // See ObjectUtils.isEmpty

  public static <E> List<E> safeListOf(E e1)       { return e1 == null ? List.of() : List.of(e1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SafeVarargs public static <T> void removeAll(Collection<T> col, T... objs)
  {
    col.removeAll(Arrays.asList(objs));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <E extends Enum<E>> E getEnumVal(int ord, Class<E> cls)
  {
    E[] vals = cls.getEnumConstants();

    if ((vals == null) || (ord < 0) || (ord > (vals.length - 1))) return null;

    return vals[ord];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String NORMALIZE_ID = "NFD; [:Nonspacing Mark:] Remove; NFC";
  private static final Transliterator transliterator1 = Transliterator.getInstance("NFD; Any-Latin; NFC; "   + NORMALIZE_ID),
                                      transliterator2 = Transliterator.getInstance("NFD; Latin-ASCII; NFC; " + NORMALIZE_ID);
  private static final Map<Character, String> charMap = new HashMap<>();

  public static String convertToEnglishChars(String input)
  {
    return convertToEnglishCharsWithMap(input, null);
  }

  public static String convertToEnglishCharsWithMap(String input, List<Integer> posMap)
  {
    StringBuilder output = new StringBuilder();

    if (posMap == null) posMap = new ArrayList<>();

    for (int inPos = 0; inPos < input.length(); inPos++)
    {
      char c = input.charAt(inPos);
      String s;

      if (c == '\u2014')
        s = String.valueOf(c);
      else
      {
        s = charMap.get(c);

        if (s == null)
          charMap.put(c, s = transliterator2.transliterate(transliterator1.transliterate(String.valueOf(c))));
      }

      output.append(s);

      for (int ndx = 0; ndx < s.length(); ndx++)
        posMap.add(inPos);
    }

    return output.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern domainPattern = Pattern.compile("^[A-Za-z\\-]+(\\.[A-Za-z\\-]+)+/?$"),
                               pathPattern = Pattern.compile(".*/\\w.*"),
                               fileExtensionPattern = Pattern.compile(".*\\.[a-zA-Z].*"),
                               whitespaceExtensionPattern = Pattern.compile(".*\\.\\h.*");

  public static boolean isStringUrl(String selText)
  {
    selText = ultraTrim(selText);

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
   * Runs a task inside the JavaFX thread with a specified delay and number of cycles.
   *
   * @param cycles the number of times the task should be executed
   * @param delayMS the delay in milliseconds before each execution of the task
   * @param runnable the task to be executed
   */
  public static void runDelayedInFXThread1(int cycles, int delayMS, Runnable runnable)
  {
    Timeline timeline = new Timeline();
    timeline.setCycleCount(cycles);

    timeline.getKeyFrames().add(new KeyFrame(Duration.millis(delayMS), event -> runnable.run()));
    timeline.play();
  }

  /**
   * Runs a task inside the JavaFX thread with a specified delay and number of cycles.
   *
   * @param cycles the number of times the task should be executed (must be positive)
   * @param delayMS the delay in milliseconds before each execution of the task (must be non-negative)
   * @param runnable the task to be executed
   * @throws IllegalArgumentException if cycles is less than 1 or delayMS is negative
   */
  public static void runDelayedInFXThread(int cycles, int delayMS, Runnable runnable)
  {
    if (cycles < 1)
      throw new IllegalArgumentException("Cycle count must be at least 1");

    if (delayMS < 0)
      throw new IllegalArgumentException("Delay must be non-negative");

    Timeline timeline = new Timeline();
    timeline.setCycleCount(cycles);

    timeline.getKeyFrames().add(new KeyFrame(Duration.millis(delayMS), event ->
    {
      try { runnable.run(); }
      catch (Exception e) { e.printStackTrace(); }  // Handle the exception or log it as needed
    }));
    timeline.play();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs a task outside the JavaFX thread after a specified delay.
   *
   * @param delayMS the delay in milliseconds before the task is executed
   * @param runnable the task to be executed
   */
  public static void runOutsideFXThread(int delayMS, Runnable runnable)
  {
    new Timer(true).schedule(new TimerTask() { @Override public void run() { runnable.run(); }}, delayMS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs a task outside the JavaFX thread immediately.
   *
   * @param runnable the task to be executed
   */
  public static void runOutsideFXThread(Runnable runnable)
  {
    new HyperThread(runnable, "Util").start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Halts the Java Virtual Machine (JVM) after a specified delay.
   * <p>
   * This method is a "nuclear option" that stops the JVM immediately and non-gracefully,
   * bypassing shutdown hooks and finalizers. It should be used only in critical situations
   * where an immediate halt is required.
   *
   * @param delayMS the delay in milliseconds before the JVM is halted
   */
  public static void nuclearOption(int delayMS)
  {
    runOutsideFXThread(delayMS, () -> Runtime.getRuntime().halt(0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Operation having no side effects that the compiler will not know how to optimize away, so
   * you can always break on it.
   * @return Always returns null
   */
  public static Object noOp()
  {
    assert Boolean.TRUE;
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Consume an object by doing nothing to it, to avoid compiler warnings and such.
   * @param obj The object to which nothing will be done.
   * @return Always returns null
   */
  public static Object noOp(Object obj)
  {
    assert Boolean.TRUE;
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runInFXThread(Runnable runnable)
  {
    runInFXThread(runnable, false);
  }

  public static void runInFXThread(Runnable runnable, boolean wait)
  {
    if (Platform.isFxApplicationThread())
    {
      runnable.run();
      return;
    }

    if (wait == false)
    {
      Platform.runLater(runnable);
      return;
    }

    final CountDownLatch doneLatch = new CountDownLatch(1);

    Platform.runLater(() ->
    {
      try
      {
        runnable.run();
      }
      finally
      {
        doneLatch.countDown();
      }
    });

    try
    {
      doneLatch.await();
    }
    catch (InterruptedException e)
    {
      e.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Executes a consumer function if the provided object is not null.
   * <p>
   * This method checks if the given object is not null. If the object is non-null, it executes the specified consumer function with the object.
   * </p>
   *
   * @param <T> the type of the object to be tested and passed to the consumer
   * @param obj the object to be tested for null
   * @param ex the consumer function to be executed if the object is non-null
   */
  public static <T> void nullSwitch(T obj, Consumer<T> ex)
  {
    if (obj != null)
      ex.accept(obj);
  }

  /**
   * Returns a default value if the provided object is null, otherwise returns the object itself.
   * <p>
   * This method checks if the given object is null. If the object is null, it returns the specified default value.
   * Otherwise, it returns the object itself.
   * </p>
   *
   * @param <T> the type of the object and the default value
   * @param obj the object to be tested for null
   * @param def the default value to return if the object is null
   * @return the object if it is non-null, otherwise the default value
   */
  public static <T> T nullSwitch(T obj, T def)
  {
    return obj == null ? def : obj;
  }

  /**
   * Applies a transformation function to an object if it is not null, otherwise returns a default value.
   * <p>
   * This method checks if the given object is null. If the object is null, it returns the specified default value.
   * If the object is non-null, it applies the specified function to the object and returns the result.
   * </p>
   *
   * @param <T1> the type of the result produced by the transformation function and the default value
   * @param <T2> the type of the object to be tested for null
   * @param obj the object to be tested for null
   * @param def the default value to return if the object is null
   * @param ex the function to apply to the object if it is non-null
   * @return the result of applying the function to the object if it is non-null, otherwise the default value
   */
  public static <T1, T2> T1 nullSwitch(T2 obj, T1 def, Function<T2, T1> ex)
  {
    return obj == null ? def : ex.apply(obj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Finds the first non-null result by applying a function to each element of an iterable.
   * <p>
   * This method streams over the given iterable, applies the specified function to each
   * element, and returns the first non-null result. If no non-null result is found, it
   * returns {@code null}.
   * </p>
   *
   * @param <T1> the type of the result produced by the function
   * @param <T2> the type of elements in the iterable
   * @param iterable the iterable to be processed
   * @param func the function to apply to each element of the iterable
   * @return the first non-null result produced by the function, or {@code null} if no
   * non-null result is found
   */
  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(Objects::nonNull).findFirst().orElse(null);
  }

  /**
   * Finds the first element in an iterable that, when transformed by a function, matches a given predicate.
   * <p>
   * This method streams over the provided iterable, applies the specified function to each element,
   * and filters the results based on the given predicate. It returns the first non-null element
   * that satisfies the predicate or {@code null} if no such element is found.
   * </p>
   *
   * @param <T1> the type of the result produced by the function and tested by the predicate
   * @param <T2> the type of elements in the iterable
   * @param iterable the iterable to be processed
   * @param func the function to apply to each element of the iterable
   * @param pred the predicate to test the transformed elements
   * @return the first non-null result that satisfies the predicate, or {@code null} if none is found
   */
  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func, Predicate<T1> pred)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(obj -> (obj != null) && pred.test(obj)).findFirst().orElse(null);
  }

  /**
   * Finds the first element in an iterable that matches a given predicate and applies a transformation function.
   * <p>
   * This method searches through the provided iterable to find the first element that satisfies the given predicate.
   * If such an element is found, it is transformed using the specified function and the result is returned.
   * If no matching element is found, the method returns the provided default value.
   * </p>
   *
   * @param <T1> the type of the result produced by the transformation function
   * @param <T2> the type of elements in the iterable
   * @param iterable the iterable to be processed
   * @param pred the predicate to test each element of the iterable
   * @param def the default value to return if no element matches the predicate
   * @param func the function to apply to the element found by the predicate
   * @return the transformed result of the first matching element, or the default value if no match is found
   */
  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, T1 def, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), def, func);
  }

  /**
   * Finds the first element in an iterable that matches a given predicate and applies a transformation function.
   * <p>
   * This method searches through the provided iterable to find the first element that satisfies the given predicate.
   * If such an element is found, it is transformed using the specified function and the result is returned.
   * If no matching element is found, the method returns {@code null}.
   * </p>
   *
   * @param <T1> the type of the result produced by the transformation function
   * @param <T2> the type of elements in the iterable
   * @param iterable the iterable to be processed
   * @param pred the predicate to test each element of the iterable
   * @param func the function to apply to the element found by the predicate
   * @return the transformed result of the first matching element, or {@code null} if no match is found
   */
  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), null, func);
  }

  /**
   * Finds the first element in an iterable that matches a given predicate.
   * <p>
   * This method streams over the provided iterable and filters the elements based on the given predicate.
   * It returns the first element that matches the predicate, or {@code null} if no such element is found.
   * </p>
   *
   * @param <T> the type of elements in the iterable
   * @param iterable the iterable to be processed
   * @param pred the predicate to test each element of the iterable
   * @return the first element that matches the predicate, or {@code null} if no match is found
   */
  public static <T> T findFirst(Iterable<T> iterable, Predicate<T> pred)
  {
    return StreamSupport.stream(iterable.spliterator(), false).filter(pred).findFirst().orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void sleepForMillis(long millis)
  {
    try { Thread.sleep(millis); }
    catch (InterruptedException e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String randomHexStr(int size)          { return randomStr(size, "0123456789abcdef"); }
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

    if ((charsStr == null) || (charsStr.isEmpty()))
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

  public static <T> int addToSortedList(List<T> list, T item, Comparator<? super T> comp)
  {
    int ndx = Collections.binarySearch(list, item, comp);
    ndx = ndx >= 0 ? ndx + 1 : ~ndx;
    list.add(ndx, item);
    return ndx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T extends Comparable<? super T>> int addToSortedList(List<T> list, T item)
  {
    int ndx = Collections.binarySearch(list, item);
    ndx = ndx >= 0 ? ndx + 1 : ~ndx;
    list.add(ndx, item);
    return ndx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static int compareNumberStrings(String str1, String str2)
  {
    MutableInt result = new MutableInt();

    if (compareNumberStrings(str1, str2, result))
      return result.getValue();

    return str1.compareToIgnoreCase(str2);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * If str1 and str2 are integers, returns the result of integer comparison.
   * If they are strings, returns the result of string comparison.
   * If one is an integer and the other isn't, the integer is sorted earlier.
   * @param str1 First string to compare
   * @param str2 Second string to compare
   * @param result Output parameter for result of the comparson
   * @return True if both strings are integers; false otherwise
   */
  public static boolean compareNumberStrings(String str1, String str2, MutableInt result)
  {
    boolean numeric1 = true, numeric2 = true;
    int int1 = 0, int2 = 0;

    if (safeStr(str1).isEmpty() && safeStr(str2).isEmpty())
    {
      result.setValue(0);
      return true;
    }

    try { int1 = Integer.parseInt(safeStr(str1)); }
    catch (NumberFormatException e) { numeric1 = false; }

    try { int2 = Integer.parseInt(safeStr(str2)); }
    catch (NumberFormatException e) { numeric2 = false; }

    if (numeric1 && numeric2)
    {
      result.setValue(int1 - int2);
      return true;
    }

    if (numeric1)
    {
      result.setValue(1);
      return true;
    }

    if (numeric2)
    {
      result.setValue(-1);
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Splits a string into the beginning of the string and a positive whole number that is at the end of the string.<br>
   * The part of the string before the number is output into the second parameter.<br>
   * Examples:<br>
   * "Hello" -> "", -1<br>
   * "Hello 2" -> "Hello ", 2<br>
   * "Hello.2" -> "Hello.", 2<br>
   * "Hello2.2" -> "", -1<br>
   * "Hello2..2" -> "Hello2..", 2<br>
   * "Hello-2" -> "Hello-", 2<br>
   * @param str The input
   * @param prefix If there was a number at the end, this will be set to the part of the string before the number, and will be blank otherwise.
   * @return The number at the end if there was one; -1 otherwise
   */
  public static int splitIntoPrefixAndNumber(String str, StringBuilder prefix)
  {
    int ndx;
    boolean lastWasDot = false;

    for (ndx = str.length() - 1; ndx >= 0; ndx--)
    {
      char c = str.charAt(ndx);

      if (c == '.')
      {
        if (lastWasDot) break;

        lastWasDot = true;
        continue;
      }

      if (Character.digit(c, 10) < 0) break;

      if (lastWasDot)
      {
        assignSB(prefix, "");
        return -1;  // The name ends with a decimal number, not a whole number
      }
    }

    ndx = ndx + (lastWasDot ? 2 : 1);  // Now ndx is the starting index of the numeric portion

    if ((ndx < 1) || (ndx >= str.length()))
    {
      assignSB(prefix, "");
      return -1;
    }

    assignSB(prefix, str.substring(0, ndx));

    return parseInt(str.substring(ndx), -1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Attempts to extract and normalize a DOI (Digital Object Identifier) from a given string.
   * Handles common OCR errors and removes trailing punctuation, except for slashes.
   *
   * <p>This method first attempts to match a DOI within the provided string. If no match is found,
   * it unescapes URL-encoded characters and tries again. Finally, it removes any trailing punctuation
   * (except slashes) from the matched DOI.</p>
   *
   * @param str the input string to search for a DOI
   * @return the normalized DOI if found, otherwise an empty string
   */
  public static String matchDOI(String str)
  {
    // DOI legal characters according to Crossref: "a-z", "A-Z", "0-9" and "-._;()/"
    // But I've seen at least one Crossref DOI that included a colon

    if (safeStr(str).isBlank()) return "";

    String doi = matchDOIiteration(str);

    if (doi.isBlank())
      doi = matchDOIiteration(unescapeURL(str));

    doi = doi.replaceAll("[\\p{Punct}&&[^/]]+$", ""); // Removes trailing punctuation except slash

    return StringUtils.removeEndIgnoreCase(doi, ".pdf");
  }

  private static final Pattern doiPattern1 = Pattern.compile("(\\A|\\D)(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)"),
                               doiPattern2 = Pattern.compile("([dD0][oO0][iI1])?\\\\s*(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");

  private static String matchDOIiteration(String str)
  {
    str = prepareForIDMatch(str, false);

    Matcher m = doiPattern1.matcher(str);

    if (m.find()) return m.group(2);

    str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0').replace('\u00B0', '0'); // \u00B0 is degree sign

    m = doiPattern1.matcher(str);

    if (m.find()) return m.group(2);

    m = doiPattern2.matcher(str);

    return m.find() ? m.group(2) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String prepareForIDMatch(String str, boolean disregardLetters)
  {
    str = str.replaceAll("\\p{Pd}", "-")  // treat all dashes the same
             .replaceAll("\\u00AD", "-")  // "soft hyphen" is not included in the \p{Pd} class
             .replace('\u0002', '/');     // sometimes slash in DOI is encoded as STX control character

    str = convertUnicodeNumeralsToAscii(str);

    if (disregardLetters)
      str = str.replace('l', '1')
               .replace('|', '1')
               .replace('I', '1')
               .replace('o', '0')
               .replace('O', '0')
               .replace('B', '8')
               .replace('S', '5')
               .replace('g', '9')
               .replace('q', '9')
               .replace('D', '0')
               .replace('\u00B0', '0'); // degree sign

    while (str.contains("--"))
      str = str.replace("--", "-");

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern UNICODE_NUMERALS_PATTERN = Pattern.compile
  (
    "[\\u2070\\u2080\\uFF10\\u24EA\\u2460\\u00B9\\u2081\\uFF11\\u00B2\\u2082\\u2072\\uFF12\\u00B3\\u2083\\u2073\\uFF13" +
    "\\u2074\\u2084\\uFF14\\u2075\\u2085\\uFF15\\u2076\\u2086\\uFF16\\u2077\\u2087\\uFF17\\u2078\\u2088\\uFF18\\u2079" +
    "\\u2089\\uFF19]"
  );

  public static String convertUnicodeNumeralsToAscii(String input)
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

      default -> unicodeChar;  // Default case for characters not explicitly matched
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern issnPattern = Pattern.compile("(\\A|\\G|[^0-9\\-])(\\d{4}-\\d{3}[\\dxX])(\\z|[^0-9\\-])");

  public static List<String> matchISSN(String str)
  {
    return matchISSN(str, null);
  }

  public static List<String> matchISSN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (safeStr(str).isEmpty()) return list;

    str = prepareForIDMatch(str, true);

    Matcher m = issnPattern.matcher(str);

    while (m.find())
    {
      String found = m.group(2).replace("-", "");
      int sum = 0;

      for (int x = 0; x < 8; x++)
      {
        char c = found.charAt(x);
        int n = c == 'X' ? 10 : parseInt(String.valueOf(c), -1);

        sum += n * (8 - x);
      }

      if ((sum > 0) && ((sum % 11) == 0))
      {
        found = m.group(2);

        if (list.contains(found) == false)
          list.add(found);
      }
    }

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISBN(String str)
  {
    return matchISBN(str, null);
  }

  public static List<String> matchISBN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (safeStr(str).isEmpty()) return list;

    matchISBNiteration(str, list);

    matchISBNiteration(str.replaceAll("\\h+", ""), list);  // remove horizontal whitespaces and check again

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern isbnPattern1 = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){12}\\d)(\\z|[^0-9\\-])"),
                               isbnPattern2 = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){9}[0-9xX])(\\z|[^0-9xX\\-])");

  private static void matchISBNiteration(String str, List<String> list)
  {
    str = prepareForIDMatch(str, true);

    Matcher m = isbnPattern1.matcher(str);

    while (m.find())
    {
      String found = m.group(2).replace("-", "");

      int n, sum = 0;
      for (int x = 0; x < 12; x++)
      {
        int coeff = ((x % 2) * 2) + 1;
        n = parseInt(String.valueOf(found.charAt(x)), -1);
        sum += coeff * n;
      }

      n = parseInt(StringUtils.right(found, 1), -1);

      if ((sum > 0) && (((10 - (sum % 10)) % 10) == n))
      {
        if (list.contains(found) == false)
          list.add(found);
      }
    }

    m = isbnPattern2.matcher(str);

    while (m.find())
    {
      String found = m.group(2).toUpperCase().replace("-", "");
      int sum1 = 0, sum2 = 0;

      for (int x = 0; x < 10; x++)
      {
        char c = found.charAt(x);
        int n = c == 'X' ? 10 : parseInt(String.valueOf(c), -1);

        sum1 += n * (10 - x);
        sum2 += n * (x + 1);
      }

      if ((sum1 > 0) && (sum2 > 0) && ((sum1 % 11) == 0) && ((sum2 % 11) == 0) && (list.contains(found) == false))
        list.add(found);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Detects the charset of the given byte array using the ICU CharsetDetector.
   *
   * <p>This method uses the ICU4J library's CharsetDetector to analyze the given byte array
   * and identify the most likely charset. It then returns the detected charset as a Charset object.</p>
   *
   * @param byteData the byte array for which to detect the charset
   * @return the detected charset as a Charset object
   * @throws IllegalArgumentException if the input byte array is null
   * @throws java.nio.charset.UnsupportedCharsetException if the detected charset is not supported
   */
  public static Charset detectCharset(byte[] byteData)
  {
    if (byteData == null)
      throw new IllegalArgumentException("Input byte array cannot be null");

    CharsetDetector detector = new CharsetDetector();

    detector.setText(byteData);

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Detects the charset of the given input stream using the ICU CharsetDetector.
   *
   * <p>This method uses the ICU4J library's CharsetDetector to analyze the given input stream
   * and identify the most likely charset. It then returns the detected charset as a Charset object.</p>
   *
   * <p>If the input stream is null, an IllegalArgumentException is thrown. If an IOException occurs
   * while setting the text of the CharsetDetector, null is returned.</p>
   *
   * @param streamData the input stream for which to detect the charset
   * @return the detected charset as a Charset object, or null if an IOException occurs
   * @throws IllegalArgumentException if the input stream is null
   */
  public static Charset detectCharset(InputStream streamData)
  {
    if (streamData == null)
      throw new IllegalArgumentException("Input byte array cannot be null");

    CharsetDetector detector = new CharsetDetector();

    try
    {
      detector.setText(streamData);
    }
    catch (IOException e)
    {
      return null;
    }

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String byteBufferToString(byte[] buf)
  {
    return new String(buf, detectCharset(buf));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean shortcutKeyIsDown(KeyEvent keyEvent)
  {
    return SystemUtils.IS_OS_MAC ? keyEvent.isMetaDown() : keyEvent.isControlDown();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String underlinedChar(char c)
  {
    return c + "\u0332";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a user-friendly message for the given throwable.
   * Handles specific exceptions with tailored messages and generic exceptions with class names.
   *
   * @param e the throwable for which to get the message
   * @return a user-friendly message for the throwable
   */
  public static String getThrowableMessage(Throwable e)
  {
    String msg = e.getMessage();

    if (safeStr(msg).isBlank() || "null".equals(String.valueOf(msg)))
      msg = "";

    if (e instanceof UnknownHostException)
      return "Unable to connect to host" + (msg.isEmpty() ? "" :  ": ") + msg;

    if (e instanceof HttpResponseException httpResponseException)
      return httpResponseException.getStatusCode() + (msg.isEmpty() ? "" : " ") + msg;

    if (e instanceof AccessDeniedException)
      return "Access denied" + (msg.isEmpty() ? "" : ". ") + msg;

    if (e instanceof IOException)
    {
      if (e.getClass().equals(FileSystemException.class))
        return msg.isEmpty() ? "File system error" : msg;

      return userFriendlyThrowableName(e) + (msg.isEmpty() ? "" :  ": ") + msg;
    }

    return msg.isEmpty() ? userFriendlyThrowableName(e) : msg;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Converts a class name to a user-friendly title by converting camel case to spaces and capitalizing words.
   *
   * <p>This method transforms a camel case class name into a more readable format by inserting spaces between words
   * and capitalizing each word. It is particularly useful for creating readable error messages or user interfaces
   * that display class names.</p>
   *
   * <p>Examples:</p>
   * <pre>
   *     {@code userFriendlyClassName(MyCustomException.class)} returns {@code "My Custom Exception"}
   *     {@code userFriendlyClassName(IOException.class)} returns {@code "IO Exception"}
   *     {@code userFriendlyClassName(AccessDeniedException.class)} returns {@code "Access Denied Exception"}
   * </pre>
   *
   * @param klass the class for which to get the user-friendly name
   * @return the user-friendly class name
   */
  public static String userFriendlyClassName(Class<?> klass)
  {
    return camelToTitle(klass.getSimpleName()).trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a user-friendly name for the given throwable by removing the "Exception" suffix if present.
   *
   * @param e the throwable for which to get the user-friendly name
   * @return the user-friendly throwable name
   */
  public static String userFriendlyThrowableName(Throwable e)
  {
    return StringUtils.removeEnd(userFriendlyClassName(e.getClass()), "Exception").trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean anyIsInstanceOf(Class<?> clazz, Object... objects)
  {
    return Arrays.stream(objects).anyMatch(clazz::isInstance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean allAreInstancesOf(Class<?> clazz, Object... objects)
  {
    return Arrays.stream(objects).allMatch(clazz::isInstance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Utility function to convert a {@link java.util.stream.Stream} into an {@link java.lang.Iterable}.
   *
   * <p>This method is particularly useful when you need to integrate streams with APIs
   * or components that operate on {@code Iterable}. It leverages the stream's {@code iterator}
   * method reference to create the {@code Iterable}.
   *
   * <p><strong>Note:</strong> Since a {@code Stream} can only be traversed once, ensure that
   * the stream has not been consumed prior to calling this function. Once the iterator method
   * is called, the stream cannot be used again.
   *
   * <p>Example usage:<pre>
   * {@code
   * Stream<String> stream = Stream.of("one", "two", "three");
   *
   * for (String s : streamToIterable(stream))
   * {
   *   System.out.println(s);
   * }
   * </pre>
   *
   * @param <T> the type of elements in the stream
   * @param stream the stream to be converted into an iterable
   * @return an iterable that uses the stream's iterator
   * @throws NullPointerException if the stream is null
   */
  public static <T> Iterable<T> streamToIterable(Stream<T> stream)
  {
    return stream::iterator;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
