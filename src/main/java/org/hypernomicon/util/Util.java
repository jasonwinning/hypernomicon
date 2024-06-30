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

import org.hypernomicon.App;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.dialogs.LockedDlgCtrlr;
import org.hypernomicon.util.filePath.FilePath;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import static java.nio.charset.StandardCharsets.*;

import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
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
import java.util.Set;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.CountDownLatch;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
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

  public static String trimLines(String input)
  {
    input = safeStr(input);

    if (ultraTrim(convertToSingleLine(input)).isEmpty()) return "";

    List<String> list = convertMultiLineStrToStrList(input, true);

    list.replaceAll(Util::ultraTrim);

    return strListToStr(list, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> convertMultiLineStrToStrList(String str, boolean emptiesOK)
  {
    List<String> list = Lists.newArrayList(str.split("\\r?\\n"));

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

    if (emptiesOK == false)
      list.removeIf(s -> ultraTrim(s).isBlank());

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String strListToStr(List<String> list, boolean emptiesOK)
  {
    return strListToStr(list, emptiesOK, false);
  }

  public static String strListToStr(List<String> list, boolean emptiesOK, boolean useSystemNewLineChar)
  {
    Stream<StringBuilder> strm = (emptiesOK ? list.stream() : list.stream().filter(one -> safeStr(one).length() > 0)).map(StringBuilder::new);
    return strm.reduce((all, one) -> all.append(useSystemNewLineChar ? System.lineSeparator() : "\n").append(one)).orElse(new StringBuilder()).toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String strListToSpaceDelimitedStr(List<String> list)
  {
    return ultraTrim(list.stream().map(Util::ultraTrim).reduce((s1, s2) -> s1 + ' ' + s2).orElse(""));
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

  public static void removeDupsInStrList(List<String> list)
  {
    Set<String> set = new HashSet<>();
    Iterator<String> it = list.iterator();

    while (it.hasNext())
    {
      String str = it.next();
      if (set.contains(str))
        it.remove();
      else
        set.add(str);
    }
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
      String str1 = ultraTrim(list1.get(ndx)), str2 = ultraTrim(list2.get(ndx));

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

  public static void showStackTrace(Throwable e)
  {
    new LockedDlgCtrlr("Error", e).showModal();
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

   userReadableDateTimeFormatter = DateTimeFormatter.ofPattern("M/d/yyyy h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),
   userReadableTimeFormatter = DateTimeFormatter.ofPattern("h:mm:ss a").withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   httpDate = DateTimeFormatter.RFC_1123_DATE_TIME.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),

   iso8601Format = DateTimeFormatter.ISO_INSTANT.withLocale(Locale.getDefault()).withZone(ZoneId.systemDefault()),
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

  public static int indexOfAny(String chars, String text)
  {
    int lowestPos = -1;

    for (char c : chars.toCharArray())
    {
      int curPos = text.indexOf(c);

      if ((curPos > -1) && ((lowestPos == -1) || (curPos < lowestPos)))
        lowestPos = curPos;
    }

    return lowestPos;
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

      word = word.toLowerCase();

      if (noCaps == false)
      {
        if ((lastChar == ':') || (lastChar == '?') || (lastChar == '/') || (start == 0))
          word = word.substring(0, 1).toUpperCase() + safeSubstring(word, 1, word.length()).toLowerCase();
        else if ((word.length() == 1) && endsWithDot)
          word = word.substring(0, 1).toUpperCase();
        else
        {
          switch (word)
          {
            case "\u00e0": // Latin small letter a with grave accent, as in 'vis a vis' or 'a la'

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
              word = word.substring(0, 1).toUpperCase() + word.substring(1).toLowerCase();
          }
        }
      }

      str = pre + word + post;
    }

    return str;
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

  public static boolean strContainsAnyStr(String container, String... strings)
  {
    return Arrays.stream(strings).parallel().anyMatch(container::contains);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String safeStr(String s)           { return s == null ? "" : s; }

  public static boolean collEmpty(Collection<?> c) { return (c == null) || c.isEmpty(); }
  public static boolean collEmpty(Map<?, ?> m)     { return (m == null) || m.isEmpty(); }

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

  public static String manifestValue(String key)
  {
    URL url = App.class.getResource("/META-INF/MANIFEST.MF");

    if (url != null) try
    {
      URLConnection c = url.openConnection();

      if (c instanceof JarURLConnection jarURLConnection)
        return safeStr(nullSwitch(jarURLConnection.getManifest(), "", manifest -> manifest.getMainAttributes().getValue(key)));
    }
    catch (IOException e) { noOp(); }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

           selText.matches("^[A-Za-z\\-]+(\\.[A-Za-z\\-]+)+/?$") ||
           selText.matches(".*/\\w.*") && selText.matches(".*\\.[a-zA-Z].*") && !selText.matches(".*\\.\\h.*");
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

  public static void runDelayedInFXThread(int cycles, int delayMS, Runnable runnable)
  {
    Timeline timeline = new Timeline();
    timeline.setCycleCount(cycles);

    timeline.getKeyFrames().add(new KeyFrame(Duration.millis(delayMS), event -> runnable.run()));
    timeline.play();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runOutsideFXThread(int delayMS, Runnable runnable)
  {
    new Timer(true).schedule(new TimerTask() { @Override public void run() { runnable.run(); }}, delayMS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void runOutsideFXThread(Runnable runnable)
  {
    new HyperThread(runnable, "Util").start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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

  public static void readResourceTextFile(String relPath, StringBuilder strBuilder, boolean keepEOLchars) throws IOException
  {
    assignSB(strBuilder, "");

    try (BufferedReader reader = new BufferedReader(new InputStreamReader(App.class.getResourceAsStream(relPath), UTF_8)))
    {
      String line;

      while ((line = reader.readLine()) != null)
      {
        if (keepEOLchars && (strBuilder.length() > 0))
          strBuilder.append('\n');

        strBuilder.append(line);
      }
    }
    catch (NullPointerException e)
    {
      throw new IOException(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T>      void nullSwitch(T  obj,         Consumer<T>      ex) { if (obj != null)           ex.accept(obj); }
  public static <T>      T    nullSwitch(T  obj, T  def                     ) { return obj == null ? def : obj           ; }
  public static <T1, T2> T1   nullSwitch(T2 obj, T1 def, Function<T2, T1> ex) { return obj == null ? def : ex.apply(obj) ; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(Objects::nonNull).findFirst().orElse(null);
  }

  public static <T1, T2> T1 findFirstHaving(Iterable<T2> iterable, Function<T2, T1> func, Predicate<T1> pred)
  {
    return StreamSupport.stream(iterable.spliterator(), false).map(func).filter(obj -> (obj != null) && pred.test(obj)).findFirst().orElse(null);
  }

  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, T1 def, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), def, func);
  }

  public static <T1, T2> T1 findFirst(Iterable<T2> iterable, Predicate<T2> pred, Function<T2, T1> func)
  {
    return nullSwitch(findFirst(iterable, pred), null, func);
  }

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

  private static String randomStr(int size, String charsStr)
  {
    if (size < 0) return "";

    char[] chars = charsStr.toCharArray(), out = new char[size];
    double numChars = chars.length;

    for (int j = 0; j < size; j++)
    {
      double x = Math.random() * numChars;
      out[j] = chars[(int)x];
    }

    return new String(out);
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

  public static int compareYears(String year1, String year2)
  {
    return compareNumberStrings(year1, year2);
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
   * Returns the non-negative integer, if any, at the beginning of the string.<br>
   * Examples:<br>
   * "123-456" -> 123<br>
   * "012; XYZ" -> 12<br>
   * "5.5" -> 5<br>
   * "-5.5" -> -1<br>
   * @param s The string
   * @return The non-negative integer that begins the string, or -1 if the string doesn't start with a non-negative integer or the number is too large to be stored as an integer.
   */
  public static int extractLeadingNumber(String s)
  {
    if (safeStr(s).isBlank()) return -1;

    int i = 0,
        radix = 10,
        len = s.length(),
        limit = -Integer.MAX_VALUE,
        multmin = limit / radix,
        result = 0;

    // Accumulating negatively avoids surprises near MAX_VALUE

    while (i < len)
    {
      if (result < multmin)
        return -1;

      int digit = Character.digit(s.charAt(i++), radix);
      if (digit < 0)
        return i == 1 ? -1 : -result;

      result *= radix;
      if (result < (limit + digit))
        return -1;

      result -= digit;
    }

    return -result;
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

// DOI legal characters according to Crossref: "a-z", "A-Z", "0-9" and "-._;()/"
// But I've seen at least one Crossref DOI that included a colon

  public static String matchDOI(String str)
  {
    String doi = matchDOIiteration(str);

    return doi.length() > 0 ? doi : matchDOIiteration(unescapeURL(str));
  }

  private static String matchDOIiteration(String str)
  {
    str = prepareForIDMatch(str, false);

    Pattern p = Pattern.compile("(\\A|\\D)(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");
    Matcher m = p.matcher(str);

    if (m.find()) return m.group(2);

    str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0').replace('\u00B0', '0'); // \u00B0 is degree sign

    m = p.matcher(str);

    if (m.find()) return m.group(2);

    p = Pattern.compile("([dD]0[i1])(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");
    m = p.matcher(str);

    return m.find() ? m.group(2) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISSN(String str)
  {
    return matchISSN(str, null);
  }

  public static List<String> matchISSN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (safeStr(str).isEmpty()) return list;

    str = prepareForIDMatch(str, true);

    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])(\\d{4}-\\d{3}[\\dxX])(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);

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

  private static String prepareForIDMatch(String str, boolean disregardLetters)
  {
    str = str.replaceAll("\\p{Pd}", "-")  // treat all dashes the same
             .replaceAll("\\u00AD", "-")  // "soft hyphen" is not included in the \p{Pd} class

        .replace('\u0002', '/'); // sometimes slash in DOI is encoded as STX control character

    if (disregardLetters)
      str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0').replace('\u00B0', '0'); // \u00B0 is degree sign

    while (str.contains("--"))
      str = str.replace("--", "-");

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void matchISBNiteration(String str, List<String> list)
  {
    str = prepareForIDMatch(str, true);

    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){12}\\d)(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);

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

    p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){9}[0-9xX])(\\z|[^0-9xX\\-])");
    m = p.matcher(str);

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

  public static Charset detectCharset(byte[] byteData)
  {
    CharsetDetector detector = new CharsetDetector();

    detector.setText(byteData);

    return Charset.forName(detector.detect().getName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Charset detectCharset(InputStream streamData)
  {
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

  public static String userFriendlyClassName(Class<?> klass)
  {
    return camelToTitle(klass.getSimpleName()).trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String userFriendlyThrowableName(Throwable e)
  {
    String className = userFriendlyClassName(e.getClass());

    return className.endsWith(" Exception") ?
      className.substring(0, className.length() - 10)
    :
      className;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
