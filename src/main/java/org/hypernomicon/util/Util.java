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

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.Exceptions;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.http.HttpResponseException;

import static org.hypernomicon.util.StringUtil.*;

import java.io.*;
import java.net.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.security.*;
import java.text.NumberFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.time.temporal.Temporal;
import java.time.temporal.TemporalAccessor;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.*;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.application.Platform;
import javafx.scene.input.*;
import javafx.util.Duration;

import javax.net.ssl.SSLHandshakeException;

import org.apache.commons.lang3.*;
import org.apache.commons.lang3.mutable.MutableInt;

import com.google.common.escape.Escaper;
import com.google.common.html.HtmlEscapers;
import com.google.common.xml.XmlEscapers;

import com.ibm.icu.text.CharsetDetector;

//---------------------------------------------------------------------------

public final class Util
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A placeholder object used to differentiate method signatures and avoid type erasure conflicts.
   * This constant is meant to be passed as an argument to indicate that the parameter is unused.
   */
  public static final Object UNUSED = new Object();

  public static final Lock globalLock = new ReentrantLock();

  public static final StopWatch stopWatch1 = new StopWatch(), stopWatch2 = new StopWatch(), stopWatch3 = new StopWatch(),
                                stopWatch4 = new StopWatch(), stopWatch5 = new StopWatch(), stopWatch6 = new StopWatch();

  public static final Escaper htmlEscaper         = HtmlEscapers.htmlEscaper(),
                              xmlContentEscaper   = XmlEscapers.xmlContentEscaper(),
                              xmlAttributeEscaper = XmlEscapers.xmlAttributeEscaper();

  private Util() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Copies the given string to the system clipboard.
   * If the string is null, the clipboard will be cleared.
   *
   * @param str the string to be copied to the clipboard, may be null
   */
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
   * Parses the given string into an integer.
   * If the string is not a valid number or is null, it returns the default value.
   *
   * @param value the string to be parsed
   * @param def the default value to return if parsing fails or if the input is null
   * @return the parsed integer value, or the default value if parsing fails or if the input is null
   */
  public static int parseInt(String value, int def)
  {
    if (value == null) return def;

    try { return Integer.parseInt(value); }
    catch (NumberFormatException e) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Parses a hexadecimal string into an integer.
   * If the string is null, invalid, or cannot be parsed, it returns the default value.
   *
   * @param value the hexadecimal string to be parsed
   * @param def the default value to return if parsing fails or if the input is null
   * @return the parsed integer value, or the default value if parsing fails or if the input is null
   */
  public static int parseHex(String value, int def)
  {
    if (value == null) return def;

    if ((value.length() > 2) && (value.charAt(0) == '0') && ((value.charAt(1) == 'x') || (value.charAt(1) == 'X')))
      value = value.substring(2);

    try { return Integer.parseInt(value, 16); }
    catch (NumberFormatException e) { return def; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Parses the given string into a long.
   * If the string is not a valid number, returns the default value.
   *
   * @param value the string to be parsed
   * @param def the default value to return if parsing fails or if the input is null
   * @return the parsed long value, or the default value if parsing fails or if the input is null
   */
  public static long parseLong(String value, long def)
  {
    if (value == null) return def;

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

  public static String escapeURL(String url, boolean removeParen)
  {
    if (removeParen)
      url = removeFirstParenthetical(url);

    return URLEncoder.encode(url.strip(), StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String unescapeURL(String url)
  {
    try
    {
      return URLDecoder.decode(safeStr(url), StandardCharsets.UTF_8);
    }
    catch (IllegalArgumentException e)
    {
      return "";
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Parses a boolean value from the given string. Recognizes various true and false representations.
   *
   * @param s the string to parse
   * @return true if the string represents a true value, false otherwise
   */
  public static boolean parseBoolean(String s)
  {
    if (s == null) return false;

    String lowerCaseValue = s.strip().toLowerCase();

    return "true"   .equals(lowerCaseValue) ||
           "yes"    .equals(lowerCaseValue) ||
           "1"      .equals(lowerCaseValue) ||
           "on"     .equals(lowerCaseValue) ||
           "enabled".equals(lowerCaseValue);
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

  public static String dateTimeToIso8601(TemporalAccessor t)         { return iso8601Format.format(t); }
  public static Instant parseIso8601(CharSequence s)                 { return Instant.from(iso8601Format.parse(s)); }

  public static String timeToUserReadableStr(TemporalAccessor t)     { return userReadableTimeFormatter.format(t); }
  public static String dateTimeToUserReadableStr(TemporalAccessor t) { return userReadableDateTimeFormatter.format(t); }

  public static String dateTimeToIso8601offset(TemporalAccessor t)   { return iso8601FormatOffset.format(t); }
  public static Instant parseIso8601offset(CharSequence s)           { return Instant.from(iso8601FormatOffset.parse(s)); }

  public static String dateTimeToHttpDate(TemporalAccessor t)        { return httpDate.format(t); }
  public static Instant parseHttpDate(CharSequence s)                { return Instant.from(httpDate.parse(s)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns a string representation of the given Locale in the format "language-country".
   * If the country code is blank, only the language code is returned.
   *
   * <p>Examples:
   * <pre>
   * {@code
   * // Example 1: Locale with both language and country
   * Locale locale1 = new Locale("en", "US");
   * String result1 = getLocaleStr(locale1); // Returns "en-US"
   *
   * // Example 2: Locale with only language
   * Locale locale2 = new Locale("fr");
   * String result2 = getLocaleStr(locale2); // Returns "fr"
   *
   * // Example 3: Locale with blank country
   * Locale locale3 = new Locale("es", "");
   * String result3 = getLocaleStr(locale3); // Returns "es"
   *
   * // Example 4: Null Locale
   * String result4 = getLocaleStr(null); // Returns ""
   * }
   * </pre>
   *
   * @param locale the Locale object
   * @return a string representation of the Locale in the format "language-country" or just "language" if the country is blank
   */
  public static String getLocaleStr(Locale locale)
  {
    String language = locale.getLanguage();
    String country = locale.getCountry();

    return strNullOrBlank(country) ?
      language
    :
      language + '-' + country;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Saves the content of the given StringBuilder to a file and returns the MD5 checksum of the content.
   * <p>
   * The content is written to the specified file using a buffer of size 65536 characters. The MD5 checksum
   * is calculated while writing the content to the file.
   *
   * @param sb the StringBuilder containing the content to be saved
   * @param filePath the path of the file where the content will be saved
   * @param encoding The character set
   * @return the MD5 checksum of the saved content as a hexadecimal string
   * @throws IOException if an I/O error occurs while writing to the file
   */
  public static String saveStringBuilderToFile(StringBuilder sb, FilePath filePath, Charset encoding) throws IOException
  {
    int bufLen = 65536;
    char[] charArray = new char[bufLen];

    MessageDigest md = newMessageDigest();

    try (OutputStream os = Files.newOutputStream(filePath.toPath());
         DigestOutputStream dos = new DigestOutputStream(os, md);
         OutputStreamWriter writer = new OutputStreamWriter(dos, encoding))
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

  /**
   * Returns the hexadecimal string representation of the digest.
   *
   * @param md the MessageDigest object containing the digest
   * @return the hexadecimal string representation of the digest
   */
  public static String digestHexStr(MessageDigest md)
  {
    return HexFormat.of().formatHex(md.digest());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a new MessageDigest instance for MD5 hashing.
   *
   * @return a new MessageDigest instance
   * @throws AssertionError if the MD5 algorithm is not available
   */
  public static MessageDigest newMessageDigest()
  {
    try
    {
      return MessageDigest.getInstance("MD5");
    }
    catch (NoSuchAlgorithmException e)
    {
      throw newAssertionError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Computes the MD5 hash of an InputStream and returns it as a hexadecimal string.
   *
   * @param is the InputStream to hash
   * @return the MD5 hash as a lowercase hexadecimal string
   * @throws IOException if an I/O error occurs
   */
  public static String md5Hex(InputStream is) throws IOException
  {
    MessageDigest md = newMessageDigest();
    byte[] buffer = new byte[8192];
    int bytesRead;

    while ((bytesRead = is.read(buffer)) != -1)
      md.update(buffer, 0, bytesRead);

    return digestHexStr(md);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true iff c is null or empty.
   * @param c the collection
   * @return Boolean return value
   */
  public static boolean collEmpty(Collection<?> c) { return (c == null) || c.isEmpty(); }  // See ObjectUtils.isEmpty

  /**
   * Returns true iff m is null or empty.
   * @param m the map
   * @return Boolean return value
   */
  public static boolean collEmpty(Map<?, ?> m)     { return (m == null) || m.isEmpty(); }  // See ObjectUtils.isEmpty

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a singleton list containing the specified element, or an empty list if the element is {@code null}.
   * <p>
   * This method ensures safe handling of {@code null} values by returning an immutable empty list when
   * the input element is {@code null}. If the element is non-null, it returns an immutable list containing
   * only that element. This is useful for safely creating lists without the risk of {@code null} values.
   * </p>
   *
   * @param <E> the type of the element
   * @param e1 the element to include in the list; may be {@code null}
   * @return an immutable list containing the element if it is non-null, or an empty list if the element is {@code null}
   * @throws NullPointerException if the element is {@code null} and the underlying {@code List.of()} implementation
   *         does not support {@code null}
   */
  public static <E> List<E> safeListOf(E e1)       { return e1 == null ? List.of() : List.of(e1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes all specified objects from the given collection.
   *
   * @param <T>  The type of elements in the collection.
   * @param col  The collection from which the specified objects will be removed.
   * @param objs The objects to be removed from the collection. This is a variable-length argument.
   *             Note that the method is annotated with {@code @SafeVarargs}, ensuring safe usage with generics.
   * @throws NullPointerException If the collection or any of the objects provided are null.
   *                              Ensure that all arguments are properly initialized.
   */
  @SafeVarargs public static <T> void removeAll(Collection<T> col, T... objs)
  {
    col.removeAll(Arrays.asList(objs));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the enum constant of the specified enum type with the specified ordinal value.
   *
   * @param <E> the enum type whose constant is to be returned
   * @param ord the ordinal value of the enum constant to be returned
   * @param cls the Class object of the enum type from which to return a constant
   * @return the enum constant with the specified ordinal value, or {@code null} if the ordinal value is out of range
   * @throws NullPointerException if {@code cls} is null
   * @throws IllegalArgumentException if {@code cls} is not an enum class
   */
  public static <E extends Enum<E>> E getEnumVal(int ord, Class<E> cls)
  {
    E[] vals = Objects.requireNonNull(cls, "Class must not be null").getEnumConstants();

    if (vals == null) throw new IllegalArgumentException("Invalid enum type");

    return (ord < 0) || (ord >= vals.length) ? null : vals[ord];
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs a task inside the JavaFX thread with a specified delay and number of cycles.
   *
   * @param cycles the number of times the task should be executed (must be positive)
   * @param delayMS the delay in milliseconds before each execution of the task (must be non-negative)
   * @param runnable the task to be executed
   * @return the {@link Timeline} controlling the scheduled executions,
   *         already started via {@link Timeline#play()}, which can be
   *         paused, stopped, or queried by the caller
   * @throws IllegalArgumentException if cycles is less than 1 or delayMS is negative
   */
  public static Timeline runDelayedInFXThread(int cycles, long delayMS, Runnable runnable)
  {
    Objects.requireNonNull(runnable, "Runnable must not be null");

    if (cycles < 1)
      throw new IllegalArgumentException("Cycle count must be at least 1");

    if (delayMS < 0)
      throw new IllegalArgumentException("Delay must be non-negative");

    Timeline timeline = new Timeline(new KeyFrame(Duration.millis(delayMS), _ ->
    {
      try { runnable.run(); }
      catch (Exception e) { logThrowable(e); }
    }));

    timeline.setCycleCount(cycles);

    timeline.play();

    return timeline;
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
   * Puts the current thread to sleep for the specified number of milliseconds.
   *
   * @param millis the length of time to sleep in milliseconds
   * @throws IllegalArgumentException if the value of millis is negative
   * @throws InterruptedException if any thread has interrupted the current thread.
   */
  public static void sleepForMillis(long millis)
  {
    try { Thread.sleep(millis); }
    catch (InterruptedException e) { Thread.currentThread().interrupt(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Calculates the difference in milliseconds between two Instant objects.
   * The result is always positive, representing the absolute magnitude of the difference.
   *
   * @param instant1 the first Instant object
   * @param instant2 the second Instant object
   * @return the positive difference in milliseconds between instant1 and instant2
   */
  public static long milliDiff(Temporal instant1, Temporal instant2)
  {
    return Math.abs(java.time.Duration.between(instant1, instant2).toMillis());
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
  public static Object noOpReturnNull()
  {
    assert Boolean.TRUE;
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Consume an object by doing nothing to it, to avoid compiler warnings and such.
   * @param ignoredObj The object to which nothing will be done.
   * @return Always returns null
   */
  public static Object noOpReturnNull(Object ignoredObj)
  {
    assert Boolean.TRUE;
    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Operation having no side effects that the compiler will not know how to optimize away, so
   * you can always break on it.
   */
  public static void noOp()
  {
    assert Boolean.TRUE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Consume an object by doing nothing to it, to avoid compiler warnings and such.
   * @param ignoredObj The object to which nothing will be done.
   */
  public static void noOp(Object ignoredObj)
  {
    assert Boolean.TRUE;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Schedules a task to run on the JavaFX Application Thread after a specified
   * number of JavaFX layout/render pulses have occurred.
   * <p>
   * A "pulse" in JavaFX is a single iteration of the UI update cycle, during
   * which layout, CSS, and rendering are processed. This method defers execution
   * by chaining {@link Platform#runLater(Runnable)} calls, decrementing the
   * {@code pulses} counter on each pulse until it reaches zero.
   * </p>
   * <p>
   * If {@code pulses} is less than or equal to zero, the {@code runnable} is
   * executed immediately if already on the JavaFX Application Thread, or
   * scheduled via {@link Platform#runLater(Runnable)} if called from a
   * background thread.
   * </p>
   * <p>
   * This method is useful when you need to wait for one or more layout passes
   * before running code that depends on updated node bounds, CSS application,
   * or scene graph state.
   * </p>
   *
   * @param pulses   the number of JavaFX pulses to wait before executing the task;
   *                 if zero or negative, the task runs immediately (or is queued
   *                 for immediate execution on the FX thread)
   * @param runnable the task to execute on the JavaFX Application Thread;
   *                 must not be {@code null}
   * @throws NullPointerException if {@code runnable} is {@code null}
   */
  public static void runInFXThreadAfterPulses(int pulses, Runnable runnable)
  {
    if (pulses <= 0)
      runInFXThread(runnable);
    else
      Platform.runLater(() -> runInFXThreadAfterPulses(pulses - 1, runnable));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Ensures that the given Runnable is executed on the JavaFX Application Thread.
   * If the current thread is the JavaFX Application Thread, it runs the Runnable directly.
   * If not, it schedules the Runnable to be executed on the JavaFX Application Thread.
   *
   * @param runnable the Runnable to be executed
   * @throws NullPointerException if the runnable is null
   * @throws InterruptedException if the current thread is interrupted while waiting for the Runnable to complete
   */
  public static void runInFXThread(Runnable runnable)
  {
    runInFXThread(runnable, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs a Runnable in the JavaFX Application Thread.
   * <p>
   * If the current thread is the JavaFX Application Thread, the Runnable is executed immediately.<br>
   * If waitForCompletion is false, the Runnable is scheduled to run on the JavaFX Application Thread and this method returns immediately.<br>
   * If waitForCompletion is true, the Runnable is scheduled to run on the JavaFX Application Thread and this method waits for its completion.
   *
   * @param runnable the Runnable to be executed in the JavaFX Application Thread
   * @param waitForCompletion if true, waits for the Runnable to finish execution before returning; if false, returns immediately after scheduling the Runnable
   * @throws NullPointerException if the runnable is null
   * @throws InterruptedException if the current thread is interrupted while waiting for the Runnable to complete
   */
  public static void runInFXThread(Runnable runnable, boolean waitForCompletion)
  {
    Objects.requireNonNull(runnable);

    if (Platform.isFxApplicationThread())
    {
      runnable.run();
      return;
    }

    if (waitForCompletion == false)
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
      Thread.currentThread().interrupt();
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
   * @param consumer the consumer function to be executed if the object is non-null
   */
  public static <T> void nullSwitch(T obj, Consumer<T> consumer)
  {
    if (obj != null)
      consumer.accept(obj);
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
   * @param function the function to apply to the object if it is non-null
   * @return the result of applying the function to the object if it is non-null, otherwise the default value
   */
  public static <T1, T2> T1 nullSwitch(T2 obj, T1 def, Function<T2, T1> function)
  {
    return obj == null ? def : function.apply(obj);
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
    return iterableToStream(iterable).map(func).filter(Objects::nonNull).findFirst().orElse(null);
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
    return iterableToStream(iterable).map(func).filter(obj -> (obj != null) && pred.test(obj)).findFirst().orElse(null);
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
    return iterableToStream(iterable).filter(pred).findFirst().orElse(null);
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
      return result.intValue();

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

    if (strNullOrEmpty(str1) && strNullOrEmpty(str2))
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

    if (strNullOrBlank(str)) return "";

    String doi = matchDOIiteration(str);

    if (doi.isBlank())
      doi = matchDOIiteration(unescapeURL(str));

    doi = doi.replaceAll("[\\p{Punct}&&[^/]]+$", ""); // Removes trailing punctuation except slash

    return Strings.CI.removeEnd(doi, ".pdf");
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

  /**
   * Prepares a string for ID matching by cleaning up common OCR artifacts/mistakes.
   *
   * @param str the input string to prepare for ID matching
   * @param disregardLetters True if the ID should not contain letters (e.g., 'I' will be interpreted as '1')
   * @return the normalized string prepared for ID matching
   */
  private static String prepareForIDMatch(String str, boolean disregardLetters)
  {
    str = str.replaceAll("[|\\uFE31\\uFE32]", "1")  // Vertical dashes
             .replaceAll("[\\p{Pd}\\u00AD]", "-")   // "soft hyphen" is not included in the \p{Pd} punctuation-dash class
             .replace('\u0002', '/');               // sometimes slash in DOI is encoded as STX control character

    if (disregardLetters)
      str = str.replace('l', '1')
               .replace('I', '1')
               .replace('o', '0')
               .replace('O', '0')
               .replace('B', '8')
               .replace('S', '5')
               .replace('g', '9')
               .replace('q', '9')
               .replace('D', '0')
               .replace('\u00B0', '0'); // degree sign

    str = convertUnicodeNumeralsToAscii(str);

    while (str.contains("--"))
      str = str.replace("--", "-");

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern issnPattern = Pattern.compile("(\\A|\\G|[^0-9\\-])(\\d{4}-\\d{3}[\\dxX])(\\z|[^0-9\\-])");

  /**
   * Matches and extracts ISSNs from the given string.
   *
   * @param str the string to search for ISSNs
   * @return a list of matched ISSNs
   */
  public static List<String> matchISSN(String str)
  {
    return matchISSN(str, null);
  }

  /**
   * Matches and extracts ISSNs from the given string, adding them to the provided list.
   *
   * @param str the string to search for ISSNs
   * @param list the list to add matched ISSNs to (if null, a new list is created)
   * @return the list of matched ISSNs
   */
  public static List<String> matchISSN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (strNullOrBlank(str)) return list;

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

  /**
   * Matches and extracts ISBNs from the given string.
   *
   * @param str the string to search for ISBNs
   * @return a list of matched ISBNs
   */
  public static List<String> matchISBN(String str)
  {
    return matchISBN(str, null);
  }

  /**
   * Matches and extracts ISBNs from the given string, adding them to the provided list.
   *
   * @param str the string to search for ISBNs
   * @param list the list to add matched ISBNs to (if null, a new list is created)
   * @return the list of matched ISBNs
   */
  public static List<String> matchISBN(String str, List<String> list)
  {
    if (list == null) list = new ArrayList<>();
    if (strNullOrBlank(str)) return list;

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

  /**
   * Converts a byte buffer to a string using the detected character set.
   *
   * <p>This method takes a byte array and decodes it into a string using the charset
   * determined by {@link #detectCharset(byte[])}.</p>
   *
   * @param buf The byte array to convert. Must not be {@code null}.
   * @return A string representation of the byte buffer using the detected charset.
   * @throws NullPointerException if {@code buf} is {@code null}.
   * @throws java.nio.charset.UnsupportedCharsetException if the detected charset is not supported.
   */
  public static String byteBufferToString(byte[] buf)
  {
    return new String(buf, detectCharset(buf));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Logs the stack trace for a throwable, preventing HDB_InternalError
   * or any Throwable having HDB_InternalError in its chain of causes
   * logged twice.
   * @param e The throwable
   */
  public static void logThrowable(Throwable e)
  {
    Exceptions.log(e);
  }

  /**
   * Creates a new NullPointerException caused by an HDB_InternalError with the specified
   * characteristics
   * @param internalErrorNum The internal error number
   * @param immediatelyLog Whether to immediately log the internal error
   * @return The new NullPointerException
   */
  public static NullPointerException newNullPointerInternalError(int internalErrorNum, boolean immediatelyLog)
  {
    HDB_InternalError internalError = new HDB_InternalError(internalErrorNum, immediatelyLog);
    NullPointerException npe = new NullPointerException(getThrowableMessage(internalError));
    npe.initCause(internalError);
    return npe;
  }

  /**
   * Returns an {@code AssertionError} that wraps an HDB_InternalError.
   *
   * @param internalErrorNum The number for the internal error
   * @return AssertionError containing the original exception
   */
  public static AssertionError newAssertionError(int internalErrorNum)
  {
    return newAssertionError(new HDB_InternalError(internalErrorNum));
  }

  /**
   * Returns an {@code AssertionError} with a user-friendly message extracted from the given throwable.
   *
   * <p>This method constructs an assertion error using {@link #getThrowableMessage(Throwable) getThrowableMessage}, ensuring
   * meaningful exception messages while preserving the original cause.</p>
   *
   * @param e the throwable that triggered the assertion failure
   * @return AssertionError containing the processed message and original exception
   */
  public static AssertionError newAssertionError(Throwable e)
  {
    return new AssertionError(getThrowableMessage(e), e);
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

    if (strNullOrBlank(msg) || "null".equals(msg))
      msg = "";

    return switch (e)
    {
      case UnknownHostException _ -> "Host not found" + (msg.isEmpty() ? "" : ": ") + msg;

      case ConnectException _ ->
      {
        if (msg.toLowerCase().contains("timed out"))
          yield "Connection timed out";

        yield msg.isEmpty() ? "Unable to connect to server" : msg;
      }

      case NoRouteToHostException _ -> msg.isEmpty() ? "Unable to connect to server" : msg;

      case SocketTimeoutException _ -> "Connection timed out";

      case SSLHandshakeException _ -> "Secure connection failed" + (msg.isEmpty() ? "" : ": ") + msg;

      case HttpResponseException _ -> msg.isEmpty() ? "Unknown server error" : msg;

      case AccessDeniedException _ -> "Access denied" + (msg.isEmpty() ? "" : ": ") + msg;

      case IOException ioException ->
      {
        if (ioException.getClass().equals(FileSystemException.class))
          yield msg.isEmpty() ? "File system error" : msg;

        yield userFriendlyThrowableName(ioException) + (msg.isEmpty() ? "" : ": ") + msg;
      }

      default -> msg.isEmpty() ? userFriendlyThrowableName(e) : msg;
    };
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
    return camelToTitle(klass.getSimpleName()).strip();
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
    return Strings.CS.removeEnd(userFriendlyClassName(e.getClass()), "Exception").strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks if any of the given objects is an instance of the specified class.
   *
   * @param clazz   The class to check against. Must not be {@code null}.
   * @param objects The objects to check. Can be empty.
   * @return {@code true} if at least one object is an instance of {@code clazz}, otherwise {@code false}.
   * @throws NullPointerException if {@code clazz} is {@code null}.
   */
  public static boolean anyIsInstanceOf(Class<?> clazz, Object... objects)
  {
    return Arrays.stream(objects).anyMatch(clazz::isInstance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks if all given objects are instances of the specified class.
   *
   * @param clazz   The class to check against. Must not be {@code null}.
   * @param objects The objects to check. Can be empty.
   * @return {@code true} if all objects are instances of {@code clazz}, otherwise {@code false}.
   * @throws NullPointerException if {@code clazz} is {@code null}.
   */
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

  /**
   * Utility function to convert an {@link java.lang.Iterable} into a {@link java.util.stream.Stream}.
   * @param <T> the type of elements in the stream
   * @param iterable the iterable to be converted into a stream
   * @return a stream of elements that would be returned by the iterable
   */
  public static <T> Stream<T> iterableToStream(Iterable<T> iterable)
  {
    return StreamSupport.stream(iterable.spliterator(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void writeCsvFile(FilePath filePath, Stream<List<String>> rows) throws IOException
  {
    try (FileWriter writer = new FileWriter(filePath.toFile()))
    {
      for (List<String> row : streamToIterable(rows))
        writer.write(String.join(",", escapeCsv(row)) + System.lineSeparator());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<String> escapeCsv(List<String> fields)
  {
    return fields.stream().map(field ->
      field.contains(",") || field.contains("\"") || field.contains("\n") ?
        '"' + field.replace("\"", "\"\"") + '"'
      :
        field).toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
