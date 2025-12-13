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

import static org.hypernomicon.util.StringUtil.*;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public final class KeywordBinding
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String userText, normalizedText, prefix;
  private final HDT_Record record;
  private final boolean startOnly;
  private final boolean endOnly;

//---------------------------------------------------------------------------

  public KeywordBinding(String rawText, HDT_Record record)
  {
    rawText = convertToEnglishChars(rawText).strip();

    boolean start = false, end = false;

    if (rawText.startsWith("^"))
    {
      start = true;
      rawText = rawText.substring(1);
    }

    if (rawText.endsWith("$"))
    {
      end = true;
      rawText = rawText.substring(0, rawText.length() - 1);
    }

    this.userText = rawText;
    this.normalizedText = normalizeText(rawText, false);
    this.record = record;
    this.startOnly = start;
    this.endOnly = end;
    this.prefix = normalizedText.length() > 3 ? normalizedText.substring(0, 3) : normalizedText;
  }

//---------------------------------------------------------------------------

  /**
   * This is the text portion of the keyword, without the ^ or $. It will have been
   * converted to English characters but will retain the spacing and casing
   * of the user's entered text.
   * @return The user text
   */
  public String getUserText()       { return userText; }

  /**
   * This is the normalized version of the String returned by {@link #getUserText getUserText}
   * that always has one space after periods and is lowercase.
   * @return The normalized text
   */
  public String getNormalizedText() { return normalizedText; }

  /**
   * This always returns the first 3 characters of the String returned by {@link #getNormalizedText getNormalizedText}.
   * @return The normalized prefix
   */
  public String getPrefix()         { return prefix; }
  public HDT_Record getRecord()     { return record; }
  boolean isStartOnly()             { return startOnly; }
  public boolean isEndOnly()        { return endOnly; }

  @Override public String toString() { return (startOnly ? '^' + userText : userText) + (endOnly ? '$' : ""); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This does similar normalization to KeywordLinkScanner.scan
   * <br>- (Optionally) Convert to English characters
   * <br>- Ensure exactly one space after every period
   * <br>- Convert to lower case
   * @return The normalized text
   */
  public static String normalizeText(String text, boolean convertToEngChar)
  {
    if (strNullOrEmpty(text)) return "";

    if (convertToEngChar)
      text = convertToEnglishChars(text);

    text = text.toLowerCase();

    return needsPeriodNormalization(text) ? collapseSpaces(text.replace(".", ". ")) : collapseSpaces(text);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean needsPeriodNormalization(CharSequence text)
  {
    int offset = 0, length = text.length();
    boolean lastWasPeriod = false;

    while (offset != length)
    {
      char curChar = text.charAt(offset);

      if (lastWasPeriod && ((curChar != ' ') || (((offset + 1) < length) && (text.charAt(offset + 1) == ' '))))
        return true;

      lastWasPeriod = (curChar == '.');
      offset++;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}

