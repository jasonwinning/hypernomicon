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

package org.hypernomicon.bib.zotero;

import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.json.JsonObj.*;

import java.time.Year;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import com.google.common.base.Objects;

//---------------------------------------------------------------------------

/**
 * This class has functionality specific to how Zotero stores and processes dates.
 */
public final class ZoteroDate
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, Integer> monthStrToInt = new HashMap<>();

  private Integer day = null,
                  month = null;

  private String year = null,
                 part = null,
                 order = "";

  private static String localeStr = null;

  private static final Pattern rawDatePattern = Pattern.compile("^(.*?)\\b([0-9]{1,4})(?:([\\-\\/\\.\\u5e74])([0-9]{1,2}))?(?:([\\-\\/\\.\\u6708])([0-9]{1,4}))?((?:\\b|[^0-9]).*?)$", Pattern.UNICODE_CHARACTER_CLASS),
                               yearPattern = Pattern.compile("^(.*?)\\b((?:circa |around |about |c\\\\.? ?)?[0-9]{1,4}(?: ?B\\.? ?C\\.?(?: ?E\\.?)?| ?C\\.? ?E\\.?| ?A\\.? ?D\\.?)|[0-9]{3,4})([^A-Za-z0-9].*)?$", Pattern.CASE_INSENSITIVE),
                               parsedDatePattern = Pattern.compile("^\\-?([0-9]{4})(?:\\-(0[1-9]|10|11|12))?(?:\\-(0[1-9]|[1-2][0-9]|30|31))?$");

  private static Pattern monthPattern = null,
                         dayPattern = null;

  private ZoteroDate() { }

//---------------------------------------------------------------------------

  public int getDay()        { return day == null ? 0 : day; }

  /**
   * Get month as a number from 1 to 12 (even though ZoteroDate internally stores it as a number from 0 to 11).
   * @return The month number from 1 to 12, or 0 if it is not set
   */
  public int getMonth()      { return month == null ? 0 : (month + 1); }

  public String getRawYear() { return safeStr(year); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setLocale(String newLocale)
  {
    if (strNullOrBlank(newLocale))
      return;

    if (Objects.equal(newLocale, localeStr))
      return;

    boolean english = newLocale.startsWith("en");

    StringBuilder sb = new StringBuilder();
    JsonObj jObj;

    try
    {
      readResourceTextFile("resources/dateFormats.json", sb, false);
      jObj = parseJsonObj(sb.toString());
    }
    catch (Exception e)
    {
      logThrowable(e);
      return;
    }

    localeStr = newLocale;

    // If no exact match, try first two characters ('de')
    if (jObj.containsKey(localeStr) == false)
      localeStr = safeSubstring(localeStr, 0, 2);

    // Try first two characters repeated ('de-DE')
    if (jObj.containsKey(localeStr) == false)
      localeStr = localeStr + '-' + localeStr.toUpperCase();

    // Look for another localeStr with same first two characters
    if (jObj.containsKey(localeStr) == false)
    {
      String finalLocale = localeStr;
      String loc = findFirst(jObj.keySet(), str -> str.startsWith(finalLocale));

      if (loc != null)
        localeStr = loc;
    }

    // If all else fails, use English
    if (jObj.containsKey(localeStr) == false)
    {
      localeStr = "en-US";
      english = true;
    }

    List<String> months = new ArrayList<>();

    JsonObj jSubObj = jObj.getObj(localeStr);

    months.addAll(JsonArray.toStrArrayList(jSubObj.getArray("short")));
    months.addAll(JsonArray.toStrArrayList(jSubObj.getArray("long")));

    if (english == false)
    {
      jSubObj = jObj.getObj("en-US");

      months.addAll(JsonArray.toStrArrayList(jSubObj.getArray("short")));
      months.addAll(JsonArray.toStrArrayList(jSubObj.getArray("long")));
    }

    String monthsStr = String.join("|", months);

    monthPattern = Pattern.compile("(.*)(?:^|[^\\p{L}])(" + monthsStr + ")[^ ]*(?: (.*)$|$)", Pattern.CASE_INSENSITIVE | Pattern.UNICODE_CASE | Pattern.UNICODE_CHARACTER_CLASS);

    monthStrToInt.clear();

    for (int ndx = 0; ndx < months.size(); ndx++)
      monthStrToInt.put(months.get(ndx).toLowerCase(), ndx % 12);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class ZoteroPart
  {
    private final String partStr,  // Some part of a date string that has not been identified as containing a day, month, or year

                         before,   // Set either to "true", "m", "d", "y", or "".
                                   // "true" means this part is before all date components so far identified
                                   // "m" means it comes before the month, etc.

                         after;    // Set either to "true", "m", "d", "y", or "".
                                   // "true" means this part is after all date components so far identified
                                   // "m" means it comes after the month, etc.

    private ZoteroPart(String partStr)
    {
      this(partStr, null, null);
    }

    private ZoteroPart(String partStr, String before)
    {
      this(partStr, before, null);
    }

    private ZoteroPart(String partStr, String before, String after)
    {
      this.partStr = safeStr(partStr);
      this.before = safeStr(before);
      this.after = safeStr(after);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Convert to single line and replace all internal space characters with ASCII 32
   */
  private static String trimInternal(String str)
  {
    str = str.replaceAll("[\\xA0\\r\\n\\s]", " ");
    str = convertToSingleLine(str);
    return str.strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String insertDateOrderPart(String dateOrder, String part, ZoteroPart partOrder)
  {
    if (strNullOrBlank(dateOrder))
      return part;

    if ("true".equalsIgnoreCase(partOrder.before))
      return part + dateOrder;

    if ("true".equalsIgnoreCase(partOrder.after))
      return dateOrder + part;

    if (strNotNullOrBlank(partOrder.before))
    {
      int pos = dateOrder.indexOf(partOrder.before);
      if (pos == -1)
        return dateOrder;

      return dateOrder.replace(partOrder.before, part + partOrder.before);
    }

    if (strNotNullOrBlank(partOrder.after))
    {
      int pos = dateOrder.indexOf(partOrder.after);
      if (pos == -1)
        return dateOrder + part;

      return dateOrder.replace(partOrder.after, part + partOrder.after);
    }

    return dateOrder + part;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Java implementation of Zotero's date parsing algorithm.
   * <br>Based on strToDate algorithm in Zotero v6.0.37
   * <br>
   * <br>Note: The parsedDate property in the format=json meta object gives the full parsed date in YYYY-MM-DD form,
   * so it is not necessary to use this algorithm to parse dates coming from Zotero.
   * <br>
   * <br>Converts a string to an object containing:<br>
   * <blockquote>
   *    day: integer form of the day<br>
   *    month: integer form of the month (indexed from 0, not 1)<br>
   *    year: 4 digit year (or, year + BC/AD/etc.)<br>
   *    part: anything that does not fall under any of the above categories<br>
   *          <blockquote>(e.g., "Summer," etc.)</blockquote>
   * </blockquote>
   *
   * @param string The input
   * @param newLocaleStr Locale string to use, e.g. "en-US". It could also just have the language part, e.g. "en".
   * @return A ZoteroDate object
   */
  public static synchronized ZoteroDate fromUserStr(String string, String newLocaleStr)
  {
    if ((newLocaleStr != null) && (newLocaleStr.equals(localeStr) == false))
      setLocale(newLocaleStr);

    if (strNullOrBlank(localeStr))
      return null;

    ZoteroDate date = new ZoteroDate();

    string = trimInternal(safeStr(string));

    if (string.isBlank())
      return date;

    List<ZoteroPart> parts = new ArrayList<>();

    // First, pattern match on entire string
    Matcher m = rawDatePattern.matcher(string);

    boolean matched = m.find();

    String group1 = "", group2 = "", group3 = "", group4 = "", group5 = "", group6 = "", group7 = "";

    if (matched)
    {
      group1 = safeStr(m.group(1));
      group2 = safeStr(m.group(2));
      group3 = safeStr(m.group(3));
      group4 = safeStr(m.group(4));
      group5 = safeStr(m.group(5));
      group6 = safeStr(m.group(6));
      group7 = safeStr(m.group(7));
    }

    if (matched &&
       ((group5.isEmpty() || group3.isEmpty()) || group3.equals(group5) || ("\u5e74".equals(group3) && "\u6708".equals(group5))) &&  // look for conventional separators
       (((group2.isEmpty() == false) && (group4.isEmpty() == false) && (group6.isEmpty() == false)) ||  // require that either all parts are found,
        (group1.isEmpty() && group7.isEmpty())))  // or else this is the entire date field
    {
      // figure out date based on parts
      if ((group2.length() == 3) || (group2.length() == 4) || "\u5e74".equals(group3))
      {
        // ISO 8601 style date (big endian)

        date.year = group2;

        int month = parseInt(group4, -1);
        date.month = month < 1 ? null : month;

        int day = parseInt(group6, -1);
        date.day = day < 1 ? null : day;

        date.order += ((group2.isEmpty() == false) ? "y" : "");
        date.order += (date.month == null ? "" : "m");
        date.order += (date.day   == null ? "" : "d");
      }
      else if ((group2.isEmpty() == false) && group4.isEmpty() && (group6.isEmpty() == false))
      {
        int month = parseInt(group2, -1);

        date.month = month < 1 ? null : month;
        date.year = group6;

        date.order += (date.month == null ? "" : "m");
        date.order += "y";
      }
      else
      {
        // local style date (middle or little endian)
        String country = localeStr != null ? localeStr.substring(3) : "US";

        if ("US".equals(country)  ||  // The United States
            "FM".equals(country)  ||  // The Federated States of Micronesia
            "PW".equals(country)  ||  // Palau
            "PH".equals(country))     // The Philippines
        {
          int month = parseInt(group2, -1);
          date.month = month < 1 ? null : month;

          int day = parseInt(group4, -1);
          date.day = day < 1 ? null : day;

          date.order += (date.month == null ? "" : "m");
          date.order += (date.day   == null ? "" : "d");
        }
        else
        {
          int month = parseInt(group4, -1);
          date.month = month < 1 ? null : month;

          int day = parseInt(group2, -1);
          date.day = day < 1 ? null : day;

          date.order += (date.day   == null ? "" : "d");
          date.order += (date.month == null ? "" : "m");
        }

        date.year = group6;
        if (group6.isEmpty() == false)
          date.order += 'y';
      }

      boolean longYear = date.year.length() > 2;

      int yearInt = parseInt(date.year, -1);
      date.year = yearInt < 1 ? null : String.valueOf(yearInt);

      if (date.month != null)
      {
        if (date.month > 12)
        {
          // swap day and month
          Integer tmp = date.day;
          date.day = date.month;
          date.month = tmp;
          date.order = date.order.replace('m', 'D')
                                 .replace('d', 'M')
                                 .replace('D', 'd')
                                 .replace('M', 'm');
        }
      }

      if (((date.month == null) || (date.month <= 12)) && ((date.day == null) || (date.day <= 31)))
      {
        // Parse pre-100 years with leading zeros (001, 0001, 012, 0012, 0123, but not 08)
        if ((date.year != null) && (yearInt < 100) && !longYear)
        {
          int year = Year.now().getValue();
          int twoDigitYear = year % 100;
          int century = year - twoDigitYear;

          if (yearInt <= twoDigitYear)
          {
            // assume this date is from our century
            date.year = String.valueOf(century + yearInt);
          }
          else
          {
            // assume this date is from the previous century
            date.year = String.valueOf(century - 100 + yearInt);
          }
        }

        if (date.month != null)
          date.month--;    // subtract one for JS style

        parts.add(new ZoteroPart(group1, "true"));
        parts.add(new ZoteroPart(group7));
      }
      else
      {
        // give up; we failed the sanity check
        date = new ZoteroDate();
        parts.add(new ZoteroPart(string));
      }
    }
    else
    {
      parts.add(new ZoteroPart(string));
    }

    // Overall date pattern matching didn't work; use pattern matching to look for individual parts instead

    // YEAR
    if (date.year == null)
    {
      for (int i = 0; i < parts.size(); i++)
      {
        m = yearPattern.matcher(parts.get(i).partStr);

        if (m.find())
        {
          date.year = m.group(2);
          date.order = insertDateOrderPart(date.order, "y", parts.get(i));

          parts.remove(i);
          parts.add(i    , new ZoteroPart(m.group(1), "true"));
          parts.add(i + 1, new ZoteroPart(m.group(3)));

          break;
        }
      }
    }

    // MONTH
    if (date.month == null)
    {
      for (int i = 0; i < parts.size(); i++)
      {
        m = monthPattern.matcher(parts.get(i).partStr);

        if (m.find())
        {
          date.month = monthStrToInt.get(m.group(2).toLowerCase());
          date.order = insertDateOrderPart(date.order, "m", parts.get(i));

          parts.remove(i);

          parts.add(i    , new ZoteroPart(m.group(1), "m"));
          parts.add(i + 1, new ZoteroPart(m.group(3), null, "m"));

          break;
        }
      }
    }

    // DAY
    if (date.day == null)
    {
      // compile day regular expression
      if (dayPattern == null)
        dayPattern = Pattern.compile("\\b([0-9]{1,2})(?:" + daySuffixes(localeStr) + ")?\\b(.*)", Pattern.CASE_INSENSITIVE);

      for (int i = 0; i < parts.size(); i++)
      {
        m = dayPattern.matcher(parts.get(i).partStr);

        if (m.find())
        {
          int day = parseInt(m.group(1), -1);

          // Sanity check
          if (day <= 31)
          {
            date.day = day;
            date.order = insertDateOrderPart(date.order, "d", parts.get(i));

            String part;

            if (m.start() > 0)
            {
              part = parts.get(i).partStr.substring(0, m.start());

              if (strNotNullOrBlank(m.group(2)))
                part += ' ' + m.group(2);
            }
            else
            {
              part =  m.group(2);
            }

            parts.remove(i);
            parts.add(i, new ZoteroPart(part));

            break;
          }
        }
      }
    }

    // Concatenate date parts
    date.part = "";

    for (ZoteroPart part : parts)
      date.part += part.partStr + ' ';

    // clean up date part
    if (date.part.isBlank() == false)
      date.part = date.part.replaceAll("^[^A-Za-z0-9]+|[^A-Za-z0-9]+$", "");

    if (date.part.isBlank())
      date.part = null;

    return date;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String daySuffixes(String localeStr)
  {
    if (localeStr.startsWith("ar")) return "|||";          // Arabic
    if (localeStr.startsWith("br")) return "st|nd|rd|th";  // Breton
    if (localeStr.startsWith("de")) return ".|.|.|.";      // German
    if (localeStr.startsWith("fa")) return "st|nd|rd|th";  // Persian
    if (localeStr.startsWith("km")) return "st|nd|rd|th";  // Central Khmer

    return switch(localeStr)
    {
      case "en-US" -> "st|nd|rd|th";     // English-United States
      case "en-GB" -> "st|nd|rd|th";     // English-United Kingdom

      case "af-ZA" -> "st|nd|rd|th";     // Afrikaans-South Africa
      case "bg-BG" -> "ви|ри|ти|ти";     // Bulgarian-Bulgaria
      case "ca-AD" -> "r|n|r|rt|é";      // Catalan-Andorra
      case "cs-CZ" -> "|||";             // Czech-Czech Republic
      case "da-DK" -> ".|.|.|.";         // Danish-Denmark
      case "el-GR" -> "st|nd|rd|th";     // Greek-Greece
      case "es-ES" -> "º|º|º|º";         // Spanish-Spain
      case "et-EE" -> "st|nd|rd|th";     // Estonian-Estonia
      case "eu-ES" -> ".|.|.|.";         // Basque-Spain
      case "fi-FI" -> ".|.|.|.";         // Finnish-Finland
      case "fr-FR" -> "er|||";           // French-France
      case "gl-ES" -> "º|||";            // Galician-Space
      case "he-IL" -> "st|nd|rd|th";     // Hebrew-Israel
      case "hr-HR" -> "st|nd|rd|th";     // Croatian-Croatia
      case "hu-HU" -> ".|.|.|.";         // Hungarian-Hungary
      case "id-ID" -> "st|nd|rd|th";     // Indonesian-Indonesia
      case "is-IS" -> "sti|nar|ji|ði";   // Icelandic-Iceland
      case "it-IT" -> "°|°|°|°";         // Italian-Italy
      case "ja-JP" -> "日|日|日|日";      // Japan-Japanese
      case "ko-KR" -> "일|일|일|일";       // Korean-South Korean
      case "lt-LT" -> "as|as|ias|as";    // Lithuanian-Lithuania
      case "mn-MN" -> "st|nd|rd|th";     // Mongolian-Mongolia
      case "nb-NO" -> ".|.|.|.";         // Norwegian Bokmål-Norway
      case "nl-NL" -> "e|e|e|e";         // Dutch-Netherlands
      case "nn-NO" -> ".|.|.|.";         // Norwegian Nynorsk-Norway
      case "pl-PL" -> ".|.|.|.";         // Polish-Poland
      case "pt-BR" -> "º|||";            // Portuguese-Brazil
      case "pt-PT" -> ".º|.º|.º|.º";     // Portuguese-Portugal
      case "ro-RO" -> "|||";             // Romanian-Romania
      case "ru-RU" -> "ый|ой|ий|й";      // Russian-Russia
      case "sk-SK" -> "|||";             // Slovak-Slovakia
      case "sl-SI" -> ".|.|.|.";         // Slovenian-Slovenia
      case "sr-RS" -> "-ог|-ог|-ег|-ог"; // Serbian-Serbia
      case "sv-SE" -> ".|.|.|.";         // Swedish-Sweden
      case "th-TH" -> "st|nd|rd|th";     // Thai-Thailand
      case "tr-TR" -> ".|.|.|.";         // Turkish-Turkey
      case "uk-UA" -> "ий|ий|ій|ий";     // Ukrainian-Ukraine
      case "vi-VN" -> "st|nd|rd|th";     // Vietnamese-Vietnam
      case "zh-CN" -> "st|nd|rd|th";     // Chinese-China
      case "zh-TW" -> "日|日|日|日";      // Chinese-Taiwan

      default -> "st|nd|rd|th";
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This needs to work like strToISO in the Zotero code base
   * @param newDate The input
   * @return A string formatted like YYYY-MM-DD or YYYY-MM or YYYY
   */
  static String bibDateToParsedDateStr(BibliographicDate newDate)
  {
    return bibDateToParsedDateStr(newDate, false);
  }

  static String bibDateToParsedDateStr(BibliographicDate newDate, boolean alwaysTenChars)
  {
    String str = "";

    if (newDate.year.numericValueWhereMinusOneEqualsOneBC() == 0)
      return str;

    int year = Math.abs(newDate.year.numericValueWhereMinusOneEqualsOneBC());

    if (alwaysTenChars)
      return String.format("%04d",year) + '-' + String.format("%02d", newDate.month) + '-' + String.format("%02d", newDate.day);

    if ((year < 1) || (year > 9999))  // Zotero cannot handle a negative year
      return str;

    str = String.format("%04d",year);

    if (newDate.month == 0)
      return str;

    str = str + '-' + String.format("%02d", newDate.month);

    if (newDate.day > 0)
      str = str + '-' + String.format("%02d", newDate.day);

    return str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicDate parsedDateStrToBibDate(String parsedDateStr, boolean yearZeroIsOneBC)
  {
    if (strNullOrBlank(parsedDateStr))
      return BibliographicDate.EMPTY_DATE;

    Matcher m = parsedDatePattern.matcher(parsedDateStr);

    if (m.find() == false)
      return BibliographicDate.EMPTY_DATE;

    String group1 = safeStr(m.group(1)),
           group2 = safeStr(m.group(2)),
           group3 = safeStr(m.group(3));

    return new BibliographicDate(parseInt(group3, 0), parseInt(group2, 0), group1, yearZeroIsOneBC);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
