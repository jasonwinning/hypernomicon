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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.stream.events.Attribute;

import org.hypernomicon.bib.zotero.ZoteroDate;
import org.hypernomicon.model.Exceptions.InvalidAttributeException;
import org.hypernomicon.model.records.RecordState;

//---------------------------------------------------------------------------

public class BibliographicDate implements Comparable<BibliographicDate>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, DateType> descToDateType = new HashMap<>();

  public enum DateType
  {
    dtUnknown(""),
    dtCreated("created"),
    dtCopyright("copyright"),
    dtIssued("issued"),
    dtPublishedDate("publishedDate"),
    dtPublicationDate("publicationDate"),
    dtPublishedPrint("published-print"),
    dtPublicationDisplayDate("publicationDisplayDate"),
    dtCoverDate("coverDate"),
    dtCoverDisplayDate("coverDisplayDate");

    public final String desc;

    DateType(String desc) { this.desc = desc; descToDateType.put(desc, this); }

    public static DateType getByDesc(String desc) { return descToDateType.getOrDefault(desc, dtUnknown); }

    public static DateType highestPriority()
    {
      int ordinal = Integer.MIN_VALUE;
      DateType highestDT = null;

      for (DateType dt : EnumSet.allOf(DateType.class))
      {
        if (dt.ordinal() > ordinal)
        {
          highestDT = dt;
          ordinal = dt.ordinal();
        }
      }

      return highestDT;
    }
  }

//---------------------------------------------------------------------------

  private static final char QUOTE = '"';

  public static final BibliographicDate EMPTY_DATE = new BibliographicDate(0, 0, "", false);

  public final int day,         // Value between 1 and 31, inclusive. 0 means unentered.
                   month;       // Value between 1 and 12, inclusive. 0 means unentered.

  public final BibliographicYear year;

//---------------------------------------------------------------------------

  public BibliographicDate(int day, int month, String yearRaw, boolean yearZeroIsOneBC)
  {
    this(day, month, new BibliographicYear(yearRaw, yearZeroIsOneBC));
  }

  public BibliographicDate(int day, int month, BibliographicYear year)
  {
    this.day = ((day >= 1) && (day <= 31)) ? day : 0;
    this.month = ((month >= 1) && (month <= 12)) ? month : 0;
    this.year = year;
  }

//---------------------------------------------------------------------------

  public String getYearStr() { return safeStr(year.rawValue); }
  public boolean hasMonth()  { return (month >= 1) && (month <= 12); }
  public boolean hasDay()    { return (day >= 1) && (day <= 31); }
  public boolean hasYear()   { return BibliographicYear.isEmpty(year) == false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isEmpty(BibliographicDate bibDate)
  {
    if (bibDate == null) return true;

    return (bibDate.hasYear() || bibDate.hasMonth() || bibDate.hasDay()) == false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern numPattern = Pattern.compile("(\\d{1,})");

//---------------------------------------------------------------------------

  public String displayToUser()
  {
    return displayToUser(false);
  }

  public String displayToUser(boolean fourDigitYear)
  {
    String str = getYearStr();

    if (fourDigitYear)
    {
      Matcher m = numPattern.matcher(str);
      if (m.find())
        str = safeSubstring(str, 0, m.start()) + String.format("%04d", parseInt(m.group(1), 0)) + safeSubstring(str, m.end(), str.length());
    }

    if ((hasDay() || hasMonth()) == false)
      return str;

    if (str.isBlank())
      str = "???";

    if (hasMonth())
      str = str + ", " + Month.of(month).getDisplayName(TextStyle.SHORT, Locale.getDefault());

    return hasDay() ?
      str + (hasMonth() ? " " : ", ") + day
    :
      str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern leadingZeroPattern = Pattern.compile("(^|[^0-9])(0{1,})([1-9])");

//---------------------------------------------------------------------------

  public static String removeLeadingZerosFromDateStr(String str)
  {
    Matcher m = leadingZeroPattern.matcher(safeStr(str));

    return m.replaceAll(result -> safeStr(m.group(1)) + safeStr(m.group(3)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  BibliographicDate setYear(String yearRaw, boolean yearZeroIsOneBC)
  {
    return new BibliographicDate(day, month, yearRaw, yearZeroIsOneBC);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicDate fromYearStr(String yearRaw, boolean yearZeroIsOneBC)
  {
    return new BibliographicDate(0, 0, yearRaw, yearZeroIsOneBC);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a new BibliograhicDate object by combining date1 and date2.
   * If date1 and date2 both have a certain date component, the component from date1 is used.
   * @param date1 The first date to compare
   * @param date2 The second date to compare
   * @return The resulting newly created BibliographicDate object
   */
  public static BibliographicDate combine(BibliographicDate date1, BibliographicDate date2)
  {
    return new BibliographicDate(date1.hasDay  () ? date1.day   : date2.day,
                                 date1.hasMonth() ? date1.month : date2.month,
                                 date1.hasYear () ? date1.year  : date2.year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicDate fromUserStr(String val)
  {
    ZoteroDate zoteroDate = ZoteroDate.fromUserStr(val, getLocaleStr(Locale.getDefault()));

    return zoteroDate == null ? EMPTY_DATE : new BibliographicDate(zoteroDate.getDay(), zoteroDate.getMonth(), zoteroDate.getRawYear(), false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return Objects.hash(day, month, year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return isEmpty(this);
    if (!(obj instanceof BibliographicDate other)) return false;

    if (isEmpty(other)) return isEmpty(this);

    return (day == other.day) && (month == other.month) && Objects.equals(year, other.year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(BibliographicDate o)
  {
    if (isEmpty(o)) return isEmpty(this) ? 0 : 1;

    int yearDiff = BibliographicYear.isEmpty(year) ?
      (BibliographicYear.isEmpty(o.year) ? 0 : -1)
    :
      year.compareTo(o.year);

    if (yearDiff != 0) return yearDiff;

    int monthDiff = hasMonth() ?
      (o.hasMonth() ? (month - o.month) : 1)
    :
      (o.hasMonth() ? -1 : 0);

    if (monthDiff != 0) return monthDiff;

    return hasDay() ?
      (o.hasDay() ? (day - o.day) : 1)
    :
      (o.hasDay() ? -1 : 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DAY_ATTR_NAME = "day",
                              MONTH_ATTR_NAME = "month",
                              RAW_YEAR_ATTR_NAME = "raw_year",
                              PARSED_YEAR_ATTR_NAME = "parsed_year";

  String toXmlAttribs()
  {
    String attribs = "", parsedYearStr = "";

    if (year.numericValueWhereMinusOneEqualsOneBC() != 0)
      parsedYearStr = String.valueOf(year.numericValueWhereMinusOneEqualsOneBC());

    if (hasDay())
      attribs = attribs + DAY_ATTR_NAME + '=' + QUOTE + day + QUOTE + ' ';

    if (hasMonth())
      attribs = attribs + MONTH_ATTR_NAME + '=' + QUOTE + month + QUOTE + ' ';

    if (strNotNullOrBlank(year.rawValue) && (((year.numericValueWhereMinusOneEqualsOneBC() > 0) && year.rawValue.equals(parsedYearStr)) == false))
      attribs = attribs + RAW_YEAR_ATTR_NAME + '=' + QUOTE + xmlContentEscaper.escape(year.rawValue) + QUOTE + ' ';

    if (year.numericValueWhereMinusOneEqualsOneBC() != 0)
      attribs = attribs + PARSED_YEAR_ATTR_NAME + '=' + QUOTE + parsedYearStr + QUOTE + ' ';

    return attribs.trim();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicDate fromXmlAttribs(Iterator<Attribute> attributesIt, RecordState xmlRecord) throws InvalidAttributeException
  {
    int day = 0, month = 0, parsedYear = 0;
    String rawYear = "";

    while (attributesIt.hasNext())
    {
      Attribute attribute = attributesIt.next();

      switch (attribute.getName().toString())
      {
        case DAY_ATTR_NAME :
          day = parseInt(attribute.getValue(), 0);
          break;

        case MONTH_ATTR_NAME :
          month = parseInt(attribute.getValue(), 0);
          break;

        case RAW_YEAR_ATTR_NAME :
          rawYear = attribute.getValue();
          break;

        case PARSED_YEAR_ATTR_NAME :
          parsedYear = parseInt(attribute.getValue(), 0);
          break;

        default:
          throw new InvalidAttributeException(xmlRecord.id, xmlRecord.type, tagBibDate, attribute.getName().toString());
      }
    }

    if ((parsedYear != 0) && rawYear.isBlank())
      rawYear = String.valueOf(parsedYear);

    return new BibliographicDate(day, month, BibliographicYear.fromRawStrAndNumberWhereMinusOneEqualsOneBC(parsedYear, rawYear));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
