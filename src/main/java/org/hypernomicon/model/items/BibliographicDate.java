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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.Util.*;

import java.time.Month;
import java.time.format.TextStyle;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;

import javax.xml.stream.events.Attribute;

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

  public static final BibliographicDate EMPTY_DATE = new BibliographicDate(0, 0, "");

  private final int day,         // Value between 1 and 31, inclusive. < 1 means unentered.
                    month;       // Value between 1 and 12, inclusive. < 1 means unentered.

  private final BibliographicYear year;

//---------------------------------------------------------------------------

  public BibliographicDate(int day, int month, String yearRaw)
  {
    this(day, month, new BibliographicYear(yearRaw));
  }

  public BibliographicDate(int day, int month, BibliographicYear year)
  {
    this.day = day;
    this.month = month;
    this.year = year;
  }

//---------------------------------------------------------------------------

  public String getYearStr() { return year.rawVal; }
  public boolean hasMonth()  { return (month >= 1) && (month <= 12); }
  public boolean hasDay()    { return (day >= 1) && (day <= 31); }
  public boolean hasYear()   { return year == null ? false : (year.isEmpty() == false); }
  public boolean isEmpty()   { return (hasYear() || hasMonth() || hasDay()) == false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String displayToUser()
  {
    String str = safeStr(year.rawVal);

    if ((hasDay() || hasMonth()) == false)
      return str;

    if (str.isBlank())
      str = "???";

    if (hasMonth())
      str = str + ", " + Month.of(month).getDisplayName(TextStyle.SHORT, Locale.getDefault());

    return hasDay() ?
      str + (hasMonth() ? " " : ", ") + String.valueOf(day)
    :
      str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibliographicDate setYear(String yearRaw)
  {
    return new BibliographicDate(day, month, yearRaw);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibliographicDate fromYearStr(String yearRaw)
  {
    return new BibliographicDate(0, 0, yearRaw);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create a new BibliograhicDate object by combining date1 and date2.
   * If date1 and date2 both have a certain date component, the component from date1 is used.
   * @param date1
   * @param date2
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

  @Override public int hashCode()
  {
    return Objects.hash(day, month, year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (!(obj instanceof BibliographicDate)) return false;

    BibliographicDate other = (BibliographicDate) obj;

    return (day == other.day) && (month == other.month) && Objects.equals(year, other.year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(BibliographicDate o)
  {
    if (o == null) return 1;

    if ((year == null) && (o.year == null)) return 0;
    if (year == null) return -1;

    int yearDiff = year.compareTo(o.year);
    if (yearDiff != 0) return yearDiff;

    if ((hasMonth() == false) && (o.hasMonth() == false))
      return 0;

    if (hasMonth() == false) return -1;
    if (o.hasMonth() == false) return 1;

    if (month != o.month) return month - o.month;

    if ((hasDay() == false) && (o.hasDay() == false))
      return 0;

    if (hasDay() == false) return -1;
    if (o.hasDay() == false) return 1;

    return day - o.day;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String DAY_ATTR_NAME = "day",
                              MONTH_ATTR_NAME = "month",
                              RAW_YEAR_ATTR_NAME = "raw_year",
                              PARSED_YEAR_ATTR_NAME = "parsed_year";

  public String toXmlAttribs()
  {
    String attribs = "", parsedYearStr = "";

    if (year.parsedVal != 0)
      parsedYearStr = String.valueOf(year.parsedVal);

    if (hasDay())
      attribs = attribs + DAY_ATTR_NAME + "=" + QUOTE + String.valueOf(day) + QUOTE + ' ';

    if (hasMonth())
      attribs = attribs + MONTH_ATTR_NAME + "=" + QUOTE + String.valueOf(month) + QUOTE + ' ';

    if ((safeStr(year.rawVal).isBlank() == false) && (year.rawVal.equals(parsedYearStr) == false))
      attribs = attribs + RAW_YEAR_ATTR_NAME + "=" + QUOTE + xmlContentEscaper.escape(year.rawVal) + QUOTE + ' ';

    if (year.parsedVal != 0)
      attribs = attribs + PARSED_YEAR_ATTR_NAME + "=" + QUOTE + parsedYearStr + QUOTE + ' ';

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

    return new BibliographicDate(day, month, new BibliographicYear(parsedYear, rawYear));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
