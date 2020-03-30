/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.bib.data;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.Util;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.Util.*;

public abstract class BibData
{

//---------------------------------------------------------------------------

  private static final Map<String, YearType> descToYearType = new HashMap<>();
 
  public static enum YearType
  {
    ytUnknown(""),
    ytCreated("created"),
    ytCopyright("copyright"),
    ytIssued("issued"),
    ytPublishedDate("publishedDate"),
    ytPublicationDate("publicationDate"),
    ytPublishedPrint("published-print"),
    ytPublicationDisplayDate("publicationDisplayDate"),
    ytCoverDate("coverDate"),
    ytCoverDisplayDate("coverDisplayDate");

    final String desc;

    private YearType(String desc) { this.desc = desc; descToYearType.put(desc, this); }

    public static YearType getByDesc(String desc) { return descToYearType.getOrDefault(desc, ytUnknown); }

    static YearType highestPriority()
    {
      int ordinal = Integer.MIN_VALUE;
      YearType highestYT = null;

      for (YearType yt : EnumSet.allOf(YearType.class))
      {
        if (yt.ordinal() > ordinal)
        {
          highestYT = yt;
          ordinal = yt.ordinal();
        }
      }

      return highestYT;
    }
  }

//---------------------------------------------------------------------------

  public abstract EntryType getEntryType();
  public abstract void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list);
  protected abstract void setEntryType(EntryType entryType);
  public abstract void setStr(BibFieldEnum bibFieldEnum, String newStr);
  public abstract List<String> getMultiStr(BibFieldEnum bibFieldEnum);
  public abstract String getStr(BibFieldEnum bibFieldEnum);
  public abstract BibAuthors getAuthors();
  public abstract HDT_Work getWork();
  public abstract boolean linkedToWork();
  public abstract HDT_WorkType getWorkType();
  public abstract void setWorkType(HDT_WorkType workType);

//---------------------------------------------------------------------------

  public void setTitle(String newTitle) { setMultiStr(bfTitle, Arrays.asList(newTitle)); }
  protected void addISBN(String newStr) { matchISBN(newStr).forEach(isbn -> addStr(bfISBNs, isbn)); }
  public void addISSN(String newStr)    { matchISSN(newStr).forEach(issn -> addStr(bfISSNs, issn)); }

  public static boolean isEmpty(BibData bd) { return bd == null ? true : EnumSet.allOf(BibFieldEnum.class).stream().noneMatch(bd::fieldNotEmpty); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean entryTypeNotEmpty()
  {
    EntryType entryType = getEntryType();
    return (entryType != null) && (entryType != etNone) && (entryType != etUnentered);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static String getMultiStrSpaceDelimited(List<String> list)
  {
    return ultraTrim(list.stream().map(Util::ultraTrim).reduce((s1, s2) -> s1 + " " + s2).orElse(""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean fieldNotEmpty(BibFieldEnum bibFieldEnum)
  {
    if (bibFieldEnum.isMultiLine())
      return getMultiStr(bibFieldEnum).size() > 0;

    return getStr(bibFieldEnum).length() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    List<String> list = getMultiStr(bibFieldEnum);
    list.add(newStr);
    setMultiStr(bibFieldEnum, list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void extractDOIandISBNs(String value)
  {
    setDOI(value);
    addISBN(value);
    addISSN(value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void setDOI(String newStr)
  {
    if (safeStr(newStr).isEmpty()) return;
    String doi = matchDOI(newStr);
    if (doi.length() > 0)
      setStr(bfDOI, doi);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Authors have to be checked separately

  public boolean fieldsAreEqual(BibFieldEnum bibFieldEnum, BibData otherBD)
  {
    if ((fieldNotEmpty(bibFieldEnum) || otherBD.fieldNotEmpty(bibFieldEnum)) == false) return true;

    switch (bibFieldEnum)
    {
      case bfDOI       : case bfURL    : case bfVolume  : case bfIssue    : case bfPages : case bfEntryType :
      case bfPublisher : case bfPubLoc : case bfEdition : case bfLanguage : case bfYear  : case bfWorkType  :

        return ultraTrim(getStr(bibFieldEnum)).equals(ultraTrim(otherBD.getStr(bibFieldEnum)));

      case bfContainerTitle : case bfTitle : case bfMisc : case bfISBNs : case bfISSNs :

        return strListsEqual(getMultiStr(bibFieldEnum), otherBD.getMultiStr(bibFieldEnum), false);

      default:

        return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static String extractYear(String text)
  {
    Matcher m = Pattern.compile("(\\A|\\D)([12]\\d\\d\\d)(\\z|\\D)").matcher(text);
    return m.find() ? m.group(2) : "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addReportStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    appendToReport(bibFieldEnum.getUserFriendlyName(), getStr(bibFieldEnum), list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addReportMultiStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    String line = getMultiStr(bibFieldEnum).stream().reduce((s1, s2) -> s1 + "; " + s2).orElse("");

    appendToReport(bibFieldEnum.getUserFriendlyName(), line, list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void appendToReport(String fieldName, String fieldVal, List<String> list)
  {
    if (fieldVal.trim().length() > 0)
      list.add(fieldName + ": " + fieldVal);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String createReport()
  {
    List<String> list = new ArrayList<>();
    BibAuthors authors = getAuthors();

    addReportStr(bfTitle         , list);
    addReportStr(bfEntryType     , list);

    appendToReport(bfAuthors    .getUserFriendlyName(), authors.getStr(AuthorType.author    ), list);
    appendToReport(bfEditors    .getUserFriendlyName(), authors.getStr(AuthorType.editor    ), list);
    appendToReport(bfTranslators.getUserFriendlyName(), authors.getStr(AuthorType.translator), list);

    addReportStr(bfYear          , list);
    addReportStr(bfDOI           , list);
    addReportMultiStr(bfISBNs    , list);
    addReportStr(bfURL           , list);
    addReportStr(bfContainerTitle, list);
    addReportStr(bfVolume        , list);
    addReportStr(bfIssue         , list);
    addReportStr(bfPages         , list);
    addReportStr(bfPublisher     , list);
    addReportStr(bfPubLoc        , list);
    addReportStr(bfEdition       , list);
    addReportStr(bfMisc          , list);
    addReportMultiStr(bfISSNs    , list);

    return strListToStr(list, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final public void copyAllFieldsFrom(BibData bd, boolean includeAuthors, boolean includeEntryType)
  {
    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum.getType())
    {
      case bftString :

        setStr(bibFieldEnum, bd.getStr(bibFieldEnum));
        break;

      case bftMultiString :

        setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum));
        break;

      case bftEntryType :

        if (includeEntryType) setEntryType(bd.getEntryType());
        break;

      case bftWorkType :

        setWorkType(bd.getWorkType());
        break;

      case bftAuthor :

        break;
    }});

    if ((includeAuthors == false) || bd.getAuthors().isEmpty()) return;

    BibAuthors authors = getAuthors();
    authors.clear();
    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EnumSet<BibFieldEnum> fieldsWithExternalData()
  {
    EnumSet<BibFieldEnum> set = EnumSet.allOf(BibFieldEnum.class);

    set.removeIf(bibFieldEnum -> { switch (bibFieldEnum)
    {
      case bfAuthors   : case bfEditors  : case bfTranslators : case bfTitle:
      case bfDOI       : case bfISBNs    : case bfMisc        : case bfYear:
      case bfEntryType : case bfWorkType : case bfURL         :

        return true;

      default:

        return fieldNotEmpty(bibFieldEnum) == false;
    }});

    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
