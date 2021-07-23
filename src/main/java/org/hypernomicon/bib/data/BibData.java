/*
 * Copyright 2015-2021 Jason Winning
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
import org.hypernomicon.bib.reports.PlainTextReportGenerator;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.Util;

import static org.hypernomicon.Const.*;
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
      return getMultiStr(bibFieldEnum).stream().allMatch(String::isBlank) == false;
    
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

  public boolean fieldsAreEqual(BibFieldEnum bibFieldEnum, BibData otherBD, boolean ignoreExtFileURL)
  {
    if ((fieldNotEmpty(bibFieldEnum) || otherBD.fieldNotEmpty(bibFieldEnum)) == false) return true;

    switch (bibFieldEnum)
    {
      case bfURL :

        if (ignoreExtFileURL)
          if ((getStr(bfURL).startsWith(EXT_1) && (otherBD.getStr(bfURL).startsWith(EXT_1) == false)) ||
              (otherBD.getStr(bfURL).startsWith(EXT_1) && (getStr(bfURL).startsWith(EXT_1) == false)))
            return true;

        // fall through

      case bfDOI       : case bfVolume  : case bfIssue    : case bfPages : case bfEntryType : case bfPubLoc :
      case bfPublisher : case bfEdition : case bfLanguage : case bfYear  : case bfWorkType  :

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

  private void addStrToReport(BibFieldEnum bibFieldEnum, ReportGenerator report)
  {
    String fieldName = bibFieldEnum.getUserFriendlyName();
    report.addField(fieldName, report.makeRow(fieldName, getStr(bibFieldEnum)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMultiStrToReport(BibFieldEnum bibFieldEnum, ReportGenerator report)
  {
    String fieldName = bibFieldEnum.getUserFriendlyName();
    report.addField(fieldName, report.makeRows(fieldName, getMultiStr(bibFieldEnum)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addCreatorsToReport(BibFieldEnum bibFieldEnum, ReportGenerator report, String str)
  {
    String fieldName = bibFieldEnum.getUserFriendlyName();
    report.addField(fieldName, report.makeRow(fieldName, str));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String createReport()
  {
    return PlainTextReportGenerator.generate(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void createReport(ReportGenerator report)
  {
    BibAuthors authors = getAuthors();

    addStrToReport     (bfEntryType     , report);
    addStrToReport     (bfTitle         , report);
    addStrToReport     (bfYear          , report);

    addCreatorsToReport(bfAuthors       , report, authors.getStr(AuthorType.author    ));
    addCreatorsToReport(bfEditors       , report, authors.getStr(AuthorType.editor    ));
    addCreatorsToReport(bfTranslators   , report, authors.getStr(AuthorType.translator));

    addStrToReport     (bfContainerTitle, report);
    addStrToReport     (bfEdition       , report);
    addStrToReport     (bfVolume        , report);
    addStrToReport     (bfIssue         , report);
    addStrToReport     (bfPages         , report);
    addStrToReport     (bfPubLoc        , report);
    addStrToReport     (bfPublisher     , report);

    addStrToReport     (bfURL           , report);
    addStrToReport     (bfDOI           , report);
    addMultiStrToReport(bfISBNs         , report);
    addStrToReport     (bfMisc          , report);
    addMultiStrToReport(bfISSNs         , report);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final public void copyAllFieldsFrom(BibData bd, boolean includeAuthors, boolean includeEntryType)
  {
    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum.getType())
    {
      case bftString      : setStr(bibFieldEnum, bd.getStr(bibFieldEnum)); break;
      case bftMultiString : setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum)); break;
      case bftEntryType   : if (includeEntryType) setEntryType(bd.getEntryType()); break;
      case bftWorkType    : setWorkType(bd.getWorkType()); break;
      case bftAuthor      : break;
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

  public static boolean externalFieldsAreSame(BibData bd1, BibData bd2)
  {
    EnumSet<BibFieldEnum> set1 = bd1.fieldsWithExternalData(),
                          set2 = bd2.fieldsWithExternalData();

    if (set1.equals(set2) == false) return false;

    return findFirst(set1, field -> bd1.fieldsAreEqual(field, bd2, false) == false) == null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
