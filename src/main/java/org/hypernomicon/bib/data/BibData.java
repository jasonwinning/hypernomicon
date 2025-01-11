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

package org.hypernomicon.bib.data;

import java.util.EnumSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * <p>{@code BibData} is the superclass for all objects that represent the bibliographic
 * data for a single work. This superclass abstracts away from the details of implementation
 * and differences between various formats and allows for interoperability between them.
 * </p>
 */
public abstract class BibData
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract EntryType getEntryType();
  public abstract void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list);
  protected abstract void setEntryType(EntryType entryType);
  public abstract void setStr(BibFieldEnum bibFieldEnum, String newStr);
  public abstract List<String> getMultiStr(BibFieldEnum bibFieldEnum);
  public abstract String getStr(BibFieldEnum bibFieldEnum);
  public abstract BibliographicDate getDate();
  public abstract void setDate(BibliographicDate newDate);
  public abstract BibAuthors getAuthors();
  public abstract HDT_Work getWork();
  public abstract HDT_WorkType getWorkType();
  protected abstract void setWorkType(HDT_WorkType workType);

//---------------------------------------------------------------------------

  public String getYearStr()            { return nullSwitch(getDate(), "", BibliographicDate::getYearStr); }
  public String getDateRawStr()         { return nullSwitch(getDate(), "", BibliographicDate::displayToUser); }
  public void setTitle(String newTitle) { setMultiStr(bfTitle, safeListOf(newTitle)); }
  void addISBN(String newStr)           { matchISBN(newStr).forEach(isbn -> addStr(bfISBNs, isbn)); }
  void addISSN(String newStr)           { matchISSN(newStr).forEach(issn -> addStr(bfISSNs, issn)); }

  public static boolean isEmpty(BibData bd) { return (bd == null) || EnumSet.allOf(BibFieldEnum.class).stream().noneMatch(bd::fieldNotEmpty); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean entryTypeNotEmpty()
  {
    EntryType entryType = getEntryType();
    return (entryType != null) && (entryType != etNone) && (entryType != etUnentered);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean fieldNotEmpty(BibFieldEnum bibFieldEnum)
  {
    if (bibFieldEnum.isMultiLine())
      return getMultiStr(bibFieldEnum).stream().allMatch(String::isBlank) == false;

    return bibFieldEnum == bfDate ?
      (BibliographicDate.isEmpty(getDate()) == false)
    :
      getStr(bibFieldEnum).length() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    List<String> list = getMultiStr(bibFieldEnum);
    list.add(newStr);
    setMultiStr(bibFieldEnum, list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void extractDOIandISBNs(String value)
  {
    setDOI(value);
    addISBN(value);
    addISSN(value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setDOI(String newStr)
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

      case bfDOI       : case bfVolume    : case bfIssue   : case bfPages    : case bfEntryType :
      case bfPubLoc    : case bfPublisher : case bfEdition : case bfLanguage : case bfWorkType  :

        return ultraTrim(getStr(bibFieldEnum)).equals(ultraTrim(otherBD.getStr(bibFieldEnum)));

      case bfContainerTitle : case bfTitle : case bfMisc : case bfISBNs : case bfISSNs :

        return strListsEqual(getMultiStr(bibFieldEnum), otherBD.getMultiStr(bibFieldEnum), false);

      case bfDate :

        return getDate().equals(otherBD.getDate());

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

  private void addDateToReport(ReportGenerator report)
  {
    String fieldName = bfDate.getUserFriendlyName();
    report.addField(fieldName, report.makeRow(fieldName, getDate().displayToUser()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addMultiStrToReport(BibFieldEnum bibFieldEnum, ReportGenerator report)
  {
    String fieldName = bibFieldEnum.getUserFriendlyName();
    report.addField(fieldName, report.makeRows(fieldName, getMultiStr(bibFieldEnum).stream()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorsToReport(BibFieldEnum bibFieldEnum, ReportGenerator report, String str)
  {
    String fieldName = bibFieldEnum.getUserFriendlyName();
    report.addField(fieldName, report.makeRow(fieldName, str));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String createReport()
  {
    return createReport(false);
  }

  public String createReport(boolean html)
  {
    ReportGenerator report = ReportGenerator.create(html);

    BibAuthors authors = getAuthors();

    addStrToReport     (bfEntryType     , report);
    addStrToReport     (bfTitle         , report);
    addDateToReport    (                  report);

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

    return report.render(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void copyAllFieldsFrom(BibData bd, boolean includeAuthors, boolean includeEntryType)
  {
    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum.getType())
    {
      case bftString      : setStr(bibFieldEnum, bd.getStr(bibFieldEnum)); break;
      case bftMultiString : setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum)); break;
      case bftEntryType   : if (includeEntryType) setEntryType(bd.getEntryType()); break;
      case bftWorkType    : setWorkType(bd.getWorkType()); break;
      case bftBibDate     : setDate(bd.getDate()); break;
      case bftAuthor      : break;
    }});

    if ((includeAuthors == false) || bd.getAuthors().isEmpty()) return;

    BibAuthors authors = getAuthors();
    authors.clear();
    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getValueCount(BibFieldEnum bibFieldEnum)
  {
    return switch (bibFieldEnum.getType())
    {
      case bftAuthor      -> (int) getAuthors().stream().filter(author -> author.getType() == AuthorType.fromBibFieldEnum(bibFieldEnum)).count();
      case bftBibDate     -> BibliographicDate.isEmpty(getDate()) ? 0 : 1;
      case bftMultiString -> nullSwitch(getMultiStr(bibFieldEnum), 0, List::size);
      case bftString      -> safeStr(getStr(bibFieldEnum)).isBlank() ? 0 : 1;
      case bftWorkType    -> getWorkType() == null ? 0 : 1;

      case bftEntryType ->
      {
        EntryType entryType = getEntryType();

        yield (entryType == null) || (entryType == etUnentered) || (entryType == etNone) ? 0 : 1;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
