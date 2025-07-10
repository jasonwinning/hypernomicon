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

import org.apache.commons.text.similarity.LevenshteinDistance;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.model.relations.ObjectGroup;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.StringUtil.*;
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
  public abstract void setStr(BibFieldEnum bibFieldEnum, String newStr);
  public abstract List<String> getMultiStr(BibFieldEnum bibFieldEnum);
  public abstract String getStr(BibFieldEnum bibFieldEnum);
  public abstract BibliographicDate getDate();
  public abstract void setDate(BibliographicDate newDate);
  public abstract BibAuthors getAuthors();
  public abstract HDT_Work getWork();
  public abstract HDT_WorkType getWorkType();

  protected abstract void setEntryType(EntryType entryType);
  protected abstract void setAllAuthors(Iterable<Author> otherAuthors);
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
      strNotNullOrEmpty(getStr(bibFieldEnum));
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
    if (strNullOrBlank(newStr)) return;
    String doi = matchDOI(newStr);
    if (strNotNullOrBlank(doi))
      setStr(bfDOI, doi);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Checks whether the bibliographic field for this BibData is the same as the passed-in BibData
   * <p>This does not check authors, editors, and translators; that has to be done separately by
   * the caller.
   * @param bibFieldEnum The enum for the bibliographic field to check
   * @param otherBD The other BibData to compare against
   * @param ignoreExtFileURL Whether external file URL should be ignored
   * @return True if the fields are equal; false otherwise
   */
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

        return getStr(bibFieldEnum).strip().equals(otherBD.getStr(bibFieldEnum).strip());

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

    addCreatorsToReport(bfAuthors       , report, authors.getStr(author    ));
    addCreatorsToReport(bfEditors       , report, authors.getStr(editor    ));
    addCreatorsToReport(bfTranslators   , report, authors.getStr(translator));

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

    setAllAuthors(bd.getAuthors());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAllAuthorsFromTable(Iterable<ObjectGroup> authGroups)
  {
    setAllAuthors(streamToIterable(iterableToStream(authGroups).map(authGroup -> new AuthorStandalone(new PersonName(authGroup.getPrimaryStr()),
                                                                                                      authGroup.getPrimary(),
                                                                                                      authGroup.getValue(tagEditor).bool,
                                                                                                      authGroup.getValue(tagTranslator).bool))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int getValueCount(BibFieldEnum bibFieldEnum)
  {
    return switch (bibFieldEnum.getType())
    {
      case bftAuthor ->
      {
        AuthorType authorType = AuthorType.fromBibFieldEnum(bibFieldEnum);

        yield (int) getAuthors().stream().filter(auth ->
          ((authorType == author    ) && auth.getIsAuthor())   ||
          ((authorType == editor    ) && auth.getIsEditor())   ||
          ((authorType == translator) && auth.getIsTrans ())).count();
      }
      case bftBibDate     -> BibliographicDate.isEmpty(getDate()) ? 0 : 1;
      case bftMultiString -> nullSwitch(getMultiStr(bibFieldEnum), 0, List::size);
      case bftString      -> strNullOrBlank(getStr(bibFieldEnum)) ? 0 : 1;
      case bftWorkType    -> getWorkType() == null ? 0 : 1;
      case bftEntryType   ->
      {
        EntryType entryType = getEntryType();

        yield ((entryType == null) || (entryType == etUnentered) || (entryType == etNone)) ? 0 : 1;
      }
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
  * Finds a work in the database with a matching DOI or, if both are books or
  * the titles are a fuzzy match, a matching ISBN.
  * @return The matching work, or null if none found.
  */
  public HDT_Work findMatchingWork()
  {
    HDT_Work work = getWork();

    if (work != null)
      return work;

    // Match DOI first
    String doi = getStr(bfDOI);

    if (doi.isEmpty() == false)
    {
      work = findFirst(db.works, _work -> _work.getDOI().equalsIgnoreCase(doi));

      if (work != null)
        return work;
    }

    // Retrieve ISBNs and title
    List<String> isbns = getMultiStr(bfISBNs);
    String title = HDT_RecordBase.makeSortKeyByType(getStr(bfTitle), RecordType.hdtWork);

    if (isbns.isEmpty() == false)
    {
      LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();
      HDT_Work bestMatch = null;
      double bestDist = Double.MAX_VALUE;

      for (HDT_Work curWork : db.works)
      {
        boolean isbnMatch = curWork.getISBNs().stream().anyMatch(isbns::contains);

        if (isbnMatch == false) continue;

        if ((curWork.getWorkTypeEnum() == WorkTypeEnum.wtBook) && (HDT_WorkType.getEnumVal(getWorkType()) == WorkTypeEnum.wtBook))
          return curWork;

        double curDist = titleDistance(alg, title, curWork.getSortKey());

        if ((curDist < LEVENSHTEIN_THRESHOLD) && (curDist < bestDist))
        {
          bestMatch = curWork;
          bestDist = curDist;
        }
      }

      return bestMatch;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final double LEVENSHTEIN_THRESHOLD = 0.25;

  static double titleDistance(LevenshteinDistance alg, String title1, String title2)
  {
    if (strNullOrBlank(title1) || strNullOrBlank(title2)) return 1.0;

    StringBuilder sbTitle1 = new StringBuilder(title1),
                  sbTitle2 = new StringBuilder(title2);

    clearAfter(sbTitle1, sbTitle2, ":");
    clearAfter(sbTitle1, sbTitle2, "?");
    clearAfter(sbTitle1, sbTitle2, "(");

    int len = Math.min(sbTitle1.length(), sbTitle2.length());

    if (len == 0) return 1.0;

    return (double)alg.apply(sbTitle1, sbTitle2) / (double)len;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void clearAfter(StringBuilder title1, StringBuilder title2, String startOfExcludedText)
  {
    int pos1 = title1.indexOf(startOfExcludedText),
        pos2 = title2.indexOf(startOfExcludedText);

    if ((pos1 < 0) != (pos2 < 0))
    {
      if (pos1 > 0) assignSB(title1, title1.substring(0, pos1).strip());
      if (pos2 > 0) assignSB(title2, title2.substring(0, pos2).strip());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
