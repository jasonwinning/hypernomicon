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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicDate.DateType;
import org.hypernomicon.model.items.BibliographicYear;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.util.json.JsonObj.JsonNodeType;

//---------------------------------------------------------------------------

public final class CrossrefBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // CrossRef API documentation: https://api.crossref.org/swagger-ui/index.html
  //                    Old URL: https://github.com/CrossRef/rest-api-doc

  private static CrossrefBibData createFromJSON(JsonObj jsonObj, String title, String yearStr, boolean isPaper, String queryDoi)
  {
    JsonArray jsonArray;

    title = safeStr(title);

    while (title.endsWith("."))
      title = title.substring(0, title.length() - 1);

    try
    {
      JsonNodeType jsonType = jsonObj.getType("message");

      if (jsonType == JsonNodeType.ARRAY)
      {
        String message = jsonObj.getArray("message").getObj(0).getStrSafe("message");

        if (message.isBlank() == false)
          errorPopup(message);

        return null;
      }

      jsonObj = jsonObj.getObj("message");
      jsonArray = jsonObj.getArray("items");

      if (jsonArray == null)
        return new CrossrefBibData(jsonObj, queryDoi);

      if (jsonArray.isEmpty())
        return null;

      if ((jsonArray.size() == 1) || strNullOrBlank(title))
        return new CrossrefBibData(jsonArray.getObj(0), queryDoi);
    }
    catch (NullPointerException | IndexOutOfBoundsException e)
    {
      return null;
    }

    int year = parseInt(yearStr, -1);
    if (year > 1850)
    {
      for (JsonObj curObj : jsonArray.getObjs())
      {
        CrossrefBibData curBD = new CrossrefBibData(curObj, queryDoi);
        int otherYear = curBD.getDate().year.numericValueWhereMinusOneEqualsOneBC();
        if (otherYear == 0) otherYear = Integer.MAX_VALUE;
        String otherTitle = curBD.getStr(bfTitle);

        while (otherTitle.endsWith("."))
          otherTitle = otherTitle.substring(0, otherTitle.length() - 1);

        if ((otherYear > 1850) && (((otherYear <= year) && isPaper) || (otherYear == year)) && otherTitle.equalsIgnoreCase(title))
          return curBD;
      }
    }

    // There are multiple matches but no exact year and title match

    LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();
    CrossrefBibData bestBD = null;
    double bestDist = Double.MAX_VALUE;
    title = HDT_RecordBase.makeSortKeyByType(title, hdtWork);

    for (JsonObj curObj : jsonArray.getObjs())
    {
      CrossrefBibData curBD = new CrossrefBibData(curObj, queryDoi);
      String curTitle = curBD.getStr(bfTitle);

      while (curTitle.endsWith("."))
        curTitle = curTitle.substring(0, curTitle.length() - 1);

      double curDist = titleDistance(alg, title, HDT_RecordBase.makeSortKeyByType(curTitle, hdtWork));

      if ((curDist < LEVENSHTEIN_THRESHOLD) && (curDist < bestDist))
      {
        bestBD = curBD;
        bestDist = curDist;
      }
    }

    return bestBD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CrossrefBibData(JsonObj jsonObj, String queryDoi)
  {
    setStr(bfDOI, jsonObj.getStrSafe("DOI"));

    EntryType entryType = parseCrossrefType(jsonObj.getStrSafe("type"));
    setEntryType(entryType);
    setWorkType(toWorkType(entryType));

    setStr(bfPages    , jsonObj.getStrSafe("page"));
    setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    setStr(bfPubLoc   , jsonObj.getStrSafe("publisher-location"));
    setStr(bfLanguage , jsonObj.getStrSafe("language"));

    setDateIfPresent(jsonObj, dtPublishedPrint);
    setDateIfPresent(jsonObj, dtIssued);
    setDateIfPresent(jsonObj, dtCreated);

    setStr(bfURL, jsonObj.getStrSafe("URL"));  // This is at least sometimes just a doi.org link

    jsonObj.condObj("resource").condObj("primary").condStr("URL", urlStr -> setStr(bfURL, urlStr));

    jsonObj.condArray("link").condObj(0).condStr("URL", link -> setStr(bfURL, link));

    setStr(bfVolume, jsonObj.getStrSafe("volume"));
    setStr(bfIssue , jsonObj.getStrSafe("issue"));

    ArrayList<String> title    = JsonArray.toStrArrayList(jsonObj.getArray("title")),
                      subtitle = JsonArray.toStrArrayList(jsonObj.getArray("subtitle"));

    if (strListsEqual(title, subtitle, true) == false)
      title.addAll(subtitle);

    setMultiStr(bfTitle, title);

    setMultiStr(bfContainerTitle, JsonArray.toStrArrayList(jsonObj.getArray("container-title")));
    setMultiStr(bfISBNs         , JsonArray.toStrArrayList(jsonObj.getArray("ISBN")));
    setMultiStr(bfISSNs         , JsonArray.toStrArrayList(jsonObj.getArray("ISSN")));

    addAuthorsFromJson(jsonObj.getArray("author"    ), AuthorType.author);
    addAuthorsFromJson(jsonObj.getArray("editor"    ), AuthorType.editor);
    addAuthorsFromJson(jsonObj.getArray("translator"), AuthorType.translator);

    if (fieldNotEmpty(bfDOI) == false)
      setDOI(queryDoi);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDateIfPresent(JsonObj jsonObj, DateType dt)
  {
    if (jsonObj.containsKey(dt.desc))
    {
      JsonArray partsArr = jsonObj.getObj(dt.desc).getArraySafe("date-parts").getArray(0);

      int year =  (partsArr.size() > 0 ? parseInt(partsArr.getLongAsStrSafe(0), 0) : 0),
          month = (partsArr.size() > 1 ? parseInt(partsArr.getLongAsStrSafe(1), 0) : 0),
          day =   (partsArr.size() > 2 ? parseInt(partsArr.getLongAsStrSafe(2), 0) : 0);

      setDate(new BibliographicDate(day, month, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(year)), dt, false);  // I don't know how Crossref stores BC years, this is just a guess.
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addAuthorsFromJson(JsonArray jsonArr, AuthorType authorType)
  {
    if (jsonArr == null)
      return;

    jsonArr.getObjs().forEach(author ->
    {
      String first = author.getStrSafe("given"), last = author.getStrSafe("family");
      if ((first + last).isBlank())
        last = author.getStrSafe("name");

      authors.add(new BibAuthor(authorType, new PersonName(first, last)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // List of types: https://api.crossref.org/v1/types

  private static EntryType parseCrossrefType(String crType)
  {
    return switch (crType)
    {
      case "book"                -> etBook;
      case "book-chapter"        -> etBookChapter;
      case "book-part"           -> etBookPart;
      case "book-section"        -> etBookSection;
      case "book-series"         -> etBookSeries;
      case "book-set"            -> etBookSet;
      case "book-track"          -> etBookTrack;
      case "component"           -> etComponent;
      case "dataset"             -> etDataSet;
      case "dissertation"        -> etThesis;
      case "edited-book"         -> etEditedBook;
      case "journal"             -> etJournal;
      case "journal-article"     -> etJournalArticle;
      case "journal-issue"       -> etJournalIssue;
      case "journal-volume"      -> etJournalVolume;
      case "monograph"           -> etMonograph;
      case "peer-review"         -> etPeerReview;
      case "posted-content"      -> etPostedContent;
      case "proceedings"         -> etConferenceProceedings;
      case "proceedings-article" -> etConferencePaper;
      case "proceedings-series"  -> etProceedingsSeries;
      case "reference-book"      -> etReferenceBook;
      case "reference-entry"     -> etReferenceEntry;
      case "report"              -> etReport;
      case "report-series"       -> etReportSeries;
      case "standard"            -> etStandard;
      case "standard-series"     -> etStandardSeries; // fall through

      default                    -> etOther;   // Applies to "other"
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getQueryUrl(String title, String yearStr, Iterable<BibAuthor> authors, boolean engCharForAuthors, String doi)
  {
    String url = "https://api.crossref.org/works", auths = "", eds = "";

    if (strNotNullOrEmpty(doi))
      return url + '/' + doi;

    if (strNullOrBlank(title)) return url;

    url = url + '?';

    if (authors != null)
    {
      for (BibAuthor author : authors)
      {
        boolean ed = author.getIsEditor(),
                tr = author.getIsTrans();

        String name = engCharForAuthors ?
          author.getName().toEngChar().getLast()
        :
          author.getName().getLast();

        if (ed)
          eds = eds + ' ' + name;
        else if (tr == false)
          auths = auths + ' ' + name;
      }
    }

    auths = auths.strip();
    if (auths.isEmpty()) auths = eds.strip();

    title = convertToEnglishChars(title.strip());
    title = title.replace(":", "");
    title = title.replace("?", "");

    yearStr = safeStr(yearStr);

    if (strNotNullOrEmpty(yearStr) && StringUtils.isNumeric(yearStr))
    {
      int year = parseInt(yearStr, -1);
      if (year > 1929)
        url = url + "query.bibliographic=" + yearStr + '&';
    }

    if (strNotNullOrEmpty(auths))
      url = url + "query.author=" + escapeURL(auths, false);

    if (strNotNullOrEmpty(title))
    {
      if (strNotNullOrEmpty(auths))
        url = url + '&';

      url = url + "query.title=" + escapeURL(title, false);
    }

    return url;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void doHttpRequest(AsyncHttpClient httpClient, String doi, Set<String> alreadyCheckedIDs,
                            Consumer<CrossrefBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, null, null, false, null, doi, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  static void doHttpRequest(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper, BibAuthors authors,
                            String doi, Set<String> alreadyCheckedIDs, Consumer<CrossrefBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, title, yearStr, isPaper, authors, true, doi, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  private static void doHttpRequest(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper, BibAuthors authors, boolean engCharForAuthors,
                                    String doi, Set<String> alreadyCheckedIDs, Consumer<CrossrefBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    if (strNotNullOrEmpty(doi) && alreadyCheckedIDs.contains(doi.toLowerCase()))
    {
      successHndlr.accept(null);
      return;
    }

    alreadyCheckedIDs.add(doi.toLowerCase());

    JsonHttpClient.getObjAsync(getQueryUrl(title, yearStr, authors, engCharForAuthors, doi), httpClient, jsonObj ->
    {
      CrossrefBibData bd = createFromJSON(jsonObj, title, yearStr, isPaper, doi);

      if ((bd == null) && engCharForAuthors && (authors != null) && authors.notAllEngCharLastNames())
      {
        doHttpRequest(httpClient, title, yearStr, isPaper, authors, false, doi, alreadyCheckedIDs, successHndlr, failHndlr);
        return;
      }

      successHndlr.accept(bd);

    }, e ->
    {
      if ((e instanceof HttpResponseException hre) && (hre.getStatusCode() == HttpStatus.SC_NOT_FOUND))
      {
        if (doi.endsWith("."))
        {
          String newDoi = doi;
          do { newDoi = StringUtils.removeEnd(newDoi, "."); } while (newDoi.endsWith("."));

          doHttpRequest(httpClient, newDoi, alreadyCheckedIDs, successHndlr, failHndlr);
          return;
        }

        successHndlr.accept(null);
        return;
      }

      failHndlr.accept(e);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
