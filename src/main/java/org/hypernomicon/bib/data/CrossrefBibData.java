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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibData.YearType.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.util.json.JsonObj.JsonNodeType;

public class CrossrefBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // CrossRef API documentation: https://github.com/CrossRef/rest-api-doc

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
          messageDialog(message, mtError);

        return null;
      }

      jsonObj = jsonObj.getObj("message");
      jsonArray = jsonObj.getArray("items");

      if (jsonArray == null)
        return new CrossrefBibData(jsonObj, queryDoi);

      if (jsonArray.size() == 0)
        return null;

      if (jsonArray.size() == 1)
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
        int otherYear = parseInt(curBD.getStr(bfYear), Integer.MAX_VALUE);
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

      curTitle = HDT_RecordBase.makeSortKeyByType(curTitle, hdtWork);
      int len = Math.min(title.length(), curTitle.length());
      double curDist = (double)(alg.apply(safeSubstring(title, 0, len), safeSubstring(curTitle, 0, len))) / (double)len;

      if (curDist < bestDist)
      {
        bestBD = curBD;
        bestDist = curDist;
      }
    }

    return bestDist > 0.25 ? null : bestBD;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private CrossrefBibData(JsonObj jsonObj, String queryDoi)
  {
    super();

    setStr(bfDOI, jsonObj.getStrSafe("DOI"));

    EntryType entryType = parseCrossrefType(jsonObj.getStrSafe("type"));
    setEntryType(entryType);
    setWorkType(toWorkType(entryType));

    setStr(bfPages    , jsonObj.getStrSafe("page"));
    setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    setStr(bfPubLoc   , jsonObj.getStrSafe("publisher-location"));

    setDateIfPresent(jsonObj, ytPublishedPrint);
    setDateIfPresent(jsonObj, ytIssued);
    setDateIfPresent(jsonObj, ytCreated);

    setStr(bfURL, jsonObj.getStrSafe("URL"));

    if (jsonObj.containsKey("link"))
    {
      JsonArray linkArray = jsonObj.getArray("link");
      if (linkArray.size() > 0)
      {
        String link = linkArray.getObj(0).getStrSafe("URL");
        if (link.isBlank() == false)
          setStr(bfURL, link);
      }
    }

    setStr(bfVolume, jsonObj.getStrSafe("volume"));
    setStr(bfIssue , jsonObj.getStrSafe("issue"));

    List<String> title    = JsonArray.toStrList(jsonObj.getArray("title")),
                 subtitle = JsonArray.toStrList(jsonObj.getArray("subtitle"));

    if (strListsEqual(title, subtitle, true) == false)
      title.addAll(subtitle);

    setMultiStr(bfTitle, title);

    setMultiStr(bfContainerTitle, JsonArray.toStrList(jsonObj.getArray("container-title")));
    setMultiStr(bfISBNs         , JsonArray.toStrList(jsonObj.getArray("ISBN")));
    setMultiStr(bfISSNs         , JsonArray.toStrList(jsonObj.getArray("ISSN")));

    addAuthorsFromJson(jsonObj.getArray("author"    ), AuthorType.author);
    addAuthorsFromJson(jsonObj.getArray("editor"    ), AuthorType.editor);
    addAuthorsFromJson(jsonObj.getArray("translator"), AuthorType.translator);

    if (fieldNotEmpty(bfDOI) == false)
      setDOI(queryDoi);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setDateIfPresent(JsonObj jsonObj, YearType yt)
  {
    if (jsonObj.containsKey(yt.desc))
      setYear(jsonObj.getObj(yt.desc).getArray("date-parts").getArray(0).getLongAsStrSafe(0), yt);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addAuthorsFromJson(JsonArray jsonArr, AuthorType aType)
  {
    if (jsonArr != null)
      jsonArr.getObjs().forEach(author -> authors.add(new BibAuthor(aType, new PersonName(author.getStrSafe("given"), author.getStrSafe("family")))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // List of types: https://api.crossref.org/v1/types

  private static EntryType parseCrossrefType(String crType)
  {
    switch (crType)
    {
      case "book"                : return etBook;
      case "book-chapter"        : return etBookChapter;
      case "book-part"           : return etBookPart;
      case "book-section"        : return etBookSection;
      case "book-series"         : return etBookSeries;
      case "book-set"            : return etBookSet;
      case "book-track"          : return etBookTrack;
      case "component"           : return etComponent;
      case "dataset"             : return etDataSet;
      case "dissertation"        : return etThesis;
      case "edited-book"         : return etEditedBook;
      case "journal"             : return etJournal;
      case "journal-article"     : return etJournalArticle;
      case "journal-issue"       : return etJournalIssue;
      case "journal-volume"      : return etJournalVolume;
      case "monograph"           : return etMonograph;
      case "other"               : return etOther;
      case "peer-review"         : return etPeerReview;
      case "posted-content"      : return etPostedContent;
      case "proceedings"         : return etConferenceProceedings;
      case "proceedings-article" : return etConferencePaper;
      case "proceedings-series"  : return etProceedingsSeries;
      case "reference-book"      : return etReferenceBook;
      case "reference-entry"     : return etReferenceEntry;
      case "report"              : return etReport;
      case "report-series"       : return etReportSeries;
      case "standard"            : return etStandard;
      case "standard-series"     : return etStandardSeries;

      default                    : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getQueryUrl(String title, String yearStr, BibAuthors authors, String doi)
  {
    String url = "https://api.crossref.org/works", auths = "", eds = "";

    if (doi.length() > 0)
      return url + "/" + doi;

    if (safeStr(title).isEmpty()) return url;

    url = url + "?";

    if (authors != null)
    {
      for (BibAuthor author : authors)
      {
        boolean ed = author.getType() == AuthorType.editor,
                tr = author.getType() == AuthorType.translator;

        String name = author.getName().toEngChar().getLast();

        if (ed)
          eds = eds + " " + name;
        else if (tr == false)
          auths = auths + " " + name;
      }
    }

    if (auths.isEmpty()) auths = eds;

    title = convertToEnglishChars(title).trim();
    title = title.replace(":", "");
    title = title.replace("?", "");

    yearStr = safeStr(yearStr);

    if ((yearStr.length() > 0) && StringUtils.isNumeric(yearStr))
    {
      int year = parseInt(yearStr, -1);
      if (year > 1929)
        url = url + "query.bibliographic=" + yearStr + "&";
    }

    if (auths.length() > 0)
      url = url + "query.author=" + escapeURL(auths, false);

    if (title.length() > 0)
    {
      if (auths.length() > 0)
        url = url + "&";

      url = url + "query.bibliographic=" + escapeURL(title, false); // query.title is deprecated
    }

    return url;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void doHttpRequest(AsyncHttpClient httpClient, String doi, Set<String> alreadyCheckedIDs,
                            Consumer<BibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, null, null, false, null, doi, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  static void doHttpRequest(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper, BibAuthors authors, String doi,
                            Set<String> alreadyCheckedIDs, Consumer<BibData> successHndlr, Consumer<Exception> failHndlr)
  {
    if ((doi.length() > 0) && alreadyCheckedIDs.contains(doi.toLowerCase()))
    {
      successHndlr.accept(null);
      return;
    }

    alreadyCheckedIDs.add(doi.toLowerCase());

    JsonHttpClient.getObjAsync(CrossrefBibData.getQueryUrl(title, yearStr, authors, doi), httpClient, jsonObj ->
    {
      BibData bd = CrossrefBibData.createFromJSON(jsonObj, title, yearStr, isPaper, doi);

      successHndlr.accept(bd);

    }, e ->
    {
      if ((e instanceof HttpResponseException) && (HttpResponseException.class.cast(e).getStatusCode() == HttpStatus.SC_NOT_FOUND))
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
