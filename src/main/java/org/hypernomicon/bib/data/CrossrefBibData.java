/*
 * Copyright 2015-2019 Jason Winning
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
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.text.similarity.LevenshteinDistance;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public class CrossrefBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static CrossrefBibData createFromJSON(JsonObj jsonObj, String title, String queryDoi)
  {
    JsonArray jsonArray;

    try
    {
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

    LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();
    CrossrefBibData bestBD = null;
    double bestDist = Double.MAX_VALUE;
    title = HDT_RecordBase.makeSortKeyByType(title, hdtWork);

    for (JsonObj curObj : jsonArray.getObjs())
    {
      CrossrefBibData curBD = new CrossrefBibData(curObj, queryDoi);
      String curTitle = HDT_RecordBase.makeSortKeyByType(curBD.getStr(bfTitle), hdtWork);
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

    setStr(bfPages, jsonObj.getStrSafe("page"));
    setStr(bfPublisher, jsonObj.getStrSafe("publisher"));

    if (jsonObj.containsKey(ytPublishedPrint.desc))
    {
      setStr(bfYear, jsonObj.getObj(ytPublishedPrint.desc).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      yearType = ytPublishedPrint;
    }
    else if (jsonObj.containsKey(ytIssued.desc))
    {
      setStr(bfYear, jsonObj.getObj(ytIssued.desc).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      yearType = ytIssued;
    }
    else if (jsonObj.containsKey(ytCreated.desc))
    {
      setStr(bfYear, jsonObj.getObj(ytCreated.desc).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      yearType = ytCreated;
    }

    setStr(bfURL, jsonObj.getStrSafe("URL"));
    setStr(bfVolume, jsonObj.getStrSafe("volume"));
    setStr(bfIssue, jsonObj.getStrSafe("issue"));

    List<String> title = JsonArray.toStrList(jsonObj.getArray("title")),
                 subtitle = JsonArray.toStrList(jsonObj.getArray("subtitle"));

    if (strListsEqual(title, subtitle, true) == false)
      title.addAll(subtitle);

    setMultiStr(bfTitle, title);

    setMultiStr(bfContainerTitle, JsonArray.toStrList(jsonObj.getArray("container-title")));
    setMultiStr(bfISBNs, JsonArray.toStrList(jsonObj.getArray("ISBN")));
    setMultiStr(bfISSNs, JsonArray.toStrList(jsonObj.getArray("ISSN")));

    addAuthorsFromJson(jsonObj.getArray("author"    ), AuthorType.author);
    addAuthorsFromJson(jsonObj.getArray("editor"    ), AuthorType.editor);
    addAuthorsFromJson(jsonObj.getArray("translator"), AuthorType.translator);

    if (fieldNotEmpty(bfDOI) == false)
      setDOI(queryDoi);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addAuthorsFromJson(JsonArray jsonArr, AuthorType aType)
  {
    if (jsonArr == null) return;

    jsonArr.getObjs().forEach(author -> authors.add(new BibAuthor(aType, new PersonName(author.getStrSafe("given"), author.getStrSafe("family")))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

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
      case "posted-content"      : return etPostedContent;
      case "proceedings"         : return etConferenceProceedings;
      case "proceedings-article" : return etConferencePaper;
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

  private static String getQueryUrl(String title, String yearStr, List<ObjectGroup> authGroups, String doi)
  {
    String url = "https://api.crossref.org/works", auths = "", eds = "";

    if (doi.length() > 0)
      return url + "/" + doi;

    if (safeStr(title).length() == 0) return url;

    url = url + "?";

    if (authGroups != null)
    {
      for (ObjectGroup authGroup : authGroups)
      {
        boolean ed = authGroup.getValue(tagEditor).bool,
                tr = authGroup.getValue(tagTranslator).bool;

        HDT_Person person = authGroup.getPrimary();
        String name;

        if (person == null)
          name = convertToEnglishChars(new PersonName(authGroup.getPrimaryStr()).getLast());
        else
          name = person.getLastNameEngChar();

        if (ed)
          eds = eds + "+" + name;
        else if (tr == false)
          auths = auths + "+" + name;
      }
    }

    if (auths.length() == 0) auths = eds;

    title = convertToEnglishChars(title).trim();
    title = title.replace(":", "");
    title = title.replace("?", "");

    title = title.replace(' ', '+');

    yearStr = safeStr(yearStr);

    if ((yearStr.length() > 0) && StringUtils.isNumeric(yearStr))
    {
      int year = parseInt(yearStr, -1);
      if (year > 1929)
        url = url + "query=" + yearStr + "&";
    }

    if (auths.length() > 0)
      url = url + "query.author=" + escapeURL(auths, false);

    if (title.length() > 0)
    {
      if (auths.length() > 0)
        url = url + "&";

      url = url + "query.title=" + escapeURL(title, false);
    }

    return url;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void doHttpRequest(AsyncHttpClient httpClient, String doi, Set<String> alreadyCheckedIDs,
                            Consumer<BibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, null, null, null, doi, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  static void doHttpRequest(AsyncHttpClient httpClient, String title, String yearStr, List<ObjectGroup> authGroups, String doi,
                            Set<String> alreadyCheckedIDs, Consumer<BibData> successHndlr, Consumer<Exception> failHndlr)
  {
    if ((doi.length() > 0) && alreadyCheckedIDs.contains(doi.toLowerCase()))
    {
      successHndlr.accept(null);
      return;
    }

    alreadyCheckedIDs.add(doi.toLowerCase());

    JsonHttpClient.getObjAsync(CrossrefBibData.getQueryUrl(title, yearStr, authGroups, doi), httpClient, jsonObj ->
    {
      BibData bd = CrossrefBibData.createFromJSON(jsonObj, title, doi);

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
