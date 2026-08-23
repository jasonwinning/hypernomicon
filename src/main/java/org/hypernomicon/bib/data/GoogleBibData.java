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

package org.hypernomicon.bib.data;

import static org.hypernomicon.App.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.TestContext.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import org.apache.commons.text.similarity.LevenshteinDistance;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.zotero.ZoteroDate;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.util.http.AsyncHttpClient;
import org.hypernomicon.util.http.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

public final class GoogleBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String queryIsbn;

  private static volatile String apiKeyForTesting = null;

//---------------------------------------------------------------------------

  @Override public boolean fromOnlineSource() { return true; }

  public String getQueryIsbn()                { return safeStr(queryIsbn); }

  static void setApiKeyForTesting(String key) { assertThatThisIsUnitTestThread(); apiKeyForTesting = key; }

  public static boolean apiKeyConfigured()    { return apiKey().isBlank() == false; }

//---------------------------------------------------------------------------

  private GoogleBibData(JsonObj jsonObj, String queryIsbn)
  {
    this.queryIsbn = queryIsbn;

    String title    = jsonObj.getStrSafe("title"),
           subtitle = jsonObj.getStrSafe("subtitle");

    addStr(bfTitle, title);

    if (title.equalsIgnoreCase(subtitle) == false)
      addStr(bfTitle, subtitle);

    setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    setEntryType(parseGoogleBooksType(jsonObj.getStrSafe("printType")));  // supposedly this will either be "BOOK" or "MAGAZINE", nothing else

    String publishedDate = jsonObj.getStrSafe(dtPublishedDate.desc);
    if (strNotNullOrEmpty(publishedDate))
      setDate(ZoteroDate.parsedDateStrToBibDate(publishedDate, false), dtPublishedDate, true);  // Date is in local ISO date format like Zotero's "parsed date"
                                                                                                // Assumption here is that Google Books years are never BC
    jsonObj.getArraySafe("authors").strStream().forEach(authStr ->
      authors.add(new AuthorStandalone(author, new PersonName(authStr))));

    jsonObj.getArraySafe("industryIdentifiers").getObjs().forEach(iiObj ->
    {
      if (iiObj.getStrSafe("type").toLowerCase().contains("isbn"))
        addISBN(iiObj.getStrSafe("identifier"));
    });

    if (fieldNotEmpty(bfISBNs) == false)
      addISBN(queryIsbn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static GoogleBibData createFromJSON(JsonObj jsonObj, String title, List<String> authKeywords, String queryIsbn)
  {
    try
    {
      JsonArray jsonArray = jsonObj.getArraySafe("items");

      if (jsonArray.isEmpty()) return null;

      // Only an ISBN lookup, which has no title to check against, takes the first result as it
      // comes; a lone title-search result is checked like any other

      if (strNullOrBlank(title))
        return new GoogleBibData(jsonArray.getObj(0).getObj("volumeInfo"), queryIsbn);

      LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();
      GoogleBibData bestBDnoAuthMatch = null, bestBDwithAuthMatch = null;
      double bestDistNoAuthMatch = Double.MAX_VALUE, bestDistWithAuthMatch = Double.MAX_VALUE;
      title = HDT_RecordBase.makeSortKeyByType(title, hdtWork);

      for (JsonObj curArrObj : jsonArray.getObjs())
      {
        JsonObj curObj = curArrObj.getObj("volumeInfo");

        boolean authMatch = (authKeywords.size() > 0) &&
                            curObj.getArraySafe("authors").strStream().anyMatch(jsonAuthStr -> authKeywords.stream().anyMatch(jsonAuthStr::contains));

        GoogleBibData curBD = new GoogleBibData(curObj, queryIsbn);

        double curDist = titleDistance(alg, title, HDT_RecordBase.makeSortKeyByType(curBD.getStr(bfTitle), hdtWork));

        if (authMatch)
        {
          if (curDist < bestDistWithAuthMatch)
          {
            bestBDwithAuthMatch = curBD;
            bestDistWithAuthMatch = curDist;
          }
        }
        else
        {
          if (curDist < bestDistNoAuthMatch)
          {
            bestBDnoAuthMatch = curBD;
            bestDistNoAuthMatch = curDist;
          }
        }
      }

      if (bestDistWithAuthMatch <= LEVENSHTEIN_THRESHOLD) return bestBDwithAuthMatch;
      return bestDistNoAuthMatch > LEVENSHTEIN_THRESHOLD ? null : bestBDnoAuthMatch;
    }
    catch (NullPointerException e)
    {
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseGoogleBooksType(String gbType) { return switch (gbType)
  {
    case "BOOK"     -> etBook;
    case "MAGAZINE" -> etMagazine;
    default         -> etOther;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static String getQueryUrl(String title, BibAuthors authors, List<String> authKeywords, CharSequence isbn)
  {
    String url = "https://www.googleapis.com/books/v1/volumes?q=";

    if (strNotNullOrEmpty(isbn))
      return withApiKey(url + "isbn:" + isbn);

    if (strNullOrBlank(title)) return withApiKey(url);

    authKeywords.clear();
    List<String> edKeywords = new ArrayList<>();

    if (authors != null)
    {
      for (Author author : authors)
      {
        boolean ed = author.getIsEditor(),
                tr = author.getIsTrans();

        String name = author.getName().toEngChar().getLast();

        if (ed)
          edKeywords.add(name);
        else if (tr == false)
          authKeywords.add(name);
      }
    }

    if (authKeywords.isEmpty())
      authKeywords.addAll(edKeywords);

    String auths = authKeywords.stream().map(keyword -> escapeURL('"' + keyword + '"', false))
                                        .collect(Collectors.joining("+"));

    title = convertToEnglishChars(title).strip();

    if (strNotNullOrEmpty(title))
      url = url + escapeURL(title, false);

    if (strNotNullOrEmpty(auths))
    {
      if (strNotNullOrEmpty(title))
        url = url + '+';

      url = url + auths;
    }

    return withApiKey(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String withApiKey(String url)
  {
    String key = apiKey();

    return key.isBlank() ? url : (url + "&key=" + escapeURL(key, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The user's Google Books API key, from preferences; empty when none is configured.
   * Google retired anonymous Books API access in 2026 (an unkeyed request is billed to a
   * shared consumer whose daily quota is configured as zero), so without a key every query
   * fails with HTTP 429, and callers skip the Google stages entirely. There is no settings
   * UI for the key yet; until one exists, the preference can be set externally.
   */
  static String apiKey()
  {
    if (apiKeyForTesting != null) return apiKeyForTesting;

    return (app == null) || (app.prefs == null) ? "" : safeStr(app.prefs.get(PrefKey.GOOGLE_BOOKS_API_KEY, ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void doHttpRequest(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs,
                            Consumer<GoogleBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, null, null, isbnIt, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  static void doHttpRequest(AsyncHttpClient httpClient, String title, BibAuthors authors, Iterator<String> isbnIt,
                            Set<String> alreadyCheckedIDs, Consumer<GoogleBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    String isbn = "";

    if (isbnIt != null)
    {
      while (isbn.isBlank() && isbnIt.hasNext())
      {
        isbn = isbnIt.next();

        // Canonicalized to ISBN-13, so the ISBN-10 and ISBN-13 forms of the same
        // book (e.g. one from the work record and one from the PDF) dedupe as one

        if (alreadyCheckedIDs.contains(convertToISBN13(isbn)))
          isbn = "";
      }
    }

    if (isbn.isBlank() && strNullOrBlank(title))
    {
      successHndlr.accept(null);
      return;
    }

    if (isbn.isBlank() == false)
      alreadyCheckedIDs.add(convertToISBN13(isbn));

    String finalIsbn = isbn;
    List<String> authKeywords = new ArrayList<>();
    String url = getQueryUrl(title, authors, authKeywords, isbn);

    JsonHttpClient.getObjAsync(url, httpClient, jsonObj ->
    {
      GoogleBibData bd = createFromJSON(jsonObj, title, authKeywords, finalIsbn);

      if ((bd == null) && (isbnIt != null) && isbnIt.hasNext())
      {
        doHttpRequest(httpClient, isbnIt, alreadyCheckedIDs, successHndlr, failHndlr);
        return;
      }

      successHndlr.accept(bd);

    }, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
