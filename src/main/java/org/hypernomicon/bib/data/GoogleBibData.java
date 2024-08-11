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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

import org.apache.commons.text.similarity.LevenshteinDistance;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public final class GoogleBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String queryIsbn;

  public String getQueryIsbn() { return safeStr(queryIsbn); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static GoogleBibData createFromJSON(JsonObj jsonObj, String title, List<String> authKeywords, String queryIsbn)
  {
    try
    {
      JsonArray jsonArray = jsonObj.getArray("items");

      if (jsonArray.isEmpty()) return null;

      if ((jsonArray.size() == 1) || safeStr(title).isBlank())
        return new GoogleBibData(jsonArray.getObj(0).getObj("volumeInfo"), queryIsbn);

      LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();
      GoogleBibData bestBDnoAuthMatch = null, bestBDwithAuthMatch = null;
      double bestDistNoAuthMatch = Double.MAX_VALUE, bestDistWithAuthMatch = Double.MAX_VALUE;
      title = HDT_RecordBase.makeSortKeyByType(title, hdtWork);

      for (JsonObj curArrObj : jsonArray.getObjs())
      {
        JsonObj curObj = curArrObj.getObj("volumeInfo");

        boolean authMatch = false;

        if (authKeywords.size() > 0)
        {
          JsonArray authArr = curObj.getArray("authors");

          if (authArr != null)
            authMatch = authArr.strStream().anyMatch(jsonAuthStr -> authKeywords.stream().anyMatch(jsonAuthStr::contains));
        }

        GoogleBibData curBD = new GoogleBibData(curObj, queryIsbn);
        String curTitle = HDT_RecordBase.makeSortKeyByType(curBD.getStr(bfTitle), hdtWork);
        int len = Math.min(title.length(), curTitle.length());
        double curDist = (double)alg.apply(safeSubstring(title, 0, len), safeSubstring(curTitle, 0, len)) / (double)len;

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

      if (bestDistWithAuthMatch <= 0.25) return bestBDwithAuthMatch;
      return bestDistNoAuthMatch > 0.25 ? null : bestBDnoAuthMatch;
    }
    catch (NullPointerException e)
    {
      return null;
    }
  }

//---------------------------------------------------------------------------
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
    setEntryType(parseGoogleBooksType(jsonObj.getStrSafe("printType"))); // supposedly this will either be "BOOK" or "MAGAZINE", nothing else

    String publishedDate = jsonObj.getStrSafe(dtPublishedDate.desc);
    if (publishedDate.length() > 0)
      setYear(publishedDate.substring(0, 4), dtPublishedDate);

    nullSwitch(jsonObj.getArray("authors"), authArray -> authArray.strStream().forEach(authStr ->
      authors.add(new BibAuthor(AuthorType.author, new PersonName(authStr)))));

    nullSwitch(jsonObj.getArray("industryIdentifiers"), iiArr -> iiArr.getObjs().forEach(iiObj ->
    {
      if (iiObj.getStrSafe("type").toLowerCase().contains("isbn"))
        addISBN(iiObj.getStrSafe("identifier"));
    }));

    if (fieldNotEmpty(bfISBNs) == false)
      addISBN(queryIsbn);
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

  private static String getQueryUrl(String title, BibAuthors authors, List<String> authKeywords, String isbn)
  {
    String url = "https://www.googleapis.com/books/v1/volumes?q=";

    if (isbn.length() > 0)
      return url + "isbn:" + isbn;

    if (safeStr(title).isEmpty()) return url;

    authKeywords.clear();
    List<String> edKeywords = new ArrayList<>();

    if (authors != null)
    {
      for (BibAuthor author : authors)
      {
        boolean ed = author.getType() == AuthorType.editor,
                tr = author.getType() == AuthorType.translator;

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
                                        .reduce((s1, s2) -> s1 + "+" + s2).orElse("");

    title = convertToEnglishChars(title).trim();

    if (title.length() > 0)
      url = url + escapeURL('"' + title + '"', false);

    if (auths.length() > 0)
    {
      if (title.length() > 0)
        url = url + '+';

      url = url + auths;
    }

    return url;
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
      while ((isbn.isBlank() && isbnIt.hasNext()))
      {
        isbn = isbnIt.next();
        if (alreadyCheckedIDs.contains(isbn.toLowerCase()))
          isbn = "";
      }
    }

    if (isbn.isBlank() && safeStr(title).isBlank())
    {
      successHndlr.accept(null);
      return;
    }

    alreadyCheckedIDs.add(isbn.toLowerCase());
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
