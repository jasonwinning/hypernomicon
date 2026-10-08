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

import static org.hypernomicon.bib.data.MarcRecord.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.TestContext.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;

import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.StringUtil;
import org.hypernomicon.util.http.*;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Searches the Library of Congress catalog through the Library's Linked Data Service,
 * <a href="https://id.loc.gov/">https://id.loc.gov/</a>, for when its SRU server cannot be used
 * (see {@link LibraryOfCongressBibData}). The service is HTTPS on the standard port and needs no
 * key. It holds the catalog in BIBFRAME, LoC's linked-data model, which describes a work apart
 * from each published edition of it (an instance), and it serves the MARCXML record that each
 * instance was converted from, which {@link LibraryOfCongressBibData} reads just as it reads the
 * SRU server's records.
 * <p>
 * A search takes two requests: a keyword search of the short labels that the service keeps for
 * suggesting entries, then the MARCXML record of the instance that the best hit leads to. An ISBN
 * is searched for among the instances, where it finds its edition. A title is searched for among
 * the works, as a phrase followed by the author's surname, which narrows the hits much as the SRU
 * server's title and author indexes do. A work's hit names its instance, and its label (the work's
 * authorized name and title), contributors and date of publication let the hits be ranked as the
 * SRU server's records are, before any record is fetched.
 * </p>
 * <p>
 * The newest records, those with the identifiers that the catalog has given since LoC moved it to
 * FOLIO in 2025, have had no MARCXML here: the service answers with an empty document, so for
 * those this finds nothing.
 * </p>
 */
final class LocLinkedDataSearch
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** One hit of a search: the instance it leads to, and for a work, what the hit says about it */
  record Hit(String instanceID, String title, List<String> lastNames, int year) { }

//---------------------------------------------------------------------------

  private static final String BASE_URL = "https://id.loc.gov/resources/";

  /**
   * How many hits of a title search to weigh. Hits are small, and only the best one's record is
   * fetched, so many more of them can be weighed than the SRU server's whole records.
   */
  private static final int MAX_TITLE_HITS = 50;

  private static final Pattern INSTANCE_ID        = Pattern.compile("[A-Za-z0-9]+"),
                               NOT_PART_OF_A_WORD = Pattern.compile("[^\\p{L}\\p{N}']+"),
                               LEADING_ARTICLE    = Pattern.compile("^(the|a|an) ");

  private static volatile String baseUrlForTesting = null;

//---------------------------------------------------------------------------

  private LocLinkedDataSearch() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  static void setBaseUrlForTesting(String url) { assertThatThisIsUnitTestThread(); baseUrlForTesting = url; }

  private static String baseUrl()              { return Objects.requireNonNullElse(baseUrlForTesting, BASE_URL); }

  static String recordUrl(String instanceID)   { return baseUrl() + "instances/" + instanceID + ".marcxml.xml"; }

  static String isbnSearchUrl(String isbn)     { return searchUrl("instances", LibraryOfCongressBibData.isbnForQuery(isbn), 1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The search for a title: the title as a phrase, which a label must contain as it stands, then
   * the surname of the first author, which narrows the hits to that author's works. The phrase
   * leaves out an English article that the title starts with, because the label may not have it
   * ("Phenomenology of perception" for "The Phenomenology of Perception"), and the phrase without
   * it is found in the label either way.
   */
  static String titleSearchUrl(String title, List<String> authKeywords)
  {
    String phrase = LEADING_ARTICLE.matcher(keywordsOf(title)).replaceFirst(""),
           query  = phrase.isEmpty() ? "" : ('"' + phrase + '"');

    if (collEmpty(authKeywords) == false)
      query = (query + ' ' + keywordsOf(authKeywords.getFirst())).strip();

    return searchUrl("works", query, MAX_TITLE_HITS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The keyword search type is a must: the default one matches labels from their start, which finds nothing for words from the middle of one */
  private static String searchUrl(String directory, String query, int count)
  {
    return baseUrl() + directory + "/suggest2/?q=" + escapeURL(query, false) + "&searchtype=keyword&count=" + count;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The words of the text, for a keyword search: its letters, digits and apostrophes, lowercase,
   * and without diacritics, which the search ignores anyway. An apostrophe is part of a word, as
   * "mind's" matches the label's "Mind's"; anything else is dropped. A quote would end the phrase
   * it is part of, and a hyphen joins words that the labels are searched by separately: "merleau
   * ponty" matches "Merleau-Ponty".
   */
  static String keywordsOf(String text)
  {
    return collapseSpaces(NOT_PART_OF_A_WORD.matcher(convertToEnglishChars(safeStr(text)).toLowerCase(Locale.ROOT)).replaceAll(" ")).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Searches by ISBN, or by title if there is none, and fetches the record that the best hit
   * leads to; the handler is given null if nothing is found. The record of a title search's hit
   * is checked again by {@link LibraryOfCongressBibData#bestMatch}, because what the hit was
   * ranked by came from its label, not from the record's own title.
   */
  static void search(AsyncHttpClient httpClient, String title, String yearStr, List<String> authKeywords, String isbn,
                     Consumer<LibraryOfCongressBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    String url = isbn.isBlank() ? titleSearchUrl(title, authKeywords) : isbnSearchUrl(isbn);

    JsonHttpClient.getObjAsync(url, httpClient, jsonObj ->
    {
      if (jsonObj == null)
      {
        failHndlr.accept(new IOException("The Library of Congress Linked Data Service did not answer with search results"));
        return;
      }

      Hit hit = bestHit(hitsIn(jsonObj), title, yearStr, authKeywords, isbn);

      if (hit == null)
      {
        successHndlr.accept(null);
        return;
      }

      XmlHttpClient.getDocAsync(recordUrl(hit.instanceID()), httpClient, doc ->
        successHndlr.accept(LibraryOfCongressBibData.bestMatch(marcRecordsIn(doc), title, yearStr, authKeywords, isbn)), failHndlr);

    }, failHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The hit whose record to fetch, or null if none will do. An ISBN search takes its first hit,
   * as the SRU server's first record is taken. A title search's hits are ranked the way the SRU
   * server's records are, by {@link LibraryOfCongressBibData#bestTitleMatch}.
   */
  static Hit bestHit(List<Hit> hits, String title, String yearStr, List<String> authKeywords, String isbn)
  {
    if (isbn.isBlank() == false)
      return hits.isEmpty() ? null : hits.getFirst();

    return LibraryOfCongressBibData.bestTitleMatch(hits, title, yearStr, authKeywords, Hit::title, Hit::lastNames, Hit::year);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The hits of a search, in the order the service ranked them. A hit among the instances is an
   * instance itself. A hit among the works names its instance, and its label, contributors and
   * dates of publication describe the work.
   */
  static List<Hit> hitsIn(JsonObj response)
  {
    List<Hit> hits = new ArrayList<>();

    for (JsonObj hitObj : response.getArraySafe("hits").getObjs())
    {
      JsonObj more = Objects.requireNonNullElseGet(hitObj.getObj("more"), JsonObj::new);

      String workInstance = more.getStrSafe("instance"),
             uri          = workInstance.isBlank() ? hitObj.getStrSafe("uri") : workInstance,
             instanceID   = uri.contains("/instances/") ? StringUtils.substringAfterLast(uri, "/") : "";

      // The identifier goes into the URL of the record, so nothing but letters and digits will do

      if (INSTANCE_ID.matcher(instanceID).matches() == false) continue;

      List<String> contributors = more.getArraySafe("contributors").strStream().filter(StringUtil::strNotNullOrBlank).toList();

      hits.add(new Hit(instanceID, titleFromLabel(hitObj.getStrSafe("aLabel"), contributors),
                       contributors.stream().map(contributor -> new PersonName(contributor).getLast()).toList(),
                       yearFrom(more.getArraySafe("pubdates"))));
    }

    return hits;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The title in a work's label. The label is the work's authorized name and title: the name of
   * the work's main entry, which is the first of its contributors, then the title, usually after a
   * period ("McDowell, John Henry. Mind and world"); or for a work entered under its title, such
   * as an edited volume, the title alone.
   */
  static String titleFromLabel(String label, List<String> contributors)
  {
    if ((contributors.isEmpty() == false) && label.startsWith(contributors.getFirst()))
      label = StringUtils.stripStart(label.substring(contributors.getFirst().length()), ". ");

    return label.strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The year that a work's first date of publication begins with, or zero if it does not begin with one: a date can be uncertain ("198X") or a range ("1884/9999") */
  static int yearFrom(JsonArray pubdates)
  {
    String year = StringUtils.left(pubdates.strStream().findFirst().orElse(""), 4);

    return ((year.length() == 4) && StringUtils.isNumeric(year)) ? parseInt(year, 0) : 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
