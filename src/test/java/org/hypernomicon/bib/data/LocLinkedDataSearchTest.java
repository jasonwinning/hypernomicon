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

import static org.junit.jupiter.api.Assertions.*;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.MarcRecord.*;

import java.net.URI;
import java.net.http.*;
import java.time.Duration;
import java.util.*;

import org.jsoup.Jsoup;
import org.jsoup.parser.Parser;

import org.json.simple.parser.ParseException;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import org.hypernomicon.bib.data.LocLinkedDataSearch.Hit;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Tests for searching the Library of Congress catalog through its Linked Data Service: the
 * search URLs, the reading of the hits, and which hit's record is fetched. The hits are real
 * ones from id.loc.gov, trimmed to the fields that are read.
 */
class LocLinkedDataSearchTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The first hits for the phrase "mind and world", with no author */
  private static final String MIND_AND_WORLD_HITS = """
    {"hits": [
      {"uri": "http://id.loc.gov/resources/works/22448678", "aLabel": "Kelsey, Sean, Ph.D. Mind and world in Aristotle's De anima",
       "more": {"contributors": ["Kelsey, Sean, Ph.D."], "pubdates": ["2022"], "instance": "http://id.loc.gov/resources/instances/22448678"}},
      {"uri": "http://id.loc.gov/resources/works/13896132", "aLabel": "Dingli, Sandra M., 1952-. On thinking and the world : John McDowell's Mind and world",
       "more": {"contributors": ["Dingli, Sandra M., 1952-"], "pubdates": ["2005"], "instance": "http://id.loc.gov/resources/instances/13896132"}},
      {"uri": "http://id.loc.gov/resources/works/636766", "aLabel": "McDowell, John Henry. Mind and world",
       "more": {"contributors": ["McDowell, John Henry"], "pubdates": ["1994"], "instance": "http://id.loc.gov/resources/instances/636766"}},
      {"uri": "http://id.loc.gov/resources/works/12832139", "aLabel": "Reading McDowell : on Mind and world",
       "more": {"contributors": ["Smith, Nicholas H. (Nicholas Hugh), 1962-"], "pubdates": ["2002"], "instance": "http://id.loc.gov/resources/instances/12832139"}},
      {"uri": "http://id.loc.gov/resources/works/690380", "aLabel": "McDowell, John Henry. Mind and world",
       "more": {"contributors": ["McDowell, John Henry"], "pubdates": ["1996"], "instance": "http://id.loc.gov/resources/instances/690380"}}
    ]}
    """;

  /** Some of the hits for the phrase "count of monte cristo" with the surname Dumas, in the order they came in; LibraryOfCongressFallbackTest serves them too */
  static final String MONTE_CRISTO_HITS = """
    {"hits": [
      {"uri": "http://id.loc.gov/resources/works/16113040", "aLabel": "Kelly, Karen, 1962-. Alexandre Dumas's The Count of Monte Cristo",
       "more": {"contributors": ["Kelly, Karen, 1962-", "Fisher, Eric Scott"], "pubdates": ["2011"], "instance": "http://id.loc.gov/resources/instances/16113040"}},
      {"uri": "http://id.loc.gov/resources/works/8214748", "aLabel": "The wife of Monte-Cristo. Being the continuation of Alexander Dumas' celebrated novel of The Count of Monte Cristo",
       "more": {"pubdates": ["1884/9999"], "instance": "http://id.loc.gov/resources/instances/8214748"}},
      {"uri": "http://id.loc.gov/resources/works/22357924", "aLabel": "Dumas, Alexandre, 1802-1870. The Count of Monte Cristo ..",
       "more": {"contributors": ["Dumas, Alexandre, 1802-1870"], "pubdates": ["1895"], "instance": "http://id.loc.gov/resources/instances/22357924"}},
      {"uri": "http://id.loc.gov/resources/works/3078640", "aLabel": "Dumas, Alexandre, 1802-1870. The Count of Monte Cristo",
       "more": {"contributors": ["Dumas, Alexandre, 1802-1870", "Buss, Robin"], "pubdates": ["1996"], "instance": "http://id.loc.gov/resources/instances/3078640"}},
      {"uri": "http://id.loc.gov/resources/works/in00024333360", "aLabel": "Dumas, Alexandre, 1802-1870. The Count of Monte Cristo",
       "more": {"contributors": ["Dumas, Alexandre, 1802-1870", "Martone, Eric"], "pubdates": ["2026"], "instance": "http://id.loc.gov/resources/instances/in00024333360"}},
      {"uri": "http://id.loc.gov/resources/works/13234345", "aLabel": "Dumas, Alexandre, 1802-1870. The Count of Monte Cristo",
       "more": {"contributors": ["Dumas, Alexandre, 1802-1870", "Buss, Robin"], "pubdates": ["2003"], "instance": "http://id.loc.gov/resources/instances/13234345"}}
    ]}
    """;

  /** The one hit for ISBN 9780140449266 among the instances; LibraryOfCongressFallbackTest serves it too */
  static final String MONTE_CRISTO_ISBN_HITS = """
    {"hits": [
      {"uri": "http://id.loc.gov/resources/instances/13234345",
       "aLabel": "Dumas, Alexandre, 1802-1870 The Count of Monte Cristo London, England; New York, N.Y.: Penguin Books, c2003",
       "more": {"identifiers": ["2003270054", "2003270054"]}}
    ]}
    """;

//---------------------------------------------------------------------------

  private static List<Hit> hitsOf(String response) throws ParseException { return LocLinkedDataSearch.hitsIn(JsonObj.parseJsonObj(response)); }

  private static int yearOf(String pubdates) throws ParseException      { return LocLinkedDataSearch.yearFrom(JsonObj.parseJson(pubdates)); }

  private static String bestTitleHitID(List<Hit> hits, String title, String yearStr, List<String> authKeywords)
  {
    Hit hit = LocLinkedDataSearch.bestHit(hits, title, yearStr, authKeywords, "");

    return hit == null ? null : hit.instanceID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void searchUrls()
  {
    assertEquals("https://id.loc.gov/resources/instances/suggest2/?q=9780140449266&searchtype=keyword&count=1",
                 LocLinkedDataSearch.isbnSearchUrl("978-0-14-044926-6"), "Only the first hit of an ISBN search is used");

    // The title as a phrase, then the surname; a plus sign is a space in a query

    String url = LocLinkedDataSearch.titleSearchUrl("Mind and World", List.of("McDowell"));

    assertTrue(url.startsWith("https://id.loc.gov/resources/works/suggest2/?q=%22mind+and+world%22+mcdowell&searchtype=keyword&"), url);

    url = LocLinkedDataSearch.titleSearchUrl("Mind and World", List.of());

    assertTrue(url.contains("?q=%22mind+and+world%22&"), "With no author, the phrase alone: " + url);

    url = LocLinkedDataSearch.titleSearchUrl("The Phenomenology of Perception", List.of("Merleau-Ponty"));

    assertTrue(url.contains("?q=%22phenomenology+of+perception%22+merleau+ponty&"), "A leading article, which the label may lack, is left out: " + url);

    assertEquals("https://id.loc.gov/resources/instances/13234345.marcxml.xml", LocLinkedDataSearch.recordUrl("13234345"));
  }

//---------------------------------------------------------------------------

  @Test
  void keywords()
  {
    assertEquals("kant's critique 2nd ed"         , LocLinkedDataSearch.keywordsOf("Kant's \"Critique\" (2nd ed.)"), "A quote would end the phrase early");
    assertEquals("knowing and being essays"       , LocLinkedDataSearch.keywordsOf("Knowing and Being: Essays"));
    assertEquals("merleau ponty"                  , LocLinkedDataSearch.keywordsOf("Merleau-Ponty"));
    assertEquals("phenomenologie de la perception", LocLinkedDataSearch.keywordsOf("Phénoménologie de la perception"));
    assertEquals(""                               , LocLinkedDataSearch.keywordsOf(null));
  }

//---------------------------------------------------------------------------

  @Test
  void hitsOfAWorkSearch() throws ParseException
  {
    List<Hit> hits = hitsOf(MONTE_CRISTO_HITS);

    assertEquals(List.of("16113040", "8214748", "22357924", "3078640", "in00024333360", "13234345"), hits.stream().map(Hit::instanceID).toList(),
                 "Each work's instance, in the order the service ranked them");

    Hit kelly = hits.getFirst(), wife = hits.get(1), dumas1895 = hits.get(2);

    assertEquals("Alexandre Dumas's The Count of Monte Cristo", kelly.title(), "The main entry's name and the period after it are not part of the title");
    assertEquals(List.of("Kelly", "Fisher"), kelly.lastNames());
    assertEquals(2011, kelly.year());

    assertTrue(wife.title().startsWith("The wife of Monte-Cristo. Being the continuation"), "A work with no main entry has its whole label for a title");
    assertEquals(List.of(), wife.lastNames());
    assertEquals(1884, wife.year(), "A range of dates begins with its year");

    assertEquals("The Count of Monte Cristo ..", dumas1895.title());
    assertEquals(List.of("Dumas", "Buss"), hits.getLast().lastNames(), "A translator is among the contributors too");

    // An instance's hit is the instance itself

    assertEquals(List.of("13234345"), hitsOf(MONTE_CRISTO_ISBN_HITS).stream().map(Hit::instanceID).toList());
  }

//---------------------------------------------------------------------------

  /** A hit is skipped unless it leads to an instance whose identifier can safely go into a URL */
  @Test
  void hitsWithoutAUsableInstanceAreSkipped() throws ParseException
  {
    String response = """
      {"hits": [
        {"uri": "http://id.loc.gov/resources/works/1", "aLabel": "A work hit that names no instance"},
        {"uri": "http://id.loc.gov/resources/works/2", "aLabel": "An empty identifier", "more": {"instance": "http://id.loc.gov/resources/instances/"}},
        {"uri": "http://id.loc.gov/resources/works/3", "aLabel": "A query in the identifier", "more": {"instance": "http://id.loc.gov/resources/instances/3?x=1"}},
        {"uri": "http://id.loc.gov/resources/works/4", "aLabel": "Fine", "more": {"instance": "http://id.loc.gov/resources/instances/4"}}
      ]}
      """;

    assertEquals(List.of("4"), hitsOf(response).stream().map(Hit::instanceID).toList());
  }

//---------------------------------------------------------------------------

  @Test
  void titlesFromLabels()
  {
    assertEquals("Mind and world", LocLinkedDataSearch.titleFromLabel("McDowell, John Henry. Mind and world", List.of("McDowell, John Henry")));

    assertEquals("The count of Monte Cristo ..", LocLinkedDataSearch.titleFromLabel("Dumas, Alexandre, 1802-1870 The count of Monte Cristo ..", List.of("Dumas, Alexandre, 1802-1870")),
                 "Some labels have no period after the name");

    assertEquals("Reading McDowell : on Mind and world", LocLinkedDataSearch.titleFromLabel("Reading McDowell : on Mind and world", List.of("Smith, Nicholas H. (Nicholas Hugh), 1962-")),
                 "An edited volume is entered under its title; its first contributor's name is not in the label");

    assertEquals("The wife of Monte-Cristo", LocLinkedDataSearch.titleFromLabel("The wife of Monte-Cristo", List.of()));
  }

//---------------------------------------------------------------------------

  @Test
  void yearOfPublication() throws ParseException
  {
    assertEquals(1994, yearOf("[\"1994\"]"));
    assertEquals(1884, yearOf("[\"1884/9999\"]"));
    assertEquals(1910, yearOf("[\"1910~/1927~\"]"));
    assertEquals(0   , yearOf("[\"198X\"]"), "An uncertain year is no year to match");
    assertEquals(0   , yearOf("[\"XXXX\"]"));
    assertEquals(0   , yearOf("[]"));
  }

//---------------------------------------------------------------------------

  /** The hits are ranked as the SRU server's records are; searched for with no author, "Mind and World" is McDowell's, not a longer title beginning with it */
  @Test
  void anExactTitleOutranksALongerOne() throws ParseException
  {
    List<Hit> hits = hitsOf(MIND_AND_WORLD_HITS);

    assertEquals("636766"  , bestTitleHitID(hits, "Mind and World", ""    , List.of()), "The first of the two exact titles, in the service's order");
    assertEquals("690380"  , bestTitleHitID(hits, "Mind and World", "1996", List.of()), "The year picks the edition");
    assertEquals("22448678", bestTitleHitID(hits, "Mind and World in Aristotle's De Anima", "", List.of()));

    assertNull(bestTitleHitID(hits, "Mind, Language and Reality", "", List.of()), "No title is close enough");
  }

//---------------------------------------------------------------------------

  @Test
  void authorAndYearPickTheEdition() throws ParseException
  {
    List<Hit> hits = hitsOf(MONTE_CRISTO_HITS);

    assertEquals("13234345", bestTitleHitID(hits, "The Count of Monte Cristo", "2003", List.of("Dumas")));
    assertEquals("3078640" , bestTitleHitID(hits, "The Count of Monte Cristo", ""    , List.of("Dumas")), "With no year, the closest title, which the 1895 edition's trailing dots make another");
    assertEquals("13234345", bestTitleHitID(hits, "The Count of Monte Cristo", "2003", List.of("Buss" )), "A translator's surname counts as the author's does");
  }

//---------------------------------------------------------------------------

  @Test
  void anIsbnSearchTakesItsFirstHit() throws ParseException
  {
    assertEquals("13234345", LocLinkedDataSearch.bestHit(hitsOf(MONTE_CRISTO_ISBN_HITS), null, "", List.of(), "9780140449266").instanceID());

    assertNull(LocLinkedDataSearch.bestHit(List.of(), null, "", List.of(), "9780140449266"));
  }

//---------------------------------------------------------------------------

  /**
   * Exercises an ISBN lookup through the live service, both requests of it. Enable with
   * {@code HN_ONLINE_TESTS=true}, like the live tests of the SRU server.
   */
  @Test
  @EnabledIfEnvironmentVariable(named = "HN_ONLINE_TESTS", matches = "true")
  void liveLinkedDataQueryTest() throws Exception
  {
    try (HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(15)).build())
    {
      HttpResponse<String> response = client.send(HttpRequest.newBuilder().uri(URI.create(LocLinkedDataSearch.isbnSearchUrl("9780140449266"))).timeout(Duration.ofSeconds(15)).GET().build(),
                                                   HttpResponse.BodyHandlers.ofString());

      assertEquals(200, response.statusCode());

      Hit hit = LocLinkedDataSearch.bestHit(hitsOf(response.body()), null, "", List.of(), "9780140449266");

      assertNotNull(hit, "Live search for a known-held ISBN found nothing");

      response = client.send(HttpRequest.newBuilder().uri(URI.create(LocLinkedDataSearch.recordUrl(hit.instanceID()))).timeout(Duration.ofSeconds(15)).GET().build(),
                             HttpResponse.BodyHandlers.ofString());

      assertEquals(200, response.statusCode());

      LibraryOfCongressBibData bd = LibraryOfCongressBibData.bestMatch(marcRecordsIn(Jsoup.parse(response.body(), "", Parser.xmlParser())), null, "", List.of(), "9780140449266");

      assertNotNull(bd, "The record for a known-held ISBN had no MARCXML");
      assertEquals("The Count of Monte Cristo", bd.getStr(bfTitle));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
