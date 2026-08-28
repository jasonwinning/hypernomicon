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
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.util.StringUtil.*;

import java.net.URI;
import java.net.http.*;
import java.time.Duration;
import java.util.*;

import org.apache.commons.lang3.StringUtils;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.parser.Parser;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.util.http.AsyncHttpClient;

//---------------------------------------------------------------------------

/**
 * Tests for parsing MARCXML records returned by the Library of Congress SRU server.
 * <p>
 * The fixtures named for a real book are real records from {@code lx2.loc.gov:210/lcdb}, trimmed
 * only of fields irrelevant to the assertions (subjects, classification, local holdings, and so
 * on). The rest are synthetic, each built to exercise one record shape, and say so.
 * </p>
 */
class LibraryOfCongressTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Wraps MARCXML records in the SRU envelope the server actually returns. */
  private static Document sruResponse(String... marcRecords)
  {
    StringBuilder sb = new StringBuilder("<?xml version=\"1.0\"?><zs:searchRetrieveResponse xmlns:zs=\"http://www.loc.gov/zing/srw/\">")
      .append("<zs:version>1.1</zs:version><zs:numberOfRecords>").append(marcRecords.length).append("</zs:numberOfRecords><zs:records>");

    for (String record : marcRecords)
      sb.append("<zs:record><zs:recordSchema>marcxml</zs:recordSchema><zs:recordData>").append(record).append("</zs:recordData></zs:record>");

    return Jsoup.parse(sb.append("</zs:records></zs:searchRetrieveResponse>").toString(), "", Parser.xmlParser());
  }

//---------------------------------------------------------------------------

  private static void getLists(BibAuthors authors, List<Author> authorList, List<Author> editorList, List<Author> translatorList)
  {
    authorList    .clear();
    editorList    .clear();
    translatorList.clear();

    authors.forEach(author ->
    {
      if (author.getIsEditor())
      {
        editorList.add(author);
        if (author.getIsTrans())
          translatorList.add(author);
      }
      else if (author.getIsTrans())
        translatorList.add(author);
      else
        authorList.add(author);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A 2003 record: a translation (041 $h is the original language), a copyright date in 260 $c, and no relator on the translator. */
  private static final String MONTE_CRISTO = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01681cam a2200421 a 4500</leader>
      <controlfield tag="008">030611s2003    enk           000 1 eng  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">   2003270054</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0140449264</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9780140449266</subfield></datafield>
      <datafield tag="041" ind1="1" ind2=" "><subfield code="a">eng</subfield><subfield code="h">fre</subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Dumas, Alexandre,</subfield><subfield code="d">1802-1870.</subfield></datafield>
      <datafield tag="240" ind1="1" ind2="0"><subfield code="a">Comte de Monte-Cristo.</subfield><subfield code="l">English</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="4"><subfield code="a">The Count of Monte Cristo /</subfield><subfield code="c">Alexandre Dumas (père) ; translated and with an introduction and notes by Robin Buss.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">London, England ;</subfield><subfield code="a">New York, N.Y. :</subfield><subfield code="b">Penguin Books,</subfield><subfield code="c">c2003.</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">xxviii, 1276 p. ;</subfield><subfield code="c">20 cm.</subfield></datafield>
      <datafield tag="440" ind1=" " ind2="0"><subfield code="a">Penguin classics</subfield></datafield>
      <datafield tag="655" ind1=" " ind2="7"><subfield code="a">Historical fiction.</subfield><subfield code="2">gsafd</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Buss, Robin.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void monteCristoTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(MONTE_CRISTO), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    // The article is part of the title in MARC, and the uniform title (240) is ignored
    assertEquals("The Count of Monte Cristo", bd.getStr(bfTitle));

    assertEquals("Penguin Books", bd.getStr(bfPublisher));

    // The place as transcribed in 260 $a, never the country code in 008 ("enk")
    assertEquals("London, England", bd.getStr(bfPubLoc));

    // ISO 639-2/B is mapped down to 639-1 so it does not conflict with Crossref's "en".
    // The original language of the translation (041 $h) is not this item's.
    assertEquals("en", bd.getStr(bfLanguage));

    // "c2003" in 260 $c is a copyright date; the coded date in 008 is the one that should win
    assertEquals(2003, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC());

    assertEquals(List.of("0140449264", "9780140449266"), bd.getMultiStr(bfISBNs));

    // The physical extent is not a page range, and a series is not a container title
    assertEquals("", bd.getStr(bfPages));
    assertEquals("", bd.getStr(bfContainerTitle));

    String misc = bd.getStr(bfMisc);
    assertTrue(misc.contains("xxviii, 1276 p. ; 20 cm."), "Extent belongs in bfMisc: " + misc);
    assertTrue(misc.contains("Series: Penguin classics"), "Series belongs in bfMisc: " + misc);
    assertTrue(misc.contains("https://lccn.loc.gov/2003270054"), "LCCN permalink belongs in bfMisc: " + misc);

    assertEquals(etBook, bd.getEntryType());

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    // The trailing comma on "Dumas, Alexandre," must not end up in the first name, and the
    // dates subfield ($d) must not end up in the name at all.
    assertEquals(1, authorList.size());
    assertEquals("Dumas, Alexandre", authorList.getFirst().getName().getLastFirst());

    // This record has no relator anywhere; Buss is identifiable as a translator only from
    // the statement of responsibility.
    assertEquals(1, translatorList.size());
    assertEquals("Buss, Robin", translatorList.getFirst().getName().getLastFirst());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** An edited volume with no main entry (no 100) and no relator on the editor; 008/24 flags "b", bibliographies. */
  private static final String CAMBRIDGE_COMPANION = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01500pam a2200385 a 4500</leader>
      <controlfield tag="008">910528s1992    enk      b    001 0 eng  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">   91021231 </subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0521365872</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0521367689</subfield><subfield code="q">pbk.</subfield></datafield>
      <datafield tag="245" ind1="0" ind2="4"><subfield code="a">The Cambridge companion to Kant /</subfield><subfield code="c">edited by Paul Guyer.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Cambridge ;</subfield><subfield code="a">New York :</subfield><subfield code="b">Cambridge University Press,</subfield><subfield code="c">1992.</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">xii, 482 p. ;</subfield><subfield code="c">23 cm.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Guyer, Paul,</subfield><subfield code="d">1948-</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void editorFromStatementOfResponsibilityTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(CAMBRIDGE_COMPANION), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    // No relator on this record at all; "edited by Paul Guyer." is the only signal
    assertEquals(0, authorList.size());
    assertEquals(1, editorList.size());
    assertEquals("Guyer, Paul", editorList.getFirst().getName().getLastFirst());

    // Editors and no plain authors makes this an edited book rather than a plain book
    assertEquals(etEditedBook, bd.getEntryType());

    // The bibliographies code among the 008 nature-of-contents codes means the item *contains*
    // bibliographical references. It must not turn this into a reference book.
    assertNotEquals(etReferenceBook, bd.getEntryType());

    assertEquals(1992, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC());
    assertEquals("Cambridge University Press", bd.getStr(bfPublisher));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A current RDA record, as LoC catalogs books today: relators present but incomplete (Guyer
   * carries only "editor" although the statement of responsibility says "translated and edited
   * by"; Wood is coded "author"), each relator given as text ($e), code and URI ($4), the
   * publication statement in 264, and an ISBN recorded as cancelled ($z).
   */
  private static final String CRITIQUE_OF_PURE_REASON = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>03268cam a22005058i 4500</leader>
      <controlfield tag="008">250825s2026    enk      b    001 0 eng  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">  2025039618</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9781009600064</subfield><subfield code="q">hardback</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9781009600057</subfield><subfield code="q">paperback</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="z">9781009600040</subfield><subfield code="q">ebook</subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Kant, Immanuel,</subfield><subfield code="d">1724-1804</subfield><subfield code="e">author</subfield><subfield code="4">aut</subfield><subfield code="4">http://id.loc.gov/vocabulary/relators/aut</subfield></datafield>
      <datafield tag="240" ind1="1" ind2="0"><subfield code="a">Kritik der praktischen Vernunft.</subfield><subfield code="l">English</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Critique of pure reason /</subfield><subfield code="c">translated and edited by Paul Guyer, University of Pennsylvania and Brown University, emeritus, Allen W. Wood, Indiana University and Stanford University, emeritus.</subfield></datafield>
      <datafield tag="250" ind1=" " ind2=" "><subfield code="a">Second edition, revised edition.</subfield></datafield>
      <datafield tag="264" ind1=" " ind2="1"><subfield code="a">Cambridge :</subfield><subfield code="b">Cambridge University Press,</subfield><subfield code="c">2025.</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">pages cm.</subfield></datafield>
      <datafield tag="490" ind1="0" ind2=" "><subfield code="a">The Cambridge edition of the works of Immanuel Kant</subfield></datafield>
      <datafield tag="504" ind1=" " ind2=" "><subfield code="a">Includes bibliographical references and index.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Guyer, Paul,</subfield><subfield code="d">1948-</subfield><subfield code="e">editor</subfield><subfield code="4">http://id.loc.gov/vocabulary/relators/edt</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Wood, Allen W.</subfield><subfield code="e">author</subfield><subfield code="4">http://id.loc.gov/vocabulary/relators/aut</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void critiqueOfPureReasonTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(CRITIQUE_OF_PURE_REASON), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertAll
    (
      () -> assertEquals("Critique of pure reason", bd.getStr(bfTitle)),

      // The record's dates disagree: the coded date in 008 says 2026, the transcribed
      // publication statement in 264 says 2025. The coded one wins, as the normalized value.
      () -> assertEquals(2026, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC()),

      () -> assertEquals("Cambridge University Press", bd.getStr(bfPublisher)),
      () -> assertEquals("Cambridge", bd.getStr(bfPubLoc)),
      () -> assertEquals("en", bd.getStr(bfLanguage)),
      () -> assertEquals("", bd.getStr(bfContainerTitle), "A series is not a container"),

      // The edition comes from its own field (250), not from the publication statement
      () -> assertTrue(bd.getStr(bfEdition).startsWith("Second edition, revised edition"), "edition=[" + bd.getStr(bfEdition) + ']'),

      // An ISBN in the cancelled-number subfield ($z) is cancelled or misprinted and must not be recorded
      () -> assertEquals(List.of("9781009600064", "9781009600057"), bd.getMultiStr(bfISBNs)),

      // Kant is the author; the statement of responsibility says both Guyer and Wood translated
      // and edited, which the relators only partly reflect
      () -> assertEquals(List.of("Kant, Immanuel"), authorList.stream().map(author -> author.getName().getLastFirst()).toList()),
      () -> assertEquals(List.of("Guyer, Paul", "Wood, Allen W."), editorList.stream().map(author -> author.getName().getLastFirst()).toList()),
      () -> assertEquals(List.of("Guyer, Paul", "Wood, Allen W."), translatorList.stream().map(author -> author.getName().getLastFirst()).toList()),

      // A translated and edited edition of Kant is a book by Kant, not an edited collection
      () -> assertEquals(etBook, bd.getEntryType())
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A 1969 record cataloged under older rules: the second editor carries the legacy text
   * relator "joint author", the first has none at all, and only the statement of
   * responsibility says they are editors. The record also carries Quine's Word and Object as a
   * subject (600) and as a name-title added entry (700 with $t), neither of which is a
   * contributor or a title of this work.
   */
  private static final String WORDS_AND_OBJECTIONS = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01319cam a2200361   4500</leader>
      <controlfield tag="008">701015r1969    ne       b    000 0 eng  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">   79495176 </subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Davidson, Donald,</subfield><subfield code="d">1917-2003.</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Words and objections.</subfield><subfield code="b">Essays on the work of W. V. Quine.</subfield><subfield code="c">Edited by Donald Davidson and Jaakko Hintikka.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Dordrecht,</subfield><subfield code="b">D. Reidel</subfield><subfield code="c">[1969]</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">vii, 366 p.</subfield><subfield code="c">23 cm.</subfield></datafield>
      <datafield tag="490" ind1="0" ind2=" "><subfield code="a">Synthese library</subfield></datafield>
      <datafield tag="500" ind1=" " ind2=" "><subfield code="a">First appeared in Synthese, vol. 19, nos. 1-2.</subfield></datafield>
      <datafield tag="600" ind1="1" ind2="0"><subfield code="a">Quine, W. V.</subfield><subfield code="q">(Willard Van Orman).</subfield><subfield code="t">Word and object.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Hintikka, Jaakko,</subfield><subfield code="d">1929-</subfield><subfield code="e">joint author.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Quine, W. V.</subfield><subfield code="q">(Willard Van Orman).</subfield><subfield code="t">Word and object.</subfield></datafield>
      <datafield tag="730" ind1="0" ind2=" "><subfield code="a">Synthese.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void wordsAndObjectionsTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(WORDS_AND_OBJECTIONS), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertAll
    (
      () -> assertTrue(bd.getStr(bfTitle).startsWith("Words and objections"), "title=[" + bd.getStr(bfTitle) + ']'),
      () -> assertTrue(bd.getStr(bfTitle).contains("Essays on the work of W. V. Quine"), "subtitle missing: [" + bd.getStr(bfTitle) + ']'),

      // "joint author" is a legacy text role and must not get Hintikka dropped as unrecordable;
      // the statement of responsibility then makes both of them editors
      () -> assertEquals(List.of(), authorList.stream().map(author -> author.getName().getLastFirst()).toList()),
      () -> assertEquals(List.of("Davidson, Donald", "Hintikka, Jaakko"), editorList.stream().map(author -> author.getName().getLastFirst()).toList()),
      () -> assertEquals(etEditedBook, bd.getEntryType()),

      // The name and title nested in the subject entry are not this work's
      () -> assertTrue(bd.getAuthors().stream().noneMatch(author -> "Quine".equals(author.getName().getLast()))),
      () -> assertFalse(bd.getStr(bfTitle).contains("Word and object")),

      () -> assertEquals(1969, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC()),
      () -> assertEquals("D. Reidel", bd.getStr(bfPublisher)),
      () -> assertEquals("Dordrecht", bd.getStr(bfPubLoc)),
      () -> assertTrue(bd.getStr(bfMisc).contains("Series: Synthese library"))
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The authorized heading for an initials-only name carries the fuller form in its own subfield, $q, under current rules. */
  private static final String WORD_AND_OBJECT = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01448cam a2200349 a 4500</leader>
      <controlfield tag="008">120709s2013    mau      b    001 0 eng  </controlfield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9780262518314</subfield><subfield code="q">pbk. : alk. paper</subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Quine, W. V.</subfield><subfield code="q">(Willard Van Orman)</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Word and object /</subfield><subfield code="c">Willard Van Orman Quine ; foreword by Patricia Smith Churchland ; preface to the new edition by Dagfinn Føllesdal.</subfield></datafield>
      <datafield tag="250" ind1=" " ind2=" "><subfield code="a">New ed.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Cambridge, Mass. :</subfield><subfield code="b">MIT Press,</subfield><subfield code="c">c2013.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void parentheticalFullerFormTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(WORD_AND_OBJECT), "", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals(1, bd.getAuthors().stream().count());

    Author author = bd.getAuthors().iterator().next();

    assertEquals("Quine", author.getName().getLast());
    assertEquals("W. V.", author.getName().getFirst(), "The fuller form in $q is a qualifier, not part of the name; the final initial keeps its period");
    assertEquals("New ed.", bd.getStr(bfEdition));

    // Older and copy-cataloged records fold the fuller form into $a, with ISBD punctuation after it
    bd = LibraryOfCongressBibData.createFromXml(sruResponse(titledRecord("", "Word and object", "Quine, W. V. (Willard Van Orman).", "1960")), "", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals("Quine, W. V.", bd.getAuthors().iterator().next().getName().getLastFirst());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A classical text in translation: a single-word name (100 with first indicator 0) with ISBD punctuation, and a translator identifiable only from the statement of responsibility. */
  private static final String REPUBLIC = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01651cam a22004094a 4500</leader>
      <controlfield tag="008">040607s2004    inu      b    001 0 eng  </controlfield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0872207366</subfield><subfield code="q">pbk.</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9780872207363</subfield><subfield code="q">(pbk.)</subfield></datafield>
      <datafield tag="041" ind1="1" ind2=" "><subfield code="a">eng</subfield><subfield code="h">grc</subfield></datafield>
      <datafield tag="100" ind1="0" ind2=" "><subfield code="a">Plato.</subfield></datafield>
      <datafield tag="240" ind1="1" ind2="0"><subfield code="a">Republic.</subfield><subfield code="l">English</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Republic /</subfield><subfield code="c">translated from the new standard Greek text, with introduction, by C.D.C. Reeve.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Indianapolis :</subfield><subfield code="b">Hackett Pub. Co.,</subfield><subfield code="c">c2004.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Reeve, C. D. C.,</subfield><subfield code="d">1948-</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void classicalTextTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(REPUBLIC), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertAll
    (
      () -> assertEquals("Republic", bd.getStr(bfTitle)),

      // "Plato." is a single-word name; the ISBD period must go and nothing must be mistaken for a first name
      () -> assertEquals(1, authorList.size()),
      () -> assertEquals("Plato", authorList.getFirst().getName().getLast()),
      () -> assertEquals("", authorList.getFirst().getName().getFirst()),

      // The translator has no role term; "by C.D.C. Reeve" in the statement of responsibility is
      // matched on the last name, so the unspaced initials there do not matter
      () -> assertEquals(List.of("Reeve, C. D. C."), translatorList.stream().map(author -> author.getName().getLastFirst()).toList()),
      () -> assertEquals(0, editorList.size()),

      () -> assertEquals(etBook, bd.getEntryType()),
      () -> assertEquals(2004, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC()),
      () -> assertEquals("en", bd.getStr(bfLanguage), "The original language of the translation (041 $h) is not this edition's")
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * LoC romanizes non-Latin scripts, and its MARC-8 heritage means the diacritics arrive
   * decomposed: "poe" + combining U+0307, "Mikhai" + combining U+0306, and the modifier prime
   * U+02B9 (a letter, not a combining mark) for a soft sign. The subject (600) is a person too,
   * and must not become an author.
   */
  private static final String BAKHTIN = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01095cam a22003251  4500</leader>
      <controlfield tag="008">800923s1963    ru       b    000 0 rus  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">   68045409 </subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Bakhtin, M. M.</subfield><subfield code="q">(Mikhail Mikhai&#x306;lovich),</subfield><subfield code="d">1895-1975</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Problemy poe&#x307;tiki Dostoevskogo.</subfield></datafield>
      <datafield tag="250" ind1=" " ind2=" "><subfield code="a">Izd. 2., perer. i dop.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Moskva,</subfield><subfield code="b">Sov. pisatel&#x2B9;,</subfield><subfield code="c">1963.</subfield></datafield>
      <datafield tag="600" ind1="1" ind2="0"><subfield code="a">Dostoyevsky, Fyodor,</subfield><subfield code="d">1821-1881</subfield><subfield code="x">Criticism and interpretation.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void romanizedWithCombiningDiacriticsTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(BAKHTIN), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    String title = bd.getStr(bfTitle);

    assertAll
    (
      // Composed (U+0117, the letter keyboards and every other source produce) so the stored text compares
      // equal to a visually identical one; the English-chars matching form drops the mark altogether
      () -> assertEquals("Problemy poėtiki Dostoevskogo", title),
      () -> assertEquals("Problemy poetiki Dostoevskogo", convertToEnglishChars(title)),

      () -> assertEquals(1, bd.getAuthors().stream().count()),
      () -> assertEquals("Bakhtin, M. M.", bd.getAuthors().iterator().next().getName().getLastFirst()),

      () -> assertEquals("Sov. pisatelʹ", bd.getStr(bfPublisher)),
      () -> assertEquals("ru", bd.getStr(bfLanguage)),
      () -> assertEquals(1963, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC())
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Volume 1 of Taylor's Philosophical papers. LoC catalogs each volume of a multi-volume work
   * as its own record, whose title is the volume's own; the work's place in the set is the
   * volume number of the series statement. The authorized form of the series is an 800 here
   * (a personal-name series entry), not an 830, so the statement is the only series field read.
   */
  private static final String HUMAN_AGENCY_AND_LANGUAGE = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01474cam a2200397 a 4500</leader>
      <controlfield tag="008">840716s1985    enk      b    001 0 eng  </controlfield>
      <datafield tag="010" ind1=" " ind2=" "><subfield code="a">   84016966 </subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0521267528</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">0521317509</subfield><subfield code="q">pbk.</subfield></datafield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Taylor, Charles,</subfield><subfield code="d">1931-</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Human agency and language /</subfield><subfield code="c">Charles Taylor.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Cambridge [Cambridgeshire] ;</subfield><subfield code="a">New York :</subfield><subfield code="b">Cambridge University Press,</subfield><subfield code="c">1985.</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">viii, 294 p. ;</subfield><subfield code="c">24 cm.</subfield></datafield>
      <datafield tag="490" ind1="1" ind2=" "><subfield code="a">Philosophical papers ;</subfield><subfield code="v">1</subfield></datafield>
      <datafield tag="800" ind1="1" ind2=" "><subfield code="a">Taylor, Charles,</subfield><subfield code="d">1931-</subfield><subfield code="t">Philosophical papers ;</subfield><subfield code="v">1.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void seriesVolumeTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(HUMAN_AGENCY_AND_LANGUAGE), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    assertAll
    (
      () -> assertEquals("Human agency and language", bd.getStr(bfTitle), "The volume's own title, with no series number worked into it"),
      () -> assertTrue(bd.getMultiStr(bfMisc).contains("Series: Philosophical papers ; 1"), "The volume number stays with the series: " + bd.getMultiStr(bfMisc)),
      () -> assertEquals(1, bd.getAuthors().stream().count(), "The series entry's name is not a second author")
    );
  }

//---------------------------------------------------------------------------

  /**
   * The first indicator of a name field says what kind of heading it is; 3 is a family, which
   * is no more a person than a corporate body is. The person in the added entry is still read.
   */
  @Test
  void familyNameHeadingIsNotAnAuthor()
  {
    String record = """
      <record xmlns="http://www.loc.gov/MARC21/slim">
        <leader>00000cam a2200000 a 4500</leader>
        <controlfield tag="008">000000s1990    xx            000 0 eng  </controlfield>
        <datafield tag="100" ind1="3" ind2=" "><subfield code="a">Medici family.</subfield></datafield>
        <datafield tag="245" ind1="1" ind2="4"><subfield code="a">The Medici archive /</subfield><subfield code="c">edited by Ann Smith.</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Smith, Ann,</subfield><subfield code="e">editor.</subfield></datafield>
      </record>
      """;

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<String> names = bd.getAuthors().stream().map(author -> author.getName().getLastFirst()).toList();

    assertEquals(List.of("Smith, Ann"), names);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Additional record shapes inspired by ones Zotero's MARCXML translator is tested against:
  // a sound recording whose only name is a performer, a computer file with an open
  // date range and a corporate body, a German national library record in RDA style (no ISBD
  // punctuation, German relator terms beside the codes, prices in the ISBN field), a record
  // derived from a web feed with a short leader, blank indicators, and a non-bibliographic
  // date, and a dataset whose only name is uncontrolled. Where Zotero's own expectations are
  // questionable (a performer or a data collector as an editor, "17" as a date), these pin the
  // reading we want instead.

  private static final String SOUND_RECORDING = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00925cjm a2200277 a 4500</leader>
      <controlfield tag="008">590817s1959    nyu           000 0 eng  </controlfield>
      <datafield tag="245" ind1="0" ind2="0"><subfield code="a">Kind of blue</subfield><subfield code="h">[sound recording] /</subfield><subfield code="c">Miles Davis.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">New York, N.Y. :</subfield><subfield code="b">Columbia,</subfield><subfield code="c">[1959]</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">1 sound disc :</subfield><subfield code="b">analog, 33 1/3 rpm ;</subfield><subfield code="c">12 in.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Davis, Miles,</subfield><subfield code="d">1926-1991.</subfield><subfield code="4">prf</subfield></datafield>
    </record>
    """;

  private static final String COMPUTER_FILE = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>01832cmm a2200349 a 4500</leader>
      <controlfield tag="008">880601m19889999ilu    g   m        eng d</controlfield>
      <datafield tag="245" ind1="0" ind2="0"><subfield code="a">Mathematica</subfield><subfield code="h">[computer file] :</subfield><subfield code="b">a system for doing mathematics by computer /</subfield><subfield code="c">Wolfram Research.</subfield></datafield>
      <datafield tag="260" ind1=" " ind2=" "><subfield code="a">Champaign, Ill. :</subfield><subfield code="b">Wolfram Research,</subfield><subfield code="c">1988-</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Wolfram Research, Inc.</subfield></datafield>
    </record>
    """;

  private static final String GERMAN_EDITED_VOLUME = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00000pam a2200000 c 4500</leader>
      <controlfield tag="008">130325s2013    gw ||||| |||| 00||||ger  </controlfield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9783518296028</subfield><subfield code="c">Broschur : EUR 20.00 (DE), EUR 20.60 (AT)</subfield><subfield code="9">978-3-518-29602-8</subfield></datafield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">3518296027</subfield><subfield code="9">3-518-29602-7</subfield></datafield>
      <datafield tag="041" ind1=" " ind2=" "><subfield code="a">ger</subfield></datafield>
      <datafield tag="245" ind1="0" ind2="0"><subfield code="a">Philosophie der Verkörperung</subfield><subfield code="b">Grundlagentexte zu einer aktuellen Debatte</subfield><subfield code="c">herausgegeben von Joerg Fingerhut, Rebekka Hufendiek und Markus Wild</subfield></datafield>
      <datafield tag="264" ind1=" " ind2="1"><subfield code="a">Berlin</subfield><subfield code="b">Suhrkamp</subfield><subfield code="c">2013</subfield></datafield>
      <datafield tag="300" ind1=" " ind2=" "><subfield code="a">543 Seiten</subfield><subfield code="c">18 cm</subfield></datafield>
      <datafield tag="490" ind1="0" ind2=" "><subfield code="a">Suhrkamp Taschenbuch Wissenschaft</subfield><subfield code="v">2060</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="0">(DE-588)1030000000</subfield><subfield code="a">Fingerhut, Joerg</subfield><subfield code="d">1978-</subfield><subfield code="e">Herausgeber</subfield><subfield code="4">edt</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Hufendiek, Rebekka</subfield><subfield code="e">Herausgeber</subfield><subfield code="4">edt</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Wild, Markus</subfield><subfield code="d">1971-</subfield><subfield code="e">Herausgeber</subfield><subfield code="4">edt</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Suhrkamp Verlag</subfield><subfield code="4">pbl</subfield></datafield>
    </record>
    """;

  private static final String FEED_DERIVED_RECORD = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>nm 22 uu 4500</leader>
      <controlfield tag="008">s ||||||||||||||||||||||</controlfield>
      <datafield tag="041" ind1="0" ind2="7"><subfield code="a"></subfield><subfield code="2">rfc3066</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Notes toward a digital design system for a library website</subfield></datafield>
      <datafield tag="260" ind1="" ind2=""><subfield code="b">The Journal of Library Software</subfield><subfield code="c">Tue, 05 Mar 2019 10:00:00 +0000</subfield></datafield>
      <datafield tag="700" ind1="1"><subfield code="a">Jane Roe &amp; John Doe</subfield></datafield>
    </record>
    """;

  private static final String UNCONTROLLED_NAME_ONLY = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <datafield tag="245" ind1=" " ind2=" "><subfield code="a">Reading-time measurements for philosophy abstracts</subfield></datafield>
      <datafield tag="720" ind1=" " ind2=" "><subfield code="a">Institute for Text Studies, Inc.</subfield><subfield code="e">Data Collector</subfield><subfield code="7">Organizational</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void performerOnlySoundRecordingTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(SOUND_RECORDING), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    assertAll
    (
      () -> assertEquals(etAudioRecording, bd.getEntryType()),
      () -> assertEquals("Kind of blue", bd.getStr(bfTitle), "The general material designation is not part of the title"),
      () -> assertEquals(0, bd.getAuthors().stream().count(), "A performer is a relator we do not record, and nothing in the statement of responsibility says otherwise"),
      () -> assertEquals("Columbia", bd.getStr(bfPublisher)),
      () -> assertEquals("New York, N.Y.", bd.getStr(bfPubLoc), "The abbreviation keeps its period"),
      () -> assertEquals(1959, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC(), "A bracketed year is still the year")
    );
  }

//---------------------------------------------------------------------------

  @Test
  void computerFileWithCorporateBodyTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(COMPUTER_FILE), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    assertAll
    (
      () -> assertEquals(etSoftware, bd.getEntryType()),
      () -> assertEquals(List.of("Mathematica", "a system for doing mathematics by computer"), bd.getMultiStr(bfTitle)),
      () -> assertEquals(List.of("Wolfram Research, Inc."), bd.getAuthors().stream().map(author -> author.getName().getLast()).toList(), "With no person named, the organization the title page credits is the author, its name kept whole"),
      () -> assertEquals("", bd.getAuthors().iterator().next().getName().getFirst(), "An organization has no first name"),
      () -> assertEquals(1988, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC(), "The start of an open date range")
    );
  }

//---------------------------------------------------------------------------

  @Test
  void germanRdaRecordTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(GERMAN_EDITED_VOLUME), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authors = bd.getAuthors().stream().toList();

    assertAll
    (
      () -> assertEquals(etEditedBook, bd.getEntryType()),
      () -> assertEquals(List.of("Philosophie der Verkörperung", "Grundlagentexte zu einer aktuellen Debatte"), bd.getMultiStr(bfTitle), "No ISBD punctuation to strip, none invented"),
      () -> assertEquals(3, authors.size(), "The publisher's corporate entry is not a contributor"),
      () -> assertTrue(authors.stream().allMatch(Author::getIsEditor), "The relator code decides when the term is in a language normalizeRole does not know"),
      () -> assertEquals(List.of("9783518296028", "3518296027"), bd.getMultiStr(bfISBNs), "The price and the hyphenated copy are not numbers"),
      () -> assertEquals("de", bd.getStr(bfLanguage)),
      () -> assertEquals("Berlin"  , bd.getStr(bfPubLoc)),
      () -> assertEquals("Suhrkamp", bd.getStr(bfPublisher)),
      () -> assertEquals(2013, bd.getDate().year.numericValueWhereMinusOneEqualsOneBC()),
      () -> assertTrue(bd.getMultiStr(bfMisc).contains("Series: Suhrkamp Taschenbuch Wissenschaft 2060"), "" + bd.getMultiStr(bfMisc))
    );
  }

//---------------------------------------------------------------------------

  @Test
  void feedDerivedRecordDoesNotDerailParsing()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(FEED_DERIVED_RECORD), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    int year = bd.getDate().year.numericValueWhereMinusOneEqualsOneBC();

    assertAll
    (
      () -> assertEquals("Notes toward a digital design system for a library website", bd.getStr(bfTitle)),
      () -> assertEquals("The Journal of Library Software", bd.getStr(bfPublisher)),
      () -> assertEquals(2019, year, "The year of a full date, never its day number; got " + bd.getDate()),
      () -> assertEquals("", bd.getStr(bfLanguage), "Fill characters and an empty language code yield no language")
    );
  }

//---------------------------------------------------------------------------

  /**
   * The page LoC serves, with HTTP 200, to an address it has blocked for excessive traffic (as
   * reported by Wikimedia, whose citation service hits the same server at scale). It must be
   * told apart from a genuine empty result and from any other non-SRU page.
   */
  @Test
  void blockPageIsNotAnEmptyResult()
  {
    String blockPage = """
      <!DOCTYPE html>
      <html><head><title>LC Distribution Alert</title></head>
      <body><p>Due to excessive traffic, your access to this system has been blocked.<br></p></body></html>
      """;

    assertTrue (LibraryOfCongressBibData.isAccessBlocked(Jsoup.parse(blockPage, "", Parser.xmlParser())));
    assertFalse(LibraryOfCongressBibData.isAccessBlocked(sruResponse()), "An empty SRU result is a miss, not a block");
    assertFalse(LibraryOfCongressBibData.isAccessBlocked(Jsoup.parse("<html><body>Access denied</body></html>", "", Parser.xmlParser())), "Only LoC's own block page counts; anything else is an ordinary failure");
    assertFalse(LibraryOfCongressBibData.isAccessBlocked(null));
  }

//---------------------------------------------------------------------------

  @Test
  void uncontrolledNameIsNotAnAuthor()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(UNCONTROLLED_NAME_ONLY), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    assertAll
    (
      () -> assertEquals("Reading-time measurements for philosophy abstracts", bd.getStr(bfTitle)),
      () -> assertEquals(0, bd.getAuthors().stream().count(), "An uncontrolled name could be anyone or anything; it never becomes a person"),
      () -> assertEquals(etOther, bd.getEntryType(), "With no leader there is nothing to type the record by")
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A work whose author is an organization: a corporate main entry with the RDA relator, echoed by the statement of responsibility */
  private static final String CORPORATE_MAIN_ENTRY = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00000cam a2200000 i 4500</leader>
      <controlfield tag="008">130514s2013    dcu      b    001 0 eng  </controlfield>
      <datafield tag="110" ind1="2" ind2=" "><subfield code="a">American Psychiatric Association,</subfield><subfield code="e">author.</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Diagnostic and statistical manual of mental disorders :</subfield><subfield code="b">DSM-5 /</subfield><subfield code="c">American Psychiatric Association.</subfield></datafield>
      <datafield tag="264" ind1=" " ind2="1"><subfield code="a">Washington, DC :</subfield><subfield code="b">American Psychiatric Publishing,</subfield><subfield code="c">[2013]</subfield></datafield>
    </record>
    """;

  /** An institute's report: an unmarked corporate added entry that the title page credits, plus a subordinate unit in the heading */
  private static final String CORPORATE_ADDED_ENTRY_CREDITED = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00000cam a2200000 a 4500</leader>
      <controlfield tag="008">990101s1999    cau           000 0 eng  </controlfield>
      <datafield tag="245" ind1="0" ind2="0"><subfield code="a">Situated reasoning :</subfield><subfield code="b">a research report /</subfield><subfield code="c">Center for the Study of Language and Information.</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Stanford University.</subfield><subfield code="b">Center for the Study of Language and Information.</subfield></datafield>
    </record>
    """;

  /** A sponsor and a conference: neither is credited by the title page, and a meeting is never a contributor */
  private static final String SPONSOR_AND_CONFERENCE = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00000cam a2200000 a 4500</leader>
      <controlfield tag="008">750101s1975    nyu           100 0 eng  </controlfield>
      <datafield tag="111" ind1="2" ind2=" "><subfield code="a">International Congress of Logic, Methodology, and Philosophy of Science</subfield><subfield code="n">(5th :</subfield><subfield code="d">1975 :</subfield><subfield code="c">London, Ont.)</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Foundational problems in the special sciences.</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Rockefeller Foundation.</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">University of Western Ontario.</subfield><subfield code="4">spn</subfield></datafield>
    </record>
    """;

  /** A person and an organization both credited: the person is the author and the organization stays out */
  private static final String PERSON_AND_ORGANIZATION = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>00000cam a2200000 a 4500</leader>
      <controlfield tag="008">050101s2005    ilu           000 0 eng  </controlfield>
      <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Roe, Jane.</subfield></datafield>
      <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Symbolic computation in practice /</subfield><subfield code="c">Jane Roe ; Wolfram Research.</subfield></datafield>
      <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Wolfram Research, Inc.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  private static List<String> authorNames(LibraryOfCongressBibData bd)
  {
    return bd.getAuthors().stream().map(author -> author.getName().getLast()).toList();
  }

//---------------------------------------------------------------------------

  @Test
  void organizationIsTheAuthorOnlyWhenNoPersonIs()
  {
    LibraryOfCongressBibData mainEntry = LibraryOfCongressBibData.createFromXml(sruResponse(CORPORATE_MAIN_ENTRY           ), "", "", new ArrayList<>(), ""),
                             credited  = LibraryOfCongressBibData.createFromXml(sruResponse(CORPORATE_ADDED_ENTRY_CREDITED), "", "", new ArrayList<>(), ""),
                             sponsor   = LibraryOfCongressBibData.createFromXml(sruResponse(SPONSOR_AND_CONFERENCE        ), "", "", new ArrayList<>(), ""),
                             withPerson= LibraryOfCongressBibData.createFromXml(sruResponse(PERSON_AND_ORGANIZATION       ), "", "", new ArrayList<>(), "");

    assertAll
    (
      () -> assertEquals(List.of("American Psychiatric Association"), authorNames(mainEntry), "A corporate main entry is the author"),
      () -> assertTrue(mainEntry.getAuthors().iterator().next().getIsAuthor()),
      () -> assertEquals(etBook, mainEntry.getEntryType(), "An organization as author makes an authored book, not an edited one"),

      () -> assertEquals(List.of("Stanford University. Center for the Study of Language and Information"), authorNames(credited), "An unmarked added entry counts when the title page credits it; the subordinate unit is part of the name"),

      () -> assertEquals(List.of(), authorNames(sponsor), "A sponsor the title page does not mention, a sponsor by relator, and a conference are none of them authors"),

      () -> assertEquals(List.of("Roe"), authorNames(withPerson), "Once a person is named, no organization joins the list")
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** One of the McDougall records, in the form LoC's 1920s cataloging gave them: the subtitle in 245 $b after a comma */
  private static String groupMindRecord(String subTitle, String publisher, String year)
  {
    return "<record xmlns=\"http://www.loc.gov/MARC21/slim\"><leader>01067cam a2200313u  4500</leader>" +
           "<controlfield tag=\"008\">820909s" + year + "    nyu           000 0 eng  </controlfield>" +
           "<datafield tag=\"100\" ind1=\"1\" ind2=\" \"><subfield code=\"a\">McDougall, William,</subfield><subfield code=\"d\">1871-1938.</subfield></datafield>" +
           "<datafield tag=\"245\" ind1=\"1\" ind2=\"4\"><subfield code=\"a\">The group mind" + (subTitle.isEmpty() ? "." : ",") + "</subfield>" +
           (subTitle.isEmpty() ? "" : "<subfield code=\"b\">" + subTitle + ",</subfield>") + "</datafield>" +
           "<datafield tag=\"260\" ind1=\" \" ind2=\" \"><subfield code=\"a\">New York,</subfield><subfield code=\"b\">" + publisher + ",</subfield>" +
           "<subfield code=\"c\">" + year + ".</subfield></datafield></record>";
  }

  /**
   * LoC's actual four hits for a title search on "The Group Mind" by McDougall. A work record
   * usually carries the short title, while the catalog has the full subtitle (here a very long
   * one), so the subtitle must not count against a candidate when the main titles agree.
   */
  @Test
  void longSubtitleDoesNotDefeatTitleSearchTest()
  {
    String longSubTitle = "a sketch of the principles of collective psychology, with some attempt to apply them to the interpretation of national life and character";

    // LoC's fourth hit, a 1973 reprint cataloged without any subtitle, is deliberately left out so
    // that every candidate carries a subtitle far longer than the title being searched for
    Document doc = sruResponse(groupMindRecord("a sketch of the principles of collective psychology", "G. P. Putnam's sons", "1920"),
                               groupMindRecord(longSubTitle, "The University Press", "1920"),
                               groupMindRecord(longSubTitle, "The University press", "1927"));

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(doc, "The Group Mind", "", new ArrayList<>(List.of("McDougall")), "");

    assertNotNull(bd, "Every candidate is the right book; one of them must be chosen");

    assertTrue(bd.getStr(bfTitle).startsWith("The group mind: a sketch"), "title=[" + bd.getStr(bfTitle) + ']');
    assertEquals("McDougall, William", bd.getAuthors().iterator().next().getName().getLastFirst());

    // The same search with the full title on the work record must also match
    bd = LibraryOfCongressBibData.createFromXml(doc, "The Group Mind: A Sketch of the Principles of Collective Psychology", "", new ArrayList<>(List.of("McDougall")), "");

    assertNotNull(bd, "A work record carrying the subtitle must match too");
  }

//---------------------------------------------------------------------------

  /**
   * All four of LoC's hits tie on title. Without a year the first one LoC returned wins (the 1973
   * reprint); with the work record's year, the edition from that year does.
   */
  @Test
  void yearSelectsAmongEditionsTest()
  {
    String longSubTitle = "a sketch of the principles of collective psychology, with some attempt to apply them to the interpretation of national life and character";

    Document doc = sruResponse(groupMindRecord("", "Arno Press", "1973"),
                               groupMindRecord("a sketch of the principles of collective psychology", "G. P. Putnam's sons", "1920"),
                               groupMindRecord(longSubTitle, "The University Press", "1920"),
                               groupMindRecord(longSubTitle, "The University press", "1927"));

    List<String> authKeywords = new ArrayList<>(List.of("McDougall"));

    assertEquals(1973, yearOfRecord(LibraryOfCongressBibData.createFromXml(doc, "The Group Mind", ""    , authKeywords, "")));
    assertEquals(1920, yearOfRecord(LibraryOfCongressBibData.createFromXml(doc, "The Group Mind", "1920", authKeywords, "")));
    assertEquals(1927, yearOfRecord(LibraryOfCongressBibData.createFromXml(doc, "The Group Mind", "1927", authKeywords, "")));

    // A year no edition has changes nothing
    assertEquals(1973, yearOfRecord(LibraryOfCongressBibData.createFromXml(doc, "The Group Mind", "1955", authKeywords, "")));

    // The year only breaks ties among acceptable titles; it cannot rescue a wrong one
    assertNull(LibraryOfCongressBibData.createFromXml(doc, "An Introduction to Social Psychology", "1920", authKeywords, ""));
  }

  private static int yearOfRecord(LibraryOfCongressBibData bd)
  {
    assertNotNull(bd);
    return bd.getDate().year.numericValueWhereMinusOneEqualsOneBC();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** One editor with no relator, the other with RDA's "editor of  compilation" (doubled space as delivered). */
  private static final String CAMBRIDGE_HANDBOOK = """
    <record xmlns="http://www.loc.gov/MARC21/slim">
      <leader>03374cam a2200409 i 4500</leader>
      <controlfield tag="008">131209s2014    enka     b    001 0 eng  </controlfield>
      <datafield tag="020" ind1=" " ind2=" "><subfield code="a">9780521871426</subfield><subfield code="q">hardcover</subfield></datafield>
      <datafield tag="245" ind1="0" ind2="4"><subfield code="a">The Cambridge handbook of artificial intelligence /</subfield><subfield code="c">edited by Keith Frankish and William M. Ramsey.</subfield></datafield>
      <datafield tag="246" ind1="3" ind2="0"><subfield code="a">Handbook of artificial intelligence</subfield></datafield>
      <datafield tag="264" ind1=" " ind2="1"><subfield code="a">Cambridge, UK :</subfield><subfield code="b">Cambridge University Press,</subfield><subfield code="c">2014.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Frankish, Keith.</subfield></datafield>
      <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Ramsey, William M.,</subfield><subfield code="d">1960-</subfield><subfield code="e">editor of  compilation.</subfield></datafield>
    </record>
    """;

//---------------------------------------------------------------------------

  @Test
  void rdaRoleTermTest()
  {
    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(CAMBRIDGE_HANDBOOK), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertEquals(List.of(), authorList.stream().map(author -> author.getName().getLastFirst()).toList());
    assertEquals(List.of("Frankish, Keith", "Ramsey, William M."), editorList.stream().map(author -> author.getName().getLastFirst()).toList());
    assertEquals(etEditedBook, bd.getEntryType());
  }

//---------------------------------------------------------------------------

  /**
   * A name whose only role term is one we do not record is normally dropped, but the statement
   * of responsibility can rescue it: LoC keeps adding relator terms, and an unrecognized one on a
   * name the statement calls an editor should not lose the editor.
   */
  @Test
  void statementOfResponsibilityRescuesUnrecognizedRoleTest()
  {
    String record = """
      <record xmlns="http://www.loc.gov/MARC21/slim">
        <leader>00000cam a2200000 a 4500</leader>
        <controlfield tag="008">000000s2000    xx            000 0 eng  </controlfield>
        <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Rescue test /</subfield><subfield code="c">edited by Eve Editor ; illustrations by Ivan Illustrator.</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Editor, Eve</subfield><subfield code="e">some future relator term</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Illustrator, Ivan</subfield><subfield code="e">illustrator</subfield></datafield>
      </record>
      """;

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals(List.of("Editor, Eve"), bd.getAuthors().stream().map(author -> author.getName().getLastFirst()).toList(),
                 "The editor is rescued by the statement; the illustrator, whose statement segment names no recordable role, is still dropped");
    assertTrue(bd.getAuthors().iterator().next().getIsEditor());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A minimal book record: the article, if any, is part of 245 $a with the nonfiling count in the second indicator */
  private static String titledRecord(String article, String title, String author, String year)
  {
    return "<record xmlns=\"http://www.loc.gov/MARC21/slim\"><leader>00000cam a2200000 a 4500</leader>" +
           "<controlfield tag=\"008\">000000s" + year + "    xx            000 0 eng  </controlfield>" +
           "<datafield tag=\"100\" ind1=\"1\" ind2=\" \"><subfield code=\"a\">" + author + "</subfield></datafield>" +
           "<datafield tag=\"245\" ind1=\"1\" ind2=\"" + (article.isEmpty() ? 0 : article.length() + 1) + "\"><subfield code=\"a\">" +
           (article.isEmpty() ? "" : article + ' ') + title + ".</subfield></datafield></record>";
  }

  /**
   * LoC catalogs "and other essays on Jewish spirituality" as part of the title proper, with no
   * subtitle, while the work record has it after a colon. Neither the full title nor the main
   * title alone is within Levenshtein range of the cataloged title, so a record whose title
   * begins with the whole main title must count as a match.
   */
  @Test
  void titleProperExtendingTheMainTitleMatchesTest()
  {
    Document doc = sruResponse(titledRecord("The", "Messianic idea in Judaism and other essays on Jewish spirituality", "Scholem, Gershom,", "1971"));

    String fullTitle = "The Messianic Idea in Judaism: And Other Essays on Jewish Spirituality";

    assertNotNull(LibraryOfCongressBibData.createFromXml(doc, fullTitle, "", new ArrayList<>(List.of("Scholem")), ""), "full title");
    assertNotNull(LibraryOfCongressBibData.createFromXml(doc, "The Messianic Idea in Judaism", "", new ArrayList<>(List.of("Scholem")), ""), "main title");

    // A different book that merely shares the opening words is still rejected
    assertNull(LibraryOfCongressBibData.createFromXml(doc, "The Messianic Idea in Islam", "", new ArrayList<>(), ""));

    // Short main titles get no such leniency
    Document mindDoc = sruResponse(titledRecord("", "Mind and world", "McDowell, John,", "1994"));
    assertNull(LibraryOfCongressBibData.createFromXml(mindDoc, "Mind", "", new ArrayList<>(), ""));

    // An exact match outranks a record that merely extends the title
    Document kripkeDoc = sruResponse(titledRecord("", "Naming and necessity revisited", "Someone, Else,", "2010"),
                                     titledRecord("", "Naming and necessity", "Kripke, Saul A.", "1980"));

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(kripkeDoc, "Naming and Necessity", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals("Naming and necessity", bd.getStr(bfTitle));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void relatorVariantsTest()
  {
    String record = """
      <record xmlns="http://www.loc.gov/MARC21/slim">
        <leader>00000cam a2200000 a 4500</leader>
        <controlfield tag="008">000000s2000    xx            000 0 eng  </controlfield>
        <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Relator variants</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Wood, Allen W.</subfield><subfield code="4">aut</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Guyer, Paul</subfield><subfield code="4">http://id.loc.gov/vocabulary/relators/edt</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Kemp Smith, Norman</subfield><subfield code="e">tr.</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Caygill, Howard</subfield><subfield code="e">translator</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Some Publisher</subfield><subfield code="e">publisher</subfield></datafield>
        <datafield tag="710" ind1="2" ind2=" "><subfield code="a">Cambridge University Press</subfield></datafield>
      </record>
      """;

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertEquals(1, authorList.size(), "A bare relator code in $4 should map to author");
    assertEquals("Wood, Allen W.", authorList.getFirst().getName().getLastFirst());

    assertEquals(1, editorList.size(), "A full relator URI in $4 should map to editor");
    assertEquals("Guyer, Paul", editorList.getFirst().getName().getLastFirst());

    assertEquals(2, translatorList.size(), "Both \"tr.\" and \"translator\" in $e should map to translator");

    // A publisher relator is dropped, and with persons named, a corporate body (710) is never an author
    assertTrue(bd.getAuthors().stream().noneMatch(author -> author.getName().getLast().contains("Publisher")));
    assertTrue(bd.getAuthors().stream().noneMatch(author -> author.getName().getFull().contains("Cambridge")));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizeRoleTest()
  {
    assertEquals(author    , LibraryOfCongressBibData.normalizeRole("aut"));
    assertEquals(author    , LibraryOfCongressBibData.normalizeRole("Author"));
    assertEquals(author    , LibraryOfCongressBibData.normalizeRole("http://id.loc.gov/vocabulary/relators/aut"));
    assertEquals(author    , LibraryOfCongressBibData.normalizeRole("cre"));

    assertEquals(editor    , LibraryOfCongressBibData.normalizeRole("edt"));
    assertEquals(editor    , LibraryOfCongressBibData.normalizeRole("editor"));
    assertEquals(editor    , LibraryOfCongressBibData.normalizeRole("http://id.loc.gov/vocabulary/relators/edt"));

    assertEquals(translator, LibraryOfCongressBibData.normalizeRole("trl"));
    assertEquals(translator, LibraryOfCongressBibData.normalizeRole("tr"));
    assertEquals(translator, LibraryOfCongressBibData.normalizeRole("Translator."));

    // Legacy text forms from older cataloging rules
    assertEquals(author, LibraryOfCongressBibData.normalizeRole("joint author"));
    assertEquals(editor, LibraryOfCongressBibData.normalizeRole("joint ed."));
    assertEquals(editor, LibraryOfCongressBibData.normalizeRole("joint editor"));

    // RDA's spelled-out forms, including the doubled space LoC actually emits
    assertEquals(editor, LibraryOfCongressBibData.normalizeRole("editor of compilation"));
    assertEquals(editor, LibraryOfCongressBibData.normalizeRole("editor of  compilation"));
    assertNull(LibraryOfCongressBibData.normalizeRole("author of introduction"));

    assertNull(LibraryOfCongressBibData.normalizeRole("pbl"));
    assertNull(LibraryOfCongressBibData.normalizeRole("publisher"));
    assertNull(LibraryOfCongressBibData.normalizeRole("ill"));
    assertNull(LibraryOfCongressBibData.normalizeRole(""));
    assertNull(LibraryOfCongressBibData.normalizeRole(null));
  }

//---------------------------------------------------------------------------

  @Test
  void rolesFromStatementOfResponsibilityTest()
  {
    assertEquals(new Roles(true, false), LibraryOfCongressBibData.rolesFromStatementOfResponsibility("edited by Paul Guyer.", "Guyer"));

    assertEquals(new Roles(false, true), LibraryOfCongressBibData.rolesFromStatementOfResponsibility(
      "Alexandre Dumas ; translated and with an introduction and notes by Robin Buss.", "Buss"));

    assertNull(LibraryOfCongressBibData.rolesFromStatementOfResponsibility(
      "Alexandre Dumas ; translated and with an introduction and notes by Robin Buss.", "Dumas"));

    // "translated and edited by" yields both roles
    assertEquals(new Roles(true, true), LibraryOfCongressBibData.rolesFromStatementOfResponsibility(
      "Immanuel Kant ; translated and edited by Paul Guyer, Allen W. Wood.", "Guyer"));

    assertNull(LibraryOfCongressBibData.rolesFromStatementOfResponsibility("edited by Paul Guyer.", "Kant"));
    assertNull(LibraryOfCongressBibData.rolesFromStatementOfResponsibility("", "Guyer"));
    assertNull(LibraryOfCongressBibData.rolesFromStatementOfResponsibility(null, "Guyer"));
  }

//---------------------------------------------------------------------------

  /** A name carrying both an editor and a translator relator keeps both flags. */
  @Test
  void multipleRelatorsTest()
  {
    String record = """
      <record xmlns="http://www.loc.gov/MARC21/slim">
        <leader>00000cam a2200000 a 4500</leader>
        <controlfield tag="008">000000s2000    xx            000 0 eng  </controlfield>
        <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Critique of pure reason</subfield></datafield>
        <datafield tag="100" ind1="1" ind2=" "><subfield code="a">Kant, Immanuel</subfield><subfield code="4">aut</subfield></datafield>
        <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Guyer, Paul</subfield><subfield code="4">edt</subfield><subfield code="4">trl</subfield></datafield>
      </record>
      """;

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "");

    assertNotNull(bd);

    List<Author> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();
    getLists(bd.getAuthors(), authorList, editorList, translatorList);

    assertEquals(1, authorList.size());
    assertEquals("Kant, Immanuel", authorList.getFirst().getName().getLastFirst());

    assertEquals(1, editorList    .size());
    assertEquals(1, translatorList.size());
    assertSame(editorList.getFirst(), translatorList.getFirst(), "One author with both flags, not two entries");
    assertEquals("Guyer, Paul", editorList.getFirst().getName().getLastFirst());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void noResultsTest()
  {
    Document doc = Jsoup.parse(
      "<?xml version=\"1.0\"?><zs:searchRetrieveResponse xmlns:zs=\"http://www.loc.gov/zing/srw/\">" +
      "<zs:version>1.1</zs:version><zs:numberOfRecords>0</zs:numberOfRecords></zs:searchRetrieveResponse>", "", Parser.xmlParser());

    assertNull(LibraryOfCongressBibData.createFromXml(doc, "Anything", "", new ArrayList<>(), ""));
  }

//---------------------------------------------------------------------------

  @Test
  void diagnosticResponseTest()
  {
    Document doc = Jsoup.parse(
      "<?xml version=\"1.0\"?><zs:searchRetrieveResponse xmlns:zs=\"http://www.loc.gov/zing/srw/\">" +
      "<zs:version>1.1</zs:version><zs:numberOfRecords>1</zs:numberOfRecords>" +
      "<zs:diagnostics><diagnostic><message>Unsupported index</message></diagnostic></zs:diagnostics>" +
      "</zs:searchRetrieveResponse>", "", Parser.xmlParser());

    assertNull(LibraryOfCongressBibData.createFromXml(doc, "Anything", "", new ArrayList<>(), ""));
  }

//---------------------------------------------------------------------------

  @Test
  void notAnSruEnvelopeTest()
  {
    // A truncated response or an intercepting proxy's error page. jsoup parses these happily,
    // so the absence of numberOfRecords is what distinguishes them from a clean miss.

    assertNull(LibraryOfCongressBibData.createFromXml(
      Jsoup.parse("<html><body>Access denied</body></html>", "", Parser.xmlParser()), "Anything", "", new ArrayList<>(), ""));

    assertNull(LibraryOfCongressBibData.createFromXml(
      Jsoup.parse("", "", Parser.xmlParser()), "Anything", "", new ArrayList<>(), ""));

    assertNull(LibraryOfCongressBibData.createFromXml(null, "Anything", "", new ArrayList<>(), ""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void candidateSelectionTest()
  {
    // With several records, the one whose title is closest to the query wins
    Document doc = sruResponse(CAMBRIDGE_COMPANION, MONTE_CRISTO);

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(doc, "The Count of Monte Cristo", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals("The Count of Monte Cristo", bd.getStr(bfTitle));

    bd = LibraryOfCongressBibData.createFromXml(doc, "The Cambridge Companion to Kant", "", new ArrayList<>(), "");

    assertNotNull(bd);
    assertEquals("The Cambridge companion to Kant", bd.getStr(bfTitle));

    // A title that matches nothing must not return a wrong record
    assertNull(LibraryOfCongressBibData.createFromXml(doc, "Zettelkasten Method For Absolute Beginners", "", new ArrayList<>(), ""));

    // A lone record gets the same check; being the only hit does not make it the right one
    assertNull(LibraryOfCongressBibData.createFromXml(sruResponse(CAMBRIDGE_COMPANION), "Mental Mechanisms: Philosophical Perspectives on Cognitive Neuroscience", "", new ArrayList<>(), ""));

    // Whereas a lone record with the right title is still accepted
    assertNotNull(LibraryOfCongressBibData.createFromXml(sruResponse(CAMBRIDGE_COMPANION), "The Cambridge Companion to Kant", "", new ArrayList<>(), ""));
  }

//---------------------------------------------------------------------------

  @Test
  void queryIsbnFallbackTest()
  {
    String record = """
      <record xmlns="http://www.loc.gov/MARC21/slim">
        <leader>00000cam a2200000 a 4500</leader>
        <controlfield tag="008">000000s2000    xx            000 0 eng  </controlfield>
        <datafield tag="245" ind1="1" ind2="0"><subfield code="a">No identifiers at all</subfield></datafield>
      </record>
      """;

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "9780140449266");

    assertNotNull(bd);
    assertEquals(List.of("9780140449266"), bd.getMultiStr(bfISBNs));
    assertEquals("9780140449266", bd.getQueryIsbn());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void entryTypeTest()
  {
    // Books: 008/24-27 are the nature-of-contents codes (m = theses, d = dictionaries), 008/29 the conference flag, 008/30 the festschrift flag
    assertEquals(etThesis       , typeOf('a', 'm', ' ', at(24, 'm'), ""));
    assertEquals(etReferenceBook, typeOf('a', 'm', ' ', at(24, 'd'), ""));
    assertEquals(etEditedBook   , typeOf('a', 'm', ' ', at(30, '1'), ""));

    // A conference publication is always the container, but a serial one is a proceedings series
    assertEquals(etConferenceProceedings, typeOf('a', 'm', ' ', at(29, '1'), ""));
    assertEquals(etProceedingsSeries    , typeOf('a', 's', ' ', at(29, '1'), ""));

    // Continuing resources: 008/21 is the type (p = periodical, j = journal, g = magazine, n = newspaper)
    assertEquals(etJournal          , typeOf('a', 's', ' ', at(21, 'p'), ""));
    assertEquals(etJournal          , typeOf('a', 's', ' ', at(21, 'j'), ""));
    assertEquals(etMagazine         , typeOf('a', 's', ' ', at(21, 'g'), ""));
    assertEquals(etNewspaper        , typeOf('a', 's', ' ', at(21, 'n'), ""));
    assertEquals(etSerialPublication, typeOf('a', 's', ' ', "", ""));

    assertEquals(etBook           , typeOf('a', 'm', ' ', "", ""));
    assertEquals(etMultiVolumeWork, typeOf('a', 'm', 'a', "", ""));   // leader/19 = a: a multipart set

    // Bibliographies (008/24 = b) mean the book *contains* a bibliography; it is still a book
    assertEquals(etBook, typeOf('a', 'm', ' ', at(24, 'b'), ""));

    // Genre/form terms (655) are matched only after the fixed fields miss, and tolerate plural/period
    assertEquals(etJournal, typeOf('a', 's', ' ', "", "<datafield tag=\"655\" ind1=\" \" ind2=\"7\"><subfield code=\"a\">Periodicals.</subfield><subfield code=\"2\">lcgft</subfield></datafield>"));

    // Other types of record (leader/06)
    assertEquals(etMap           , typeOf('e', 'm', ' ', "", ""));
    assertEquals(etMusicScore    , typeOf('c', 'm', ' ', "", ""));
    assertEquals(etAudioRecording, typeOf('j', 'm', ' ', "", ""));
    assertEquals(etSoftware      , typeOf('m', 'm', ' ', "", ""));

    // Projected medium must not claim to be a film or a videorecording unless 008/33 says which
    assertEquals(etAudiovisualMaterial, typeOf('g', 'm', ' ', "", ""));
    assertEquals(etFilm               , typeOf('g', 'm', ' ', at(33, 'm'), ""));
    assertEquals(etVideoRecording     , typeOf('g', 'm', ' ', at(33, 'v'), ""));
  }

//---------------------------------------------------------------------------

  /** A 008 field with a single position set: {@code at(24, 'm')} puts an "m" at position 24 and spaces elsewhere in 18-34 */
  private static String at(int pos, char code)
  {
    StringBuilder sb = new StringBuilder("                 ");   // positions 18 to 34
    sb.setCharAt(pos - 18, code);
    return sb.toString();
  }

  private static EntryType typeOf(char recordType, char level, char multipartLevel, String positions18to34, String extraFields)
  {
    String leader = "00000c" + recordType + level + " a2200000 a" + multipartLevel + "4500",   // leader/06 type, /07 level, /19 multipart level
           f008   = "000000s2000    xx " + StringUtils.rightPad(positions18to34, 17) + "eng  ";

    String record = "<record xmlns=\"http://www.loc.gov/MARC21/slim\"><leader>" + leader + "</leader>" +
                    "<controlfield tag=\"008\">" + f008 + "</controlfield>" +
                    "<datafield tag=\"245\" ind1=\"1\" ind2=\"0\"><subfield code=\"a\">Type probe</subfield></datafield>" + extraFields + "</record>";

    return LibraryOfCongressBibData.createFromXml(sruResponse(record), "", "", new ArrayList<>(), "").getEntryType();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void queryUrlTest()
  {
    List<String> authKeywords = new ArrayList<>();

    String url = LibraryOfCongressBibData.getQueryUrl(null, null, authKeywords, "978-0-14-044926-6");
    assertTrue(url.startsWith("http://lx2.loc.gov:210/lcdb?"), url);
    assertTrue(url.contains("operation=searchRetrieve"), url);
    assertTrue(url.contains("recordSchema=marcxml"), url);
    assertTrue(url.contains("bath.isbn%3D9780140449266"), "Hyphens must be stripped from the ISBN: " + url);

    // Only the indexes LoC documents for its FOLIO gateway: dc.title and dc.author, never the
    // undocumented bath.title and bath.author, which happen to work through a mapping an update could drop
    url = LibraryOfCongressBibData.getQueryUrl("Naming and Necessity", null, authKeywords, "");
    assertTrue(url.contains("dc.title"), url);
    assertFalse(url.contains("dc.author"), url);
    assertFalse(url.contains("bath.title") || url.contains("bath.author"), url);

    // CQL has no escape LoC honors, so syntactic characters are removed rather than escaped.
    // The only quotes left should be the pair delimiting the CQL phrase.
    url = LibraryOfCongressBibData.getQueryUrl("Kant's \"Critique\" (2nd ed.)", null, authKeywords, "");
    assertEquals(2, url.split("%22", -1).length - 1, "Only the CQL phrase delimiters should remain: " + url);
    assertFalse(url.contains("%28"), "Parens from the title must not reach the query: " + url);
    assertFalse(url.contains("%29"), "Parens from the title must not reach the query: " + url);

    // An apostrophe is not CQL syntax; turning it into a space breaks the phrase ("Mind s I" finds nothing)
    assertTrue(url.contains("Kant%27s"), "The apostrophe must survive: " + url);
  }

//---------------------------------------------------------------------------

  @Test
  void mainTitleForRetryTest()
  {
    // LoC's record is "Knowing and being: essays"; the extra words in the work's subtitle sink the full-title search
    assertEquals("Knowing and Being", LibraryOfCongressBibData.mainTitleForRetry("Knowing and Being: Essays by Michael Polanyi"));

    // A subtitle after a question mark
    assertEquals("What Is Life", LibraryOfCongressBibData.mainTitleForRetry("What Is Life? The Physical Aspect of the Living Cell"));

    // No subtitle to drop
    assertNull(LibraryOfCongressBibData.mainTitleForRetry("Republic"));

    // The question mark is removed by CQL sanitizing anyway, so the retry would repeat the same query
    assertNull(LibraryOfCongressBibData.mainTitleForRetry("What Is This Thing Called Science?"));

    assertNull(LibraryOfCongressBibData.mainTitleForRetry(": Essays"));
    assertNull(LibraryOfCongressBibData.mainTitleForRetry(""));
    assertNull(LibraryOfCongressBibData.mainTitleForRetry(null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void dateParsingTest()
  {
    // BibliographicDate.fromUserStr handles these already
    assertEquals(1992, yearOf("1992"));
    assertEquals(1992, yearOf("[1992]"));
    assertEquals(2003, yearOf("2003."));

    // ISO forms, as emitted for MARC-encoded dates: month and day must survive, not just the year
    BibliographicDate isoDate = LibraryOfCongressBibData.parseMarcDate("2003-06-11");
    assertEquals(2003, isoDate.year.numericValueWhereMinusOneEqualsOneBC());
    assertEquals(6, isoDate.month);
    assertEquals(11, isoDate.day);

    isoDate = LibraryOfCongressBibData.parseMarcDate("2003-06");
    assertEquals(2003, isoDate.year.numericValueWhereMinusOneEqualsOneBC());
    assertEquals(6, isoDate.month);
    assertFalse(isoDate.hasDay());

    // The gap it does not handle: a copyright or phonogram marker glued to the digits
    assertEquals(2003, yearOf("c2003"));
    assertEquals(1998, yearOf("p1998"));
    assertEquals(1992, yearOf("[c1992]"));

    // Placeholder years must still be rejected
    assertEquals(0, yearOf("19uu"));
    assertEquals(0, yearOf("20--"));
    assertEquals(0, yearOf(""));
    assertEquals(0, yearOf(null));
  }

//---------------------------------------------------------------------------

  private static int yearOf(String rawStr)
  {
    return LibraryOfCongressBibData.parseMarcDate(rawStr).year.numericValueWhereMinusOneEqualsOneBC();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Exercises URL construction and parsing against the live SRU server.
   * <p>
   * Gated behind an environment variable because the server is plain HTTP on port 210, which
   * is far more likely to be blocked than 443; running this unconditionally would fail the
   * build on any network that filters it. Enable with {@code HN_ONLINE_TESTS=true}.
   * </p>
   */
  @Test
  @EnabledIfEnvironmentVariable(named = "HN_ONLINE_TESTS", matches = "true")
  void liveSruQueryTest() throws Exception
  {
    List<String> authKeywords = new ArrayList<>();
    String url = LibraryOfCongressBibData.getQueryUrl(null, null, authKeywords, "9780140449266");

    HttpResponse<String> response;

    try (HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(15)).build())
    {
      response = client.send(HttpRequest.newBuilder().uri(URI.create(url)).timeout(Duration.ofSeconds(15)).GET().build(),
                             HttpResponse.BodyHandlers.ofString());
    }

    assertEquals(200, response.statusCode());

    LibraryOfCongressBibData bd = LibraryOfCongressBibData.createFromXml(
      Jsoup.parse(response.body(), "", Parser.xmlParser()), "", "", authKeywords, "9780140449266");

    assertNotNull(bd, "Live LoC query for a known-held ISBN returned nothing");
    assertFalse(bd.getStr(bfTitle).isBlank(), "Live LoC record had no title");
    assertTrue(bd.getMultiStr(bfISBNs).contains("9780140449266"));
  }

//---------------------------------------------------------------------------

  /**
   * LoC's configuration page documents an HTTPS form of the SRU endpoint,
   * {@code https://lx2.loc.gov/sru/lcdb}, but port 443 on that host is served by the LCCN
   * Permalink application, which answers every SRU request with a 404 error page. Plain
   * HTTP on port 210 is what works, at the cost of being blocked on some networks. The day
   * this assertion fails is the day the HTTPS endpoint starts working: switch
   * {@code SRU_BASE} to it, drop the port-210 caveats from the class javadoc, and delete
   * this test.
   */
  @Test
  @EnabledIfEnvironmentVariable(named = "HN_ONLINE_TESTS", matches = "true")
  void httpsSruEndpointStillDoesNotWorkTest() throws Exception
  {
    List<String> authKeywords = new ArrayList<>();
    String url = LibraryOfCongressBibData.getQueryUrl(null, null, authKeywords, "9780140449266").replace("http://lx2.loc.gov:210/lcdb", "https://lx2.loc.gov/sru/lcdb");

    assertTrue(url.startsWith("https://lx2.loc.gov/sru/lcdb?"), url);

    HttpResponse<String> response;

    try (HttpClient client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(15)).build())
    {
      response = client.send(HttpRequest.newBuilder().uri(URI.create(url)).timeout(Duration.ofSeconds(15)).GET().build(),
                             HttpResponse.BodyHandlers.ofString());
    }

    LibraryOfCongressBibData bd = (response.statusCode() == 200) ?
      LibraryOfCongressBibData.createFromXml(Jsoup.parse(response.body(), "", Parser.xmlParser()), "", "", authKeywords, "9780140449266")
    :
      null;

    assertNull(bd, "LoC's HTTPS SRU endpoint now answers with a real record (HTTP " + response.statusCode() + "). " +
                   "Switch SRU_BASE to https://lx2.loc.gov/sru/lcdb and delete this test.");
  }

//---------------------------------------------------------------------------

  /** The dedupe set is keyed by canonical ISBN-13, so the ISBN-10 form of an
   *  already-queried book is skipped without a request; with every ISBN skipped
   *  and no title, the request reports a clean miss synchronously. */
  @Test
  void isbn10FormOfCheckedIsbn13IsSkipped()
  {
    Set<String> checkedIDs = new HashSet<>(Set.of("9780140449266"));
    List<LibraryOfCongressBibData> results = new ArrayList<>();

    LibraryOfCongressBibData.doHttpRequest(new AsyncHttpClient(), List.of("0140449264").iterator(), checkedIDs, results::add, Assertions::fail);

    assertEquals(1, results.size(), "must complete synchronously with no network involved");
    assertNull(results.getFirst());
    assertEquals(Set.of("9780140449266"), checkedIDs, "nothing new was added, and no blank entry either");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
