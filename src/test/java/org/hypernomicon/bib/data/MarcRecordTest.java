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

import java.util.List;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.parser.Parser;

import org.junit.jupiter.api.Test;

import static org.hypernomicon.bib.data.MarcRecord.*;

//---------------------------------------------------------------------------

/**
 * Tests for the MARCXML record model: the structural reading of leader, control fields, and
 * data fields, independent of what any tag means.
 */
class MarcRecordTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** An SRU envelope whose own record elements must not be mistaken for MARC records */
  private static final String SRU_RESPONSE = """
    <?xml version="1.0"?><zs:searchRetrieveResponse xmlns:zs="http://www.loc.gov/zing/srw/">
      <zs:version>1.1</zs:version><zs:numberOfRecords>1</zs:numberOfRecords>
      <zs:records><zs:record><zs:recordSchema>marcxml</zs:recordSchema><zs:recordData>
        <record xmlns="http://www.loc.gov/MARC21/slim">
          <leader>01681cam a2200421 a 4500</leader>
          <controlfield tag="008">030611s2003    enk           000 1 eng  </controlfield>
          <datafield tag="245" ind1="1" ind2="4"><subfield code="a">The Count of Monte Cristo /</subfield><subfield code="c">Alexandre Dumas.</subfield></datafield>
          <datafield tag="260" ind1=" " ind2=" "><subfield code="a">London ;</subfield><subfield code="a">New York :</subfield><subfield code="b">Penguin,</subfield><subfield code="c">c2003.</subfield></datafield>
          <datafield tag="700" ind1="1" ind2=" "><subfield code="a">Ramsey, William M.,</subfield><subfield code="e">editor of  compilation.</subfield></datafield>
        </record>
      </zs:recordData></zs:record></zs:records>
    </zs:searchRetrieveResponse>
    """;

  /** The same record as id.loc.gov serves it: every element carrying a namespace prefix */
  private static final String PREFIXED_RECORD = """
    <marcxml:record xmlns:marcxml="http://www.loc.gov/MARC21/slim">
      <marcxml:leader>01681cam a2200421 a 4500</marcxml:leader>
      <marcxml:controlfield tag="008">030611s2003    enk           000 1 eng  </marcxml:controlfield>
      <marcxml:datafield tag="245" ind1="1" ind2="4"><marcxml:subfield code="a">The Count of Monte Cristo /</marcxml:subfield></marcxml:datafield>
    </marcxml:record>
    """;

//---------------------------------------------------------------------------

  private static Document parse(String xml) { return Jsoup.parse(xml, "", Parser.xmlParser()); }

//---------------------------------------------------------------------------

  @Test
  void envelopeRecordsAreNotMarcRecords()
  {
    List<MarcRecord> records = marcRecordsIn(parse(SRU_RESPONSE));

    assertEquals(1, records.size(), "Only the element with a leader and data fields is a MARC record; zs:record is the envelope's");
    assertEquals("01681cam a2200421 a 4500", records.getFirst().leader());
  }

//---------------------------------------------------------------------------

  @Test
  void positionalDataKeepsItsWhitespace()
  {
    MarcRecord rec = marcRecordsIn(parse(SRU_RESPONSE)).getFirst();

    // The fixed field is space-padded; a whitespace-normalizing read would shift every position
    assertEquals("2003", rec.controlChars("008", 7, 11));
    assertEquals("enk" , rec.controlChars("008", 15, 18));
    assertEquals("eng" , rec.controlChars("008", 35, 38));
    assertEquals('1'   , rec.controlChar("008", 33));
    assertEquals('a'   , rec.leaderChar(6));
    assertEquals('m'   , rec.leaderChar(7));

    // Beyond the end of a field, or of an absent field, positions read as spaces rather than failing
    assertEquals("    ", rec.controlChars("008", 60, 64));
    assertEquals(' '   , rec.controlChar("006", 0));
    assertEquals(""    , rec.controlField("006"));
  }

//---------------------------------------------------------------------------

  @Test
  void dataFieldAccess()
  {
    MarcRecord rec = marcRecordsIn(parse(SRU_RESPONSE)).getFirst();

    DataField title = rec.firstField("245");

    assertNotNull(title);
    assertEquals('1', title.ind1());
    assertEquals('4', title.ind2());
    assertEquals("The Count of Monte Cristo /", title.subfield('a'));
    assertEquals("", title.subfield('b'), "An absent subfield reads as empty");
    assertTrue(title.has('c'));
    assertFalse(title.has('b'));

    DataField imprint = rec.firstField("260");

    assertEquals(List.of("London ;", "New York :"), imprint.subfields('a'), "Every occurrence of a repeated subfield, in order");
    assertEquals("London ;", imprint.subfield('a'), "The first occurrence");
    assertEquals("London ; New York : Penguin, c2003.", imprint.join("abc"));

    assertEquals("Alexandre Dumas.", rec.firstSubfield("245", 'c'));
    assertEquals("", rec.firstSubfield("100", 'a'), "An absent field reads as empty");
    assertNull(rec.firstField("100"));
    assertEquals(0, rec.fields("100").size());

    // A doubled space inside a subfield survives, as LoC actually emits it in "editor of  compilation"
    assertEquals("editor of  compilation.", rec.firstSubfield("700", 'e'));
  }

//---------------------------------------------------------------------------

  @Test
  void namespacePrefixIsIgnored()
  {
    List<MarcRecord> records = marcRecordsIn(parse(PREFIXED_RECORD));

    assertEquals(1, records.size());
    assertEquals("2003", records.getFirst().controlChars("008", 7, 11));
    assertEquals("The Count of Monte Cristo /", records.getFirst().firstSubfield("245", 'a'));
  }

//---------------------------------------------------------------------------

  @Test
  void isbdSeparatorsAreStrippedButNotAnAbbreviationsPeriod()
  {
    assertEquals("The Count of Monte Cristo", stripISBD("The Count of Monte Cristo /"));
    assertEquals("Cambridge"                , stripISBD("Cambridge :"));
    assertEquals("London, England"          , stripISBD("London, England ;"));
    assertEquals("Hackett Pub. Co."         , stripISBD("Hackett Pub. Co.,"));
    assertEquals(""                         , stripISBD(null));
  }

//---------------------------------------------------------------------------

  @Test
  void emptyDocumentHasNoRecords()
  {
    assertTrue(marcRecordsIn(null).isEmpty());
    assertTrue(marcRecordsIn(parse("")).isEmpty());
    assertTrue(marcRecordsIn(parse("<html><body>Access denied</body></html>")).isEmpty());
  }

//---------------------------------------------------------------------------

  /**
   * The shapes marc4j's reader tests guard against, which real-world MARCXML producers do emit:
   * a collection of several records; a bare record as the document root, carrying an OAI-PMH
   * identifier and datestamp as extra children (Koha does this); comments; a data field with
   * no indicator attributes at all; a subfield with no code attribute; a data field or control
   * field with no tag. None of these may derail the reading of the record around them.
   */
  @Test
  void toleratesTheShapesOtherReadersHaveMetInTheWild()
  {
    String collection = """
      <?xml version="1.0" encoding="UTF-8"?>
      <collection xmlns="http://www.loc.gov/MARC21/slim">
        <record type="Bibliographic">
          <leader>00714cam a2200205 a 4500</leader>
          <controlfield tag="008">020805s2002    nyu    j      000 1 eng  </controlfield>
          <datafield tag="245" ind1="1" ind2="0"><subfield code="a">Summerland /</subfield></datafield>
        </record>
        <record>
          <leader>00759cam a2200229 a 4500</leader>
          <!-- a comment where a field could be -->
          <datafield tag="911"><subfield code="a">indicatorless</subfield></datafield>
          <datafield tag="020" ind1=" " ind2=" "><subfield>no code attribute</subfield><subfield code="a">0679450041</subfield></datafield>
          <datafield ind1=" " ind2=" "><subfield code="a">no tag attribute</subfield></datafield>
          <controlfield>no tag either</controlfield>
          <datafield tag="245" ind1="1" ind2="4"><subfield code="a">The amazing adventures of Kavalier and Clay :</subfield></datafield>
        </record>
      </collection>
      """;

    List<MarcRecord> records = marcRecordsIn(parse(collection));

    assertEquals(2, records.size(), "Every record of a collection, whether or not it carries a type attribute");
    assertEquals("Summerland /", records.get(0).firstSubfield("245", 'a'));

    MarcRecord damaged = records.get(1);

    assertEquals("The amazing adventures of Kavalier and Clay :", damaged.firstSubfield("245", 'a'), "Fields after the damaged ones are still read");

    DataField indicatorless = damaged.firstField("911");

    assertNotNull(indicatorless);
    assertEquals(' ', indicatorless.ind1(), "A missing indicator reads as blank, as marc4j and the MARC standard treat it");
    assertEquals(' ', indicatorless.ind2());

    assertEquals(List.of("0679450041"), damaged.firstField("020").subfields('a'), "A subfield with no code is skipped; the coded one beside it is not");
    assertEquals("", damaged.controlField("008"), "A control field with no tag is not mistaken for any real one");
    assertNull(damaged.firstField("100"), "Nor is a data field with no tag");

    String bareRecord = """
      <record xmlns="http://www.loc.gov/MARC21/slim"><identifier>KOHA-LFL:5</identifier><datestamp>2016-01-21T13:51:42Z</datestamp>
        <leader>00757cam a22002055a 4500</leader>
        <controlfield tag="008">970205s1996    enk           000 0 eng  </controlfield>
        <datafield ind1="1" ind2="0" tag="245"><subfield code="a">Cinema and architecture :</subfield></datafield>
      </record>
      """;

    records = marcRecordsIn(parse(bareRecord));

    assertEquals(1, records.size(), "A record can be the document root, with foreign children ignored");
    assertEquals("1996", records.getFirst().controlChars("008", 7, 11));
    assertEquals("Cinema and architecture :", records.getFirst().firstSubfield("245", 'a'));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
