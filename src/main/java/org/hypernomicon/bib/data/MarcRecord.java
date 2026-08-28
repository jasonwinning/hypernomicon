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

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import com.ibm.icu.text.Normalizer2;

//---------------------------------------------------------------------------

/**
 * A MARC 21 bibliographic record as delivered in MARCXML (MARC is the format libraries have
 * exchanged catalog records in since the 1960s; MARC 21 is its current edition and MARCXML its
 * XML form): the leader (a fixed 24-character header summarizing the record), the control
 * fields (tags 001 to 009, whose values are positional), and the data fields (a tag, two
 * indicators, which are single characters qualifying how the field is read, and subfields, the
 * field's coded parts). This is deliberately a thin model: it knows the structure of MARC, and its
 * nested constant classes name the fields, subfields, positions, and codes that readers of a
 * record use, so that the readers never need the raw numbers and letters. What to do with a
 * field's value stays with the code reading the record.
 * <p>
 * Element names are matched by local name, so a record parses the same whether it arrives
 * in a default namespace (as from the LoC SRU server; SRU, Search/Retrieve via URL, is the query
 * protocol library catalogs expose) or with a prefix (as from id.loc.gov).
 * The names below follow the MARC 21 Format for Bibliographic Data:
 * <a href="https://www.loc.gov/marc/bibliographic/">https://www.loc.gov/marc/bibliographic/</a>
 * </p>
 */
final class MarcRecord
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The leader: positions, and the values found at them */
  static final class Leader
  {
    static final int TYPE_OF_RECORD      = 6,
                     BIBLIOGRAPHIC_LEVEL = 7,
                     MULTIPART_LEVEL     = 19;

    // Type of record (leader/06): what kind of thing the item is. Language material is text,
    // and a projected medium is film or video.

    static final char LANGUAGE_MATERIAL            = 'a',
                      NOTATED_MUSIC                = 'c',
                      MANUSCRIPT_NOTATED_MUSIC     = 'd',
                      CARTOGRAPHIC_MATERIAL        = 'e',
                      MANUSCRIPT_CARTOGRAPHIC      = 'f',
                      PROJECTED_MEDIUM             = 'g',
                      NONMUSICAL_SOUND_RECORDING   = 'i',
                      MUSICAL_SOUND_RECORDING      = 'j',
                      TWO_DIMENSIONAL_GRAPHIC      = 'k',
                      COMPUTER_FILE                = 'm',
                      MANUSCRIPT_LANGUAGE_MATERIAL = 't';

    // Bibliographic level (leader/07): whether the record describes a whole or a part. A serial
    // is a publication issued in ongoing parts (a journal, a newspaper), an integrating resource
    // one updated in place (a website, a loose-leaf service), and a component part an article
    // or chapter within a larger work.

    static final char SERIAL_COMPONENT_PART = 'b',
                      INTEGRATING_RESOURCE  = 'i',
                      SERIAL                = 's';

    // Multipart resource record level (leader/19): whether the record is for a set of volumes or for one of them

    static final char SET = 'a';
  }

//---------------------------------------------------------------------------

  /**
   * Field 008, the fixed-length data elements (coded facts about the item, each at a fixed
   * position): positions, and the codes found at them. From position 18 on, the meaning of a
   * position depends on the type of record in the leader.
   */
  static final class FixedField
  {
    static final String TAG = "008";

    /** Positions 07-10, the first date; {@code END} is exclusive, as for every range here */
    static final int DATE1_START = 7, DATE1_END = 11;

    /** Positions 35-37, the language code */
    static final int LANGUAGE_START = 35, LANGUAGE_END = 38;

    // Continuing resources (publications with no planned end: serials, and websites and
    // databases updated in place): position 21, type of continuing resource

    static final int  TYPE_OF_CONTINUING_RESOURCE = 21;
    static final char PERIODICAL = 'p', JOURNAL = 'j', MAGAZINE = 'g', NEWSPAPER = 'n', MONOGRAPHIC_SERIES = 'm', DATABASE = 'd', WEBSITE = 'w';

    // Books: positions 24-27, up to four nature-of-contents codes (what kind of work the book
    // is: a dictionary, a thesis, and so on)

    static final int  NATURE_OF_CONTENTS_START = 24, NATURE_OF_CONTENTS_END = 28;
    static final char ABSTRACTS = 'a', CATALOGS = 'c', DICTIONARIES = 'd', ENCYCLOPEDIAS = 'e', HANDBOOKS = 'f',
                      LEGAL_ARTICLES = 'g', PATENT_DOCUMENT = 'j', LEGISLATION = 'l', THESES = 'm', SURVEYS_OF_LITERATURE = 'n',
                      REVIEWS = 'o', DIRECTORIES = 'r', TECHNICAL_REPORTS = 't', STANDARDS = 'u', LEGAL_CASES = 'v',
                      LAW_REPORTS = 'w', TREATIES = 'z', OFFPRINTS = '2';

    // Books: positions 29 and 30, the conference publication and festschrift (a volume of
    // essays honoring a scholar) flags

    static final int  CONFERENCE_PUBLICATION = 29, FESTSCHRIFT = 30;
    static final char FLAG_SET = '1';

    // Books: position 33, literary form (fiction, letters, speeches, and so on)

    static final int  LITERARY_FORM = 33;
    static final char LETTERS = 'i', SPEECHES = 's';

    // Visual materials: position 33, type of visual material

    static final int  TYPE_OF_VISUAL_MATERIAL = 33;
    static final char MOTION_PICTURE = 'm', VIDEORECORDING = 'v';
  }

//---------------------------------------------------------------------------

  /** 010, Library of Congress Control Number */
  static final class Lccn
  {
    static final String TAG = "010";
    static final char NUMBER = 'a';
  }

  /** 020, International Standard Book Number */
  static final class Isbn
  {
    static final String TAG = "020";
    static final char NUMBER = 'a';
  }

  /** 022, International Standard Serial Number */
  static final class Issn
  {
    static final String TAG = "022";
    static final char NUMBER = 'a';
  }

  /** 024, Other standard identifier: with first indicator 7, the kind of identifier is named in subfield 2 */
  static final class OtherStandardIdentifier
  {
    static final String TAG = "024";
    static final char SOURCE_SPECIFIED_IN_SUBFIELD_2 = '7';
    static final char NUMBER = 'a', SOURCE_OF_NUMBER = '2';
    static final String DOI = "doi";
  }

  /** 041, Language code (subfield h, the original language of a translation, is deliberately not named: it is not the item's language) */
  static final class LanguageCode
  {
    static final String TAG = "041";
    static final char LANGUAGE = 'a';
  }

  /**
   * The subfields every name heading shares, personal or corporate: e and 4 carry the relator,
   * the named party's role (author, editor, translator, and so on), as a term or as a code; t
   * turns the heading into a name-title pair naming a related work rather than a contributor.
   */
  static final class NameHeading
  {
    static final char RELATOR_TERM = 'e', RELATOR_CODE = '4', TITLE_OF_WORK = 't';
  }

  /**
   * 100 and 700, the main entry and added entry personal names (the main entry is the name a
   * record is filed under, usually the first author; added entries are the other names it can
   * be found by).
   */
  static final class PersonalName
  {
    static final String MAIN_ENTRY_TAG = "100", ADDED_ENTRY_TAG = "700";
    static final char NAME = 'a';

    /** First indicator value: the heading (the name as filed) is a family ("Medici family"), not a person */
    static final char FAMILY_NAME = '3';
  }

  /**
   * 110 and 710, the main entry and added entry corporate names: an organization named as
   * responsible for the work, whose subordinate unit is part of the name ("Stanford University.
   * Center for the Study of Language and Information"). A conference (111, 711) is a different
   * kind of heading and is not named here.
   */
  static final class CorporateName
  {
    static final String MAIN_ENTRY_TAG = "110", ADDED_ENTRY_TAG = "710";
    static final char NAME = 'a', SUBORDINATE_UNIT = 'b';
    static final String NAME_AND_SUBORDINATE_UNIT = "" + NAME + SUBORDINATE_UNIT;
  }

  /**
   * 245, Title statement: the title, the remainder of the title (what a title page presents as
   * the subtitle), and the statement of responsibility (the title page's own wording of who
   * did what: "edited by Paul Guyer")
   */
  static final class TitleStatement
  {
    static final String TAG = "245";
    static final char TITLE = 'a', REMAINDER_OF_TITLE = 'b', STATEMENT_OF_RESPONSIBILITY = 'c';
  }

  /** 250, Edition statement */
  static final class EditionStatement
  {
    static final String TAG = "250";
    static final char EDITION = 'a';
  }

  /**
   * 260 (pre-RDA) and 264 (RDA), the publication statement (RDA, Resource Description and
   * Access, is the cataloging code in force since 2013; 260 is the older field, the imprint);
   * a 264's second indicator says which event it records
   */
  static final class Publication
  {
    static final String IMPRINT_TAG = "260", PRODUCTION_PUBLICATION_TAG = "264";
    static final char PLACE = 'a', NAME = 'b', DATE = 'c';
    static final char PUBLICATION = '1', COPYRIGHT_NOTICE = '4';
  }

  /** 300, Physical description: extent (the page or volume count), other physical details, dimensions */
  static final class PhysicalDescription
  {
    static final String TAG = "300";
    static final String EXTENT_DETAILS_AND_DIMENSIONS = "abc";
  }

  /**
   * 490, the series statement as it appears on the item; 830, the authorized form of the series
   * (the standardized name a cataloger files it under); 440, the obsolete field that once
   * served as both
   */
  static final class Series
  {
    static final String STATEMENT_TAG = "490", OBSOLETE_STATEMENT_TAG = "440", ADDED_ENTRY_TAG = "830";
    private static final char TITLE = 'a', VOLUME = 'v';

    static final String TITLE_AND_VOLUME = "" + TITLE + VOLUME;
  }

  /** 655, Genre/form term (what the item is, as a heading: "Festschriften", "Dictionaries") */
  static final class GenreForm
  {
    static final String TAG = "655";
    static final char TERM = 'a';
  }

  /** 773, Host item entry: for a record that describes part of a larger work (an article, a chapter, a paper in proceedings), the larger work */
  static final class HostItem
  {
    static final String TAG = "773";
    static final char TITLE = 't';
  }

//---------------------------------------------------------------------------

  private record Subfield(char code, String value) { }

//---------------------------------------------------------------------------

  record DataField(String tag, char ind1, char ind2, List<Subfield> subfields)
  {
    /** The value of the first subfield with this code, or an empty string */
    String subfield(char code)
    {
      return subfields.stream().filter(sf -> sf.code == code).map(Subfield::value).findFirst().orElse("");
    }

    /** The values of every subfield with this code, in record order */
    List<String> subfields(char code)
    {
      return subfields.stream().filter(sf -> sf.code == code).map(Subfield::value).toList();
    }

    boolean has(char code)
    {
      return subfields.stream().anyMatch(sf -> sf.code == code);
    }

    /** The values of every subfield whose code is one of {@code codes}, in record order, joined by a space */
    String join(String codes)
    {
      return String.join(" ", subfields.stream().filter(sf -> codes.indexOf(sf.code) >= 0).map(Subfield::value).toList());
    }
  }

//---------------------------------------------------------------------------

  private static final Normalizer2 NFC = Normalizer2.getNFCInstance();

  private final String leader;
  private final Map<String, String> controlFields = new HashMap<>();
  private final List<DataField> dataFields = new ArrayList<>();

//---------------------------------------------------------------------------

  private MarcRecord(Element recordElement)
  {
    String leaderStr = "";

    // wholeText, not text: the leader and the control fields are positional and space-padded,
    // and jsoup's text() normalizes whitespace, which would shift every position

    for (Element fieldElement : recordElement.children())
    {
      switch (localName(fieldElement))
      {
        case "leader" -> leaderStr = fieldElement.wholeText();

        case "controlfield" -> controlFields.putIfAbsent(fieldElement.attr("tag"), fieldElement.wholeText());

        case "datafield" ->
        {
          List<Subfield> subfields = new ArrayList<>();

          // Subfield text is composed to NFC: MARC's MARC-8 heritage (MARC-8 being the pre-Unicode
          // character set of MARC records) delivers diacritics decomposed (a base letter followed by a
          // combining mark), while keyboards and every other source produce the precomposed letter.
          // Composition is canonical, so nothing is lost, and the stored text then compares equal to a
          // visually identical one instead of differing invisibly.

          for (Element subfieldElement : fieldElement.children())
            if ("subfield".equals(localName(subfieldElement)) && (subfieldElement.attr("code").length() == 1))
              subfields.add(new Subfield(subfieldElement.attr("code").charAt(0), NFC.normalize(subfieldElement.wholeText())));

          dataFields.add(new DataField(fieldElement.attr("tag"), indicator(fieldElement, "ind1"), indicator(fieldElement, "ind2"), List.copyOf(subfields)));
        }

        default -> { }
      }
    }

    leader = leaderStr;
  }

//---------------------------------------------------------------------------

  String leader()                             { return leader; }
  List<DataField> fields(String tag)          { return dataFields.stream().filter(field -> field.tag.equals(tag)).toList(); }
  DataField firstField(String tag)            { return dataFields.stream().filter(field -> field.tag.equals(tag)).findFirst().orElse(null); }
  String controlField(String tag)             { return controlFields.getOrDefault(tag, ""); }

  /** The value of the first subfield with this code in the first field with this tag, or an empty string */
  String firstSubfield(String tag, char code) { return nullSwitch(firstField(tag), "", field -> field.subfield(code)); }

  /** The character at this position of the leader, or a space if the leader is too short */
  char leaderChar(int pos)                    { return charAt(leader, pos); }

  /** The character at this position of the control field, or a space if the field is absent or too short */
  char controlChar(String tag, int pos)       { return charAt(controlField(tag), pos); }

  private static char charAt(String str, int pos)         { return pos < str.length() ? str.charAt(pos) : ' '; }
  private static String localName(Element el)             { String name = el.normalName(); return name.substring(name.indexOf(':') + 1); }
  private static char indicator(Element el, String attr)  { String str = el.attr(attr); return str.isEmpty() ? ' ' : str.charAt(0); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The characters from {@code start} inclusive to {@code end} exclusive of the control field,
   * padded with spaces if the field is shorter than that, so positional data can always be
   * read without a length check.
   */
  String controlChars(String tag, int start, int end)
  {
    String str = controlField(tag);

    StringBuilder sb = new StringBuilder();

    for (int pos = start; pos < end; pos++)
      sb.append(charAt(str, pos));

    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Every MARC record in the document, in document order: the record elements of an SRU
   * response, or the single record of a document that is nothing but one. A record element is
   * recognized by having a leader or a data field, which is what tells it apart from the SRU
   * envelope's own record elements.
   */
  static List<MarcRecord> marcRecordsIn(Document doc)
  {
    if (doc == null) return List.of();

    return doc.getAllElements().stream()
      .filter(el -> "record".equals(localName(el)))
      .filter(el -> el.children().stream().anyMatch(child -> { String name = localName(child); return "leader".equals(name) || "datafield".equals(name); }))
      .map(MarcRecord::new)
      .toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Strips the ISBD separator punctuation MARC data carries at the end of a subfield (" /",
   * " :", " ;", ","), which belongs to the display form rather than to the value. (ISBD, the
   * International Standard Bibliographic Description, is the convention that puts a space and
   * a slash before a statement of responsibility, a space and a colon before a subtitle, and so
   * on.) A terminal period is left alone: it may be an abbreviation's ("Hackett Pub. Co."), and
   * only the caller knows whether the value is one where the period is punctuation.
   */
  static String stripISBD(String str)
  {
    str = stripSafe(str);

    while ((str.isEmpty() == false) && (" /:;,".indexOf(str.charAt(str.length() - 1)) >= 0))
      str = str.substring(0, str.length() - 1);

    return str.strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
