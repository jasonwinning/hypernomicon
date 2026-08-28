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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.bib.data.MarcRecord.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.function.Consumer;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Strings;
import org.apache.commons.text.similarity.LevenshteinDistance;

import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.model.authors.Author.Roles;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicDate.DateType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_RecordBase;
import org.hypernomicon.util.StringUtil;
import org.hypernomicon.util.http.*;

//---------------------------------------------------------------------------

/**
 * Bibliographic data retrieved from the Library of Congress catalog.
 * <p>
 * The loc.gov JSON API does not expose catalog records (it only covers material
 * digitized on loc.gov), so this queries the LoC SRU server instead, asking for the
 * records as MARCXML: the catalog's native form, which every other output format
 * is converted from. SRU documentation: <a href="https://www.loc.gov/standards/sru/">
 * https://www.loc.gov/standards/sru/</a>; the MARC 21 bibliographic format:
 * <a href="https://www.loc.gov/marc/bibliographic/">https://www.loc.gov/marc/bibliographic/</a>
 * </p>
 * <p>
 * Note that the SRU endpoint is plain HTTP on a nonstandard port, which some networks
 * block. Callers must treat a failure here as a reason to move on to another source
 * rather than as a fatal error.
 * </p>
 */
public final class LibraryOfCongressBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** One record from a title search that is close enough to be considered, with what {@link #CANDIDATE_ORDER} ranks it by */
  private record Candidate(LibraryOfCongressBibData bd, double dist, boolean authMatch, boolean yearMatch) { }

  /** The Library of Congress answered with its block page instead of a search result; see {@link #isAccessBlocked(Document)} */
  @SuppressWarnings("serial")
  static final class AccessBlockedException extends Exception
  {
    AccessBlockedException() { super("The Library of Congress has blocked this network address for excessive traffic. Its catalog cannot be searched until the block is lifted."); }
  }

//---------------------------------------------------------------------------

  private static final String SRU_BASE = "http://lx2.loc.gov:210/lcdb";

  private static final int MAX_RECORDS_ISBN  = 10,
                           MAX_RECORDS_TITLE = 20;

  private final String queryIsbn;

//---------------------------------------------------------------------------

  private LibraryOfCongressBibData(MarcRecord marcRecord, String queryIsbn)
  {
    this.queryIsbn = queryIsbn;

    setTitleFrom(marcRecord);
    addAuthors(marcRecord);

    // Pre-RDA records carry the publication statement in 260; RDA records in 264, one field
    // per event, with the second indicator saying which (1 = publication, 4 = copyright)

    DataField publication = publicationField(marcRecord);

    if (publication != null)
    {
      setStr(bfPubLoc   , stripISBD(publication.subfield(Publication.PLACE)));
      setStr(bfPublisher, stripISBD(publication.subfield(Publication.NAME )));
    }

    setStr(bfEdition , stripISBD(marcRecord.firstSubfield(EditionStatement.TAG, EditionStatement.EDITION)));
    setStr(bfLanguage, languageFrom(marcRecord));

    setDatesFrom(marcRecord);

    // An ISBN field with a cancelled number instead of a number is a cancelled or misprinted
    // ISBN. The number may carry a qualifier in older records ("0140449264 (pbk.)"); addISBN
    // extracts the number itself.

    marcRecord.fields(Isbn.TAG).forEach(field -> addISBN(field.subfield(Isbn.NUMBER)));
    marcRecord.fields(Issn.TAG).forEach(field -> addISSN(field.subfield(Issn.NUMBER)));

    marcRecord.fields(OtherStandardIdentifier.TAG).stream()
      .filter(field -> (field.ind1() == OtherStandardIdentifier.SOURCE_SPECIFIED_IN_SUBFIELD_2) &&
                       OtherStandardIdentifier.DOI.equalsIgnoreCase(field.subfield(OtherStandardIdentifier.SOURCE_OF_NUMBER).strip()))
      .forEach(field -> setDOI(field.subfield(OtherStandardIdentifier.NUMBER)));

    if (fieldNotEmpty(bfISBNs) == false)
      addISBN(queryIsbn);

    // The host item is the larger work that a record for part of it (an article or a chapter)
    // says it appears in; bfContainerTitle drives the larger-work title. A series is not a
    // container.

    List<String> hostTitles = marcRecord.fields(HostItem.TAG).stream().map(field -> titlePart(field.subfield(HostItem.TITLE))).filter(StringUtil::strNotNullOrBlank).toList();

    if (hostTitles.isEmpty() == false)
      setMultiStr(bfContainerTitle, hostTitles);

    setMiscFrom(marcRecord);

    // Only the entry type is set. BibDataStandalone.setWorkType is a no-op and getWorkType
    // derives from the entry type, so calling it would just force a database dependency here.

    setEntryType(parseMarcType(marcRecord, hasEditorsButNoAuthors()));
  }

//---------------------------------------------------------------------------

  public String getQueryIsbn()                      { return safeStr(queryIsbn); }

  /** A title or series value: the ISBD separators go, and so does the terminal period, which for these is punctuation */
  private static String titlePart(String str)      { return Strings.CS.removeEnd(stripISBD(str), ".").strip(); }

  /** The same test HDT_Work and BibEntry apply when choosing whose names represent a work */
  private boolean hasEditorsButNoAuthors()          { return authors.stream().noneMatch(Author::getIsAuthor) && authors.stream().anyMatch(Author::getIsEditor); }

  @Override public boolean fromOnlineSource()       { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The title proper and the remainder of the title (what a title page presents as a subtitle)
   * come from the title statement. The article is part of the title in MARC, so there is no
   * nonfiling text to compose back on. The uniform title (240) and variant titles (246) are not
   * this edition's.
   */
  private void setTitleFrom(MarcRecord marcRecord)
  {
    DataField field = marcRecord.firstField(TitleStatement.TAG);

    if (field == null) return;

    String title    = titlePart(field.subfield(TitleStatement.TITLE)),
           subTitle = titlePart(field.subfield(TitleStatement.REMAINDER_OF_TITLE));

    addStr(bfTitle, title);

    if ((strNotNullOrBlank(subTitle)) && (title.equalsIgnoreCase(subTitle) == false))
      addStr(bfTitle, subTitle);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Personal names come from the main entry and the added entries, in record order. A family
   * heading ("Medici family"), which the first indicator distinguishes from a person's name, is
   * not a person, and an added entry with a title of a work names a related work rather than a
   * contributor. Conferences (111, 711) are meetings, never contributors.
   * <p>
   * An organization is accepted only when the record yields no person at all, and then as a
   * single unsplit name with no first name, as CrossrefBibData does with an author that has
   * only a name.
   * </p>
   */
  private void addAuthors(MarcRecord marcRecord)
  {
    String statementOfResp = marcRecord.firstSubfield(TitleStatement.TAG, TitleStatement.STATEMENT_OF_RESPONSIBILITY);

    addPersonalAuthors(marcRecord, statementOfResp);

    if (authors.isEmpty() == false) return;

    marcRecord.fields(CorporateName.MAIN_ENTRY_TAG ).forEach(field -> addCorporateAuthor(field, statementOfResp, true ));
    marcRecord.fields(CorporateName.ADDED_ENTRY_TAG).forEach(field -> addCorporateAuthor(field, statementOfResp, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addPersonalAuthors(MarcRecord marcRecord, String statementOfResp)
  {
    List<DataField> nameFields = new ArrayList<>(marcRecord.fields(PersonalName.MAIN_ENTRY_TAG));
    nameFields.addAll(marcRecord.fields(PersonalName.ADDED_ENTRY_TAG));

    for (DataField field : nameFields)
    {
      if (field.has(NameHeading.TITLE_OF_WORK) || (field.ind1() == PersonalName.FAMILY_NAME)) continue;

      PersonName personName = personNameFrom(field);

      if ((personName == null) || personName.isEmpty()) continue;

      // The statement of responsibility is consulted even when relators are present, not only
      // for the many records that omit them. It is transcribed from the title page, while the
      // relators are often incomplete: a "translated and edited by" name coded only as an
      // editor, or a second translator-editor coded as an author.

      Roles roles    = rolesFrom(field),
            sorRoles = rolesFromStatementOfResponsibility(statementOfResp, personName.getLast());

      if (roles == null)
      {
        // Every relator on this name was one we do not record (publisher, illustrator, and so
        // on), so unless the statement of responsibility says otherwise, the name is not a
        // contributor we want. The statement gets the last word because LoC's relator vocabulary
        // keeps growing faster than normalizeRole can follow it.

        if (hasAnyRelator(field) && (sorRoles == null)) continue;

        roles = Roles.AUTHOR;   // Same default as GoogleBibData: treat an unmarked name as an author
      }

      authors.add(new AuthorStandalone(roles.combinedWith(sorRoles), personName));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addCorporateAuthor(DataField field, String statementOfResp, boolean mainEntry)
  {
    if (field.has(NameHeading.TITLE_OF_WORK)) return;

    String name = cleanCorporateName(field.join(CorporateName.NAME_AND_SUBORDINATE_UNIT));

    if (name.isBlank()) return;

    // The title page credits the body as it calls itself, without the heading's qualifiers
    // ("Wolfram Research" for the heading "Wolfram Research, Inc."), and may credit only the
    // subordinate unit ("Center for the Study of Language and Information" under "Stanford
    // University."). Either form counts as the statement of responsibility naming the body.

    String parentBody = StringUtils.substringBefore(cleanCorporateName(field.subfield(CorporateName.NAME)), ",").strip(),
           subUnit    = cleanCorporateName(field.subfield(CorporateName.SUBORDINATE_UNIT));

    Roles roles    = rolesFrom(field),
          sorRoles = rolesFromStatementOfResponsibility(statementOfResp, parentBody);

    if (sorRoles == null)
      sorRoles = rolesFromStatementOfResponsibility(statementOfResp, subUnit);

    if (roles == null)
    {
      if (hasAnyRelator(field))
      {
        if (sorRoles == null) return;   // A publisher, sponsor, or the like, and the title page does not say otherwise
      }
      else if ((mainEntry == false) && (statementOfResponsibilityNames(statementOfResp, parentBody) == false)
                                    && (statementOfResponsibilityNames(statementOfResp, subUnit   ) == false))
        return;                         // An unmarked added entry with no word from the title page

      roles = Roles.AUTHOR;
    }

    authors.add(new AuthorStandalone(roles.combinedWith(sorRoles), new PersonName("", name)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The last words of an organization's name that end in a period which is the word's own, not the heading's terminal punctuation */
  private static final Set<String> ABBREVIATED_LAST_WORDS = Set.of("inc", "ltd", "co", "corp", "assn", "dept", "univ", "bros", "pty");

  /**
   * A corporate heading ends with a period that is punctuation ("Rockefeller Foundation.")
   * unless its last word is an abbreviation ("Wolfram Research, Inc."), which keeps it.
   */
  private static String cleanCorporateName(String str)
  {
    str = stripISBD(str);

    if (str.endsWith(".") == false) return str;

    String lastWord = Strings.CS.removeEnd(str.substring(str.lastIndexOf(' ') + 1), ".").toLowerCase(Locale.ROOT);

    return ABBREVIATED_LAST_WORDS.contains(lastWord) ? str : Strings.CS.removeEnd(str, ".").strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean statementOfResponsibilityNames(String sor, String name)
  {
    String target = convertToEnglishChars(safeStr(name)).toLowerCase(Locale.ROOT).strip();

    return (target.isBlank() == false) && convertToEnglishChars(safeStr(sor)).toLowerCase(Locale.ROOT).contains(target);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Only the name subfield is the name. The others qualify it rather than extend it: numeration,
   * titles and other words associated with the name, dates, and the fuller form of an
   * initials-only name ("Quine, W. V." with the fuller form "(Willard Van Orman)").
   */
  private static PersonName personNameFrom(DataField field)
  {
    String name = cleanNamePart(field.subfield(PersonalName.NAME));

    return name.isBlank() ? null : new PersonName(name);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern TRAILING_ABBREV_DOT = Pattern.compile("(?<=\\p{L}{2})\\.$", Pattern.UNICODE_CHARACTER_CLASS),
                               PARENTHETICAL       = Pattern.compile("\\s*\\([^)]*\\)");

  /**
   * Removes what a heading carries beyond the name itself: a parenthetical qualifier (the
   * fuller form has its own subfield under current rules, but older records fold it into the
   * name), and the ISBD terminal punctuation: a trailing comma or semicolon always, and a
   * trailing period only when at least two letters precede it, so that an initial such as the
   * "A." in "Kripke, Saul A." is preserved.
   */
  private static String cleanNamePart(String str)
  {
    str = PARENTHETICAL.matcher(stripSafe(str)).replaceAll("");

    str = StringUtils.stripEnd(str, " ,;");

    return TRAILING_ABBREV_DOT.matcher(str).replaceAll("").strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The relators on a name field: a relator term is text, a relator code is a code or its URI;
   * either may repeat. A name can carry several, e.g. "translated and edited by", and all of
   * them count.
   *
   * @return the roles for this name, or null if no recordable relator was found. Use
   *         {@link #hasAnyRelator(DataField)} to tell "had relators, none recordable" from
   *         "had no relators at all"; the two need opposite handling.
   */
  private static Roles rolesFrom(DataField field)
  {
    boolean isEditor = false, isTrans = false, sawKnownRole = false;

    for (String relator : relatorsOf(field))
    {
      AuthorType authorType = normalizeRole(relator);

      if (authorType == null) continue;

      sawKnownRole = true;

      if      (authorType == editor    ) isEditor = true;
      else if (authorType == translator) isTrans  = true;
    }

    return sawKnownRole ? new Roles(isEditor, isTrans) : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<String> relatorsOf(DataField field)
  {
    List<String> relators = new ArrayList<>(field.subfields(NameHeading.RELATOR_TERM));
    relators.addAll(field.subfields(NameHeading.RELATOR_CODE));
    return relators;
  }

  private static boolean hasAnyRelator(DataField field) { return field.has(NameHeading.RELATOR_TERM) || field.has(NameHeading.RELATOR_CODE); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Normalizes a relator to an AuthorType. LoC records these inconsistently: as a bare MARC
   * relator code ("aut"), as a full relator URI ("http://id.loc.gov/vocabulary/relators/edt"),
   * or as free text ("author", "editor", "tr", "translator"). Records cataloged under older
   * rules prefix the text form with "joint" ("joint author", "joint ed.") for a co-responsible
   * added entry; the prefix carries no information about the role itself.
   *
   * @return the matching AuthorType, or null if the role is one we do not record
   */
  static AuthorType normalizeRole(String roleStr)
  {
    String str = stripSafe(roleStr).toLowerCase(Locale.ROOT);

    str = StringUtils.substringAfterLast(str, '/').isEmpty() ? str : StringUtils.substringAfterLast(str, '/');

    str = Strings.CS.removeStart(collapseSpaces(str).strip(), "joint ");

    return switch (StringUtils.stripEnd(str, " .,").strip())
    {
      case "aut", "author", "cre", "creator"         -> author;

      case "edt", "editor", "edc", "edm", "ed",
           "editor of compilation",                                 // RDA spells out the relator codes edc and edm
           "editor of moving image work"             -> editor;

      case "trl", "tr", "trans", "translator", "trc" -> translator;

      default                                        -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern EDITED_BY = Pattern.compile("\\bedited\\b"       , Pattern.CASE_INSENSITIVE),
                               TRANS_BY  = Pattern.compile("\\btrans(lated)?\\b", Pattern.CASE_INSENSITIVE);

  /**
   * Infers a role from the statement of responsibility (245 $c), for records where the name
   * has no relator. For example "The Cambridge Companion to Kant" records Paul Guyer's role only
   * as "edited by Paul Guyer."
   *
   * @return the inferred roles, or null if the name is not found or its segment is unmarked
   */
  static Roles rolesFromStatementOfResponsibility(String sor, String lastName)
  {
    if (strNullOrBlank(sor) || strNullOrBlank(lastName)) return null;

    String targetName = convertToEnglishChars(lastName).toLowerCase(Locale.ROOT).strip();

    if (targetName.isBlank()) return null;

    for (String segment : convertToEnglishChars(sor).split(";"))
    {
      if (segment.toLowerCase(Locale.ROOT).contains(targetName) == false) continue;

      // "translated and edited by X" counts as both

      boolean isEditor = EDITED_BY.matcher(segment).find(),
              isTrans  = TRANS_BY .matcher(segment).find();

      return (isEditor || isTrans) ? new Roles(isEditor, isTrans) : null;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The field holding the publication statement: the RDA field whose second indicator marks it
   * as publication, failing that any RDA field that is not the copyright notice, failing that
   * the pre-RDA imprint.
   */
  private static DataField publicationField(MarcRecord marcRecord)
  {
    List<DataField> rdaFields = marcRecord.fields(Publication.PRODUCTION_PUBLICATION_TAG);

    return rdaFields.stream().filter(field -> field.ind2() == Publication.PUBLICATION).findFirst().orElseGet(() ->
           rdaFields.stream().filter(field -> field.ind2() != Publication.COPYRIGHT_NOTICE).findFirst().orElse(marcRecord.firstField(Publication.IMPRINT_TAG)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern COPYRIGHT_MARKER = Pattern.compile("^[\\[\\s]*[cp©℗]\\s*(?=\\d{4})", Pattern.CASE_INSENSITIVE);

  /**
   * Applies the record's dates, lowest priority first. BibDataStandalone.setDate lets a later
   * call of equal or higher DateType priority win, so the coded date in the 008 fixed field is
   * applied last: it is the normalized value and should take precedence over the transcribed
   * forms in 260 and 264, which carry brackets, copyright markers, and the publisher's own
   * wording.
   */
  private void setDatesFrom(MarcRecord marcRecord)
  {
    for (DataField field : marcRecord.fields(Publication.IMPRINT_TAG))
      for (String str : field.subfields(Publication.DATE))
        applyDate(str, isCopyrightDate(str) ? dtCopyright : dtIssued);

    for (DataField field : marcRecord.fields(Publication.PRODUCTION_PUBLICATION_TAG))
      for (String str : field.subfields(Publication.DATE))
        applyDate(str, ((field.ind2() == Publication.COPYRIGHT_NOTICE) || isCopyrightDate(str)) ? dtCopyright : dtIssued);

    // The first date in the fixed field is a four-digit year when known ("2003"); unknown digits
    // are "u" ("19uu"), which BibliographicDate rejects. For a reprint it is the reprint's date,
    // which is the edition in hand.

    String codedDate = marcRecord.controlChars(FixedField.TAG, FixedField.DATE1_START, FixedField.DATE1_END);

    if (StringUtils.isNumeric(codedDate))
      applyDate(codedDate, dtIssued);
  }

//---------------------------------------------------------------------------

  /** A leading c/p/(c)/copyright sign marks a copyright or phonogram date rather than an issue date */
  private static boolean isCopyrightDate(String str) { return COPYRIGHT_MARKER.matcher(stripSafe(str)).find(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void applyDate(String rawStr, DateType dateType)
  {
    BibliographicDate date = parseMarcDate(rawStr);

    if (BibliographicDate.isEmpty(date) == false)
      setDate(date, dateType, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * BibliographicDate.fromUserStr handles nearly every form a transcribed date takes, including
   * bracketed years, trailing periods, and full ISO dates, and correctly rejects placeholders
   * like "19uu". The one gap is a copyright marker glued to the digits ("c2003", "p1998",
   * "[c1992]"), because there is no word boundary between the letter and the first digit.
   * Stripping that marker first is all that is needed.
   */
  static BibliographicDate parseMarcDate(String rawStr)
  {
    String str = stripSafe(rawStr);

    return str.isBlank() ?
      BibliographicDate.EMPTY_DATE
    :
      BibliographicDate.fromUserStr(COPYRIGHT_MARKER.matcher(str).replaceFirst(""));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * LoC records the language as an ISO 639-2/B code ("eng") while Crossref uses 639-1 ("en").
   * Without this, the merge dialog would show a conflict on essentially every book.
   */
  private static final Map<String, String> LANG_3_TO_2 = Map.ofEntries
  (
    Map.entry("eng", "en"), Map.entry("fre", "fr"), Map.entry("fra", "fr"), Map.entry("ger", "de"),
    Map.entry("deu", "de"), Map.entry("spa", "es"), Map.entry("ita", "it"), Map.entry("dut", "nl"),
    Map.entry("nld", "nl"), Map.entry("por", "pt"), Map.entry("rus", "ru"), Map.entry("chi", "zh"),
    Map.entry("zho", "zh"), Map.entry("jpn", "ja"), Map.entry("ara", "ar"), Map.entry("heb", "he"),
    Map.entry("lat", "la"), Map.entry("gre", "el"), Map.entry("ell", "el"), Map.entry("pol", "pl"),
    Map.entry("swe", "sv"), Map.entry("dan", "da"), Map.entry("nor", "no"), Map.entry("fin", "fi"),
    Map.entry("cze", "cs"), Map.entry("ces", "cs"), Map.entry("tur", "tr"), Map.entry("kor", "ko"),
    Map.entry("hun", "hu")
  );

  /**
   * The language of the item is in the fixed field; the language code field repeats it (and
   * lists the others for a multilingual item), while its original-language subfield is a
   * translation's source language, which is not this item's. The fixed field is preferred and
   * the language code field is the fallback for when it is blank or a fill character.
   */
  private static String languageFrom(MarcRecord marcRecord)
  {
    String code = marcRecord.controlChars(FixedField.TAG, FixedField.LANGUAGE_START, FixedField.LANGUAGE_END).strip().toLowerCase(Locale.ROOT);

    if ((code.length() != 3) || code.contains("|"))
      code = marcRecord.firstSubfield(LanguageCode.TAG, LanguageCode.LANGUAGE).strip().toLowerCase(Locale.ROOT);

    return code.isBlank() ? "" : LANG_3_TO_2.getOrDefault(code, code);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setMiscFrom(MarcRecord marcRecord)
  {
    List<String> misc = new ArrayList<>();

    String statementOfResp = marcRecord.firstSubfield(TitleStatement.TAG, TitleStatement.STATEMENT_OF_RESPONSIBILITY).strip();
    if (strNotNullOrBlank(statementOfResp))
      misc.add(statementOfResp);

    // The physical description is free text such as "xxviii, 1276 p. ; 20 cm." It deliberately
    // does not go into bfPages, which is currently a start-end page range that is split on "-".
    // When a structured page-range type exists, this is the one place that needs to change.

    DataField physicalDescription = marcRecord.firstField(PhysicalDescription.TAG);
    if (physicalDescription != null)
    {
      String extent = physicalDescription.join(PhysicalDescription.EXTENT_DETAILS_AND_DIMENSIONS).strip();
      if (strNotNullOrBlank(extent))
        misc.add(extent);
    }

    // The series statement is the series as it appears on the item and the series added entry
    // the authorized form of the same series; the statement is shown, or the authorized form
    // when there is no statement. The volume number is where the item's place in the series
    // lives ("Philosophical papers ; 1"): LoC catalogs the volumes of a multi-volume work as
    // separate records and puts the number here, not in the title.

    List<DataField> seriesFields = new ArrayList<>(marcRecord.fields(Series.STATEMENT_TAG));
    seriesFields.addAll(marcRecord.fields(Series.OBSOLETE_STATEMENT_TAG));
    if (seriesFields.isEmpty()) seriesFields.addAll(marcRecord.fields(Series.ADDED_ENTRY_TAG));

    for (DataField field : seriesFields)
    {
      String seriesTitle = titlePart(field.join(Series.TITLE_AND_VOLUME));
      if (strNotNullOrBlank(seriesTitle))
        misc.add("Series: " + seriesTitle);
    }

    String lccn = lccnFrom(marcRecord);
    if (strNotNullOrBlank(lccn))
      misc.add("LoC catalog: https://lccn.loc.gov/" + lccn);

    if (misc.isEmpty() == false)
      setMultiStr(bfMisc, misc);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The LCCN is padded to a fixed width with spaces ("   2003270054", "    79495176 "); older values may carry a revision suffix after a space */
  private static String lccnFrom(MarcRecord marcRecord)
  {
    String str = marcRecord.firstSubfield(Lccn.TAG, Lccn.NUMBER).strip();
    int ndx = str.indexOf(' ');
    return ndx < 0 ? str : str.substring(0, ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines the entry type from the coded fields, in order of specificity: what the fixed
   * fields say the item is (a thesis, a conference publication, a dictionary), then any
   * genre/form term, then the broad type of record and bibliographic level from the leader.
   * For the types where EntryType distinguishes a monograph from a serial, the bibliographic
   * level picks the variant.
   */
  private static EntryType parseMarcType(MarcRecord marcRecord, boolean editedCollection)
  {
    char recordType = marcRecord.leaderChar(Leader.TYPE_OF_RECORD);

    boolean serial = isSerial(marcRecord);

    EntryType fromCodes = fromFixedFields(marcRecord, recordType, serial);

    if (fromCodes != null) return fromCodes;

    EntryType fromTerms = marcRecord.fields(GenreForm.TAG).stream().map(field -> fromLooseGenreTerm(stripPlural(normGenre(field.subfield(GenreForm.TERM))), serial))
                                                          .filter(Objects::nonNull).findFirst().orElse(null);

    if (fromTerms != null) return fromTerms;

    return switch (recordType)
    {
      case Leader.CARTOGRAPHIC_MATERIAL, Leader.MANUSCRIPT_CARTOGRAPHIC       -> etMap;
      case Leader.NOTATED_MUSIC,         Leader.MANUSCRIPT_NOTATED_MUSIC      -> etMusicScore;
      case Leader.NONMUSICAL_SOUND_RECORDING, Leader.MUSICAL_SOUND_RECORDING  -> etAudioRecording;

      // Projected medium covers film, video, and broadcast alike. The fixed-field pass above
      // already routes motion pictures and videorecordings, so arriving here means LoC did not
      // say which it is; etAudiovisualMaterial is what fromWorkType(wtRecording) yields.

      case Leader.PROJECTED_MEDIUM             -> etAudiovisualMaterial;
      case Leader.TWO_DIMENSIONAL_GRAPHIC      -> etArtwork;
      case Leader.COMPUTER_FILE                -> etSoftware;
      case Leader.MANUSCRIPT_LANGUAGE_MATERIAL -> etManuscript;
      case Leader.LANGUAGE_MATERIAL            -> fromBibliographicLevel(marcRecord, serial, editedCollection);
      default                                  -> etOther;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean isSerial(MarcRecord marcRecord)
  {
    char level = marcRecord.leaderChar(Leader.BIBLIOGRAPHIC_LEVEL);

    return (level == Leader.SERIAL) || (level == Leader.INTEGRATING_RESOURCE) || (level == Leader.SERIAL_COMPONENT_PART);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** For language material, the bibliographic level and the multipart level decide between the book-like types */
  private static EntryType fromBibliographicLevel(MarcRecord marcRecord, boolean serial, boolean editedCollection)
  {
    if (serial) return etSerialPublication;

    if (marcRecord.leaderChar(Leader.MULTIPART_LEVEL) == Leader.SET) return etMultiVolumeWork;

    return editedCollection ? etEditedBook : etBook;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The fixed-field codes that say what kind of thing the item is. For books (language material
   * that is not a serial), the nature-of-contents positions hold up to four codes, and separate
   * positions flag a conference publication and a festschrift and give the literary form. For
   * continuing resources, one position gives the type. For projected media, one position says
   * whether the item is a motion picture or a videorecording. Codes that do not change the
   * answer are deliberately absent so they fall through to the leader.
   */
  private static EntryType fromFixedFields(MarcRecord marcRecord, char recordType, boolean serial)
  {
    if (recordType == Leader.PROJECTED_MEDIUM)
    {
      return switch (marcRecord.controlChar(FixedField.TAG, FixedField.TYPE_OF_VISUAL_MATERIAL))
      {
        case FixedField.MOTION_PICTURE -> etFilm;
        case FixedField.VIDEORECORDING -> etVideoRecording;
        default                        -> null;
      };
    }

    if ((recordType != Leader.LANGUAGE_MATERIAL) && (recordType != Leader.MANUSCRIPT_LANGUAGE_MATERIAL)) return null;

    if (serial)
    {
      // A serial flagged as a conference publication is a proceedings series, whatever its type says

      if (marcRecord.controlChar(FixedField.TAG, FixedField.CONFERENCE_PUBLICATION) == FixedField.FLAG_SET) return etProceedingsSeries;

      return switch (marcRecord.controlChar(FixedField.TAG, FixedField.TYPE_OF_CONTINUING_RESOURCE))
      {
        case FixedField.PERIODICAL, FixedField.JOURNAL -> etJournal;
        case FixedField.MAGAZINE                       -> etMagazine;
        case FixedField.NEWSPAPER                      -> etNewspaper;
        case FixedField.MONOGRAPHIC_SERIES             -> etBookSeries;
        case FixedField.DATABASE                       -> etOnlineDatabase;
        case FixedField.WEBSITE                        -> etWebPage;
        default                                        -> null;
      };
    }

    // Deliberately absent from this group: bibliographies, indexes, and statistics. Those mean
    // the item *contains* such material, not that it is such a work. Spot-checking LoC title
    // searches for "philosophy" shows the bibliographies code set on ordinary philosophy
    // monographs, so mapping it to a reference-book type would mislabel a large share of
    // exactly the books this database is for.

    for (char code : marcRecord.controlChars(FixedField.TAG, FixedField.NATURE_OF_CONTENTS_START, FixedField.NATURE_OF_CONTENTS_END).toCharArray())
    {
      EntryType entryType = switch (code)
      {
        case FixedField.THESES                                   -> etThesis;
        case FixedField.DICTIONARIES, FixedField.ENCYCLOPEDIAS,
             FixedField.HANDBOOKS,    FixedField.DIRECTORIES     -> etReferenceBook;
        case FixedField.CATALOGS                                 -> etCatalog;
        case FixedField.ABSTRACTS                                -> etAbstract;
        case FixedField.LEGAL_ARTICLES, FixedField.OFFPRINTS     -> etJournalArticle;
        case FixedField.REVIEWS                                  -> etCommentary;
        case FixedField.TECHNICAL_REPORTS                        -> etTechnicalReport;
        case FixedField.LAW_REPORTS                              -> etReport;
        case FixedField.PATENT_DOCUMENT                          -> etPatent;
        case FixedField.STANDARDS                                -> etStandard;
        case FixedField.LEGISLATION, FixedField.TREATIES         -> etStatute;
        case FixedField.LEGAL_CASES                              -> etCase;
        case FixedField.SURVEYS_OF_LITERATURE                    -> etSurvey;
        default                                                  -> null;
      };

      if (entryType != null) return entryType;
    }

    // Always the container: the flag means the item contains conference papers, and LoC
    // catalogs volumes and serials, never individual papers.

    if (marcRecord.controlChar(FixedField.TAG, FixedField.CONFERENCE_PUBLICATION) == FixedField.FLAG_SET) return etConferenceProceedings;

    if (marcRecord.controlChar(FixedField.TAG, FixedField.FESTSCHRIFT) == FixedField.FLAG_SET) return etEditedBook;

    return switch (marcRecord.controlChar(FixedField.TAG, FixedField.LITERARY_FORM))
    {
      case FixedField.LETTERS  -> etLetter;
      case FixedField.SPEECHES -> etPresentation;
      default                  -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String normGenre(String str)
  {
    return StringUtils.stripEnd(stripSafe(str).toLowerCase(Locale.ROOT), " .,");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String stripPlural(String str)
  {
    return str.length() > 3 ? Strings.CS.removeEnd(str, "s") : str;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Genre/form terms (655 $a) come from several vocabularies and are noisy (plural, trailing
   * period, sometimes not in English), so they are only consulted after the fixed-field codes
   * miss. The keys look odd because a trailing "s" has already been stripped.
   */
  private static EntryType fromLooseGenreTerm(String term, boolean serial) { return switch (term)
  {
    case "periodical"   -> etJournal;
    case "dictionarie",
         "encyclopedia" -> etReferenceBook;
    case "congresse",
         "conference paper and proceeding"
                        -> serial ? etProceedingsSeries : etConferenceProceedings;
    case "festschrift"  -> etEditedBook;
    case "newspaper"    -> etNewspaper;
    case "map"          -> etMap;
    case "catalog"      -> etCatalog;
    case "abstract"     -> etAbstract;
    case "score"        -> etMusicScore;
    case "software"     -> etSoftware;
    case "manuscript"   -> etManuscript;
    default             -> null;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the SRU query URL. As in GoogleBibData, {@code authKeywords} is an out-parameter:
   * it is cleared and filled here, then reused when scoring candidate records.
   */
  static String getQueryUrl(String title, BibAuthors authors, List<String> authKeywords, CharSequence isbn)
  {
    if (strNotNullOrEmpty(isbn))
      return sruUrl("bath.isbn=" + isbn.toString().replaceAll("[^0-9Xx]", ""), MAX_RECORDS_ISBN);

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

    String cqlTitle = sanitizeForCql(title);

    if (cqlTitle.isBlank() && authKeywords.isEmpty())
      return sruUrl("", MAX_RECORDS_TITLE);

    // Only indexes LoC documents for its FOLIO gateway (loc.gov/z3950/lcserver.html) are used:
    // dc.title, dc.author, and bath.isbn. The bath.title and bath.author indexes still answer
    // identically, but only through an undocumented mapping that an update could drop.

    if (cqlTitle.isBlank())
      return sruUrl("dc.author=\"" + sanitizeForCql(authKeywords.getFirst()) + '"', MAX_RECORDS_TITLE);

    String cql = "dc.title=\"" + cqlTitle + '"';

    if (authKeywords.isEmpty() == false)
    {
      String authName = sanitizeForCql(authKeywords.getFirst());

      if (authName.isBlank() == false)
        cql = cql + " and dc.author=\"" + authName + '"';
    }

    return sruUrl(cql, MAX_RECORDS_TITLE);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String sruUrl(String cql, int maxRecords)
  {
    return SRU_BASE + "?version=1.1&operation=searchRetrieve&recordSchema=marcxml&recordPacking=xml" +
           "&maximumRecords=" + maxRecords + "&query=" + escapeURL(cql, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern CQL_SPECIAL = Pattern.compile("[\"()=<>/\\\\*?]");

  /**
   * CQL has no escape sequence that LoC honors reliably, so characters with syntactic meaning
   * are removed rather than escaped. An apostrophe is not one of them and must be left alone:
   * LoC's phrase index matches "Mind's" whether the apostrophe is present or deleted, but not
   * when it has been turned into a space.
   */
  private static String sanitizeForCql(String str)
  {
    String result = convertToEnglishChars(safeStr(str)).strip();

    result = CQL_SPECIAL.matcher(result).replaceAll(" ");

    return collapseSpaces(result).strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * LoC's title index requires every word of the phrase to be present, so a subtitle that differs
   * at all from the cataloged one ("Essays by Michael Polanyi" against LoC's "essays") finds
   * nothing. A title search that misses is therefore retried with the main title alone.
   *
   * @return the main title to retry with, or null if dropping the subtitle would not change the
   *         query (no subtitle, or one the CQL sanitizing already removed)
   */
  static String mainTitleForRetry(String title)
  {
    title = safeStr(title);

    int ndx = StringUtils.indexOfAny(title, ":?");

    if (ndx <= 0) return null;

    String mainTitle = title.substring(0, ndx).strip();

    return sanitizeForCql(mainTitle).equalsIgnoreCase(sanitizeForCql(title)) ? null : mainTitle;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A caller LoC has blocked for excessive traffic is not refused: it gets HTTP 200 and an HTML
   * page ("LC Distribution Alert: Due to excessive traffic, your access to this system has been
   * blocked"). Without this check the page would read as a search with no results, for every
   * search, with nothing to tell the user why.
   */
  static boolean isAccessBlocked(Document doc)
  {
    if ((doc == null) || (doc.selectFirst("*|numberOfRecords") != null)) return false;

    String text = doc.text().toLowerCase(Locale.ROOT);

    return text.contains("access to this system has been blocked") || text.contains("lc distribution alert");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Ranks the records whose title is close enough to the one searched for. An author match
   * outranks everything else. Next comes a date matching the work record's year, which is what
   * tells an original edition from its reprints when their titles are identical. Only then does
   * title distance decide, and a remaining tie keeps the order LoC returned the records in.
   */
  private static final Comparator<Candidate> CANDIDATE_ORDER =
    Comparator.comparing          (Candidate::authMatch, Comparator.reverseOrder())
              .thenComparing      (Candidate::yearMatch, Comparator.reverseOrder())
              .thenComparingDouble(Candidate::dist);

  /** The distance assigned to a record whose title extends the main title being searched for: accepted, but below an exact match */
  private static final double PREFIX_MATCH_DISTANCE = 0.1;

  private static final int MIN_WORDS_FOR_PREFIX_MATCH = 3;

//---------------------------------------------------------------------------

  static LibraryOfCongressBibData createFromXml(Document doc, String title, String yearStr, List<String> authKeywords, String queryIsbn)
  {
    if (doc == null) return null;

    // Use a prefix-agnostic selector: LoC has served both zs: and srw:, and SRU 2.0 drops the prefix

    Element numberOfRecords = doc.selectFirst("*|numberOfRecords");

    if (numberOfRecords == null)
    {
      // Not an SRU envelope at all. jsoup is lenient enough that a truncated response or an
      // intercepting proxy's error page would otherwise be indistinguishable from a clean miss.

      System.out.println("Library of Congress: response was not an SRU envelope");
      return null;
    }

    if (parseInt(numberOfRecords.text().strip(), 0) == 0) return null;

    Element diagnostics = doc.selectFirst("*|diagnostics");

    if (diagnostics != null)
    {
      System.out.println("Library of Congress SRU diagnostic: " + diagnostics.text().strip());
      return null;
    }

    List<MarcRecord> records = marcRecordsIn(doc);

    if (records.isEmpty()) return null;

    // An ISBN lookup has no title to check against. A title search must check even a lone
    // record: a single hit is no more likely to be the right book than the best of several.

    if (strNullOrBlank(title))
      return new LibraryOfCongressBibData(records.getFirst(), queryIsbn);

    LevenshteinDistance alg = LevenshteinDistance.getDefaultInstance();

    String sortTitle     = HDT_RecordBase.makeSortKeyByType(title, hdtWork),
           mainSortTitle = HDT_RecordBase.makeSortKeyByType(Objects.requireNonNullElse(mainTitleForRetry(title), title), hdtWork);

    boolean mainTitleLongEnough = mainSortTitle.split(" ").length >= MIN_WORDS_FOR_PREFIX_MATCH;

    int year = parseInt(yearStr, 0);

    List<Candidate> candidates = new ArrayList<>();

    for (MarcRecord marcRecord : records)
    {
      LibraryOfCongressBibData curBD = new LibraryOfCongressBibData(marcRecord, queryIsbn);

      String curSortTitle = HDT_RecordBase.makeSortKeyByType(curBD.getStr(bfTitle), hdtWork);

      double dist = titleDistance(alg, sortTitle, curSortTitle);

      // LoC often catalogs what a title page presents as a subtitle as part of the title proper
      // ("Messianic idea in Judaism and other essays on Jewish spirituality", no colon), which
      // the distance measure treats as forty characters of mismatch. A record whose title
      // begins with the whole main title being searched for and goes on from there is a match,
      // ranked just below an exact one. Very short main titles are excluded: "Mind" must not
      // claim "Mind and world".

      if ((dist > LEVENSHTEIN_THRESHOLD) && mainTitleLongEnough && curSortTitle.startsWith(mainSortTitle + ' '))
        dist = PREFIX_MATCH_DISTANCE;

      if (dist > LEVENSHTEIN_THRESHOLD) continue;

      boolean authMatch = (collEmpty(authKeywords) == false) &&
                          curBD.getAuthors().stream().anyMatch(curAuthor ->
                            authKeywords.stream().anyMatch(keyword -> curAuthor.getName().getLast().equalsIgnoreCase(keyword))),

              yearMatch = (year > 0) && (curBD.getDate().year.numericValueWhereMinusOneEqualsOneBC() == year);

      candidates.add(new Candidate(curBD, dist, authMatch, yearMatch));
    }

    return candidates.stream().min(CANDIDATE_ORDER).map(Candidate::bd).orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void doHttpRequest(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs,
                            Consumer<LibraryOfCongressBibData> successHndlr, Consumer<Exception> failHndlr)
  {
    doHttpRequest(httpClient, null, "", null, isbnIt, alreadyCheckedIDs, successHndlr, failHndlr);
  }

  static void doHttpRequest(AsyncHttpClient httpClient, String title, String yearStr, BibAuthors authors, Iterator<String> isbnIt,
                            Set<String> alreadyCheckedIDs, Consumer<LibraryOfCongressBibData> successHndlr, Consumer<Exception> failHndlr)
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

    XmlHttpClient.getDocAsync(url, httpClient, doc ->
    {
      if (isAccessBlocked(doc))
      {
        failHndlr.accept(new AccessBlockedException());
        return;
      }

      LibraryOfCongressBibData bd = createFromXml(doc, title, yearStr, authKeywords, finalIsbn);

      if ((bd == null) && (isbnIt != null) && isbnIt.hasNext())
      {
        doHttpRequest(httpClient, title, yearStr, authors, isbnIt, alreadyCheckedIDs, successHndlr, failHndlr);
        return;
      }

      if ((bd == null) && finalIsbn.isBlank())
      {
        String mainTitle = mainTitleForRetry(title);

        if (mainTitle != null)
        {
          doHttpRequest(httpClient, mainTitle, yearStr, authors, null, alreadyCheckedIDs, successHndlr, failHndlr);
          return;
        }
      }

      successHndlr.accept(bd);

    }, e ->
    {
      if ((e instanceof HttpResponseException hre) && (hre.getStatusCode() == HttpStatusCode.SC_NOT_FOUND))
      {
        successHndlr.accept(null);
        return;
      }

      failHndlr.accept(e);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
