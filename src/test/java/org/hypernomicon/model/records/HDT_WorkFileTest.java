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

package org.hypernomicon.model.records;

import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_WorkFile.FileNameAuthor;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.settings.WorkFileNamingSettingsCtrlr.WorkFileNameComponent;

import org.junit.jupiter.api.*;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.*;
import java.util.stream.Stream;

import static org.hypernomicon.Const.FileNamePrefKey.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.junit.jupiter.api.Assumptions.*;
import static org.junit.jupiter.params.provider.Arguments.arguments;

//---------------------------------------------------------------------------

/**
 * Comprehensive unit tests for the work-file-name construction pipeline in
 * {@link HDT_WorkFile#makeFileName}.
 * <p>
 * The tests cover every component type (author last names, title, year, translators, editors,
 * container title, publisher), the before / within / after separators for each, subtitle stripping,
 * year derivation from a work's date (month and day excluded; a year string that contains a space,
 * such as a BC year), the "treat editors as authors" / POSIX / lowercase checkboxes, the maximum
 * character limit, the per-component "exclude work types" feature, and the behavior when arbitrary
 * combinations of components have no data.
 * <p>
 * They are deliberately written against the externally observable result of {@code makeFileName}
 * rather than its current internal structure, so that they keep their meaning even if the
 * implementation is later refactored in a way that no longer links these behaviors together.
 */
class HDT_WorkFileTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // These mirror WorkFileNameComponentType.prefValue, which is part of the persisted settings
  // format and is therefore stable. The enum field is package-private to the settings package, so
  // it cannot be referenced directly from here.

  private static final int FNC_BLANK     = 0,
                           FNC_AUTHORS   = 1,
                           FNC_TITLE     = 2,
                           FNC_YEAR      = 3,
                           FNC_TRANS     = 4,
                           FNC_EDITORS   = 5,
                           FNC_CONTAINER = 6,
                           FNC_PUBLISHER = 7;

  private static final List<FileNameAuthor> NO_AUTHORS = List.of();

  private static TestHyperDB db;
  private static HDT_WorkType book, paper;

  private static final Map<String, String> origValues = new LinkedHashMap<>();

//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    db = TestHyperDB.instance();

    book  = HDT_WorkType.get(WorkTypeEnum.wtBook );
    paper = HDT_WorkType.get(WorkTypeEnum.wtPaper);

    // Preferences are stored as strings internally, so snapshot/restore uniformly regardless of the
    // value's logical type, leaving the database's real naming settings untouched after the run.

    List<String> keys = new ArrayList<>(List.of(COMPONENT_COUNT, MAX_CHAR, POSIX, LOWERCASE, TREAT_ED_AS_AUTHOR));

    for (int ndx = 1; ndx <= 8; ndx++)
    {
      String key = String.valueOf(ndx);

      keys.add(COMPONENT       + key);
      keys.add(BEFORE_SEP      + key);
      keys.add(WITHIN_SEP      + key);
      keys.add(AFTER_SEP       + key);
      keys.add(EXCL_WORK_TYPES + key);
    }

    for (String key : keys)
      origValues.put(key, db.prefs.get(key, null));
  }

//---------------------------------------------------------------------------

  @AfterAll
  static void tearDownOnce()
  {
    origValues.forEach((key, value) ->
    {
      if (value == null)
        db.prefs.remove(key);
      else
        db.prefs.put(key, value);
    });
  }

//---------------------------------------------------------------------------

  /**
   * Reset the naming flags to a known baseline before each test so that results are deterministic
   * regardless of the template database's defaults. Individual tests override flags as needed.
   */
  @BeforeEach
  void setUp()
  {
    db.prefs.putBoolean(POSIX             , false);
    db.prefs.putBoolean(LOWERCASE         , false);
    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, true );
    db.prefs.putInt    (MAX_CHAR          , 255  );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record Comp(int type, String before, String within, String after, String exclIDs) { }

  private static Comp comp(int type, String before, String within, String after) { return new Comp(type, before, within, after, ""); }

  /**
   * A regular author. The name follows the "Last, First" form produced by
   * {@code PersonName.getLastFirst()}, which is what reaches the filename pipeline.
   */
  private static FileNameAuthor author    (String name) { return new FileNameAuthor(name, false, false); }
  private static FileNameAuthor editor    (String name) { return new FileNameAuthor(name, true , false); }
  private static FileNameAuthor translator(String name) { return new FileNameAuthor(name, false, true ); }

  /**
   * Writes the given naming scheme to preferences and returns the loaded component list.
   */
  private static List<WorkFileNameComponent> scheme(Comp... comps)
  {
    db.prefs.putInt(COMPONENT_COUNT, comps.length);

    for (int ndx = 0; ndx < comps.length; ndx++)
    {
      String key = String.valueOf(ndx + 1);
      Comp cmp = comps[ndx];

      db.prefs.putInt(COMPONENT    + key, cmp.type   ());
      db.prefs.put(BEFORE_SEP      + key, cmp.before ());
      db.prefs.put(WITHIN_SEP      + key, cmp.within ());
      db.prefs.put(AFTER_SEP       + key, cmp.after  ());
      db.prefs.put(EXCL_WORK_TYPES + key, cmp.exclIDs());
    }

    return WorkFileNameComponent.loadFromPrefs();
  }

  private static String fileName(List<FileNameAuthor> authors, HDT_WorkType workType, String year, String title, String container, String publisher, String ext, List<WorkFileNameComponent> components)
  {
    return HDT_WorkFile.makeFileName(authors, workType, year, title, container, publisher, ext, components);
  }

  private static String authorsOnly  (List<FileNameAuthor> authors, List<WorkFileNameComponent> comps) { return fileName(authors   , null, "" , ""   , ""       , ""       , "", comps); }
  private static String titleOnly    (String title                , List<WorkFileNameComponent> comps) { return fileName(NO_AUTHORS, null, "" , title, ""       , ""       , "", comps); }
  private static String yearOnly     (String year                 , List<WorkFileNameComponent> comps) { return fileName(NO_AUTHORS, null, year, ""  , ""       , ""       , "", comps); }
  private static String containerOnly(String container            , List<WorkFileNameComponent> comps) { return fileName(NO_AUTHORS, null, "" , ""   , container, ""       , "", comps); }
  private static String publisherOnly(String publisher            , List<WorkFileNameComponent> comps) { return fileName(NO_AUTHORS, null, "" , ""   , ""       , publisher, "", comps); }

//---------------------------------------------------------------------------
//--- Each component type pulls the correct value ---------------------------
//---------------------------------------------------------------------------

  @Test
  void authorComponentUsesLastName()
  {
    assertEquals("Smith", authorsOnly(List.of(author("Smith, John")), scheme(comp(FNC_AUTHORS, "", " ", ""))));
  }

  @Test
  void editorComponentUsesLastName()
  {
    assertEquals("Doe", authorsOnly(List.of(editor("Doe, Jane")), scheme(comp(FNC_EDITORS, "", " ", ""))));
  }

  @Test
  void translatorComponentUsesLastName()
  {
    assertEquals("Lee", authorsOnly(List.of(translator("Lee, Sam")), scheme(comp(FNC_TRANS, "", " ", ""))));
  }

  @Test
  void titleComponentUsesTitle()
  {
    assertEquals("On Justice", titleOnly("On Justice", scheme(comp(FNC_TITLE, "", " ", ""))));
  }

  @Test
  void containerComponentUsesContainerTitle()
  {
    assertEquals("Mind", containerOnly("Mind", scheme(comp(FNC_CONTAINER, "", " ", ""))));
  }

  @Test
  void yearComponentUsesYear()
  {
    assertEquals("2023", yearOnly("2023", scheme(comp(FNC_YEAR, "", " ", ""))));
  }

  @Test
  void publisherComponentUsesPublisher()
  {
    assertEquals("Routledge", publisherOnly("Routledge", scheme(comp(FNC_PUBLISHER, "", " ", ""))));
  }

//---------------------------------------------------------------------------
//--- Before / within / after separators for every component type -----------
//---------------------------------------------------------------------------

  @Test
  void separatorsForAuthorComponent()
  {
    assertEquals("[Smith_Jones]", authorsOnly(List.of(author("Smith, J"), author("Jones, M")), scheme(comp(FNC_AUTHORS, "[", "_", "]"))));
  }

  @Test
  void separatorsForEditorComponent()
  {
    assertEquals("[Doe_Roe]", authorsOnly(List.of(editor("Doe, J"), editor("Roe, K")), scheme(comp(FNC_EDITORS, "[", "_", "]"))));
  }

  @Test
  void separatorsForTranslatorComponent()
  {
    assertEquals("[Lee_Ng]", authorsOnly(List.of(translator("Lee, S"), translator("Ng, P")), scheme(comp(FNC_TRANS, "[", "_", "]"))));
  }

  @Test
  void separatorsForTitleComponent()
  {
    assertEquals("[On_Justice]", titleOnly("On Justice", scheme(comp(FNC_TITLE, "[", "_", "]"))));
  }

  @Test
  void separatorsForContainerComponent()
  {
    assertEquals("[Mind_Journal]", containerOnly("Mind Journal", scheme(comp(FNC_CONTAINER, "[", "_", "]"))));
  }

  @Test
  void separatorsForPublisherComponent()
  {
    assertEquals("[Oxford_University_Press]", publisherOnly("Oxford University Press", scheme(comp(FNC_PUBLISHER, "[", "_", "]"))));
  }

  /**
   * The year separator test doubles as the "year string that contains a space" case: the within
   * separator replaces the space in a BC year just as it would any other space.
   */
  @Test
  void separatorsForYearComponentWithSpaceInYear()
  {
    String year = new BibliographicDate(0, 0, "500 B.C.", false).getYearStr();
    assertEquals("500 B.C.", year);

    assertEquals("[500_B.C.]", yearOnly(year, scheme(comp(FNC_YEAR, "[", "_", "]"))));
  }

//---------------------------------------------------------------------------
//--- Subtitle stripping for titles and container titles --------------------
//---------------------------------------------------------------------------

  @Test
  void titleSubtitleStrippedAtColon()
  {
    assertEquals("Main", titleOnly("Main: A Subtitle", scheme(comp(FNC_TITLE, "", " ", ""))));
  }

  @Test
  void titleStrippedAtSeparatorDelimiters()
  {
    // The cut is made at the first of : ? | , the subtitle/separator delimiters.

    assertEquals("What"     , titleOnly("What? Really"     , scheme(comp(FNC_TITLE, "", " ", ""))));
    assertEquals("Cognition", titleOnly("Cognition | Notes", scheme(comp(FNC_TITLE, "", " ", ""))));
  }

  @Test
  void titleWithoutDelimiterKeptWhole()
  {
    assertEquals("Plain Title", titleOnly("Plain Title", scheme(comp(FNC_TITLE, "", " ", ""))));
  }

  @Test
  void containerSubtitleStrippedAtColon()
  {
    assertEquals("Journal", containerOnly("Journal: Special Issue", scheme(comp(FNC_CONTAINER, "", " ", ""))));
  }

  @Test
  void containerStrippedAtSeparatorDelimiter()
  {
    assertEquals("Cognition", containerOnly("Cognition | Special Issue", scheme(comp(FNC_CONTAINER, "", " ", ""))));
  }

  @Test
  void slashConvertedToHyphenNotTreatedAsDelimiter()
  {
    // A slash is meaningful (Either/Or, TCP/IP, And/Or), so it is converted rather than cut at.

    assertEquals("Either-Or"       , titleOnly    ("Either/Or"      , scheme(comp(FNC_TITLE    , "", " ", ""))));
    assertEquals("Logic - Language", containerOnly("Logic / Language", scheme(comp(FNC_CONTAINER, "", " ", ""))));
  }

  @Test
  void embeddedIllegalPunctuationRemovedNotTruncated()
  {
    // Illegal characters that are not subtitle separators (such as * and ") are deleted in place by
    // the final sanitization, leaving the surrounding text intact rather than truncating there.

    assertEquals("C-algebra Reports", containerOnly("C*-algebra Reports"  , scheme(comp(FNC_CONTAINER, "", " ", ""))));
    assertEquals("Studies in Logic" , titleOnly    ("Studies in \"Logic\"", scheme(comp(FNC_TITLE    , "", " ", ""))));
  }

  @Test
  void titleAndContainerStrippedIdentically()
  {
    String input = "Logic / Language: A Study";

    assertEquals(
      titleOnly    (input, scheme(comp(FNC_TITLE    , "", " ", ""))),
      containerOnly(input, scheme(comp(FNC_CONTAINER, "", " ", ""))));

    assertEquals("Logic - Language", titleOnly(input, scheme(comp(FNC_TITLE, "", " ", ""))));
  }

  @Test
  void stripSubtitleRules()
  {
    assertEquals("Mind"        , HDT_WorkFile.stripSubtitle("Mind: A Quarterly Review"));
    assertEquals("What"        , HDT_WorkFile.stripSubtitle("What? Really"));
    assertEquals("Cognition"   , HDT_WorkFile.stripSubtitle("Cognition | Notes"));
    assertEquals("Main"        , HDT_WorkFile.stripSubtitle("Main : A Subtitle"));
    assertEquals("Either-Or"   , HDT_WorkFile.stripSubtitle("Either/Or"));
    assertEquals("C*-algebras" , HDT_WorkFile.stripSubtitle("C*-algebras"));
    assertEquals("Plain Title" , HDT_WorkFile.stripSubtitle("Plain Title"));
    assertEquals(""            , HDT_WorkFile.stripSubtitle(null));
  }

//---------------------------------------------------------------------------
//--- Year derivation from a work's date ------------------------------------
//---------------------------------------------------------------------------

  /**
   * A work dated to a specific month and day still contributes only its year to the file name.
   */
  @Test
  void monthAndDayExcludedFromYear()
  {
    String year = new BibliographicDate(15, 3, "2023", false).getYearStr();
    assertEquals("2023", year);

    assertEquals("2023", yearOnly(year, scheme(comp(FNC_YEAR, "", " ", ""))));
  }

//---------------------------------------------------------------------------
//--- Author / editor / translator role separation --------------------------
//---------------------------------------------------------------------------

  @Test
  void authorComponentExcludesEditorsAndTranslators()
  {
    List<FileNameAuthor> people = List.of(author("Adams, A"), editor("Ed, E"), translator("Tr, T"));

    assertEquals("Adams", authorsOnly(people, scheme(comp(FNC_AUTHORS, "", " ", ""))));
  }

  @Test
  void eachRoleComponentSelectsOnlyItsRole()
  {
    List<FileNameAuthor> people = List.of(author("Adams, A"), editor("Ed, E"), translator("Tr, T"));

    String result = fileName(people, null, "", "", "", "", "",
      scheme(comp(FNC_AUTHORS, "A-", " ", ";"), comp(FNC_EDITORS, "E-", " ", ";"), comp(FNC_TRANS, "T-", " ", ";")));

    assertEquals("A-Adams;E-Ed;T-Tr;", result);
  }

  @Test
  void authorNameWithoutCommaUsedWhole()
  {
    assertEquals("Plato", authorsOnly(List.of(author("Plato")), scheme(comp(FNC_AUTHORS, "", " ", ""))));
  }

//---------------------------------------------------------------------------
//--- "If there are no authors, treat editors as authors" -------------------
//---------------------------------------------------------------------------

  @Test
  void editorsTreatedAsAuthorsWhenEnabledAndNoAuthors()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_AUTHORS, "A(", "_", ")"), comp(FNC_EDITORS, "E(", "_", ")"));
    List<FileNameAuthor> editorsOnly = List.of(editor("Doe, J"));

    // Enabled: the editors are rendered with the author component's separators.
    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, true);
    assertEquals("A(Doe)", fileName(editorsOnly, null, "", "", "", "", "", comps));

    // Disabled: the editors keep the editor component's separators.
    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, false);
    assertEquals("E(Doe)", fileName(editorsOnly, null, "", "", "", "", "", comps));
  }

  @Test
  void editorsNotTreatedAsAuthorsWhenARealAuthorIsPresent()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_AUTHORS, "A(", "_", ")"), comp(FNC_EDITORS, "E(", "_", ")"));
    List<FileNameAuthor> people = List.of(author("Smith, J"), editor("Doe, J"));

    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, true);
    assertEquals("A(Smith)E(Doe)", fileName(people, null, "", "", "", "", "", comps));
  }

  @Test
  void editorsNotTreatedAsAuthorsWithoutAnAuthorComponent()
  {
    // The substitution requires an author component to borrow separators from; with none present,
    // the editors keep their own separators even when every contributor is an editor.

    List<WorkFileNameComponent> comps = scheme(comp(FNC_EDITORS, "E(", "_", ")"));

    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, true);
    assertEquals("E(Doe)", fileName(List.of(editor("Doe, J")), null, "", "", "", "", "", comps));
  }

//---------------------------------------------------------------------------
//--- Exclude work types, for each component type ---------------------------
//---------------------------------------------------------------------------

  private static Stream<Arguments> componentTypesWithPresentValue()
  {
    return Stream.of
    (
      arguments(FNC_AUTHORS  , "Smith"),
      arguments(FNC_TITLE    , "Title"),
      arguments(FNC_YEAR     , "2023" ),
      arguments(FNC_TRANS    , "Lee"  ),
      arguments(FNC_EDITORS  , "Doe"  ),
      arguments(FNC_CONTAINER, "Cont" ),
      arguments(FNC_PUBLISHER, "Pub"  )
    );
  }

  @ParameterizedTest
  @MethodSource("componentTypesWithPresentValue")
  void excludedWorkTypesApplyPerComponent(int componentType, String expectedWhenPresent)
  {
    assumeTrue((book != null) && (paper != null), "Template DB lacks the standard work types");

    List<FileNameAuthor> people = List.of(author("Smith, John"), editor("Doe, Jane"), translator("Lee, Sam"));

    List<WorkFileNameComponent> comps = scheme(new Comp(componentType, "", " ", "", String.valueOf(book.getID())));

    // Excluded when the work's type is in the component's exclusion set.
    assertEquals("", fileName(people, book, "2023", "Title", "Cont", "Pub", "", comps));

    // Present for a non-excluded type, and present when the work type is unspecified.
    assertEquals(expectedWhenPresent, fileName(people, paper, "2023", "Title", "Cont", "Pub", "", comps));
    assertEquals(expectedWhenPresent, fileName(people, null , "2023", "Title", "Cont", "Pub", "", comps));
  }

//---------------------------------------------------------------------------
//--- Duplicate and blank components ----------------------------------------
//---------------------------------------------------------------------------

  @Test
  void duplicateComponentTypeRenderedOnlyOnce()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_AUTHORS, "1(", "_", ")"), comp(FNC_AUTHORS, "2(", "_", ")"));

    assertEquals("1(Smith)", authorsOnly(List.of(author("Smith, J")), comps));
  }

  @Test
  void blankComponentIgnored()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_BLANK, "x", "_", "y"), comp(FNC_AUTHORS, "", "_", ""));

    assertEquals("Smith", authorsOnly(List.of(author("Smith, J")), comps));
  }

//---------------------------------------------------------------------------
//--- Every combination of present / absent component data ------------------
//---------------------------------------------------------------------------

  /**
   * With all seven components in the scheme, every one of the 128 combinations of which components
   * have data must produce exactly the concatenation of the rendered pieces for the present
   * components, in scheme order. This confirms, among other things, that a component with no data
   * contributes nothing at all (not even its before / after separators), and that absent components
   * do not disturb the ordering of the present ones.
   */
  @Test
  void everyCombinationOfMissingComponents()
  {
    db.prefs.putBoolean(TREAT_ED_AS_AUTHOR, false);  // keep the editor slot independent of the author slot

    List<WorkFileNameComponent> comps = scheme
    (
      comp(FNC_AUTHORS  , "Au[", "_", "]"),
      comp(FNC_TITLE    , "Ti[", "_", "]"),
      comp(FNC_YEAR     , "Yr[", "_", "]"),
      comp(FNC_TRANS    , "Tr[", "_", "]"),
      comp(FNC_EDITORS  , "Ed[", "_", "]"),
      comp(FNC_CONTAINER, "Co[", "_", "]"),
      comp(FNC_PUBLISHER, "Pu[", "_", "]")
    );

    String[] pieces = { "Au[Aaa]", "Ti[Bbb]", "Yr[2001]", "Tr[Ccc]", "Ed[Ddd]", "Co[Eee]", "Pu[Fff]" };

    for (int mask = 0; mask < 128; mask++)
    {
      List<FileNameAuthor> people = new ArrayList<>();

      if ((mask & (1 << 0)) != 0) people.add(author    ("Aaa"));
      if ((mask & (1 << 3)) != 0) people.add(translator("Ccc"));
      if ((mask & (1 << 4)) != 0) people.add(editor    ("Ddd"));

      String title     = ((mask & (1 << 1)) != 0) ? "Bbb"  : "",
             year      = ((mask & (1 << 2)) != 0) ? "2001" : "",
             container = ((mask & (1 << 5)) != 0) ? "Eee"  : "",
             publisher = ((mask & (1 << 6)) != 0) ? "Fff"  : "";

      StringBuilder expected = new StringBuilder();

      for (int ndx = 0; ndx < pieces.length; ndx++)
        if ((mask & (1 << ndx)) != 0)
          expected.append(pieces[ndx]);

      assertEquals(expected.toString(), fileName(people, null, year, title, container, publisher, "", comps), "mask=" + mask);
    }
  }

//---------------------------------------------------------------------------
//--- Enforce POSIX ---------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void posixStripsSpacesAndPunctuation()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "(", " ", ")"));

    assertEquals("(On Justice)", titleOnly("On Justice", comps));

    db.prefs.putBoolean(POSIX, true);
    assertEquals("OnJustice", titleOnly("On Justice", comps));
  }

  @Test
  void posixStripsLeadingDashes()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "-", " ", ""));

    assertEquals("-X", titleOnly("X", comps));

    db.prefs.putBoolean(POSIX, true);
    assertEquals("X", titleOnly("X", comps));
  }

  @Test
  void posixStripsLeadingDashExposedByCharacterFilter()
  {
    // POSIX removes the '(' here, which would leave the result starting with a dash unless the
    // leading-dash strip runs after the character filter rather than only before it. A leading
    // '-' is treated as an option flag by many command-line tools.

    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "(", " ", ""));

    db.prefs.putBoolean(POSIX, true);
    assertEquals("Draft", titleOnly("-Draft", comps));
  }

  @Test
  void diacriticsAlwaysConvertedToEnglish()
  {
    // convertToEnglishChars runs on every result via removeInvalidFileNameChars, so this holds
    // regardless of the POSIX setting.

    List<WorkFileNameComponent> comps = scheme(comp(FNC_AUTHORS, "", " ", ""));

    assertEquals("Schrodinger", authorsOnly(List.of(author("Schrödinger, Erwin")), comps));

    db.prefs.putBoolean(POSIX, true);
    assertEquals("Schrodinger", authorsOnly(List.of(author("Schrödinger, Erwin")), comps));
  }

//---------------------------------------------------------------------------
//--- Enforce lowercase -----------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void lowercaseAppliedWhenEnabled()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "", "_", ""));

    assertEquals("On_Justice", titleOnly("On Justice", comps));

    db.prefs.putBoolean(LOWERCASE, true);
    assertEquals("on_justice", titleOnly("On Justice", comps));
  }

//---------------------------------------------------------------------------
//--- Maximum number of characters ------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void maxCharTruncatesWithoutExtension()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "", "", ""));

    db.prefs.putInt(MAX_CHAR, 5);
    assertEquals("Const", titleOnly("Constitution", comps));

    db.prefs.putInt(MAX_CHAR, 255);
    assertEquals("Constitution", titleOnly("Constitution", comps));

    db.prefs.putInt(MAX_CHAR, 1000);
    assertEquals("Constitution", titleOnly("Constitution", comps));
  }

  @Test
  void maxCharTruncatesAccountingForExtension()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "", "", ""));

    // The limit covers the extension and its separator, so the stem is shortened to fit.
    db.prefs.putInt(MAX_CHAR, 10);
    assertEquals("Consti.pdf", fileName(NO_AUTHORS, null, "", "Constitution", "", "", "pdf", comps));

    db.prefs.putInt(MAX_CHAR, 255);
    assertEquals("Constitution.pdf", fileName(NO_AUTHORS, null, "", "Constitution", "", "", "pdf", comps));

    db.prefs.putInt(MAX_CHAR, 1000);
    assertEquals("Constitution.pdf", fileName(NO_AUTHORS, null, "", "Constitution", "", "", "pdf", comps));
  }

  @Test
  void maxCharSmallerThanExtensionDoesNotThrow()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "", "", ""));

    // A limit below the extension length cannot be honored (the extension itself is never
    // shortened), but it must not throw; the stem is emptied.
    db.prefs.putInt(MAX_CHAR, 2);
    assertEquals(".pdf", fileName(NO_AUTHORS, null, "", "Constitution", "", "", "pdf", comps));
  }

  @Test
  void nonPositiveMaxCharDoesNotThrow()
  {
    List<WorkFileNameComponent> comps = scheme(comp(FNC_TITLE, "", "", ""));

    db.prefs.putInt(MAX_CHAR, 0);
    assertEquals("", titleOnly("Constitution", comps));

    db.prefs.putInt(MAX_CHAR, -5);
    assertEquals("", titleOnly("Constitution", comps));

    db.prefs.putInt(MAX_CHAR, -5);
    assertEquals(".pdf", fileName(NO_AUTHORS, null, "", "Constitution", "", "", "pdf", comps));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
