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

package org.hypernomicon.util.file;

import static org.junit.jupiter.api.Assertions.*;

import java.io.IOException;
import java.nio.file.*;
import java.util.function.Function;

import org.hypernomicon.util.file.FilenameRules.CaseFoldingMode;
import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

//---------------------------------------------------------------------------

/**
 * Tests for {@link FilenameRules} including probing verification against actual filesystem behavior.
 * <p>
 * These tests use {@link Files#isSameFile} as the ground truth for filesystem equivalence,
 * then verify that probed rules correctly predict that behavior.
 */
class FilenameRulesTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @TempDir
  Path tempDir;

//---------------------------------------------------------------------------
//region Probing Tests: Verify probed rules match actual filesystem behavior
//---------------------------------------------------------------------------

  @Test
  void probe_detectsCaseSensitivity_matchesFilesystem() throws IOException
  {
    FilenameRules rules = FilenameRules.detect(tempDir);

    // Create a file
    Path lower = tempDir.resolve("testcase.txt");
    Files.createFile(lower);

    try
    {
      Path upper = tempDir.resolve("TESTCASE.TXT");
      boolean filesystemSaysEquivalent = Files.exists(upper) && Files.isSameFile(lower, upper);

      assertEquals(rules.caseInsensitive(), filesystemSaysEquivalent,
          "Probed caseInsensitive=" + rules.caseInsensitive() +
          " but filesystem says equivalent=" + filesystemSaysEquivalent);
    }
    finally
    {
      Files.deleteIfExists(lower);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void probe_detectsNormalization_matchesFilesystem() throws IOException
  {
    FilenameRules rules = FilenameRules.detect(tempDir);

    // é as single codepoint (U+00E9); é as e + combining accent (U+0065 U+0301)
    Path nfc = tempDir.resolve("caf\u00E9.txt"),
         nfd = tempDir.resolve("cafe\u0301.txt");

    try
    {
      Files.createFile(nfc);
    }
    catch (Exception e)
    {
      // Some filesystems may not support these characters: skip test
      Assumptions.assumeTrue(false, "Filesystem doesn't support Unicode normalization test characters");
      return;
    }

    try
    {
      boolean filesystemSaysEquivalent = Files.exists(nfd) && Files.isSameFile(nfc, nfd);

      assertEquals(rules.unicodeCompInsensitive(), filesystemSaysEquivalent,
          "Probed unicodeCompInsensitive=" + rules.unicodeCompInsensitive() +
          " but filesystem says equivalent=" + filesystemSaysEquivalent);
    }
    finally
    {
      Files.deleteIfExists(nfc);
      // nfd might be same file, try anyway
      try { Files.deleteIfExists(nfd); } catch (Exception ignored) { }
    }
  }

//---------------------------------------------------------------------------

  @Test
  void probe_detectsTrailingDotTrimming_matchesFilesystem() throws IOException
  {
    FilenameRules rules = FilenameRules.detect(tempDir);

    Path withoutDot = tempDir.resolve("testdot"),
         withDot = tempDir.resolve("testdot.");

    try
    {
      Files.createFile(withoutDot);
    }
    catch (Exception e)
    {
      Assumptions.assumeTrue(false, "Could not create test file");
      return;
    }

    try
    {
      boolean filesystemSaysEquivalent = Files.exists(withDot) && Files.isSameFile(withoutDot, withDot);

      // trimsTrailingDotsAndSpaces should match filesystem behavior
      if (filesystemSaysEquivalent)
      {
        assertTrue(rules.trimsTrailingDotsAndSpaces(),
            "Filesystem treats 'testdot' and 'testdot.' as equivalent but rules say no trimming");
      }
      else
      {
        assertFalse(rules.trimsTrailingDotsAndSpaces(),
            "Filesystem treats 'testdot' and 'testdot.' as distinct but rules say trimming");
      }
    }
    finally
    {
      Files.deleteIfExists(withoutDot);
      try { Files.deleteIfExists(withDot); } catch (Exception ignored) { }
    }
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_agreesWithFilesystem_forCaseVariants() throws IOException
  {
    FilenameRules rules = FilenameRules.detect(tempDir);
    Function<String, String> normalizer = rules::normalize;

    Path file = tempDir.resolve("KeyMakerTest.txt");
    Files.createFile(file);

    try
    {
      Path upperVariant = tempDir.resolve("KEYMAKERTEST.TXT");
      boolean filesystemSaysEquivalent = Files.exists(upperVariant) && Files.isSameFile(file, upperVariant);

      String key1 = normalizer.apply("KeyMakerTest.txt"),
             key2 = normalizer.apply("KEYMAKERTEST.TXT");
      boolean normalizerSaysEquivalent = key1.equals(key2);

      assertEquals(filesystemSaysEquivalent, normalizerSaysEquivalent,
          "Filesystem says equivalent=" + filesystemSaysEquivalent +
          " but normalizer produced keys '" + key1 + "' and '" + key2 + '\'');
    }
    finally
    {
      Files.deleteIfExists(file);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_agreesWithFilesystem_forNormalizationVariants() throws IOException
  {
    FilenameRules rules = FilenameRules.detect(tempDir);
    Function<String, String> normalizer = rules::normalize;

    // NFC form; NFD form (combining accents)
    String nfcName = "r\u00E9sum\u00E9.txt",
           nfdName = "re\u0301sume\u0301.txt";

    Path file = tempDir.resolve(nfcName);

    try
    {
      Files.createFile(file);
    }
    catch (Exception e)
    {
      Assumptions.assumeTrue(false, "Filesystem doesn't support test characters");
      return;
    }

    try
    {
      Path nfdVariant = tempDir.resolve(nfdName);
      boolean filesystemSaysEquivalent = Files.exists(nfdVariant) && Files.isSameFile(file, nfdVariant);

      String key1 = normalizer.apply(nfcName),
             key2 = normalizer.apply(nfdName);
      boolean normalizerSaysEquivalent = key1.equals(key2);

      assertEquals(filesystemSaysEquivalent, normalizerSaysEquivalent,
          "Filesystem says equivalent=" + filesystemSaysEquivalent +
          " but normalizer produced different equality result");
    }
    finally
    {
      Files.deleteIfExists(file);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void filenameMap_probed_agreesWithFilesystem() throws IOException
  {
    FilenameMap<String> map = new FilenameMap<>(FilenameRules.detect(tempDir));

    Path file = tempDir.resolve("MapTest.txt");
    Files.createFile(file);

    try
    {
      map.put("MapTest.txt", "value");

      Path upperVariant = tempDir.resolve("MAPTEST.TXT");
      boolean filesystemSaysEquivalent = Files.exists(upperVariant) && Files.isSameFile(file, upperVariant),

              mapSaysEquivalent = map.containsKey("MAPTEST.TXT");

      assertEquals(filesystemSaysEquivalent, mapSaysEquivalent,
          "Filesystem says equivalent=" + filesystemSaysEquivalent +
          " but FilenameMap.containsKey returned " + mapSaysEquivalent);
    }
    finally
    {
      Files.deleteIfExists(file);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void detect_throwsNotDirectoryException_whenGivenFile() throws IOException
  {
    Path file = tempDir.resolve("testfile.txt");
    Files.createFile(file);

    try
    {
      assertThrows(NotDirectoryException.class, () -> FilenameRules.detect(file));
    }
    finally
    {
      Files.deleteIfExists(file);
    }
  }

//---------------------------------------------------------------------------

  @Test
  void detect_throwsNullPointerException_whenGivenNull()
  {
    assertThrows(NullPointerException.class, () -> FilenameRules.detect((Path) null));
  }

//---------------------------------------------------------------------------
//endregion
//region Rules-based Tests: Test specific rule configurations
//---------------------------------------------------------------------------

  @Test
  void rules_caseInsensitive_normalizerFoldsCaseForAscii()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertEquals(normalizer.apply("File.TXT"), normalizer.apply("file.txt"));
    assertEquals(normalizer.apply("FILE.TXT"), normalizer.apply("file.txt"));
    assertEquals(normalizer.apply("FiLe.TxT"), normalizer.apply("file.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_caseSensitive_normalizerPreservesCase()
  {
    FilenameRules rules = new FilenameRules(false, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertNotEquals(normalizer.apply("File.txt"), normalizer.apply("file.txt"));
    assertNotEquals(normalizer.apply("FILE.TXT"), normalizer.apply("file.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_caseInsensitive_normalizerFoldsUnicode()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Greek
    assertEquals(normalizer.apply("\u03A3igma.txt"), normalizer.apply("\u03C3igma.txt")); // Σ vs σ

    // Accented Latin
    assertEquals(normalizer.apply("\u00C9.txt"), normalizer.apply("\u00E9.txt")); // É vs é
  }

//---------------------------------------------------------------------------

  @Test
  void rules_unicodeCompInsensitive_normalizesToNFC()
  {
    FilenameRules rules = new FilenameRules(true, true, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // NFC: é as single codepoint; NFD: e + combining acute
    String nfc = "caf\u00E9.txt",
           nfd = "cafe\u0301.txt";

    assertEquals(normalizer.apply(nfc), normalizer.apply(nfd));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_unicodeCompSensitive_preservesNFCvsNFD()
  {
    FilenameRules rules = new FilenameRules(false, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // NFC: é as single codepoint; NFD: e + combining acute
    String nfc = "caf\u00E9.txt",
           nfd = "cafe\u0301.txt";

    // With unicodeCompInsensitive=false, NFC and NFD should remain distinct
    assertNotEquals(normalizer.apply(nfc), normalizer.apply(nfd));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_trimsTrailingDotsAndSpaces()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertEquals(normalizer.apply("file"), normalizer.apply("file."));
    assertEquals(normalizer.apply("file"), normalizer.apply("file.."));
    assertEquals(normalizer.apply("file"), normalizer.apply("file "));
    assertEquals(normalizer.apply("file"), normalizer.apply("file. "));
    assertEquals(normalizer.apply("file"), normalizer.apply("file . "));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_preservesTrailingDotsAndSpaces_whenDisabled()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // With trimsTrailingDotsAndSpaces=false, trailing chars should be preserved
    assertNotEquals(normalizer.apply("file"), normalizer.apply("file."));
    assertNotEquals(normalizer.apply("file"), normalizer.apply("file "));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_fullCaseFolding_expandsEszett()
  {
    FilenameRules rulesSimple = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE),
                  rulesFull = new FilenameRules(true, false, false, false, CaseFoldingMode.FULL);

    Function<String, String> simple = rulesSimple::normalize,
                             full = rulesFull::normalize;

    // With simple folding, ß stays ß
    assertNotEquals(simple.apply("stra\u00DFe.txt"), simple.apply("strasse.txt"));

    // With full folding, ß → ss
    assertEquals(full.apply("stra\u00DFe.txt"), full.apply("strasse.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void rules_ignoresDefaultIgnorables_stripsZeroWidthCharacters()
  {
    FilenameRules rulesIgnore = new FilenameRules(true, false, false, true, CaseFoldingMode.SIMPLE),
                  rulesKeep = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);

    Function<String, String> ignore = rulesIgnore::normalize,
                             keep = rulesKeep::normalize;

    // Zero-width joiner (U+200D)
    String withZWJ = "file\u200D.txt",
           withoutZWJ = "file.txt";

    // With ignoresDefaultIgnorables=true, ZWJ is stripped
    assertEquals(ignore.apply(withZWJ), ignore.apply(withoutZWJ));

    // With ignoresDefaultIgnorables=false, they are different
    assertNotEquals(keep.apply(withZWJ), keep.apply(withoutZWJ));
  }

  @Test
  void rules_ignoresDefaultIgnorables_stripsVariationSelectors()
  {
    FilenameRules rulesIgnore = new FilenameRules(true, false, false, true, CaseFoldingMode.SIMPLE);
    Function<String, String> ignore = rulesIgnore::normalize;

    // Variation selector (U+FE0F): commonly used with emoji
    String withVS = "file\uFE0F.txt",
           withoutVS = "file.txt";

    assertEquals(ignore.apply(withVS), ignore.apply(withoutVS));
  }

  @Test
  void rules_ignoresDefaultIgnorables_stripsZeroWidthNonBreakingSpace()
  {
    FilenameRules rulesIgnore = new FilenameRules(true, false, false, true, CaseFoldingMode.SIMPLE);
    Function<String, String> ignore = rulesIgnore::normalize;

    // Zero-width no-break space / BOM (U+FEFF)
    String withBOM = "\uFEFFfile.txt",
           withoutBOM = "file.txt";

    assertEquals(ignore.apply(withBOM), ignore.apply(withoutBOM));
  }

//---------------------------------------------------------------------------
//endregion
//region Combined Rules Edge Cases
//---------------------------------------------------------------------------

  @Test
  void combinedRules_caseAndNormalization_bothApply()
  {
    FilenameRules rules = new FilenameRules(true, true, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Uppercase É in NFC vs lowercase é in NFD
    String upperNFC = "\u00C9.txt",  // É (single codepoint)
           lowerNFD = "e\u0301.txt"; // e + combining acute

    assertEquals(normalizer.apply(upperNFC), normalizer.apply(lowerNFD));
  }

  @Test
  void combinedRules_caseAndTrimming_bothApply()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertEquals(normalizer.apply("FILE"), normalizer.apply("file."));
    assertEquals(normalizer.apply("FILE"), normalizer.apply("file "));
    assertEquals(normalizer.apply("FILE"), normalizer.apply("file. "));
  }

  @Test
  void combinedRules_normalizationAndTrimming_bothApply()
  {
    FilenameRules rules = new FilenameRules(true, true, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // NFC with trailing dot; NFD without trailing
    String nfcTrailing = "caf\u00E9.",
           nfdClean = "cafe\u0301";

    assertEquals(normalizer.apply(nfcTrailing), normalizer.apply(nfdClean));
  }

  @Test
  void combinedRules_allRulesEnabled_allApply()
  {
    FilenameRules rules = new FilenameRules(true, true, true, true, CaseFoldingMode.FULL);
    Function<String, String> normalizer = rules::normalize;

    // Combine: uppercase, NFD, trailing dot, ZWJ, and ß
    String complex = "STRA\u00DFE\u200De\u0301.",  // STRAßE + ZWJ + NFD é + trailing dot
           simple = "strassee\u0301";              // strasse + NFC é

    // After normalization: both should map to same normalized form
    // Note: ß→ss with FULL folding, ZWJ stripped, é normalized to NFC, trailing trimmed
    assertEquals(normalizer.apply(complex), normalizer.apply(simple));
  }

//---------------------------------------------------------------------------
//endregion
//region Trailing Trim Edge Cases
//---------------------------------------------------------------------------

  @Test
  void rules_trimsTrailing_multipleDotsAndSpaces()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertEquals(normalizer.apply("file"), normalizer.apply("file..."));
    assertEquals(normalizer.apply("file"), normalizer.apply("file   "));
    assertEquals(normalizer.apply("file"), normalizer.apply("file... "));
    assertEquals(normalizer.apply("file"), normalizer.apply("file . . . "));
  }

  @Test
  void rules_trimsTrailing_preservesMiddleDotsAndSpaces()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Dots and spaces in the middle should be preserved
    assertNotEquals(normalizer.apply("file.name"), normalizer.apply("filename"));
    assertNotEquals(normalizer.apply("file name"), normalizer.apply("filename"));
  }

  @Test
  void rules_trimsTrailing_entirelyDotsAndSpaces_becomesEmpty()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Filenames that are entirely dots/spaces become empty after trimming
    assertEquals("", normalizer.apply("..."));
    assertEquals("", normalizer.apply("   "));
    assertEquals("", normalizer.apply(". . ."));
  }

//---------------------------------------------------------------------------
//endregion
//region Unicode Edge Cases
//---------------------------------------------------------------------------

  @Test
  void rules_caseInsensitive_handlesSurrogatePairs()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Deseret alphabet has case pairs in the surrogate pair range
    // U+10428 (DESERET SMALL LETTER LONG I) is lowercase of U+10400 (DESERET CAPITAL LETTER LONG I)
    String upper = "\uD801\uDC00.txt",  // U+10400
           lower = "\uD801\uDC28.txt";  // U+10428

    // Case folding should make them equal
    assertEquals(normalizer.apply(upper), normalizer.apply(lower));
  }

  @Test
  void rules_unicodeCompInsensitive_handlesMultipleCombiningMarks()
  {
    FilenameRules rules = new FilenameRules(true, true, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // á with two combining marks in NFD: a + acute + dot below
    // Same character in NFC: ạ́ (a with dot below and acute, precomposed where possible)
    String multiCombining = "a\u0301\u0323.txt",
           nfcEquivalent = "\u1EA1\u0301.txt";  // ạ + combining acute

    // Both should normalize to the same form
    assertEquals(normalizer.apply(nfcEquivalent), normalizer.apply(multiCombining));
  }

  @Test
  void rules_caseInsensitive_greekFinalSigma()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Greek has two lowercase sigmas: σ (medial) and ς (final)
    // Both should fold to the same form
    String medial = "\u03C3.txt",  // σ
           finals = "\u03C2.txt",  // ς
           upper = "\u03A3.txt";   // Σ

    // σ and Σ should match
    assertEquals(normalizer.apply(upper), normalizer.apply(medial));
    // ς and Σ should also match (case folding converts ς to σ)
    assertEquals(normalizer.apply(upper), normalizer.apply(finals));
  }

//---------------------------------------------------------------------------
//endregion
//region Validation Tests
//---------------------------------------------------------------------------

  @Test
  void normalizer_allowsEmptyString()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertDoesNotThrow(() -> normalizer.apply(""));
    assertEquals("", normalizer.apply(""));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_rejectsDot()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertThrows(IllegalArgumentException.class, () -> normalizer.apply("."));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_rejectsDotDot()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertThrows(IllegalArgumentException.class, () -> normalizer.apply(".."));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_rejectsNulCharacter()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertThrows(IllegalArgumentException.class, () -> normalizer.apply("file\0.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_rejectsForwardSlash()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertThrows(IllegalArgumentException.class, () -> normalizer.apply("path/file.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_rejectsBackslash()
  {
    String os = System.getProperty("os.name", "").toLowerCase();
    Assumptions.assumeTrue(os.contains("win"), "Backslash is only rejected on Windows");

    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertThrows(IllegalArgumentException.class, () -> normalizer.apply("path\\file.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_allowsBackslash_onNonWindows()
  {
    String os = System.getProperty("os.name", "").toLowerCase();
    Assumptions.assumeFalse(os.contains("win"), "Test only runs on non-Windows");

    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    // Backslash is a valid filename character on Unix
    assertDoesNotThrow(() -> normalizer.apply("path\\file.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void normalizer_allowsNull_returnsNull()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    Function<String, String> normalizer = rules::normalize;

    assertNull(normalizer.apply(null));
  }

//---------------------------------------------------------------------------
//endregion
//region FilenameMap with Rules Tests
//---------------------------------------------------------------------------

  @Test
  void filenameMap_withCaseInsensitiveRules_matchesCaseVariants()
  {
    FilenameRules rules = new FilenameRules(true, false, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("File.txt", "value");

    assertEquals("value", map.get("FILE.TXT"));
    assertEquals("value", map.get("file.txt"));
    assertTrue(map.containsKey("FILE.TXT"));
  }

//---------------------------------------------------------------------------

  @Test
  void filenameMap_withCaseSensitiveRules_distinguishesCaseVariants()
  {
    FilenameRules rules = new FilenameRules(false, false, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("File.txt", "value1");
    map.put("FILE.TXT", "value2");

    assertEquals(2, map.size());
    assertEquals("value1", map.get("File.txt"));
    assertEquals("value2", map.get("FILE.TXT"));
  }

//---------------------------------------------------------------------------

  @Test
  void filenameMap_withNormalizationRules_matchesNormalizationVariants()
  {
    FilenameRules rules = new FilenameRules(true, true, false, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    // Put with NFC form
    map.put("caf\u00E9.txt", "coffee");

    // Get with NFD form
    assertEquals("coffee", map.get("cafe\u0301.txt"));
  }

//---------------------------------------------------------------------------

  @Test
  void filenameMap_withTrimmingRules_matchesTrailingVariants()
  {
    FilenameRules rules = new FilenameRules(true, false, true, false, CaseFoldingMode.SIMPLE);
    FilenameMap<String> map = new FilenameMap<>(rules);

    map.put("file", "value");

    assertEquals("value", map.get("file."));
    assertEquals("value", map.get("file.."));
    assertEquals("value", map.get("file "));
  }

//---------------------------------------------------------------------------
//endregion
//region CaseFoldingMode Tests
//---------------------------------------------------------------------------

  @Test
  void caseFoldingMode_fromPrefVal_returnsSimpleForUnknownValue()
  {
    assertEquals(CaseFoldingMode.SIMPLE, CaseFoldingMode.fromPrefVal("unknown"));
    assertEquals(CaseFoldingMode.SIMPLE, CaseFoldingMode.fromPrefVal(""));
    assertEquals(CaseFoldingMode.SIMPLE, CaseFoldingMode.fromPrefVal("FULL")); // case-sensitive
  }

//---------------------------------------------------------------------------

  @Test
  void caseFoldingMode_fromPrefVal_returnsCorrectValue()
  {
    assertEquals(CaseFoldingMode.SIMPLE, CaseFoldingMode.fromPrefVal("simple"));
    assertEquals(CaseFoldingMode.FULL, CaseFoldingMode.fromPrefVal("full"));
  }

//---------------------------------------------------------------------------
//endregion
//region Heuristic Tests
//---------------------------------------------------------------------------

  @Test
  void heuristicForCurrentOs_returnsNonNull()
  {
    FilenameRules rules = FilenameRules.heuristicForCurrentOS();

    assertNotNull(rules);
    assertNotNull(rules.caseFoldingMode());
  }

//---------------------------------------------------------------------------

  /**
   * Tests that the Windows heuristic assumes case-insensitivity as a safe default.
   * <p>
   * Note: While Windows is consistently case-insensitive (unlike macOS/APFS),
   * this still tests heuristic assumptions. For maximum accuracy in edge cases
   * (e.g., WSL, mounted volumes), use {@link FilenameRules#detect(Path)}.
   */
  @Test
  void heuristicForCurrentOs_windowsDefaultsAssumeCaseInsensitive()
  {
    String os = System.getProperty("os.name", "").toLowerCase();
    Assumptions.assumeTrue(os.contains("win"), "Test only runs on Windows");

    FilenameRules rules = FilenameRules.heuristicForCurrentOS();

    assertTrue(rules.caseInsensitive(),
        "Heuristic should assume case-insensitivity on Windows");
    assertTrue(rules.trimsTrailingDotsAndSpaces(),
        "Heuristic should assume Windows trims trailing dots/spaces");
  }

//---------------------------------------------------------------------------

  /**
   * Tests that the macOS heuristic assumes case-insensitivity as a safe default.
   * <p>
   * IMPORTANT: This tests the heuristic's default assumptions, NOT actual macOS behavior.
   * APFS volumes can be configured as case-sensitive or case-insensitive. The heuristic
   * assumes case-insensitivity because that's the most common configuration, but for
   * accurate behavior you should use {@link FilenameRules#detect(Path)} to probe the actual filesystem.
   */
  @Test
  void heuristicForCurrentOs_macDefaultsAssumeCaseInsensitive()
  {
    String os = System.getProperty("os.name", "").toLowerCase();
    Assumptions.assumeTrue(os.contains("mac"), "Test only runs on macOS");

    FilenameRules rules = FilenameRules.heuristicForCurrentOS();

    // These are DEFAULT ASSUMPTIONS, not guaranteed behavior.
    // Use FilenameRules.detect(path) for actual filesystem probing.
    assertTrue(rules.caseInsensitive(),
        "Heuristic should assume case-insensitivity as the common default on macOS");
    assertTrue(rules.unicodeCompInsensitive(),
        "Heuristic should assume normalization-insensitivity as APFS normalizes by default");
  }

//---------------------------------------------------------------------------
//endregion
//---------------------------------------------------------------------------

}
