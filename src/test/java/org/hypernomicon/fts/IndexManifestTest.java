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

package org.hypernomicon.fts;

import java.util.Set;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.fts.IndexManifest.StaleScope;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Tests for how {@link IndexManifest} decides which index entries a configuration
 * change makes stale, and for how it interprets what the 1.36 releases left on disk.
 */
class IndexManifestTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int SCHEMA_VERSION = 2;

  /** The manifest file as the 1.36 release wrote it, and the hash that release stamped on
   *  its metadata snapshots; 1.36.1 shipped the same indexing code and configuration, so it
   *  wrote the same. The hash was computed outside this code base (sha256sum over the
   *  canonical string that release built), so it pins the frozen formula to the real value. */
  private static final String VERSION_136_MANIFEST = """
    {
      "manifestFormatVersion": 1,
      "indexSchemaVersion": 2,
      "analyzerClass": "org.apache.lucene.analysis.standard.StandardTokenizer+LowerCaseFilter+ASCIIFoldingFilter",
      "indexableExtensions": ["doc", "docx", "epub", "htm", "html", "odt", "pdf", "ppt", "pptx", "rtf", "srt", "txt", "vtt"],
      "luceneVersion": "10.5.1",
      "tikaVersion": "4.0.0",
      "configHash": "6d5e0b38c700b8829d2dd532ae4e512c89ac5b06cbab43c483a55f2d3f300565"
    }
    """,

    VERSION_136_CONFIG_HASH = "6d5e0b38c700b8829d2dd532ae4e512c89ac5b06cbab43c483a55f2d3f300565";

//---------------------------------------------------------------------------

  private static IndexManifest current()
  {
    return IndexManifest.computeCurrent(Set.of("pdf", "txt"), SCHEMA_VERSION);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void nothingIsStaleUnderTheSameConfiguration()
  {
    assertTrue(current().staleScopeSince(current()).isNothing());
  }

//---------------------------------------------------------------------------

  @Test void aChangeToTheIndexableExtensionsMakesNothingStale()
  {
    IndexManifest builtUnder = IndexManifest.computeCurrent(Set.of("pdf"), SCHEMA_VERSION);

    assertTrue(current().staleScopeSince(builtUnder).isNothing());
    assertEquals("", current().describeDifferences(builtUnder));
  }

//---------------------------------------------------------------------------

  @Test void anExtractorUpgradeMakesOnlyThatExtractorsEntriesStale()
  {
    for (ExtractorKind upgraded : ExtractorKind.values())
    {
      IndexManifest builtUnder = current().withExtractorVersion(upgraded, "an older version");
      StaleScope scope = current().staleScopeSince(builtUnder);

      assertFalse(scope.isNothing());

      for (ExtractorKind kind : ExtractorKind.values())
        assertEquals(kind == upgraded, scope.includes(kind), upgraded + " upgraded; entry produced by " + kind);

      assertFalse(scope.includes(null), "an entry that no extractor handled does not depend on an extractor's version");

      assertTrue(current().describeDifferences(builtUnder).contains("an older version -> " + current().extractorVersion(upgraded)));
    }
  }

//---------------------------------------------------------------------------

  @Test void aSchemaChangeMakesEveryEntryStale()
  {
    StaleScope scope = current().staleScopeSince(IndexManifest.computeCurrent(Set.of("pdf", "txt"), SCHEMA_VERSION - 1));

    for (ExtractorKind kind : ExtractorKind.values())
      assertTrue(scope.includes(kind));

    assertTrue(scope.includes(null), "a change that affects every entry includes one that no extractor handled");
  }

//---------------------------------------------------------------------------

  @Test void everythingIsStaleWhenTheConfigurationItWasBuiltUnderIsUnknown()
  {
    StaleScope scope = current().staleScopeSince(null);

    assertTrue(scope.includes(ExtractorKind.PDFJS));
    assertTrue(scope.includes(ExtractorKind.TIKA));
    assertTrue(scope.includes(null));
  }

//---------------------------------------------------------------------------

  @Test void aManifestSurvivesARoundTripThroughJson()
  {
    IndexManifest reloaded = IndexManifest.fromJson(current().toJson());

    assertTrue(current().staleScopeSince(reloaded).isNothing());
    assertEquals(current().toJson().toString(), reloaded.toJson().toString());
  }

//---------------------------------------------------------------------------

  @Test void thePdfjsVersionIsReadFromTheBundledLibrary()
  {
    String version = current().extractorVersion(ExtractorKind.PDFJS);

    assertTrue(version.matches("\\d+\\.\\d+\\.\\d+"), "expected the bundled pdf.js library's version, got: " + version);
  }

//---------------------------------------------------------------------------

  @Test void theTikaVersionIsDetected()
  {
    String version = current().extractorVersion(ExtractorKind.TIKA);

    assertTrue(version.matches("\\d+\\.\\d+.*"), "expected the Tika version, got: " + version);
  }

//---------------------------------------------------------------------------

  @Test void aVersion136ManifestIsWhatItsSnapshotWasBuiltUnder() throws Exception
  {
    IndexManifest stored = IndexManifest.fromJson(JsonObj.parseJsonObj(VERSION_136_MANIFEST)),
                  builtUnder = IndexManifest.forLegacySnapshot(stored, VERSION_136_CONFIG_HASH);

    assertNotNull(builtUnder, "the frozen formula should reproduce the hash the 1.36 releases stamped on their snapshots");

    assertEquals("4.0.0"  , builtUnder.extractorVersion(ExtractorKind.TIKA));
    assertEquals("6.3.289", builtUnder.extractorVersion(ExtractorKind.PDFJS), "the 1.36 releases did not record the pdf.js version they bundled");
  }

//---------------------------------------------------------------------------

  @Test void aLegacySnapshotTheManifestDoesNotAccountForIsBuiltUnderNothingKnown() throws Exception
  {
    IndexManifest stored = IndexManifest.fromJson(JsonObj.parseJsonObj(VERSION_136_MANIFEST));

    assertNull(IndexManifest.forLegacySnapshot(stored, "0000"), "a hash the manifest's fields do not produce");
    assertNull(IndexManifest.forLegacySnapshot(stored, ""    ), "a snapshot with no hash at all");
    assertNull(IndexManifest.forLegacySnapshot(null  , VERSION_136_CONFIG_HASH), "no manifest file");

    // A change to any hashed field of the manifest breaks the correspondence

    JsonObj edited = JsonObj.parseJsonObj(VERSION_136_MANIFEST);
    edited.put("tikaVersion", "3.3.1");

    assertNull(IndexManifest.forLegacySnapshot(IndexManifest.fromJson(edited), VERSION_136_CONFIG_HASH));
  }

//---------------------------------------------------------------------------

  @Test void extractorKindIsInferredFromTheExtensionForLegacyEntries()
  {
    assertEquals(ExtractorKind.PDFJS, ExtractorKind.inferFromPath("Papers/Some Paper.PDF"));
    assertEquals(ExtractorKind.TIKA , ExtractorKind.inferFromPath("Papers/notes.docx"));
    assertEquals(ExtractorKind.TIKA , ExtractorKind.inferFromPath("Papers/no extension"));

    assertEquals(ExtractorKind.PDFJS, ExtractorKind.fromJsonName("pdfjs"));
    assertNull(ExtractorKind.fromJsonName(null));
    assertNull(ExtractorKind.fromJsonName("something else"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
