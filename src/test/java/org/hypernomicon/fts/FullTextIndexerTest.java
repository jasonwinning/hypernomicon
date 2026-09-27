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

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import java.security.MessageDigest;
import java.util.*;
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.fts.FullTextIndexer.IndexerState;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.util.file.*;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Tests for the index lifecycle of {@link FullTextIndexer}, focused on the in-place
 * reindex behavior: when the indexing configuration changes (schema version bump,
 * analyzer change, library upgrade), the existing index is NOT wiped. Instead, every
 * metadata entry is loaded as stale and each file is re-extracted in place while the
 * old index contents remain searchable. The per-file stale flags are persisted with
 * the metadata snapshot, so an interrupted reindex resumes where it left off rather
 * than starting over. An upgrade of one text extractor is narrower: only the entries
 * that extractor produced go stale.
 * <p>
 * The tests drive real filesystem sessions (plain-text files, so extraction goes
 * through Tika with no pdf.js/Chromium involvement) and observe re-extraction via a
 * content swap that preserves the file's mtime and size: only a bypass of the
 * unchanged-file skip can pick up the new content.
 */
class FullTextIndexerTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int SCHEMA_V1 = 1, SCHEMA_V2 = 2;

  private static final String METADATA_FILENAME = "metadata.json",
                              MANIFEST_FILENAME = "index-manifest.json";

  @TempDir Path tempDir;

  private Path dbRoot, indexDir;
  private RegistryAccessor registry;
  private FullTextIndexer indexer;

//---------------------------------------------------------------------------

  /** This class activates the FilePathRegistry for its own roots; the shared
   *  TestHyperDB session (possibly opened by an earlier test class in the same
   *  JVM) owns the registry otherwise, and populateForTesting refuses to run
   *  while it is online. instance() reopens it for any later test class. */
  @BeforeAll static void closeSharedTestDB()
  {
    TestHyperDB.closeIfOpen();
  }

  @BeforeEach void setUp() throws IOException
  {
    dbRoot   = Files.createDirectory(tempDir.resolve("db"));
    indexDir = Files.createDirectory(tempDir.resolve("index"));
  }

  @AfterEach void tearDown()
  {
    if (indexer != null)
    {
      indexer.close();
      indexer = null;
    }

    FilePathRegistryTestHelper.deactivate();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Path writeDbFile(String name, String content) throws IOException
  {
    Path file = dbRoot.resolve(name);
    Files.writeString(file, content, StandardCharsets.UTF_8);
    return file;
  }

//---------------------------------------------------------------------------

  /** Activates the registry with the db root itself pre-interned alongside the files,
   *  mirroring production populate(), whose walk interns the root directory. Without
   *  this, FilePath.of(dbRoot) misses the registry's normalized-key tier and falls into
   *  the toRealPath tier, which on macOS resolves the JUnit temp dir through the
   *  /var -> /private/var symlink — a different identity space than the raw-interned
   *  files, making the indexer's relativePath() produce ../-style keys. */
  private void activateRegistry(Path... files)
  {
    Path[] paths = new Path[files.length + 1];
    paths[0] = dbRoot;
    System.arraycopy(files, 0, paths, 1, files.length);

    registry = FilePathRegistryTestHelper.activateForTesting(dbRoot, paths);
  }

//---------------------------------------------------------------------------

  /** Opens a session: creates a fresh indexer instance (as a new application launch
   *  would) and brings it online under the given schema version. */
  private FullTextIndexer openSession(int schemaVersion) throws IOException
  {
    return openSession(schemaVersion, null, null);
  }

//---------------------------------------------------------------------------

  /** Opens a session in which one extractor reports the given version instead of
   *  its real one, as it would after an upgrade of that extractor. */
  private FullTextIndexer openSession(int schemaVersion, ExtractorKind upgradedKind, String upgradedVersion) throws IOException
  {
    return openSession(schemaVersion, upgradedKind, upgradedVersion, null);
  }

//---------------------------------------------------------------------------

  /** Opens a session whose live set of indexable file types is the given one instead of
   *  the production set, as a later version that indexes another type would have. */
  private FullTextIndexer openSession(int schemaVersion, Set<String> indexableExtensions) throws IOException
  {
    return openSession(schemaVersion, null, null, indexableExtensions);
  }

//---------------------------------------------------------------------------

  private FullTextIndexer openSession(int schemaVersion, ExtractorKind upgradedKind, String upgradedVersion, Set<String> indexableExtensions) throws IOException
  {
    indexer = new FullTextIndexer();
    indexer.setSchemaVersionForTesting(schemaVersion);

    if (upgradedKind != null)
      indexer.setExtractorVersionForTesting(upgradedKind, upgradedVersion);

    if (indexableExtensions != null)
      indexer.setIndexableExtensionsForTesting(indexableExtensions);

    indexer.bringOnline(FilePath.of(dbRoot), FilePath.of(indexDir), registry);
    return indexer;
  }

//---------------------------------------------------------------------------

  private void closeSession()
  {
    indexer.close();
    indexer = null;
  }

//---------------------------------------------------------------------------

  private static void buildAndAwait(FullTextIndexer idx) throws Exception
  {
    idx.startIndexing(1);
    awaitTrue(() -> idx.getState() == IndexerState.MAINTAINING, "initial build should complete");
  }

//---------------------------------------------------------------------------

  /** Whether a search for {@code queryStr} returns the given relative path. */
  private static boolean found(FullTextIndexer idx, String queryStr, String relPath) throws Exception
  {
    return idx.searchLight(queryStr, 10, null, null, null).results().stream()
      .anyMatch(result -> result.path().equals(relPath));
  }

//---------------------------------------------------------------------------

  private static void awaitTrue(Callable<Boolean> condition, String message) throws Exception
  {
    long deadline = System.currentTimeMillis() + 15_000;

    while (condition.call() == false)
    {
      if (System.currentTimeMillis() > deadline)
        fail(message);

      Thread.sleep(50);
    }
  }

//---------------------------------------------------------------------------

  /** Replaces the file's content while restoring its mtime and preserving its size,
   *  so the unchanged-file skip cannot tell that anything happened. Only a
   *  re-extraction that bypasses the skip can surface the new content. */
  private static void swapContentPreservingIdentity(Path file, String newContent) throws IOException
  {
    FileTime mtime = Files.getLastModifiedTime(file);
    long size = Files.size(file);

    Files.writeString(file, newContent, StandardCharsets.UTF_8);

    assertEquals(size, Files.size(file), "test content swap must preserve file size");
    Files.setLastModifiedTime(file, mtime);
  }

//---------------------------------------------------------------------------

  /** Parses the metadata snapshot, applies the edit, and writes it back. Used to
   *  fabricate legacy and mid-reindex snapshot states. */
  private void editMetadata(Consumer<JsonObj> edit) throws Exception
  {
    Path metadataPath = indexDir.resolve(METADATA_FILENAME);

    JsonObj root = JsonObj.parseJsonObj(Files.readString(metadataPath, StandardCharsets.UTF_8));
    edit.accept(root);

    Files.writeString(metadataPath, root.toString(), StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------

  private static JsonObj entryFor(JsonObj metadataRoot, String relPath)
  {
    return metadataRoot.getArray("files").objStream()
      .filter(obj -> relPath.equals(obj.getStr("path")))
      .findFirst().orElseThrow(() -> new AssertionError("no metadata entry for " + relPath));
  }

//---------------------------------------------------------------------------

  private JsonObj readMetadata() throws Exception
  {
    return JsonObj.parseJsonObj(Files.readString(indexDir.resolve(METADATA_FILENAME), StandardCharsets.UTF_8));
  }

//---------------------------------------------------------------------------

  /**
   * Rewrites the index directory's files into the form version 1.36 (and 1.36.1, which
   * shipped the same indexing code and configuration) left them in.
   * That version stamped the metadata snapshot with a hash of the whole configuration
   * (including the list of indexable extensions) and did not record which extractor
   * handled each file; the configuration itself was only in the manifest file, which
   * had no pdf.js version. The hash is computed here independently, with the formula
   * that version used.
   *
   * @param tikaVersion the Tika version to claim the index was built with, or null for the real one
   * @param extensions  the indexable extensions to claim, or null for the real ones
   */
  private void rewriteIndexFilesAsVersion136(String tikaVersion, List<String> extensions) throws Exception
  {
    Path manifestPath = indexDir.resolve(MANIFEST_FILENAME);
    JsonObj manifest = JsonObj.parseJsonObj(Files.readString(manifestPath, StandardCharsets.UTF_8));

    manifest.put("manifestFormatVersion", Long.valueOf(1));
    manifest.remove("pdfjsVersion");

    if (tikaVersion != null)
      manifest.put("tikaVersion", tikaVersion);

    if (extensions != null)
    {
      JsonArray extArr = new JsonArray();
      extensions.forEach(extArr::add);
      manifest.put("indexableExtensions", extArr);
    }

    String canonical = "indexSchemaVersion=" + manifest.getLong("indexSchemaVersion", -1)
                     + "|analyzerClass=" + manifest.getStr("analyzerClass")
                     + "|indexableExtensions=" + String.join(",", manifest.getArray("indexableExtensions").strStream().toList())
                     + "|luceneVersion=" + manifest.getStr("luceneVersion")
                     + "|tikaVersion=" + manifest.getStr("tikaVersion");

    String configHash = HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(canonical.getBytes(StandardCharsets.UTF_8)));

    manifest.put("configHash", configHash);
    Files.writeString(manifestPath, manifest.toString(), StandardCharsets.UTF_8);

    editMetadata(root ->
    {
      root.remove("builtUnder");
      root.put("configHash", configHash);
      root.getArray("files").objStream().forEach(entry -> entry.remove("extractor"));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void schemaChangeReindexesInPlaceWhileStayingSearchable() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    // The snapshot should record the configuration its entries were built under, and
    // which extractor handled each file

    JsonObj metadataRoot = readMetadata();
    assertEquals(SCHEMA_V1, metadataRoot.getObj("builtUnder").getLong("indexSchemaVersion", -1), "metadata snapshot should record the manifest it was built under");
    assertEquals("tika", entryFor(metadataRoot, "a.txt").getStr("extractor"), "a plain-text file is extracted by Tika");

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    openSession(SCHEMA_V2);

    // The old index must remain searchable across the config change, before any re-extraction

    assertTrue(found(indexer, "alpha", "a.txt"), "index should stay searchable after a schema change, not be wiped");
    assertTrue(indexer.getStatistics().contains("Awaiting re-extraction after configuration change: 1"));

    buildAndAwait(indexer);
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "stale file should be re-extracted despite unchanged mtime and size");
    assertFalse(found(indexer, "alpha", "a.txt"), "re-extraction should replace the old document");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void unchangedConfigurationSkipsUnchangedFiles() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    buildAndAwait(openSession(SCHEMA_V1));

    // Reopen before judging: the build reports completion before its final commit
    // refreshes the searcher, so only a fresh session shows the durable index

    closeSession();
    openSession(SCHEMA_V1);

    assertTrue (found(indexer, "alpha", "a.txt"), "unchanged file should keep its existing document");
    assertFalse(found(indexer, "bravo", "a.txt"), "unchanged file should not have been re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void addingAnIndexableExtensionDoesNotReextractExistingEntries() throws Exception
  {
    Path txtFile  = writeDbFile("a.txt",  "alpha alpha alpha"),
         textFile = writeDbFile("b.text", "charlie charlie charlie");  // plain text under a type the first session does not index

    activateRegistry(txtFile, textFile);

    buildAndAwait(openSession(SCHEMA_V1, Set.of("txt")));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the txt file");

    assertFalse(indexer.isFileIndexed(FilePath.of(textFile)), "a file whose type is outside the live set is not indexed");
    closeSession();

    swapContentPreservingIdentity(txtFile, "bravo bravo bravo");

    openSession(SCHEMA_V1, Set.of("txt", "text"));

    // Only the list of indexable types differs between the sessions: nothing is stale

    assertFalse(indexer.getStatistics().contains("Awaiting re-extraction"), indexer.getStatistics());

    buildAndAwait(indexer);

    awaitTrue(() -> found(indexer, "charlie", "b.text"), "the build should pick up the newly indexable file");

    // Checked only now: the build reports MAINTAINING before its final commit refreshes the
    // searcher, and the new file becoming searchable is what proves that refresh has happened

    assertTrue (found(indexer, "alpha", "a.txt"), "an entry indexed before the change keeps its document");
    assertFalse(found(indexer, "bravo", "a.txt"), "an entry indexed before the change is not re-extracted");
    assertTrue (indexer.isFileIndexed(FilePath.of(textFile)), "the File Manager's search item gate sees the new entry");

    closeSession();

    JsonObj metadataRoot = readMetadata();

    assertEquals("tika", entryFor(metadataRoot, "b.text").getStr("extractor"));
    assertTrue(metadataRoot.getObj("builtUnder").getArray("indexableExtensions").strStream().anyMatch("text"::equals),
               "the snapshot records the set it was built under");
  }

//---------------------------------------------------------------------------

  @Test void snapshotWithNoRecordOfItsConfigurationIsTreatedAsStale() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    // A snapshot that says nothing about the configuration it was built under (as
    // written by the first version with full-text search): every entry must be
    // treated as stale and re-extracted in place

    editMetadata(root -> root.remove("builtUnder"));

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "entries from a legacy snapshot should be re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void tikaUpgradeReextractsOnlyWhatTikaExtracted() throws Exception
  {
    Path fileA = writeDbFile("a.txt", "alpha alpha alpha"),
         fileB = writeDbFile("b.txt", "gamma gamma gamma");

    activateRegistry(fileA, fileB);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt") && found(indexer, "gamma", "b.txt"), "initial build should index both files");
    closeSession();

    // Relabel b.txt's entry as one that pdf.js produced, which stands in for a PDF
    // without bringing a browser into the test

    editMetadata(root -> entryFor(root, "b.txt").put("extractor", "pdfjs"));

    swapContentPreservingIdentity(fileA, "bravo bravo bravo");
    swapContentPreservingIdentity(fileB, "delta delta delta");

    openSession(SCHEMA_V1, ExtractorKind.TIKA, "a newer Tika");

    assertTrue(indexer.getStatistics().contains("Awaiting re-extraction after configuration change: 1"), indexer.getStatistics());

    buildAndAwait(indexer);
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "an entry Tika produced should be re-extracted after a Tika upgrade");

    assertTrue (found(indexer, "gamma", "b.txt"), "an entry pdf.js produced should keep its existing document after a Tika upgrade");
    assertFalse(found(indexer, "delta", "b.txt"), "an entry pdf.js produced should not be re-extracted after a Tika upgrade");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void pdfjsUpgradeReextractsOnlyWhatPdfjsExtracted() throws Exception
  {
    Path fileA = writeDbFile("a.txt", "alpha alpha alpha"),
         fileB = writeDbFile("b.txt", "gamma gamma gamma");

    activateRegistry(fileA, fileB);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt") && found(indexer, "gamma", "b.txt"), "initial build should index both files");
    closeSession();

    editMetadata(root -> entryFor(root, "b.txt").put("extractor", "pdfjs"));

    swapContentPreservingIdentity(fileA, "bravo bravo bravo");
    swapContentPreservingIdentity(fileB, "delta delta delta");

    openSession(SCHEMA_V1, ExtractorKind.PDFJS, "a newer pdf.js");

    assertTrue(indexer.getStatistics().contains("Awaiting re-extraction after configuration change: 1"), indexer.getStatistics());

    buildAndAwait(indexer);
    awaitTrue(() -> found(indexer, "delta", "b.txt"), "an entry pdf.js produced should be re-extracted after a pdf.js upgrade");

    assertTrue (found(indexer, "alpha", "a.txt"), "an entry Tika produced should keep its existing document after a pdf.js upgrade");
    assertFalse(found(indexer, "bravo", "a.txt"), "an entry Tika produced should not be re-extracted after a pdf.js upgrade");

    closeSession();

    // The re-extraction records the extractor that really handled the file

    assertEquals("tika", entryFor(readMetadata(), "b.txt").getStr("extractor"));
  }

//---------------------------------------------------------------------------

  @Test void snapshotFromVersion136CarriesOverWithoutReextraction() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    // The extension list differs from the current one, as it does once a later
    // version makes another file type indexable

    rewriteIndexFilesAsVersion136(null, List.of("pdf", "txt"));

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    openSession(SCHEMA_V1);

    assertFalse(indexer.getStatistics().contains("Awaiting re-extraction"), indexer.getStatistics());

    // The snapshot is rewritten in the current form as soon as it has been read, not
    // at the next periodic save: from here on the manifest file no longer matches the
    // old snapshot, so a session that ended before a save would otherwise leave every
    // entry stale at the next launch

    JsonObj metadataRoot = readMetadata();

    assertNotNull(metadataRoot.getObj("builtUnder"), "the snapshot should be rewritten with the manifest it was built under");
    assertFalse(metadataRoot.containsKey("configHash"));
    assertEquals("tika", entryFor(metadataRoot, "a.txt").getStr("extractor"), "the extractor should be inferred from the extension");

    buildAndAwait(indexer);

    // Reopen before judging: the build reports completion before its final commit
    // refreshes the searcher, so only a fresh session shows the durable index

    closeSession();
    openSession(SCHEMA_V1);

    assertTrue (found(indexer, "alpha", "a.txt"), "an entry carried over from version 1.36 should keep its existing document");
    assertFalse(found(indexer, "bravo", "a.txt"), "an entry carried over from version 1.36 should not be re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void snapshotFromVersion136BuiltWithAnotherTikaMarksOnlyNonPdfEntriesStale() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    rewriteIndexFilesAsVersion136("an older Tika", null);

    // An entry for a PDF, which the extension identifies as pdf.js's. There is no such
    // file, and indexing is never started in this session, so nothing looks for it.

    editMetadata(root ->
    {
      JsonObj pdfEntry = entryFor(root, "a.txt").deepCopy();
      pdfEntry.put("path", "b.pdf");
      root.getArray("files").add(pdfEntry);
    });

    openSession(SCHEMA_V1);

    assertTrue(indexer.getStatistics().contains("Awaiting re-extraction after configuration change: 1"), indexer.getStatistics());

    closeSession();

    JsonObj metadataRoot = readMetadata();

    assertTrue (entryFor(metadataRoot, "a.txt").getBoolean("stale", false));
    assertFalse(entryFor(metadataRoot, "b.pdf").getBoolean("stale", false));
    assertEquals("pdfjs", entryFor(metadataRoot, "b.pdf").getStr("extractor"));
  }

//---------------------------------------------------------------------------

  @Test void snapshotFromVersion136ThatDoesNotMatchTheManifestFileIsTreatedAsStale() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    rewriteIndexFilesAsVersion136(null, null);

    // The manifest file no longer describes what the snapshot was built under

    editMetadata(root -> root.put("configHash", "0000"));

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "entries of a snapshot that cannot be interpreted should be re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void persistedStaleFlagsResumeSelectively() throws Exception
  {
    Path fileA = writeDbFile("a.txt", "alpha alpha alpha"),
         fileB = writeDbFile("b.txt", "gamma gamma gamma");

    activateRegistry(fileA, fileB);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt") && found(indexer, "gamma", "b.txt"), "initial build should index both files");
    closeSession();

    // Fabricate a mid-reindex snapshot: the recorded manifest is current but only a.txt is
    // still stale, as if a reindex was interrupted after b.txt had been re-extracted

    editMetadata(root -> entryFor(root, "a.txt").put("stale", Boolean.TRUE));

    swapContentPreservingIdentity(fileA, "bravo bravo bravo");
    swapContentPreservingIdentity(fileB, "delta delta delta");

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "stale entry should be re-extracted on resume");

    assertTrue (found(indexer, "gamma", "b.txt"), "non-stale entry should keep its existing document");
    assertFalse(found(indexer, "delta", "b.txt"), "non-stale entry should not have been re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void staleNoTextFileGetsFreshAttempt() throws Exception
  {
    Path file = writeDbFile("c.txt", "     ");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    assertFalse(indexer.isFileIndexed(FilePath.of(file)), "whitespace-only file should have no extractable text");
    closeSession();

    // Under the old configuration nothing was extractable; a config change means the
    // extractor may now succeed, so the unchanged NO_TEXT skip must not apply

    swapContentPreservingIdentity(file, "delta");

    buildAndAwait(openSession(SCHEMA_V2));
    awaitTrue(() -> found(indexer, "delta", "c.txt"), "stale NO_TEXT file should get a fresh extraction attempt");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void staleFlagsSurviveASessionWithNoIndexingProgress() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    // A session that sees the new config but never starts indexing (e.g. background
    // indexing disabled, or the app exits first). Its metadata snapshot is written
    // with the CURRENT manifest, so the per-file stale flags it persists are the
    // only thing keeping the pending reindex alive.

    openSession(SCHEMA_V2);
    closeSession();

    buildAndAwait(openSession(SCHEMA_V2));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "stale flags persisted without progress should still drive the reindex");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void statisticsMarkEmptyFilesAmongThoseWithoutText() throws Exception
  {
    Path emptyFile = writeDbFile("empty.txt", ""),
         blankFile = writeDbFile("blank.txt", "     ");

    activateRegistry(emptyFile, blankFile);

    buildAndAwait(openSession(SCHEMA_V1));

    String stats = indexer.getStatistics();

    assertTrue(stats.contains("No extractable text: 2 (empty files: 1)"), stats);
    assertTrue(stats.contains("\n  empty.txt (empty file)"), stats);

    // A file that has content but no text is listed without the marker

    assertTrue(stats.contains("\n  blank.txt"), stats);
    assertFalse(stats.contains("blank.txt (empty file)"), stats);

    closeSession();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
