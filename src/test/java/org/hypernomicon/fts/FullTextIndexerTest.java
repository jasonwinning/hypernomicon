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
import java.util.concurrent.Callable;
import java.util.function.Consumer;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import static org.junit.jupiter.api.Assertions.*;

import org.hypernomicon.fts.FullTextIndexer.IndexerState;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.util.file.*;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Tests for the index lifecycle of {@link FullTextIndexer}, focused on the in-place
 * reindex behavior: when the indexing configuration changes (schema version bump,
 * analyzer change, library upgrade), the existing index is NOT wiped. Instead, every
 * metadata entry is loaded as stale and each file is re-extracted in place while the
 * old index contents remain searchable. The per-file stale flags are persisted with
 * the metadata snapshot, so an interrupted reindex resumes where it left off rather
 * than starting over.
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

  private static final String METADATA_FILENAME = "metadata.json";

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
    indexer = new FullTextIndexer();
    indexer.setSchemaVersionForTesting(schemaVersion);
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
//---------------------------------------------------------------------------

  @Test void schemaChangeReindexesInPlaceWhileStayingSearchable() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    // The snapshot should record the config hash its entries were built under

    JsonObj metadataRoot = JsonObj.parseJsonObj(Files.readString(indexDir.resolve(METADATA_FILENAME), StandardCharsets.UTF_8));
    assertFalse(metadataRoot.getStrSafe("configHash").isBlank(), "metadata snapshot should be stamped with the config hash");

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

    assertTrue (found(indexer, "alpha", "a.txt"), "unchanged file should keep its existing document");
    assertFalse(found(indexer, "bravo", "a.txt"), "unchanged file should not have been re-extracted");

    closeSession();
  }

//---------------------------------------------------------------------------

  @Test void metadataWithoutConfigHashIsTreatedAsStale() throws Exception
  {
    Path file = writeDbFile("a.txt", "alpha alpha alpha");
    activateRegistry(file);

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "alpha", "a.txt"), "initial build should index the file");
    closeSession();

    // A snapshot from before config-hash stamping (any pre-existing index in the
    // field): every entry must be treated as stale and re-extracted in place

    editMetadata(root -> root.keySet().remove("configHash"));

    swapContentPreservingIdentity(file, "bravo bravo bravo");

    buildAndAwait(openSession(SCHEMA_V1));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "entries from a legacy snapshot should be re-extracted");

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

    // Fabricate a mid-reindex snapshot: the config hash is current but only a.txt is
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
    // with the CURRENT config hash, so the per-file stale flags it persists are the
    // only thing keeping the pending reindex alive.

    openSession(SCHEMA_V2);
    closeSession();

    buildAndAwait(openSession(SCHEMA_V2));
    awaitTrue(() -> found(indexer, "bravo", "a.txt"), "stale flags persisted without progress should still drive the reindex");

    closeSession();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
