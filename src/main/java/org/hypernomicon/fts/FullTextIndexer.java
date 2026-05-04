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

import static org.hypernomicon.fts.FileIndexEntry.IndexStatus.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.LowerCaseFilter;
import org.apache.lucene.analysis.miscellaneous.ASCIIFoldingFilter;
import org.apache.lucene.analysis.standard.StandardTokenizer;
import org.apache.lucene.document.*;
import org.apache.lucene.index.*;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.search.*;
import org.apache.lucene.search.uhighlight.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.apache.lucene.util.BytesRef;

import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;
import org.apache.tika.Tika;

import javafx.application.Platform;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.RegistryAccessor;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

/**
 * Core full-text indexer for the database. Receives filesystem events via
 * {@link #queueEvent}, extracts text from supported file types using Apache Tika,
 * and maintains a Lucene index for search.
 * <p>
 * Lifecycle: {@link #bringOnline} opens the Lucene index and makes it searchable;
 * {@link #startIndexing} launches the background indexing thread; {@link #close}
 * commits, saves metadata, and shuts down. The background thread handles
 * event processing, initial build, and periodic consistency checks.
 */
public class FullTextIndexer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public record SearchResult(String path, float score, List<PageMatch> pageMatches, ScoreDoc scoreDoc)
  {
    public record PageMatch(int pageNumber, int startOffset, int endOffset, String snippet, float score, List<HitRange> hitRanges) {}

    public record HitRange(int start, int end) {}
  }

  public record SearchBatch(List<SearchResult> results, boolean hasMore) {}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Mutually-exclusive lifecycle positions of the indexer.
   * <ul>
   *   <li>{@code CLOSED} : not online; nothing open, not queryable. Initial state.
   *   <li>{@code ONLINE_INDEXING_DISABLED} : Lucene index open and searchable, but
   *       no background indexing thread; existing contents are queryable, no new
   *       files are indexed.
   *   <li>{@code BUILDING} : background thread running the initial build.
   *   <li>{@code MAINTAINING} : initial build complete; background thread idle,
   *       awaiting filesystem events.
   *   <li>{@code INCREMENTAL_INDEXING} : background thread processing a batch of
   *       filesystem events after the initial build.
   * </ul>
   * Coarse conditions that span several states ({@link #isQueryable},
   * {@link #isIndexingEnabled}, {@link #isInitialBuildComplete}) are derived predicates,
   * not enum values.
   */
  public enum IndexerState { CLOSED, ONLINE_INDEXING_DISABLED, BUILDING, MAINTAINING, INCREMENTAL_INDEXING }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Bump when application-level indexing behavior changes that the auto-detected
   *  fields (analyzer class, Lucene/Tika version, extensions) cannot catch:
   *  custom tokenization, fuzzy search settings, text preprocessing, document field structure, etc. */
  private static final int INDEX_SCHEMA_VERSION = 1;

  private static final Set<String> INDEXABLE_EXTENSIONS = Set.of
  (
    "pdf", "doc", "docx", "ppt", "pptx", "epub", "html", "htm", "odt", "rtf", "txt", "srt", "vtt"
  );

  public static boolean isIndexableExtension(String ext)
  {
    return (ext != null) && INDEXABLE_EXTENSIONS.contains(ext.toLowerCase());
  }

  private record ExtractionResult(String text, int[] pageOffsets, int pageCount) {}

  private static final FieldType CONTENT_FIELD_TYPE;

  static
  {
    CONTENT_FIELD_TYPE = new FieldType();
    CONTENT_FIELD_TYPE.setIndexOptions(IndexOptions.DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS);
    CONTENT_FIELD_TYPE.setTokenized(true);
    CONTENT_FIELD_TYPE.setStored(true);
    CONTENT_FIELD_TYPE.freeze();
  }

  private static final int MAX_TEXT_LENGTH              = -1,  // No limit
                           MAX_RESULTS_PER_FILE         = 20,
                           SIZE_STABILITY_RETRIES       = 5;

  private static final long COMMIT_INTERVAL_MS          = 30_000,
                            CONSISTENCY_INTERVAL_MS     = 300_000,
                            LARGE_FILE_THRESHOLD        = 50 * 1024 * 1024,  // 50 MB
                            MIN_EXTRACTION_INTERVAL_MS  = 20_000,
                            SIZE_STABILITY_DELAY_MS     = 500,

                            // Per-pool wait for in-flight workers to release the IndexWriter during shutdown.

                            POOL_DRAIN_TIMEOUT_MS       = 10_000,

                            // close() join budgets. The non-fast budget must exceed the background thread's
                            // worst-case shutdown drain (its finally block awaits both worker pools, each up
                            // to POOL_DRAIN_TIMEOUT_MS) plus a margin for the thread to exit, so a clean drain
                            // always completes the join before it fires and close() never operates the writer
                            // while the background thread is still alive. The fast budget is short because
                            // requestStop() already gave the thread a head start.

                            FAST_CLOSE_JOIN_MS          = 3_000,
                            CLOSE_JOIN_MS               = (2 * POOL_DRAIN_TIMEOUT_MS) + 5_000;

  private final LinkedBlockingQueue<IndexEvent> eventQueue = new LinkedBlockingQueue<>();

  /** Paths with a CREATE or MODIFY event currently in the queue or being
   *  processed. Used to drop duplicate events before they reach the queue, so
   *  a burst of filesystem events from a single save (cloud sync metadata
   *  touches, Windows save patterns that produce DELETE+CREATE, etc.)
   *  coalesces to one extraction. A path enters the set when {@link #queueEvent}
   *  first sees it and leaves when {@link #processEvent} finishes handling it. */
  private final Set<String> inFlightPaths = ConcurrentHashMap.newKeySet();

  /** Per-path timestamp of the most recent extraction start. Used to rate-limit
   *  re-extraction of the same file to at most once per
   *  {@link #MIN_EXTRACTION_INTERVAL_MS}. Files being modified continuously
   *  (stress tests, unusual workflows) will still be re-indexed, just not more
   *  often than the interval allows; any staleness is bounded by it. */
  private final Map<String, Long> lastExtractionStart = new ConcurrentHashMap<>();

  /** Tracks per-file indexing state (mtime, size, extraction status) for build
   *  resumption and change detection. When a build is interrupted, the next
   *  startup loads this map from {@code metadata.json} and skips files whose
   *  mtime and size are unchanged, avoiding redundant text extraction.
   *  <p>
   *  This is intentionally separate from the Lucene index. Lucene stores
   *  searchable content; this map stores filesystem identity for O(1) change
   *  detection. Storing mtime/size as Lucene stored fields instead would
   *  require a per-file term lookup on startup to reconstruct the same map,
   *  versus a single JSON deserialize into a HashMap.
   *  @see #writeMetadataSnapshot(String)
   *  @see #loadMetadata() */
  private final ConcurrentHashMap<String, FileIndexEntry> metadataMap = new ConcurrentHashMap<>();

  /** In-session skip set for files that failed extraction. Populated from
   *  {@link #metadataMap} NO_TEXT entries on startup by {@link #loadMetadata()};
   *  FAILED entries are deliberately excluded so they get a retry during the
   *  initial build. Also populated during the current session when extraction
   *  fails. Cleared for a file on CREATE or MODIFY events. */
  private final Set<String> extractionFailures = ConcurrentHashMap.newKeySet();

  private RegistryAccessor registry;
  private IndexManifest currentManifest;
  private Tika tika;
  private Directory luceneDir;
  private IndexWriter writer;
  private SearcherManager searcherMgr;
  private Analyzer analyzer;
  private HyperThread backgroundThread;
  private FilePath dbRoot, indexDir, manifestPath;
  private volatile int threadCount;
  private long lastCommitTime, lastConsistencyTime;

  private volatile ExecutorService buildWorkerPool, buildLargeFileExecutor;
  private volatile ScheduledExecutorService buildProgressReporter;
  private volatile Runnable statusListener;

  private volatile IndexerState state = IndexerState.CLOSED;
  private volatile boolean stopRequested;
  private volatile int buildTotalFiles, buildProcessedFiles;

//---------------------------------------------------------------------------

  /**
   * Signal the indexer to stop processing without waiting for threads to finish
   * or closing resources. Call this early in the shutdown sequence so the indexer
   * stops doing work while other shutdown steps proceed; call {@link #close} later
   * to join threads and close the IndexWriter.
   *
   * <p>Sets the {@code stopRequested} flag so that {@code close()} takes the fast
   * path: shorter join timeout, {@code writer.rollback()} instead of
   * {@code writer.commit()}, and no {@code writeMetadataSnapshot()} call. This keeps
   * application shutdown responsive when a full rebuild is in progress. At most
   * ~30 seconds of indexed work (one commit interval) is lost; the next startup
   * recaptures it via the mtime check in {@code initialBuild()}.</p>
   */
  public void requestStop()                        { stopRequested = true; }

  public int getIndexedFileCount()                 { return metadataMap.size(); }
  public int getBuildTotalFiles()                  { return buildTotalFiles; }
  public int getBuildProcessedFiles()              { return buildProcessedFiles; }
  public IndexerState getState()                   { return state; }
  public void setStatusListener(Runnable listener) { this.statusListener = listener; }

  /** Whether the Lucene index is open and searchable; true in every state except {@code CLOSED}. */
  public boolean isQueryable()                     { return state != IndexerState.CLOSED; }

  /** Whether the background indexing thread is active (building, maintaining, or processing events). */
  public boolean isIndexingEnabled()               { return (state == IndexerState.BUILDING) || (state == IndexerState.MAINTAINING) || (state == IndexerState.INCREMENTAL_INDEXING); }

  /** Whether the initial build has finished. */
  private boolean isInitialBuildComplete()         { return (state == IndexerState.MAINTAINING) || (state == IndexerState.INCREMENTAL_INDEXING); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void queueEvent(IndexEvent event)
  {
    if (isIndexingEnabled() == false) return;

    // Dedupe CREATE and MODIFY events for the same path while one is already
    // queued or in-flight. Cloud sync services commonly fire multiple events
    // for a single logical save (upload state + xattr update + placeholder
    // hydration + etc.), and Windows save patterns produce DELETE+CREATE
    // cycles that look like fresh files but are really the same content.
    // Only the first event needs to trigger work; the rest would just
    // re-extract the same file.

    IndexEvent.Kind kind = event.kind();

    if ((kind == IndexEvent.Kind.MODIFY) || (kind == IndexEvent.Kind.CREATE))
      if (inFlightPaths.add(relativePath(event.newPath())) == false)
        return;

    eventQueue.add(event);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Bring the indexer online: open the Lucene index and make it searchable. This
   * does the unconditional setup (schema housekeeping, Lucene open, metadata load)
   * and leaves the indexer in {@code ONLINE_INDEXING_DISABLED}.
   * To start active background indexing, the caller then invokes
   * {@link #startIndexing}; if it does not, the existing index contents stay
   * searchable but no new files are indexed.
   *
   * @param dbRoot   the database root folder
   * @param indexDir the index storage directory
   * @param registry the file path registry for iterating database files
   */
  public void bringOnline(FilePath dbRoot, FilePath indexDir, RegistryAccessor registry) throws IOException
  {
    this.dbRoot = dbRoot;
    this.indexDir = indexDir;
    this.registry = registry;

    FilePath lucenePath = indexDir.resolve("lucene");
    Files.createDirectories(lucenePath.toPath());

    manifestPath = indexDir.resolve("index-manifest.json");
    FilePath metadataPath = indexDir.resolve("metadata.json");

    // Schema versioning: detect config changes and wipe stale index.
    // Only wipe when the stored manifest exists but mismatches (genuine schema change).
    // A missing manifest is not a reason to wipe; the build resumes via metadataMap.

    currentManifest = IndexManifest.computeCurrent(INDEXABLE_EXTENSIONS, INDEX_SCHEMA_VERSION);
    IndexManifest storedManifest = IndexManifest.loadFrom(manifestPath);

    if ((storedManifest != null) && (currentManifest.matches(storedManifest) == false))
    {
      System.out.println("Full-text indexer: schema mismatch: " + currentManifest.describeDifferences(storedManifest));

      if (isLucenePopulated(lucenePath) || metadataPath.exists())
      {
        System.out.println("Full-text indexer: wiping stale index due to schema change");
        FileDeletion.ofDirContentsOnly(lucenePath).nonInteractiveLogErrors().execute();
        FileDeletion.ofFile(metadataPath).nonInteractiveLogErrors().execute();
        Files.createDirectories(lucenePath.toPath());
      }
    }

    // Metadata/Lucene mismatch rule: if one exists without the other, wipe both

    boolean lucenePopulated = isLucenePopulated(lucenePath),
            metadataExists = metadataPath.exists();

    if (lucenePopulated != metadataExists)
    {
      System.out.println("Full-text indexer: metadata/Lucene mismatch (metadata=" + metadataExists
        + ", lucene=" + lucenePopulated + "); wiping both");
      FileDeletion.ofDirContentsOnly(lucenePath).nonInteractiveLogErrors().execute();
      FileDeletion.ofFile(metadataPath).nonInteractiveLogErrors().execute();
      Files.createDirectories(lucenePath.toPath());
    }

    // Write manifest now so that even a partial build is recognized as valid
    // on next startup. If the config changes later, the mismatch triggers a wipe.

    currentManifest.saveTo(manifestPath);

    Logger.getLogger("org.apache.lucene.internal.vectorization.VectorizationProvider").setLevel(Level.SEVERE);

    // Custom analyzer: StandardTokenizer + LowerCaseFilter + ASCIIFoldingFilter, no stop
    // word removal. ASCIIFoldingFilter folds diacritics to ASCII so search is accent
    // insensitive (e.g., "Dupre" matches "Dupré"). Stop words are deliberately omitted:
    // they caused phrase searches to silently match wrong results (e.g., "was born of a
    // woman" matching "born to a woman" because "was", "of", "a", "to" were all removed).

    analyzer = new Analyzer()
    {
      @SuppressWarnings("resource")
      @Override protected TokenStreamComponents createComponents(String fieldName)
      {
        StandardTokenizer tokenizer = new StandardTokenizer();
        return new TokenStreamComponents(tokenizer, new ASCIIFoldingFilter(new LowerCaseFilter(tokenizer)));
      }
    };

    luceneDir = FSDirectory.open(lucenePath.toPath());

    IndexWriterConfig config = new IndexWriterConfig(analyzer);
    config.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);
    writer = new IndexWriter(luceneDir, config);

    searcherMgr = new SearcherManager(writer, null);

    System.out.println("Full-text indexer: index directory = " + indexDir);

    loadMetadata();

    state = IndexerState.ONLINE_INDEXING_DISABLED;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Start active background indexing. Initializes Tika and launches the background
   * thread, which runs the initial build and then processes filesystem events.
   * Transitions the indexer from {@code ONLINE_INDEXING_DISABLED} to {@code BUILDING};
   * must be called after {@link #bringOnline}.
   *
   * @param threadCount number of worker threads for the initial build; -1 for automatic, 0 or 1 for single-threaded
   */
  public void startIndexing(int threadCount)
  {
    this.threadCount = threadCount;

    tika = new Tika();
    tika.setMaxStringLength(MAX_TEXT_LENGTH);

    stopRequested = false;
    lastCommitTime = System.currentTimeMillis();
    lastConsistencyTime = lastCommitTime;

    state = IndexerState.BUILDING;

    backgroundThread = new HyperThread("FullTextIndex", this::backgroundLoop);
    backgroundThread.setDaemon(true);
    backgroundThread.setPriority(Thread.MIN_PRIORITY);
    backgroundThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Close the indexer, join all threads, and release all resources.
   *
   * <p>If {@link #requestStop()} was called beforehand (two-phase shutdown),
   * takes a fast path: shorter join timeout, {@code writer.rollback()} instead
   * of commit, no metadata save. This loses at most ~30 seconds of work (one
   * commit interval); the next startup recaptures it via the mtime check.
   * {@code rollback()} leaves the index in a clean state at the last committed
   * point, so no Lucene crash recovery is needed.</p>
   *
   * <p>If called directly (no prior {@code requestStop()}), performs a full
   * {@code writer.commit()} and {@code writeMetadataSnapshot()} so nothing is lost.</p>
   *
   * <p>Safe to call in any state: building/maintaining, online with indexing
   * disabled (resources open but no background thread), or already closed. All
   * fields are null-checked.</p>
   *
   * @see #requestStop()
   */
  public void close()
  {
    boolean fast = stopRequested;
    stopRequested = true;

    // Signal multi-threaded build executors to stop. Workers check the
    // stopRequested flag and exit within one file-processing cycle. Use shutdown()
    // (not shutdownNow()) to avoid interrupting threads mid-Lucene-write;
    // ClosedByInterruptException on NIO channels puts the IndexWriter into
    // an unrecoverable state.

    if (buildProgressReporter  != null) { buildProgressReporter .shutdown(); buildProgressReporter = null; }
    if (buildWorkerPool        != null)   buildWorkerPool       .shutdown();
    if (buildLargeFileExecutor != null)   buildLargeFileExecutor.shutdown();

    // Join the background thread, which awaits worker termination in its finally block
    // before returning. The non-fast join budget exceeds that block's worst-case drain,
    // so a clean shutdown finishes the join before it fires; only then is it safe to
    // operate the IndexWriter below, with no thread still writing to it.

    if (backgroundThread != null)
    {
      eventQueue.add(IndexEvent.shutdown());

      try { backgroundThread.join(fast ? FAST_CLOSE_JOIN_MS : CLOSE_JOIN_MS); }
      catch (InterruptedException e) { Thread.currentThread().interrupt(); }

      // If the thread is still alive, a worker is stuck past the drain window. The writer
      // commit/close below would then run concurrently with the background thread; this is
      // why commitAndSave() no-ops once stopRequested is set (so the background thread is
      // not committing here). Log the case so the rare stuck worker is visible.

      if (backgroundThread.isAlive())
        System.out.println("Full-text indexer: background thread still alive after "
          + (fast ? FAST_CLOSE_JOIN_MS : CLOSE_JOIN_MS) + "ms join; proceeding with teardown");

      backgroundThread = null;
    }

    buildWorkerPool = null;
    buildLargeFileExecutor = null;

    // Transition to CLOSED only after the background thread has been joined.
    // The BG thread also writes `state` (initialBuild, drainAndProcessEvents);
    // setting CLOSED before the join would race with those writes and could
    // leave a stale non-CLOSED state after close() returns. stopRequested, set
    // above, is the actual signal the BG thread and workers obey, so shutdown
    // does not depend on this write happening early.

    state = IndexerState.CLOSED;

    try
    {
      if (writer != null)
      {
        if (fast)
        {
          // Fast path: discard uncommitted changes and close the writer.
          // The index remains clean at the last committed point. Lost work
          // (at most one commit interval) is recaptured on next startup.

          writer.rollback();
        }
        else
        {
          try
          {
            writer.commit();
            writeMetadataSnapshot(buildMetadataJson());
          }
          finally
          {
            writer.close();
          }
        }

        writer = null;
      }

      if (searcherMgr != null) { searcherMgr.close(); searcherMgr = null; }
      if (luceneDir   != null) { luceneDir  .close(); luceneDir   = null; }
    }
    catch (IOException e)
    {
      logThrowable(e);
    }

    analyzer = null;
    tika = null;
    currentManifest = null;
    manifestPath = null;
    metadataMap.clear();
    eventQueue.clear();
    extractionFailures.clear();

    fireStatusListener();
    statusListener = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void backgroundLoop()
  {
    while (stopRequested == false)
    {
      try
      {
        if ((drainAndProcessEvents(1000) == false) && (isInitialBuildComplete() == false))
          initialBuild();

        long now = System.currentTimeMillis();

        if (isInitialBuildComplete() && ((now - lastConsistencyTime) >= CONSISTENCY_INTERVAL_MS))
        {
          consistencyCheck();
          lastConsistencyTime = System.currentTimeMillis();
        }

        if ((now - lastCommitTime) >= COMMIT_INTERVAL_MS)
          commitAndSave();
      }
      catch (InterruptedException e)
      {
        Thread.currentThread().interrupt();
        break;
      }
      catch (Exception e)
      {
        System.out.println("Full-text indexer: background thread error");
        logThrowable(e);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean drainAndProcessEvents(long timeoutMs) throws IOException, InterruptedException
  {
    IndexEvent event = eventQueue.poll(timeoutMs, TimeUnit.MILLISECONDS);
    if (event == null) return false;

    // Reflect incremental indexing in the status display, but only when not in
    // the middle of the initial build. During BUILDING, events are still
    // processed (a deletion mid-build must still be honored) but the state
    // stays BUILDING so the status display keeps showing initial-build progress.

    boolean wasMaintaining = (state == IndexerState.MAINTAINING);

    if (wasMaintaining)
    {
      state = IndexerState.INCREMENTAL_INDEXING;
      fireStatusListener();
    }

    processEvent(event);

    List<IndexEvent> batch = new ArrayList<>();
    eventQueue.drainTo(batch);

    for (IndexEvent e : batch)
      processEvent(e);

    // Delay the transition back so the UI has time to show the animation

    if (wasMaintaining)
      Platform.runLater(() ->
      {
        if (state == IndexerState.INCREMENTAL_INDEXING)
          state = IndexerState.MAINTAINING;

        fireStatusListener();
      });

    searcherMgr.maybeRefresh();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void processEvent(IndexEvent event) throws IOException
  {
    switch (event.kind())
    {
      case CREATE, MODIFY ->
      {
        if (event.isDir()) return;

        FilePath filePath = event.newPath();
        String relPath = relativePath(filePath);

        try
        {
          indexWithMitigations(filePath, relPath);
        }
        finally
        {
          inFlightPaths.remove(relPath);
        }
      }

      case DELETE ->
      {
        if (event.isDir())
        {
          String prefix = relativePath(event.oldPath()) + '/';
          List<String> toRemove = metadataMap.keySet().stream().filter(key -> key.startsWith(prefix)).toList();

          for (String key : toRemove)
            removeFile(key);
        }
        else
        {
          String relPath = relativePath(event.oldPath());

          if (metadataMap.containsKey(relPath))
            removeFile(relPath);
        }
      }

      case MOVE ->
      {
        if (event.isDir())
        {
          String oldPrefix = relativePath(event.oldPath()) + '/',
                 newPrefix = relativePath(event.newPath()) + '/';

          Map<String, FileIndexEntry> toRename = new LinkedHashMap<>();

          for (Map.Entry<String, FileIndexEntry> entry : metadataMap.entrySet())
            if (entry.getKey().startsWith(oldPrefix))
              toRename.put(entry.getKey(), entry.getValue());

          for (Map.Entry<String, FileIndexEntry> entry : toRename.entrySet())
          {
            String oldRelPath = entry.getKey(),
                   newRelPath = newPrefix + oldRelPath.substring(oldPrefix.length());

            renameFile(oldRelPath, newRelPath);
          }
        }
        else
        {
          String oldRelPath = relativePath(event.oldPath());

          if (metadataMap.containsKey(oldRelPath))
            renameFile(oldRelPath, relativePath(event.newPath()));
          else if (isIndexable(event.newPath()))
            indexFile(event.newPath());
        }
      }

      case OVERFLOW ->
      {
        if (isInitialBuildComplete())
          consistencyCheck();
      }

      case SHUTDOWN -> { /* handled by stopRequested flag check in backgroundLoop */ }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether {@code filePath}'s current mtime and size match what {@code existing}
   * recorded; i.e. the file has not changed on disk since it was indexed.
   * Only the comparison is shared: callers handle the {@link IOException}
   * from the filesystem probe differently (skip, continue, attempt indexing).
   */
  private static boolean isFileUnchanged(FilePath filePath, FileIndexEntry existing) throws IOException
  {
    return (existing.mtime() == filePath.lastModified().toEpochMilli()) && (existing.size() == filePath.size());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Shared CREATE/MODIFY handling. Caller is responsible for the
   * {@link #inFlightPaths} entry: it's added in {@link #queueEvent} and removed
   * in the caller's finally block. Applies the mtime+size skip, rate-limit,
   * and size-stability mitigations before extracting.
   */
  private void indexWithMitigations(FilePath filePath, String relPath) throws IOException
  {
    if (isIndexable(filePath) == false) return;

    // Skip if the file hasn't actually changed since the last index. Covers
    // the common cloud-sync case where MODIFY fires for an attribute-only
    // touch (xattr update, sync-state bookkeeping), and the Windows
    // DELETE+CREATE save pattern when the resulting file matches what we
    // already indexed.

    FileIndexEntry existing = metadataMap.get(relPath);
    if ((existing != null) && (existing.status() == INDEXED))
    {
      try
      {
        if (isFileUnchanged(filePath, existing)) return;
      }
      catch (IOException e) { /* fall through and attempt indexing */ }
    }

    // Rate-limit re-extraction of the same file. Preserves responsiveness to
    // single-save edits while preventing continuous-save workflows (stress
    // tests, save-on-annotation PDF readers, cloud-sync churn) from
    // saturating the extractor. Index staleness is bounded by
    // MIN_EXTRACTION_INTERVAL_MS.

    Long lastStart = lastExtractionStart.get(relPath);
    if (lastStart != null)
    {
      long sinceLast = System.currentTimeMillis() - lastStart;
      if (sinceLast < MIN_EXTRACTION_INTERVAL_MS)
      {
        try { Thread.sleep(MIN_EXTRACTION_INTERVAL_MS - sinceLast); }
        catch (InterruptedException e) { Thread.currentThread().interrupt(); return; }
      }
    }

    // Wait for the file to finish being written before we try to extract.
    // A CREATE or MODIFY event that arrives during a cloud-sync download can
    // fire while the file is still partial; extracting then produces garbage
    // text, which gets persisted as FAILED and sticks around until the next
    // startup. If the file never stabilizes, skip and let a later event retry.

    if (waitForSizeStability(filePath) == false) return;

    lastExtractionStart.put(relPath, System.currentTimeMillis());
    extractionFailures.remove(relPath);
    indexFile(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initialBuild()
  {
    // Collect and sort indexable files by size ascending

    List<FilePath> indexableFiles = new ArrayList<>();

    for (FilePath filePath : registry.allPaths())
      if ((filePath.isDirectory() == false) && isIndexable(filePath))
        indexableFiles.add(filePath);

    indexableFiles.sort(Comparator.comparingLong(filePath ->
    {
      try { return filePath.size(); }
      catch (IOException e) { return 0L; }
    }));

    int totalIndexable = indexableFiles.size();
    buildTotalFiles = totalIndexable;
    buildProcessedFiles = 0;

    System.out.println("Full-text indexer: initial build starting (" + totalIndexable + " indexable files, " + metadataMap.size() + " already indexed)");

    fireStatusListener();

    int workerThreads = Math.max(threadCount < 0 ? Runtime.getRuntime().availableProcessors() - 2 : threadCount, 1);

    // Split into small files (parallel) and large files (sequential on dedicated thread)

    List<FilePath> smallFiles = new ArrayList<>(), largeFiles = new ArrayList<>();

    for (FilePath filePath : indexableFiles)
    {
      try
      {
        if (filePath.size() >= LARGE_FILE_THRESHOLD)
          largeFiles.add(filePath);
        else
          smallFiles.add(filePath);
      }
      catch (IOException e) { smallFiles.add(filePath); }
    }

    System.out.println("Full-text indexer: " + smallFiles.size() + " small, "
      + largeFiles.size() + " large, " + workerThreads + " worker thread" + (workerThreads == 1 ? "" : "s"));

    AtomicInteger docCount = new AtomicInteger(), skipped = new AtomicInteger(), failed = new AtomicInteger();
    long startTime = System.currentTimeMillis();

    // Progress reporting thread

    buildProgressReporter = Executors.newSingleThreadScheduledExecutor(runnable ->
    {
      HyperThread hyperThread = new HyperThread("FTI-Progress", runnable);
      hyperThread.setDaemon(true);
      return hyperThread;
    });

    buildProgressReporter.scheduleAtFixedRate(() ->
    {
      if (stopRequested) return;

      int processed = docCount.get() + skipped.get() + failed.get();
      buildProcessedFiles = processed;
      System.out.println("Full-text indexer: " + processed + '/' + totalIndexable
        + " processed (" + docCount.get() + " indexed, " + skipped.get() + " up-to-date, " + failed.get() + " failed)");
      fireStatusListener();

    }, 5, 5, TimeUnit.SECONDS);

    // Shared work queue: workers pull files one at a time instead of receiving
    // thousands of pre-submitted tasks. When stopRequested is set, workers
    // stop polling and exit within one file-processing cycle.

    ConcurrentLinkedQueue<FilePath> workQueue = new ConcurrentLinkedQueue<>(smallFiles);

    // Worker pool for small files

    buildWorkerPool = Executors.newFixedThreadPool(workerThreads, runnable ->
    {
      HyperThread hyperThread = new HyperThread("FTI-Worker", runnable);
      hyperThread.setDaemon(true);
      hyperThread.setPriority(Thread.MIN_PRIORITY);
      return hyperThread;
    });

    for (int ndx = 0; ndx < workerThreads; ndx++)
    {
      buildWorkerPool.submit(() ->
      {
        FilePath filePath;

        while ((stopRequested == false) && ((filePath = workQueue.poll()) != null))
          processOneFile(filePath, docCount, skipped, failed);
      });
    }

    // Dedicated thread for large files

    buildLargeFileExecutor = Executors.newSingleThreadExecutor(runnable ->
    {
      HyperThread hyperThread = new HyperThread("FTI-LargeFile", runnable);
      hyperThread.setDaemon(true);
      hyperThread.setPriority(Thread.MIN_PRIORITY);
      return hyperThread;
    });

    buildLargeFileExecutor.submit(() -> processFileList(largeFiles, docCount, skipped, failed));

    // All tasks submitted; signal no more will follow

    buildWorkerPool.shutdown();
    buildLargeFileExecutor.shutdown();

    try
    {
      // Wait for workers, processing queued filesystem events during the wait
      // so that file creates, deletes, renames, and modifications are reflected
      // in search without waiting for the entire build to finish.

      waitForExecutorToFinish(buildWorkerPool);

      waitForExecutorToFinish(buildLargeFileExecutor);
    }
    finally
    {
      workQueue.clear();

      // Wait for in-flight workers to release the IndexWriter before the background thread
      // returns. On a stop/interrupt, waitForExecutorToFinish above bailed immediately, so this
      // is the real drain. Restore the interrupt flag only after both pools are handled, so an
      // interrupt during one pool's wait can't skip the next.

      boolean interrupted = false;

      if (buildWorkerPool != null)
      {
        interrupted = shutdownAndAwait(buildWorkerPool, "worker pool");
        buildWorkerPool = null;
      }

      if (buildLargeFileExecutor != null)
      {
        interrupted |= shutdownAndAwait(buildLargeFileExecutor, "large-file pool");
        buildLargeFileExecutor = null;
      }

      if (interrupted)
        Thread.currentThread().interrupt();

      if (buildProgressReporter != null)
      {
        buildProgressReporter.shutdownNow();
        buildProgressReporter = null;
      }
    }

    int processed = docCount.get() + skipped.get() + failed.get();

    if ((stopRequested == false) && (processed >= totalIndexable))
    {
      System.out.println("Full-text indexer: initial build complete in " + elapsedStr(startTime) + ". "
        + docCount.get() + " indexed, " + skipped.get() + " up-to-date, " + failed.get() + " failed.");

      state = IndexerState.MAINTAINING;
    }
    else
    {
      System.out.println("Full-text indexer: initial build interrupted after " + elapsedStr(startTime) + ". "
        + processed + '/' + totalIndexable + " processed ("
        + docCount.get() + " indexed, " + skipped.get() + " up-to-date, " + failed.get() + " failed).");
    }

    commitAndSave();

    fireStatusListener();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Blocks until {@code executor} terminates, draining and processing queued
   * filesystem events during the wait so that file creates, deletes, renames,
   * and modifications are reflected in search before the whole build finishes,
   * and committing once per {@link #COMMIT_INTERVAL_MS}. Returns early if a stop
   * has been requested or the waiting thread is interrupted.
   */
  private void waitForExecutorToFinish(ExecutorService executor)
  {
    while ((stopRequested == false) && (executor.isTerminated() == false))
    {
      try { drainAndProcessEvents(500); }
      catch (InterruptedException e) { Thread.currentThread().interrupt(); return; }
      catch (IOException e) { logThrowable(e); }

      if ((System.currentTimeMillis() - lastCommitTime) >= COMMIT_INTERVAL_MS)
        commitAndSave();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Shuts down {@code pool} and waits up to 10s for in-flight workers to release the IndexWriter.
   * Logs a warning if they don't finish in time (the background thread will then proceed while a
   * worker may still hold the writer). Returns {@code true} if the wait was interrupted; the caller
   * restores the interrupt flag once all pools have been drained, so an interrupt during one pool's
   * wait doesn't skip the next.
   */
  private static boolean shutdownAndAwait(ExecutorService pool, String poolName)
  {
    pool.shutdown();

    try
    {
      if (pool.awaitTermination(POOL_DRAIN_TIMEOUT_MS, TimeUnit.MILLISECONDS) == false)
        System.out.println("Full-text indexer: " + poolName + " did not terminate within " + POOL_DRAIN_TIMEOUT_MS + "ms; IndexWriter may still be in use.");

      return false;
    }
    catch (InterruptedException e)
    {
      return true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void processFileList(List<FilePath> files, AtomicInteger docCount, AtomicInteger skipped, AtomicInteger failed)
  {
    for (FilePath filePath : files)
    {
      if (stopRequested) return;

      processOneFile(filePath, docCount, skipped, failed);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void processOneFile(FilePath filePath, AtomicInteger docCount, AtomicInteger skipped, AtomicInteger failed)
  {
    if (stopRequested) { skipped.incrementAndGet(); return; }

    String relPath = relativePath(filePath);
    FileIndexEntry existing = metadataMap.get(relPath);

    if (existing != null)
    {
      try
      {
        if (isFileUnchanged(filePath, existing))
        {
          if (existing.status() != FAILED)
          {
            skipped.incrementAndGet();
            return;
          }

          // FAILED with unchanged mtime/size: retry in case the failure was
          // transient (virus scanner lock, Dropbox sync, etc.)
        }
      }
      catch (IOException e) { skipped.incrementAndGet(); return; }

      // File changed on disk or retrying a prior failure; clear any prior
      // failure so it gets a fresh attempt

      extractionFailures.remove(relPath);
    }

    try
    {
      indexFile(filePath);

      if (extractionFailures.contains(relPath))
        failed.incrementAndGet();
      else
        docCount.incrementAndGet();
    }
    catch (IOException e)
    {
      System.out.println("Full-text indexer: error indexing " + filePath + ": " + getThrowableMessage(e));
      failed.incrementAndGet();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void consistencyCheck() throws IOException
  {
    Collection<FilePath> allPaths = registry.allPaths();
    Map<String, FilePath> indexableInRegistry = new HashMap<>();

    for (FilePath filePath : allPaths)
      if ((filePath.isDirectory() == false) && isIndexable(filePath))
        indexableInRegistry.put(relativePath(filePath), filePath);

    boolean changed = false;

    // Remove stale entries (in metadata but no longer in registry)

    List<String> toRemove = metadataMap.keySet().stream().filter(key -> indexableInRegistry.containsKey(key) == false).toList();

    for (String key : toRemove)
    {
      removeFile(key);
      changed = true;
    }

    // Add missing entries and reindex stale entries

    for (Map.Entry<String, FilePath> entry : indexableInRegistry.entrySet())
    {
      if (stopRequested) return;

      String relPath = entry.getKey();
      FileIndexEntry existing = metadataMap.get(relPath);

      if (existing != null)
      {
        try
        {
          if (isFileUnchanged(entry.getValue(), existing))
            continue;
        }
        catch (IOException e) { continue; }

        // File changed on disk; clear any prior failure so it gets a fresh attempt

        extractionFailures.remove(relPath);
      }

      indexFile(entry.getValue());
      changed = true;
    }

    if (changed)
      commitAndSave();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Polls the file's size a few times with {@link #SIZE_STABILITY_DELAY_MS}
   * between probes to determine whether the file is mid-write. Returns
   * {@code true} when two consecutive probes agree, {@code false} if the size
   * keeps changing past {@link #SIZE_STABILITY_RETRIES} attempts (or if size
   * cannot be read, implying the file was deleted or is locked).
   */
  private static boolean waitForSizeStability(FilePath filePath)
  {
    try
    {
      long lastSize = filePath.size();

      for (int attempt = 0; attempt < SIZE_STABILITY_RETRIES; attempt++)
      {
        Thread.sleep(SIZE_STABILITY_DELAY_MS);
        long size = filePath.size();
        if (size == lastSize) return true;
        lastSize = size;
      }

      return false;
    }
    catch (IOException e)             { return false; }
    catch (InterruptedException e)    { Thread.currentThread().interrupt(); return false; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds a Lucene {@link Document} with the index's field schema. The single
   * place that encodes the document structure (field names, field types,
   * pageOffsets encoding), so a schema change is one edit rather than three.
   *
   * @param path        the document path (stored, indexed as a single term)
   * @param content     the extracted text (tokenized, stored)
   * @param pageOffsets page boundary offsets, or null for non-paginated content
   * @param pageCount   the page count, or null to omit the pageCount field even
   *                    when pageOffsets is present
   */
  private static Document buildLuceneDocument(String path, String content, int[] pageOffsets, Integer pageCount)
  {
    Document doc = new Document();
    doc.add(new StringField("path", path, Field.Store.YES));
    doc.add(new Field("content", content, CONTENT_FIELD_TYPE));

    if (pageOffsets != null)
    {
      doc.add(new StoredField("pageOffsets", encodePageOffsets(pageOffsets)));

      if (pageCount != null)
        doc.add(new StoredField("pageCount", pageCount));
    }

    return doc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void indexFile(FilePath filePath) throws IOException
  {
    String relPath = relativePath(filePath);

    // Capture mtime and size before extraction so the metadata records the
    // filesystem state that corresponds to the extracted content. If the file
    // changes during extraction, the recorded mtime will be stale, and the
    // next consistency check will correctly flag it for re-indexing.

    long mtime, size;

    try
    {
      mtime = filePath.lastModified().toEpochMilli();
      size = filePath.size();
    }
    catch (IOException e)
    {
      // Couldn't stat the file: it likely vanished or was locked between enumeration and now
      // (common with cloud sync). Log and flag it as a failure for this pass so processOneFile
      // counts it as failed rather than silently indexed. No metadata entry is written, so a
      // later consistency check or rebuild reindexes it if it reappears.

      System.out.println("Full-text indexer: could not read attributes of " + filePath + ": " + getThrowableMessage(e));
      extractionFailures.add(relPath);
      return;
    }

    if (size == 0)
    {
      markAsNoText(relPath, mtime, size);
      return;
    }

    ExtractionResult result = extractText(filePath);

    if (stopRequested) return;

    if (result == null)
    {
      markAsFailed(relPath, mtime, size);
      extractionFailures.add(relPath);
      return;
    }

    if (result.text().isBlank())
    {
      System.out.println("Full-text indexer: no text extracted from " + filePath);
      markAsNoText(relPath, mtime, size);
      extractionFailures.add(relPath);
      return;
    }

    Document doc = buildLuceneDocument(relPath, result.text(), result.pageOffsets(), result.pageCount());

    writer.updateDocument(new Term("path", relPath), doc);

    putMetadataEntry(relPath, mtime, size, INDEXED);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void putMetadataEntry(String relPath, long mtime, long size, FileIndexEntry.IndexStatus status)
  {
    metadataMap.put(relPath, new FileIndexEntry(mtime, size, status));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Marks a file as having no extractable text: removes any prior Lucene
   * document and records a NO_TEXT metadata entry. The two operations are a
   * consistency contract; the Lucene index and the metadata map must not
   * disagree about a file, so they are paired in one helper.
   */
  private void markAsNoText(String relPath, long mtime, long size) throws IOException
  {
    writer.deleteDocuments(new Term("path", relPath));
    putMetadataEntry(relPath, mtime, size, NO_TEXT);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Marks a file as having failed extraction: removes any prior Lucene
   * document and records a FAILED metadata entry. See {@link #markAsNoText}
   * for the delete/metadata consistency contract.
   */
  private void markAsFailed(String relPath, long mtime, long size) throws IOException
  {
    writer.deleteDocuments(new Term("path", relPath));
    putMetadataEntry(relPath, mtime, size, FAILED);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeFile(String relativePath) throws IOException
  {
    writer.deleteDocuments(new Term("path", relativePath));
    metadataMap.remove(relativePath);
    extractionFailures.remove(relativePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Handle a file rename. When both the old and new extensions are indexable, this
   * attempts to reuse the stored content from the existing Lucene document rather than
   * re-extracting via Tika/PDFBox, since content-based MIME detection in
   * {@link #extractText} means the extracted text is independent of the file extension.
   * <p>
   * Falls back to full re-extraction if the old document is not yet visible in the
   * searcher (e.g., CREATE followed by RENAME in the same event batch before a
   * searcher refresh).
   */
  private void renameFile(String oldRelPath, String newRelPath) throws IOException
  {
    FileIndexEntry oldEntry = metadataMap.remove(oldRelPath);
    extractionFailures.remove(oldRelPath);

    FilePath newFile = dbRoot.resolve(newRelPath);
    boolean newIsIndexable = newFile.exists() && isIndexable(newFile);

    // Try to reuse stored content when both old and new are indexable

    if (newIsIndexable && (searcherMgr != null))
    {
      boolean reused = withSearcher(searcher ->
      {
        int docID = findDocIDByPath(searcher, oldRelPath);

        if (docID < 0) return false;

        Document oldDoc = searcher.storedFields().document(docID);
        String content = oldDoc.get("content");

        if (content == null) return false;

        writer.deleteDocuments(new Term("path", oldRelPath));

        String pageOffsetsStr = oldDoc.get("pageOffsets");

        Document newDoc = buildLuceneDocument
        (
          newRelPath, content, decodePageOffsets(pageOffsetsStr),
          (pageOffsetsStr != null) ? oldDoc.getField("pageCount").numericValue().intValue() : null
        );

        writer.addDocument(newDoc);

        FileIndexEntry.IndexStatus status;
        long mtime, size;

        if (oldEntry != null)
        {
          status = oldEntry.status();
          mtime = oldEntry.mtime();
          size = oldEntry.size();
        }
        else
        {
          status = INDEXED;
          mtime = newFile.lastModified().toEpochMilli();
          size = newFile.size();
        }

        putMetadataEntry(newRelPath, mtime, size, status);
        return true;
      });

      if (reused) return;
    }

    // Fall back: stored content not available (CREATE+RENAME before searcher refresh)
    // or new extension is not indexable

    writer.deleteDocuments(new Term("path", oldRelPath));

    if (newIsIndexable)
      indexFile(newFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ExtractionResult extractText(FilePath filePath)
  {
    if (stopRequested || (tika == null)) return null;

    if (getMediaType(filePath).toString().contains("pdf"))
    {
      ExtractionResult pdfResult = extractPdfPageByPage(filePath);

      if (pdfResult != null)
        return pdfResult;

      if (stopRequested) return null;

      // PDFTextStripper failed or produced degenerate output; fall back to Tika

      System.out.println("Full-text indexer: PDFTextStripper fallback to Tika for " + filePath);
    }

    return extractViaTika(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ExtractionResult extractPdfPageByPage(FilePath filePath)
  {
    try (PDDocument doc = Loader.loadPDF(filePath.toFile()))
    {
      int pageCount = doc.getNumberOfPages();
      if (pageCount == 0) return null;

      PDFTextStripper stripper = new PDFTextStripper();
      stripper.setSortByPosition(true);
      stripper.setLineSeparator("\n");

      StringBuilder fullText = new StringBuilder();
      int[] pageOffsets = new int[pageCount + 1];  // +1 for trailing sentinel

      for (int page = 1; page <= pageCount; page++)
      {
        if (stopRequested) return null;

        pageOffsets[page - 1] = fullText.length();
        stripper.setStartPage(page);
        stripper.setEndPage(page);
        fullText.append(stripper.getText(doc));
      }

      pageOffsets[pageCount] = fullText.length();  // trailing sentinel

      // Text density check: if nearly empty, likely image-only or encrypted

      if ((fullText.length() / pageCount) < 10)
        return null;

      return new ExtractionResult(fullText.toString(), pageOffsets, pageCount);
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: PDFTextStripper failed for " + filePath + ": " + getThrowableMessage(e));
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ExtractionResult extractViaTika(FilePath filePath)
  {
    try
    {
      String text = tika.parseToString(filePath.toPath());
      return new ExtractionResult(text, null, 0);
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: failed to extract " + filePath + ": " + getThrowableMessage(e));
      return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean isLucenePopulated(FilePath lucenePath)
  {
    try (var stream = Files.list(lucenePath.toPath()))
    {
      return stream.findFirst().isPresent();
    }
    catch (IOException e) { return false; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class PageAwareFormatter extends PassageFormatter
  {
    private int[] pageOffsets;
    private List<SearchResult.PageMatch> lastResult = List.of();

    private void setPageOffsets(int[] offsets) { this.pageOffsets = offsets; }
    private List<SearchResult.PageMatch> getLastResult() { return lastResult; }

  //---------------------------------------------------------------------------

    @Override public Object format(Passage[] passages, String content)
    {
      List<SearchResult.PageMatch> matches = new ArrayList<>();

      for (Passage passage : passages)
      {
        String snippet = content.substring(passage.getStartOffset(), passage.getEndOffset());

        int pageNum = 0;
        if ((pageOffsets != null) && (passage.getNumMatches() > 0))
          pageNum = findPage(pageOffsets, passage.getMatchStarts()[0]);

        List<SearchResult.HitRange> hitRanges = new ArrayList<>();
        int passageStart = passage.getStartOffset(),
            passageEnd   = passage.getEndOffset();

        for (int ndx = 0; ndx < passage.getNumMatches(); ndx++)
        {
          // Clamp to the passage; a wide-slop phrase match can extend past the passage
          // boundary, which would put the hit range out of the snippet's bounds.

          int hitStart = Math.max(passage.getMatchStarts()[ndx], passageStart) - passageStart,
              hitEnd   = Math.min(passage.getMatchEnds  ()[ndx], passageEnd  ) - passageStart;

          if (hitEnd > hitStart)
            hitRanges.add(new SearchResult.HitRange(hitStart, hitEnd));
        }

        matches.add(new SearchResult.PageMatch
        (
          pageNum, passage.getStartOffset(), passage.getEndOffset(),
          snippet, passage.getScore(), hitRanges
        ));
      }

      lastResult = matches;

      return matches;
    }

  //---------------------------------------------------------------------------

    /**
     * Binary search for the largest ndx where {@code offsets[ndx] <= charOffset}.
     * Returns a 1-based page number for PDFs.
     */
    private static int findPage(int[] offsets, int charOffset)
    {
      int lo = 0, hi = offsets.length - 2;  // -2 because last element is sentinel

      while (lo <= hi)
      {
        int mid = (lo + hi) >>> 1;

        if (offsets[mid] <= charOffset)
          lo = mid + 1;
        else
          hi = mid - 1;
      }

      return lo;  // 1-based: page 1 if charOffset is in [offsets[0], offsets[1])
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String encodePageOffsets(int[] offsets)
  {
    return Arrays.stream(offsets).mapToObj(String::valueOf).collect(Collectors.joining(","));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int[] decodePageOffsets(String encoded)
  {
    if (strNullOrEmpty(encoded)) return null;

    return Arrays.stream(encoded.split(",")).mapToInt(Integer::parseInt).toArray();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean isIndexable(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return false;

    if (db.xmlPath().contains(filePath))
      return false;

    if (FilePath.isTemporaryFile(filePath.getNameOnly().toString())) return false;

    String ext = filePath.getExtensionOnly();
    return strNotNullOrBlank(ext) && INDEXABLE_EXTENSIONS.contains(ext.toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String relativePath(FilePath filePath)
  {
    FilePath relative = dbRoot.relativize(filePath);
    return (relative != null) ? relative.toString().replace('\\', '/') : filePath.toString().replace('\\', '/');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Commits the Lucene index, refreshes the searcher, and saves the metadata map.
   *  Called only from the background thread to avoid concurrent commit races.
   *  <p>
   *  The metadata is snapshotted <em>before</em> the Lucene commit because worker
   *  threads may call {@code writer.updateDocument()} then
   *  {@code putMetadataEntry()} between a commit and the subsequent metadata
   *  write, which would save metadata for documents not yet in the committed
   *  index. Snapshotting first guarantees that every entry in the saved metadata
   *  has a corresponding Lucene buffer entry (because {@link #indexFile} writes
   *  the buffer before the map), and the commit flushes all buffered entries.
   *  So committed Lucene >= saved metadata.
   *  <p>
   *  On crash between the commit and the metadata write, metadata may lag behind
   *  Lucene; the next build's mtime/size check in {@link #processOneFile}
   *  re-indexes any files whose metadata was lost. */
  private void commitAndSave()
  {
    // Never commit once shutdown has begun: close() performs the authoritative final commit
    // (non-fast path) or rollback (fast path) after joining this thread, and a commit here
    // would race that writer teardown. stopRequested is set as close()'s first action, before
    // it waits to join the background thread, so this guard reliably sees the shutdown.

    if ((writer == null) || stopRequested) return;

    try
    {
      String metadataSnapshot = buildMetadataJson();

      writer.commit();
      searcherMgr.maybeRefresh();

      writeMetadataSnapshot(metadataSnapshot);
      lastCommitTime = System.currentTimeMillis();
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: commitAndSave failed");
      logThrowable(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FunctionalInterface
  private interface SearcherTask<T>
  {
    T execute(IndexSearcher searcher) throws IOException;
  }

//---------------------------------------------------------------------------

  /**
   * Acquires an {@link IndexSearcher} from {@link #searcherMgr}, runs {@code task}
   * against it, and guarantees the searcher is released. Centralizes the
   * acquire/finally-release pair so individual search methods cannot leak a
   * searcher by forgetting the {@code finally}.
   * <p>
   * {@link IOException} from the task (or from acquire/release) propagates to the
   * caller. Error handling genuinely differs per call site (log-and-return-default
   * for queries, propagate for the rename event handler), so it is left to each
   * caller rather than fixed here.
   */
  private <T> T withSearcher(SearcherTask<T> task) throws IOException
  {
    IndexSearcher searcher = searcherMgr.acquire();

    try
    {
      return task.execute(searcher);
    }
    finally
    {
      searcherMgr.release(searcher);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Finds the Lucene internal doc ID of the single document whose "path" field
   * equals {@code relPath}. Centralizes the path-term single-document lookup
   * (the "path" field name and the {@code search(query, 1)} idiom); callers
   * fetch whichever stored fields they need from the returned ID, since the
   * field set genuinely differs per call site.
   *
   * @return the Lucene doc ID, or -1 if no document has that path
   */
  private static int findDocIDByPath(IndexSearcher searcher, String relPath) throws IOException
  {
    TopDocs topDocs = searcher.search(new TermQuery(new Term("path", relPath)), 1);

    return (topDocs.scoreDocs.length == 0) ? -1 : topDocs.scoreDocs[0].doc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Fast search without highlighting. Returns results with null {@code pageMatches},
   * suitable for immediate display while highlighting proceeds in the background.
   *
   * @param queryStr     the Lucene query
   * @param maxResults   maximum number of filtered results to return
   * @param fileMask     comma-separated glob patterns or null/blank for all files
   * @param pathScope    set of relative paths to restrict results to, or null for all files
   * @param folderPrefix relative path prefix to restrict results to, or null for all folders
   * @return batch containing lightweight results and a flag indicating whether more results are available
   */
  public SearchBatch searchLight(String queryStr, int maxResults, String fileMask,
                                 Set<String> pathScope, String folderPrefix) throws ParseException
  {
    return doSearch(null, queryStr, maxResults, fileMask, pathScope, folderPrefix);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Continue a previous light search without highlighting.
   *
   * @param after        the {@link ScoreDoc} from the last result of the previous batch
   * @param queryStr     the same Lucene query string used in the initial search
   * @param maxResults   maximum number of filtered results to return
   * @param fileMask     comma-separated glob patterns or null/blank for all files
   * @param pathScope    set of relative paths to restrict results to, or null for all files
   * @param folderPrefix relative path prefix to restrict results to, or null for all folders
   * @return batch containing lightweight results and a flag indicating whether more results are available
   */
  public SearchBatch searchLightAfter(ScoreDoc after, String queryStr, int maxResults, String fileMask,
                                      Set<String> pathScope, String folderPrefix) throws ParseException
  {
    return doSearch(after, queryStr, maxResults, fileMask, pathScope, folderPrefix);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Highlights a batch of previously returned light search results. Re-looks up
   * each document by path to get a current doc ID, loads pageOffsets, and runs
   * the {@link UnifiedHighlighter} with the POSTINGS strategy.
   *
   * @param queryStr     the same Lucene query string used in the original search
   * @param lightResults results from {@link #searchLight} with null {@code pageMatches}
   * @return highlighted results with populated {@code pageMatches}; documents
   *         that are no longer in the index are silently skipped
   */
  public List<SearchResult> highlightResults(String queryStr, List<SearchResult> lightResults) throws ParseException
  {
    List<SearchResult> highlighted = new ArrayList<>();

    if ((searcherMgr == null) || (analyzer == null) || lightResults.isEmpty())
      return highlighted;

    QueryParser parser = new QueryParser("content", analyzer);
    parser.setDefaultOperator(QueryParser.Operator.AND);
    Query query = parser.parse(queryStr);

    try
    {
      return withSearcher(searcher ->
      {
        PageAwareFormatter formatter = new PageAwareFormatter();

        UnifiedHighlighter highlighter = UnifiedHighlighter.builder(searcher, analyzer)
          .withFormatter(formatter)
          .withMaxLength(Integer.MAX_VALUE - 1)
          .build();

        StoredFields storedFields = searcher.storedFields();

        for (SearchResult light : lightResults)
        {
          // Re-lookup by path to get a current doc ID (safe across searcher refreshes)

          int docID = findDocIDByPath(searcher, light.path());

          if (docID < 0) continue;

          Document doc = storedFields.document(docID, Set.of("pageOffsets"));

          ScoreDoc scoreDoc = new ScoreDoc(docID, light.score());
          highlighted.add(buildResult(light.path(), doc.get("pageOffsets"), scoreDoc, query, formatter, highlighter));
        }

        return highlighted;
      });
    }
    catch (IOException e)
    {
      logThrowable(e);
      return highlighted;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SearchBatch doSearch(ScoreDoc after, String queryStr, int maxResults, String fileMask,
                               Set<String> pathScope, String folderPrefix) throws ParseException
  {
    List<SearchResult> results = new ArrayList<>();

    if ((searcherMgr == null) || (analyzer == null)) return new SearchBatch(results, false);

    QueryParser parser = new QueryParser("content", analyzer);
    parser.setDefaultOperator(QueryParser.Operator.AND);
    Query query = parser.parse(queryStr);

    if (query instanceof MatchNoDocsQuery)
      return new SearchBatch(results, false);

    if ((query instanceof BooleanQuery bq) && bq.clauses().isEmpty())
      return new SearchBatch(results, false);

    // Apply path scope filter at the Lucene level via TermInSetQuery

    Query searchQuery;

    if (collEmpty(pathScope) == false)
    {
      List<BytesRef> terms = pathScope.stream().map(BytesRef::new).toList();
      searchQuery = new BooleanQuery.Builder()
        .add(query, BooleanClause.Occur.MUST)
        .add(new TermInSetQuery("path", terms), BooleanClause.Occur.FILTER)
        .build();
    }
    else if (strNotNullOrEmpty(folderPrefix))
    {
      searchQuery = new BooleanQuery.Builder()
        .add(query, BooleanClause.Occur.MUST)
        .add(new PrefixQuery(new Term("path", folderPrefix)), BooleanClause.Occur.FILTER)
        .build();
    }
    else
    {
      searchQuery = query;
    }

    List<PathMatcher> matchers = parseFileMask(fileMask);
    boolean hasMore;

    try
    {
      hasMore = withSearcher(searcher ->
      {
        boolean more = false;

        StoredFields storedFields = searcher.storedFields();
        Set<String> fieldSet = Set.of("path");

        // Pull batches from Lucene until we have enough filtered results or Lucene is exhausted

        ScoreDoc cursor = after;
        int fetchSize = Math.max(maxResults + 1, 200);  // +1 to detect if more results exist

        while (results.size() < maxResults)
        {
          TopDocs topDocs = (cursor == null)
            ? searcher.search(searchQuery, fetchSize)
            : searcher.searchAfter(cursor, searchQuery, fetchSize);

          if (topDocs.scoreDocs.length == 0) break;

          for (ScoreDoc scoreDoc : topDocs.scoreDocs)
          {
            Document doc = storedFields.document(scoreDoc.doc, fieldSet);

            String path = doc.get("path");

            if (matchers.isEmpty() == false)
            {
              Path filenamePath = Path.of(path).getFileName();

              if (matchers.stream().noneMatch(matcher -> matcher.matches(filenamePath)))
              {
                cursor = scoreDoc;
                continue;
              }
            }

            if (results.size() >= maxResults)
            {
              more = true;
              break;
            }

            results.add(new SearchResult(path, scoreDoc.score, null, scoreDoc));

            cursor = scoreDoc;
          }

          if (more || (topDocs.scoreDocs.length < fetchSize))
            break;
        }

        return more;
      });
    }
    catch (IOException e)
    {
      logThrowable(e);
      hasMore = false;
    }

    return new SearchBatch(results, hasMore);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static SearchResult buildResult(String path, String pageOffsetsStr, ScoreDoc scoreDoc,
                                          Query query, PageAwareFormatter formatter,
                                          UnifiedHighlighter highlighter) throws IOException
  {
    formatter.setPageOffsets(decodePageOffsets(pageOffsetsStr));
    formatter.lastResult = List.of();

    TopDocs singleDoc = new TopDocs(new TotalHits(1, TotalHits.Relation.EQUAL_TO), new ScoreDoc[] { scoreDoc });

    highlighter.highlight("content", query, singleDoc, MAX_RESULTS_PER_FILE);

    return new SearchResult(path, scoreDoc.score, formatter.getLastResult(), scoreDoc);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("resource")
  private static List<PathMatcher> parseFileMask(String fileMask)
  {
    if (strNullOrBlank(fileMask)) return List.of();

    return Arrays.stream(fileMask.split(","))
                 .map(String::trim)
                 .filter(p -> p.isEmpty() == false)
                 .map(p -> FileSystems.getDefault().getPathMatcher("glob:" + p))
                 .toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void fireStatusListener()
  {
    Runnable listener = statusListener;
    if (listener != null)
      Platform.runLater(listener);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String buildMetadataJson()
  {
    JsonArray arr = new JsonArray();

    for (Map.Entry<String, FileIndexEntry> entry : metadataMap.entrySet())
      arr.add(entry.getValue().toJson(entry.getKey()));

    JsonObj root = new JsonObj();
    root.put("files", arr);

    return root.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void writeMetadataSnapshot(String json) throws IOException
  {
    indexDir.resolve("metadata.json").saveCharSequenceAtomically(json, StandardCharsets.UTF_8);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadMetadata()
  {
    FilePath metadataFile = indexDir.resolve("metadata.json");

    if (metadataFile.exists() == false) return;

    try
    {
      String json = Files.readString(metadataFile.toPath(), StandardCharsets.UTF_8);
      JsonObj root = JsonObj.parseJsonObj(json);
      JsonArray files = root.getArray("files");

      if (files == null) return;

      int failedCount = 0;

      for (JsonObj obj : files.getObjs())
      {
        String path = obj.getStr("path");

        if (path != null)
        {
          FileIndexEntry entry = FileIndexEntry.fromJson(obj);
          metadataMap.put(path, entry);

          if (entry.status() == NO_TEXT)
          {
            extractionFailures.add(path);
            failedCount++;
          }
          else if (entry.status() == FAILED)
          {
            // Don't add to extractionFailures: FAILED entries get a fresh
            // retry during the next initial build (the file may have been
            // transiently locked by a virus scanner, Dropbox sync, etc.).
            // If the retry fails again, indexFile() re-adds to the set.

            failedCount++;
          }
        }
      }

      System.out.println("Full-text indexer: loaded metadata for " + metadataMap.size() + " files (" + failedCount + " previously failed)");
    }
    catch (Exception e)
    {
      System.out.println("Full-text indexer: failed to load metadata, will re-index");
      logThrowable(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
