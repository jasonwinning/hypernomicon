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

import static org.hypernomicon.fts.FTSUtil.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.Query;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.fts.FullTextIndexer.ExtractionResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.searchKeys.Keyword;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * UI-agnostic service that computes and caches FTS hit sets: which passages of
 * a file match the active query, and the hit data needed to highlight them,
 * expressed in Hypernomicon extraction-text coordinates that the viewer
 * translates. This class owns what used to be spread across
 * {@code FTSQueryCtrlr} (the per-file match cache,
 * the request-deduplication set, the search-generation staleness counter, and
 * the "FTS-highlight" worker thread) and the highlight coordinators (the hit
 * computation pipelines), with one deliberate difference from the code it
 * replaces:
 * <ul>
 *   <li><b>Converted-office alignment is part of the computed value.</b>
 *       {@link #computeConvertedPdfHits} always returns the Tika-to-pdf.js
 *       coordinate alignment alongside the hit JSON, whether or not the viewer
 *       ends up applying the highlights. (The old pipeline only published the
 *       alignment state if the viewer accepted the hits, so passage-click
 *       navigation silently degraded whenever hit delivery failed.)</li>
 * </ul>
 * Delivery to the viewer stays with the callers; the service computes values
 * and never touches the preview window.
 * <p>
 * One instance per FTS query controller, matching the old per-controller cache
 * and worker-thread lifetimes. A <i>generation</i> begins with each executed
 * search ({@link #beginGeneration}); asynchronous work captures the generation
 * at request time and is dropped, before caching and before invoking its
 * callback, if a newer search has begun. The generation counter is written only
 * on the JavaFX Application Thread (single-writer model; volatile provides the
 * required visibility to worker threads).
 */
public final class HitSetService
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The active query, as needed to compute matches for one file: the original
   * Lucene query string, or a prebuilt search-key query with its ad-hoc keyword
   * lookup; plus an optional filter applied to highlighted results (used for
   * record-scoped searches, whose page-range restrictions live in UI-layer
   * classes this package cannot reference).
   *
   * @param queryStr       the original Lucene query string (ignored if {@code searchKeyQuery} is non-null)
   * @param searchKeyQuery a prebuilt search-key query, or {@code null} to re-parse {@code queryStr}
   * @param keyLookup      the ad-hoc keyword lookup for search-key mode, or {@code null} for a plain query
   * @param resultFilter   filter applied to highlighted results before caching, or {@code null} for none
   */
  public record QueryDescriptor(String queryStr, Query searchKeyQuery,
                                Function<String, Iterable<Keyword>> keyLookup,
                                UnaryOperator<List<SearchResult>> resultFilter) { }

//---------------------------------------------------------------------------

  /**
   * Hit data for a paged (PDF) viewer.
   *
   * @param hitsJson       per-page hit JSON for the viewer, or {@code null} if there are no hits to apply
   * @param firstMatchPage 1-based page the viewer should open to, or -1 if the caller decides
   * @param alignment      Tika-to-pdf.js coordinate alignment for converted office documents,
   *                       or {@code null} for native PDFs (whose match coordinates need no translation)
   */
  public record PagedHits(String hitsJson, int firstMatchPage, ConvertedPdfAlignment alignment) { }

  /**
   * Hit data for non-PDF content loaded directly into the browser: context
   * windows around each match, which JS locates in the DOM and wraps.
   */
  public record DirectHits(String hitsJson) { }

//---------------------------------------------------------------------------

  /**
   * Coordinate translation between the Tika-extracted text a file was indexed
   * from (the coordinates of its {@link PageMatch}es) and the normalized,
   * header-stripped pdf.js extraction of its LibreOffice-converted PDF. Used
   * only to derive a converted-PDF page number; the viewer's hit-offset space
   * is the raw extraction (see {@link #computeConvertedPdfHits}). Built during
   * {@link #computeConvertedPdfHits}; consumed by passage-click navigation via
   * {@link #pageForPassage}.
   */
  public record ConvertedPdfAlignment(String convertedPdfNormText, ArrayList<Integer> convertedPdfPosMap,
                                      int[] convertedPdfPageOffsets, String tikaNormText, int[] tikaReverseMap)
  {
    /**
     * Maps a Tika-coordinate match to the 1-based page number in the converted
     * PDF that the viewer displays, or -1 if no mapping can be found.
     */
    public int pageForPassage(PageMatch tikaMatch)
    {
      return findConvertedPdfPage(tikaMatch, tikaReverseMap, tikaNormText,
        convertedPdfNormText, convertedPdfPosMap, convertedPdfPageOffsets);
    }
  }

//---------------------------------------------------------------------------

  /**
   * The indexed/extracted text a hit computation reads: a thin slice of
   * {@link FullTextIndexer}, isolated so contract tests can substitute fakes
   * without an index or a pdf.js extractor pool.
   */
  public interface TextSource
  {
    /** Extracts a PDF's text via the pdf.js extractor pool. */
    ExtractionResult extractPdfText(FilePath filePath);

    /** The Lucene-stored (Tika-extracted) text the file was indexed from. */
    String getStoredContent(String relativePath);

    /** Per-page character offsets into the stored text, or {@code null} for non-paged files. */
    int[] getPageOffsets(String relativePath);

    Analyzer getAnalyzer();

    static TextSource of(FullTextIndexer indexer)
    {
      return new TextSource()
      {
        @Override public ExtractionResult extractPdfText(FilePath filePath) { return indexer.extractPdfText(filePath); }
        @Override public String getStoredContent(String relativePath)       { return indexer.getStoredContent(relativePath); }
        @Override public int[] getPageOffsets(String relativePath)          { return indexer.getPageOffsets(relativePath); }
        @Override public Analyzer getAnalyzer()                             { return indexer.getAnalyzer(); }
      };
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int MAX_PASSAGES_PER_FILE = 10_000;

  private final Map<String, List<PageMatch>> matchCache = new ConcurrentHashMap<>();
  private final Set<String> matchesRequested = ConcurrentHashMap.newKeySet();

  private final ExecutorService executor = Executors.newSingleThreadExecutor(runnable ->
  {
    HyperThread hyperThread = new HyperThread("FTS-highlight", runnable);
    hyperThread.setDaemon(true);
    return hyperThread;
  });

  private volatile int generation;
  private volatile QueryDescriptor query;

//---------------------------------------------------------------------------

  /**
   * Begins a new search generation: stores the query descriptor, clears the
   * match cache and request-deduplication state, and invalidates all
   * outstanding asynchronous work from prior generations. Call on the JavaFX
   * Application Thread only (single-writer model).
   */
  public void beginGeneration(QueryDescriptor query)
  {
    generation++;
    this.query = query;
    matchCache.clear();
    matchesRequested.clear();
  }

  /** The active query descriptor, or {@code null} if no search has begun. */
  public QueryDescriptor query() { return query; }

//---------------------------------------------------------------------------

  /** Cached matches for the file, or {@code null} if not computed yet. An empty
   *  list means the file was computed to have no in-scope matches. */
  public List<PageMatch> cachedMatches(String path) { return matchCache.get(path); }

//---------------------------------------------------------------------------

  /**
   * Submits a task to the service's single worker thread. Used by callers whose
   * pipelines interleave hit computation with await points and viewer delivery
   * of their own ({@code FTSQueryCtrlr.launchConvertedHitPipeline}); tasks
   * queue behind, and never run concurrently with, the service's own match
   * computations.
   */
  public void execute(Runnable task) { executor.execute(task); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Computes the matches for one file against the active query on the worker
   * thread, caches them, and invokes {@code onMatchesFX} on the JavaFX
   * Application Thread. Deduplicated per generation: repeated requests for the
   * same path are no-ops while the first is pending or after it has cached.
   * The callback is not invoked (and the cache not written) if a new
   * generation has begun in the meantime.
   */
  public void requestMatches(String path, Consumer<List<PageMatch>> onMatchesFX)
  {
    if (matchesRequested.add(path) == false) return;

    QueryDescriptor query = this.query;

    if ((query == null) || ((query.queryStr() == null) && (query.searchKeyQuery() == null))) return;

    int gen = generation;

    executor.submit(() ->
    {
      if (generation != gen) return;

      FullTextIndexer indexer = db.getFullTextIndexer();
      if (indexer == null) return;

      try
      {
        SearchResult light = new SearchResult(path, 0f, null, null);
        List<SearchResult> results = (query.searchKeyQuery() != null)
          ? indexer.highlightResults(query.searchKeyQuery(), query.keyLookup(), List.of(light), MAX_PASSAGES_PER_FILE)
          : indexer.highlightResults(query.queryStr(), List.of(light), MAX_PASSAGES_PER_FILE);

        if (generation != gen) return;

        if (query.resultFilter() != null)
          results = query.resultFilter().apply(results);

        List<PageMatch> matches = results.isEmpty()
          ? List.of()
          : nullSwitch(results.getFirst().pageMatches(), List.of());

        matchCache.put(path, matches);

        runInFXThread(() ->
        {
          if (generation == gen)
            onMatchesFX.accept(matches);
        });
      }
      catch (ParseException e) { /* query was valid when search ran */ }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Computes and caches matches for a batch of light search results on the
   * worker thread, then invokes {@code onComputedFX} on the JavaFX Application
   * Thread with the highlighted (and filtered, if the query has a result
   * filter) results. Used by record-scoped searches, which highlight up front
   * so rows with no in-scope matches never appear. The callback is not invoked
   * (and the cache not written) if a new generation has begun in the meantime.
   */
  public void computeMatchesForBatch(List<SearchResult> lightResults, Consumer<List<SearchResult>> onComputedFX)
  {
    QueryDescriptor query = this.query;
    if (query == null) return;

    int gen = generation;

    executor.submit(() ->
    {
      if (generation != gen) return;

      FullTextIndexer indexer = db.getFullTextIndexer();
      if (indexer == null) return;

      try
      {
        List<SearchResult> highlighted = (query.searchKeyQuery() != null)
          ? indexer.highlightResults(query.searchKeyQuery(), query.keyLookup(), lightResults, MAX_PASSAGES_PER_FILE)
          : indexer.highlightResults(query.queryStr(), lightResults, MAX_PASSAGES_PER_FILE);

        if (generation != gen) return;

        if (query.resultFilter() != null)
          highlighted = query.resultFilter().apply(highlighted);

        List<SearchResult> finalHighlighted = highlighted;

        for (SearchResult sr : finalHighlighted)
        {
          List<PageMatch> matches = sr.pageMatches();

          if (matches != null)
          {
            matchCache.put(sr.path(), matches);
            matchesRequested.add(sr.path());
          }
        }

        runInFXThread(() ->
        {
          if (generation == gen)
            onComputedFX.accept(finalHighlighted);
        });
      }
      catch (ParseException e) { /* query was valid when search ran */ }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the per-page hit JSON for a native PDF from its indexed page
   * offsets, or returns {@code null} if the index has no page offsets for it.
   */
  public static PagedHits pdfHits(TextSource source, String indexPath, List<PageMatch> matches)
  {
    int[] pageOffsets = source.getPageOffsets(indexPath);

    return pageOffsets == null ? null : new PagedHits(buildAllHitsJson(matches, pageOffsets), -1, null);
  }

//---------------------------------------------------------------------------

  /**
   * Builds the hit JSON for non-PDF content loaded directly into the browser
   * (context windows around each match, located in the stored text), or
   * returns {@code null} if the index has no stored content for it.
   */
  public static DirectHits directContentHits(TextSource source, String indexPath, List<PageMatch> matches)
  {
    String storedContent = source.getStoredContent(indexPath);

    return storedContent == null ? null : new DirectHits(buildDirectContentHitsJson(matches, storedContent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The converted-office hit pipeline: extracts the converted PDF's text via
   * pdf.js, strips LibreOffice page-header metadata, normalizes both the
   * converted-PDF text and the Tika-indexed text for passage-click alignment,
   * searches the converted PDF for matches, and builds per-page hit JSON plus
   * the first-match page. Runs on the calling thread
   * ({@code FTSQueryCtrlr.launchConvertedHitPipeline} submits it via
   * {@link #execute}) and performs no viewer delivery.
   * <p>
   * The returned {@link ConvertedPdfAlignment} is always populated when
   * extraction succeeds, including when there are no matches; passage-click
   * navigation works from the alignment alone and must not depend on whether
   * highlights were delivered.
   *
   * @param source        the text source (live indexer, or a fake under test)
   * @param query         the query to search the converted text with
   * @param indexPath     the source file's DB-relative index path (for its stored Tika text)
   * @param convertedPath the LibreOffice-converted PDF
   * @param dbRootPathStr the DB root path with backslash separators, for stripping
   *                      LibreOffice per-page headers; {@code null} skips stripping
   * @return the computed hits and alignment, or {@code null} if text extraction came up empty
   * @throws RuntimeException if {@code query.queryStr()} no longer parses (unexpected;
   *                          it parsed when the search ran)
   */
  public static PagedHits computeConvertedPdfHits(TextSource source, QueryDescriptor query,
                                                  String indexPath, FilePath convertedPath, String dbRootPathStr)
  {
    // Extract text from the converted PDF via the pdf.js extractor pool

    ExtractionResult extraction = source.extractPdfText(convertedPath);

    if ((extraction == null) || strNullOrBlank(extraction.text()))
      return null;

    // Strip the leaked page-header text from the converted PDF: LibreOffice's headless
    // export materializes the source document's own stashed header/footer fields (file
    // path, page number, save date) on each page; see stripConvertedPdfHeaders.
    // The page offsets must be adjusted in tandem since stripping changes character positions.

    String pdfRawText = extraction.text();
    int[] adjustedPageOffsets = extraction.pageOffsets().clone();

    if (dbRootPathStr != null)
      pdfRawText = stripConvertedPdfHeaders(pdfRawText, dbRootPathStr, adjustedPageOffsets);

    // Normalize both texts for passage-click navigation:
    // 1. convertToEnglishCharsWithMap: Unicode to ASCII with position tracking
    // 2. toLowerCase
    // 3. collapseWhitespace: all whitespace runs to single space, with position tracking
    // Position maps chain: normalized output pos to original text pos

    ArrayList<Integer> pdfPosMap = new ArrayList<>();
    String normPdfText = normalizeForMatching(pdfRawText, pdfPosMap);

    String tikaText = source.getStoredContent(indexPath);
    ArrayList<Integer> tikaPosMapFwd = new ArrayList<>();
    String normTikaText = (tikaText != null) ? normalizeForMatching(tikaText, tikaPosMapFwd) : "";
    int[] tikaRevMap = (tikaText != null) ? buildReversePositionMap(tikaPosMapFwd, tikaText.length()) : new int[0];

    ConvertedPdfAlignment alignment = new ConvertedPdfAlignment(normPdfText, pdfPosMap, adjustedPageOffsets, normTikaText, tikaRevMap);

    // Build the query

    Query luceneQuery = query.searchKeyQuery();

    if (luceneQuery == null)
    {
      try
      {
        @SuppressWarnings("resource")
        var parser = FullTextIndexer.createQueryParser(source.getAnalyzer());
        luceneQuery = parser.parse(query.queryStr());
      }
      catch (Exception e)
      {
        throw new RuntimeException("Unable to re-parse FTS query for converted-PDF search", e);
      }
    }

    // Search the converted PDF's text using a temporary in-memory Lucene index

    List<PageMatch> convertedMatches = FullTextIndexer.searchExtractedText(extraction.text(), extraction.pageOffsets(), luceneQuery);

    if (query.keyLookup() != null)
      convertedMatches = rescanHitRanges(convertedMatches, query.keyLookup());

    if (convertedMatches.isEmpty())
      return new PagedHits(null, 1, alignment);

    // Build per-page hit JSON using the RAW page offsets (not the
    // header-stripped adjustedPageOffsets). The viewer's pdf.js renders the
    // converted PDF as-is, including LibreOffice's per-page header text, so
    // its textDivs concatenate to the raw extraction text. The match offsets
    // in convertedMatches are also in raw-text coordinates (we passed
    // extraction.text()/extraction.pageOffsets() to searchExtractedText above).
    // Using adjustedPageOffsets here would introduce a per-page drift equal
    // to the cumulative header strip, eventually exceeding page-text length
    // and causing the viewer to drop those hits.

    String allHitsJson = buildAllHitsJson(convertedMatches, extraction.pageOffsets());

    // Determine the first page with a match; this is where the viewer will open

    int firstMatchPage = convertedMatches.stream()
      .mapToInt(PageMatch::pageNumber)
      .filter(p -> p > 0)
      .min().orElse(1);

    return new PagedHits(allHitsJson, firstMatchPage, alignment);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
