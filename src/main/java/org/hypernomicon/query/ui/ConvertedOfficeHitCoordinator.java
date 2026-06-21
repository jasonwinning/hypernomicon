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

package org.hypernomicon.query.ui;

import static org.hypernomicon.fts.FTSUtil.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;
import java.util.function.Function;

import org.apache.lucene.search.Query;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.searchKeys.Keyword;
import org.hypernomicon.previewWindow.ConversionSession;
import org.hypernomicon.previewWindow.ConversionSession.NoOfficeInstallationException;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;

//---------------------------------------------------------------------------

/**
 * Highlight coordinator for office documents (docx, doc, rtf, ppt, odt, etc.)
 * that need LibreOffice conversion to PDF before they can be previewed.
 * Owns the {@link ConversionSession} extraction subscription, the background
 * extract-and-search pipeline, and the Tika↔pdf.js coordinate-translation
 * state used by passage-click navigation.
 * <p>
 * The conversion and viewer load are deliberately ordered so the first page
 * shown to the user is already the first-match page: extract, strip LibreOffice
 * headers, normalize, run the query against the converted PDF text, determine
 * first-match page, <em>then</em> call {@link PreviewWindow#loadConvertedPDF}
 * and apply hits.
 */
final class ConvertedOfficeHitCoordinator extends FileHighlightCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String queryStr;
  private final Query searchKeyQuery;
  private final Function<String, Iterable<Keyword>> keyLookup;
  private final ExecutorService executor;

  private CompletableFuture<FilePath> extractionFuture;

  // Coordinate-translation state; populated by the background task on success,
  // consumed by pageForPassage() on the FX thread for passage-click navigation.
  // Reads/writes cross threads but the state is only read after the runLater
  // that writes it; no additional synchronization needed beyond that ordering.

  private String convertedPdfNormText, tikaNormText;
  private ArrayList<Integer> convertedPdfPosMap;
  private int[] convertedPdfPageOffsets, tikaReverseMap;

  // Pipeline-outcome signals consumed by FTSQueryCtrlr.setPreview's same-file
  // branch (see viewerLoaded/failedBeforeViewerLoad in the base class).
  // Written by the background task (or its runLater continuations), read on
  // the FX thread; volatile suffices since each is an independent flag.

  private volatile boolean viewerLoaded, failedBeforeViewerLoad;

//---------------------------------------------------------------------------

  /**
   * @param row            the currently-selected FTS result row (converted office file)
   * @param indexer        the live {@link FullTextIndexer}
   * @param queryStr       the original Lucene query string (ignored if {@code searchKeyQuery} is non-null)
   * @param searchKeyQuery a prebuilt search-key query, or {@code null} to re-parse {@code queryStr}
   * @param keyLookup      the ad-hoc keyword lookup for search-key mode, or {@code null} for a plain query
   * @param executor       the executor to run the conversion/extraction pipeline on
   */
  ConvertedOfficeHitCoordinator(FTSResultRow row, FullTextIndexer indexer, String queryStr, Query searchKeyQuery,
                                Function<String, Iterable<Keyword>> keyLookup, ExecutorService executor)
  {
    super(row, indexer);

    this.queryStr = queryStr;
    this.searchKeyQuery = searchKeyQuery;
    this.keyLookup = keyLookup;
    this.executor = executor;
  }

//---------------------------------------------------------------------------

  @Override boolean viewerLoaded()           { return viewerLoaded; }
  @Override boolean failedBeforeViewerLoad() { return failedBeforeViewerLoad; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void start()
  {
    PreviewWindow.clearAllHits(pvsQueriesTab);

    FilePath filePath = db.getRootPath(row.path());
    String mimetypeStr = getMediaType(filePath).toString();

    ConversionSession session = PreviewWindow.getOrCreateSession(pvsQueriesTab, mimetypeStr, filePath);
    if (session == null) return;

    extractionFuture = session.subscribeExtraction();
    PreviewWindow.enqueueForConversion(pvsQueriesTab, session);

    executor.submit(this::runExtractAndHighlight);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void doDispose()
  {
    if (extractionFuture != null)
    {
      extractionFuture.cancel(true);
      extractionFuture = null;
    }

    PreviewWindow.clearAllHits(pvsQueriesTab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Maps a passage index (over the Tika-extracted matches shown in the context
   * pane) to a 1-based page number in the converted PDF that the viewer
   * displays. Returns -1 if the alignment state hasn't been populated yet
   * (the background extraction+normalization task hasn't run or was
   * cancelled), or if the inputs are out of range.
   *
   * @param passageNdx zero-based index into {@code matches}
   * @param matches    Tika-coordinate {@link PageMatch} list for the file
   *                   (typically from the highlight cache or {@code row.result().pageMatches()})
   * @return the 1-based converted-PDF page number, or -1 if no mapping is available
   */
  @Override int pageForPassage(int passageNdx, List<PageMatch> matches)
  {
    if ((convertedPdfNormText == null) || (tikaNormText == null) ||
        (matches == null) || (passageNdx < 0) || (passageNdx >= matches.size())) return -1;

    return findConvertedPdfPage(matches.get(passageNdx), tikaReverseMap, tikaNormText,
      convertedPdfNormText, convertedPdfPosMap, convertedPdfPageOffsets);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs on {@code executor}. Waits for LibreOffice conversion, then delegates
   * to {@link #extractAndApplyHits}. Conversion failures show the
   * unable-to-preview indicator (this pipeline has no display subscriber, so
   * nothing else would ever report them), except for the no-office-installation
   * failure, which OfficePreviewer already reported via the wrapper's alt
   * display at enqueue time; failures in the hit pipeline fall back to
   * displaying the converted PDF at page 1 with no highlights.
   */
  private void runExtractAndHighlight()
  {
    if (isDisposed()) return;

    // Block until LibreOffice finishes conversion

    FilePath convertedPath;

    try { convertedPath = extractionFuture.get(60, TimeUnit.SECONDS); }
    catch (Exception e)
    {
      failedBeforeViewerLoad = true;  // every exit from this catch ends the pipeline without a viewer load

      // Cancellation means this coordinator was disposed or the session was superseded,
      // and interruption means the executor is shutting down; in both cases the UI has
      // already moved on (or is tearing down), so stay quiet. The no-office failure is
      // a settings condition, not an error: OfficePreviewer already drove the wrapper's
      // no-office alt display before failing the session, so showing the generic
      // indicator here would overwrite the more specific message. Anything else is a
      // real conversion failure the user would otherwise never see.

      if   ((e instanceof CancellationException)
         || (e instanceof InterruptedException)
         || (e.getCause() instanceof CancellationException)
         || (e.getCause() instanceof NoOfficeInstallationException)
         || isDisposed())
        return;

      // Log the real failure (timeout, conversion error) before showing the indicator;
      // this pipeline has no display subscriber, so otherwise nothing records why it failed.

      logThrowable(e);

      Platform.runLater(() ->
      {
        if (isDisposed() == false)
          PreviewWindow.setUnable(pvsQueriesTab, db.getRootPath(row.path()));
      });

      return;
    }

    if (isDisposed()) return;

    try
    {
      extractAndApplyHits(convertedPath);
    }
    catch (Throwable e)
    {
      // The hit pipeline failed but the converted PDF itself is fine; fall back to
      // displaying it at page 1 with no highlights. Without this catch the failure
      // would be invisible: executor.submit captures unchecked exceptions in a
      // Future that nothing reads.

      logThrowable(e);

      Platform.runLater(() ->
      {
        if (isDisposed()) return;

        viewerLoaded = true;
        PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, 1, row.resolvedRecord());
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Extracts the converted PDF's text via pdf.js, strips LibreOffice
   * page-header metadata, normalizes both the converted-PDF text and the
   * Tika-indexed text for passage-click alignment, searches the converted PDF
   * for matches, builds per-page hit JSON, and finally (on the FX thread)
   * loads the converted PDF at the first-match page and applies hits.
   */
  private void extractAndApplyHits(FilePath convertedPath)
  {
    // Extract text from the converted PDF via the pdf.js extractor pool

    FullTextIndexer.ExtractionResult extraction = indexer.extractPdfText(convertedPath);

    if ((extraction == null) || strNullOrBlank(extraction.text()))
    {
      Platform.runLater(() ->
      {
        if (isDisposed()) return;

        viewerLoaded = true;
        PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, 1, row.resolvedRecord());
      });
      return;
    }

    if (isDisposed()) return;

    // Strip LibreOffice header/footer metadata from the converted PDF text.
    // LibreOffice inserts the source file path, page number, and save date at each page.
    // The page offsets must be adjusted in tandem since stripping changes character positions.

    String pdfRawText = extraction.text();
    int[] adjustedPageOffsets = extraction.pageOffsets().clone();

    if (db.isLoaded())
    {
      String dbRoot = db.getRootPath().toString().replace('/', '\\');
      pdfRawText = stripConvertedPdfHeaders(pdfRawText, dbRoot, adjustedPageOffsets);
    }

    // Normalize both texts for passage-click navigation:
    // 1. convertToEnglishCharsWithMap: Unicode to ASCII with position tracking
    // 2. toLowerCase
    // 3. collapseWhitespace: all whitespace runs to single space, with position tracking
    // Position maps chain: normalized output pos to original text pos

    ArrayList<Integer> pdfPosMap = new ArrayList<>();
    String normPdfText = normalizeForMatching(pdfRawText, pdfPosMap);

    String tikaText = indexer.getStoredContent(row.path());
    ArrayList<Integer> tikaPosMapFwd = new ArrayList<>();
    String normTikaTextLocal = (tikaText != null) ? normalizeForMatching(tikaText, tikaPosMapFwd) : "";
    int[] tikaRevMap = (tikaText != null) ? buildReversePositionMap(tikaPosMapFwd, tikaText.length()) : new int[0];

    // Build the query

    Query query = searchKeyQuery;

    if (query == null)
    {
      try
      {
        @SuppressWarnings("resource")
        var parser = FullTextIndexer.createQueryParser(indexer.getAnalyzer());
        query = parser.parse(queryStr);
      }
      catch (Exception e)
      {
        // The query parsed when the search ran, so this is unexpected; propagate so
        // runExtractAndHighlight logs it and falls back to a highlight-free load.

        throw new RuntimeException("Unable to re-parse FTS query for converted-PDF search", e);
      }
    }

    // Search the converted PDF's text using a temporary in-memory Lucene index

    List<PageMatch> convertedMatches = FullTextIndexer.searchExtractedText(extraction.text(), extraction.pageOffsets(), query);

    if (keyLookup != null)
      convertedMatches = rescanHitRanges(convertedMatches, keyLookup);

    if (isDisposed()) return;

    if (convertedMatches.isEmpty())
    {
      Platform.runLater(() ->
      {
        if (isDisposed()) return;

        viewerLoaded = true;
        PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, 1, row.resolvedRecord());
      });

      return;
    }

    // Build per-page hit JSON using the RAW page offsets (not the
    // header-stripped adjustedPageOffsets). The viewer's pdf.js renders the
    // converted PDF as-is, including LibreOffice's per-page header text, so
    // its textDivs concatenate to the raw extraction text. The match offsets
    // in convertedMatches are also in raw-text coordinates (we passed
    // extraction.text()/extraction.pageOffsets() to searchExtractedText above).
    // Using adjustedPageOffsets here would introduce a per-page drift equal
    // to the cumulative header strip, eventually exceeding page-text length
    // and causing applyHitsToPage to drop hits silently.

    String allHitsJson = buildAllHitsJson(convertedMatches, extraction.pageOffsets());

    // Determine the first page with a match; this is where the viewer will open

    int firstMatchPage = convertedMatches.stream()
      .mapToInt(PageMatch::pageNumber)
      .filter(p -> p > 0)
      .min().orElse(1);

    Platform.runLater(() ->
    {
      if (isDisposed()) return;

      // Load the converted PDF at the correct first-match page.
      // All page determination happened above; no rendering was involved.

      viewerLoaded = true;
      PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, firstMatchPage, row.resolvedRecord());

      boolean hitsApplied = (allHitsJson != null) && PreviewWindow.setAllHits(pvsQueriesTab, allHitsJson);

      if (hitsApplied)
      {
        convertedPdfNormText = normPdfText;
        convertedPdfPosMap = pdfPosMap;
        convertedPdfPageOffsets = adjustedPageOffsets;
        tikaNormText = normTikaTextLocal;
        tikaReverseMap = tikaRevMap;
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
