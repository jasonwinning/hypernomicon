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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;
import java.util.concurrent.*;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.fts.HitSetService;
import org.hypernomicon.fts.HitSetService.ConvertedPdfAlignment;
import org.hypernomicon.fts.HitSetService.PagedHits;
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
 * Owns the {@link ConversionSession} extraction subscription and the
 * await/error/delivery policy around the hit pipeline; the pipeline's
 * computation itself (extraction, header stripping, alignment, matching, hit
 * JSON) lives in {@link HitSetService#computeConvertedPdfHits}.
 * <p>
 * The conversion and viewer load are deliberately ordered so the first page
 * shown to the user is already the first-match page: compute first, <em>then</em>
 * call {@link PreviewWindow#loadConvertedPDF} and apply hits.
 */
final class ConvertedOfficeHitCoordinator extends FileHighlightCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HitSetService hitService;
  private final HitSetService.QueryDescriptor query;

  private CompletableFuture<FilePath> extractionFuture;

  // Tika-to-pdf.js coordinate alignment; written on the FX thread when the
  // pipeline delivers (whether or not hits were applied), consumed by
  // pageForPassage() on the FX thread for passage-click navigation.

  private ConvertedPdfAlignment alignment;

  // Pipeline-outcome signals consumed by FTSQueryCtrlr.setPreview's same-file
  // branch (see viewerLoaded/failedBeforeViewerLoad in the base class).
  // Written by the background task (or its runLater continuations), read on
  // the FX thread; volatile suffices since each is an independent flag.

  private volatile boolean viewerLoaded, failedBeforeViewerLoad;

//---------------------------------------------------------------------------

  /**
   * @param row        the currently-selected FTS result row (converted office file)
   * @param indexer    the live {@link FullTextIndexer}
   * @param hitService the hit-set service whose worker thread runs the pipeline;
   *                   its active query descriptor is captured at construction
   */
  ConvertedOfficeHitCoordinator(FTSResultRow row, FullTextIndexer indexer, HitSetService hitService)
  {
    super(row, indexer);

    this.hitService = hitService;
    this.query = hitService.query();
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

    hitService.execute(this::runExtractAndHighlight);
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
   * displays. Returns -1 if the alignment hasn't been delivered yet (the
   * background pipeline hasn't run or was cancelled), or if the inputs are out
   * of range.
   *
   * @param passageNdx zero-based index into {@code matches}
   * @param matches    Tika-coordinate {@link PageMatch} list for the file
   *                   (typically from the highlight cache or {@code row.result().pageMatches()})
   * @return the 1-based converted-PDF page number, or -1 if no mapping is available
   */
  @Override int pageForPassage(int passageNdx, List<PageMatch> matches)
  {
    if ((alignment == null) || (matches == null) || (passageNdx < 0) || (passageNdx >= matches.size())) return -1;

    return alignment.pageForPassage(matches.get(passageNdx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs on the hit service's worker thread. Waits for LibreOffice conversion,
   * computes the hit set via {@link HitSetService#computeConvertedPdfHits},
   * then (on the FX thread) loads the converted PDF at the first-match page,
   * applies hits, and publishes the alignment. Conversion failures show the
   * unable-to-preview indicator (this pipeline has no display subscriber, so
   * nothing else would ever report them), except for the no-office-installation
   * failure, which the office-preview display path already reported via the
   * wrapper's alt display; failures in the hit pipeline fall back to
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
      // a settings condition, not an error: the display path already drove the wrapper's
      // no-office alt display before the session failed, so showing the generic
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

    PagedHits hits;

    try
    {
      String dbRootPathStr = db.isLoaded() ? db.getRootPath().toString().replace('/', '\\') : null;

      hits = HitSetService.computeConvertedPdfHits(HitSetService.TextSource.of(indexer), query, row.path(), convertedPath, dbRootPathStr);
    }
    catch (Throwable e)
    {
      // The hit pipeline failed but the converted PDF itself is fine; fall back to
      // displaying it at page 1 with no highlights. Without this catch the failure
      // would be invisible: the worker thread's submit captures unchecked exceptions
      // in a Future that nothing reads.

      logThrowable(e);

      loadWithoutHits(convertedPath);
      return;
    }

    if (isDisposed()) return;

    if (hits == null)  // text extraction came up empty; no hits or alignment possible
    {
      loadWithoutHits(convertedPath);
      return;
    }

    PagedHits finalHits = hits;

    Platform.runLater(() ->
    {
      if (isDisposed()) return;

      // Load the converted PDF at the correct first-match page.
      // All page determination happened above; no rendering was involved.

      viewerLoaded = true;
      PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, finalHits.firstMatchPage(), row.resolvedRecord());

      if (finalHits.hitsJson() != null)
        PreviewWindow.setAllHits(pvsQueriesTab, finalHits.hitsJson());

      // The alignment is published whether or not hits were applied: passage-click
      // navigation works from the alignment alone and must not depend on delivery.

      alignment = finalHits.alignment();
    });
  }

//---------------------------------------------------------------------------

  private void loadWithoutHits(FilePath convertedPath)
  {
    Platform.runLater(() ->
    {
      if (isDisposed()) return;

      viewerLoaded = true;
      PreviewWindow.loadConvertedPDF(pvsQueriesTab, db.getRootPath(row.path()), convertedPath, 1, row.resolvedRecord());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
