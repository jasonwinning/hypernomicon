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

import java.util.List;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;

//---------------------------------------------------------------------------

/**
 * Owns the full lifecycle of FTS hit highlighting for one active file in the
 * preview window: loading the file into the viewer, computing hit locations,
 * applying highlights, and (for converted office docs) coordinating with a
 * {@code ConversionSession} for the LibreOffice-to-PDF conversion pipeline.
 * <p>
 * One coordinator per active file; construct a fresh one on file switch and
 * call {@link #dispose} on the outgoing one. Stale coordinators become
 * unreferenced, so async work checks {@link #isDisposed} at each await point
 * rather than comparing a generation counter.
 * <p>
 * Concrete subclasses select behavior by file type:
 * <ul>
 *   <li>{@link PdfHitCoordinator}: native PDFs with per-page offsets</li>
 *   <li>{@link DirectContentHitCoordinator}: HTML/TXT loaded directly into the browser</li>
 *   <li>{@link ConvertedOfficeHitCoordinator}: office docs converted to PDF via
 *       {@code ConversionSession}; owns the extraction future and the Tika/pdf.js
 *       coordinate-translation state used by passage-click navigation</li>
 * </ul>
 */
abstract class FileHighlightCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final FTSResultRow row;
  protected final FullTextIndexer indexer;

  private volatile boolean disposed;

//---------------------------------------------------------------------------

  protected FileHighlightCoordinator(FTSResultRow row, FullTextIndexer indexer)
  {
    this.row = row;
    this.indexer = indexer;
  }

//---------------------------------------------------------------------------

  final String path()        { return row.path(); }
  final boolean isDisposed() { return disposed; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Kicks off whatever work the coordinator needs to do: loading the file
   * into the viewer, computing hits, etc. Called once, synchronously, after
   * construction.
   */
  abstract void start();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Marks this coordinator as disposed and performs subclass-specific cleanup
   * (cancel in-flight futures, release session subscribers, clear viewer hits).
   * Idempotent: repeated calls are safe.
   */
  final void dispose()
  {
    if (disposed) return;
    disposed = true;
    doDispose();
  }

  protected abstract void doDispose();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * For passage-click navigation: returns the 1-based viewer page to scroll
   * to for the given passage index, or -1 if the coordinator cannot map
   * passage indices to pages (e.g., non-PDF, or converted office before
   * extraction alignment has completed).
   * <p>
   * Default returns -1. Only {@link ConvertedOfficeHitCoordinator} overrides.
   *
   * @param passageNdx zero-based index into the context pane's passage list,
   *                   as emitted by the {@code selectPassage} JS function
   * @param matches    the active file's {@link PageMatch} list (Tika-extracted
   *                   coordinates for converted-office files); the coordinator
   *                   indexes into this list using {@code passageNdx}
   * @return the 1-based viewer page number, or -1 if no mapping is available
   */
  int pageForPassage(int passageNdx, List<PageMatch> matches)
  {
    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether the viewer has been loaded with this coordinator's file, so a
   * same-file navigation request can safely route through
   * {@code PreviewWindow.setPreview} (whose already-loaded check then
   * short-circuits to page navigation instead of falling through to
   * {@code showFile}).
   * <p>
   * Default true: for natively-viewable files, {@code FTSQueryCtrlr.setPreview}
   * loads the viewer synchronously before constructing the coordinator. Only
   * {@link ConvertedOfficeHitCoordinator} overrides, because its viewer load
   * happens at the end of the async conversion/extraction pipeline.
   */
  boolean viewerLoaded()
  {
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether this coordinator's pipeline ended without ever loading the viewer
   * (e.g., no office installation was configured, or the conversion failed).
   * A same-file request should then rebuild the coordinator to retry the full
   * pipeline rather than waiting for a load that will never come.
   * <p>
   * Default false; only {@link ConvertedOfficeHitCoordinator} overrides.
   */
  boolean failedBeforeViewerLoad()
  {
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
