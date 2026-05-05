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

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;

//---------------------------------------------------------------------------

/**
 * Owns the full lifecycle of FTS hit highlighting for one active file in the
 * preview window: loading the file into the viewer, computing hit locations,
 * and applying highlights.
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

}
