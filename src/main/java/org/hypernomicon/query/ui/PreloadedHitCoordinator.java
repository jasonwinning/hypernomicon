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

import java.util.List;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;

//---------------------------------------------------------------------------

/**
 * Base for coordinators whose hit data is available synchronously when the
 * coordinator starts, so the file can be loaded into the viewer and its hits
 * pushed in a single pass. {@link #start()} loads the preview at the start
 * page, clears any prior hits, and ships the JSON returned by
 * {@link #buildHitsJson()}; subclasses supply only that method.
 */
abstract class PreloadedHitCoordinator extends FileHighlightCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final List<PageMatch> matches;
  private final int startPage;

//---------------------------------------------------------------------------

  PreloadedHitCoordinator(FTSResultRow row, FullTextIndexer indexer, List<PageMatch> matches, int startPage)
  {
    super(row, indexer);
    this.matches = matches;
    this.startPage = startPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override final void start()
  {
    PreviewWindow.setPreview(pvsQueriesTab, db.getRootPath(row.path()), startPage, -1, row.resolvedRecord());

    if (isDisposed()) return;

    PreviewWindow.clearAllHits(pvsQueriesTab);

    String hitsJson = buildHitsJson();
    if (hitsJson == null) return;

    PreviewWindow.setAllHits(pvsQueriesTab, hitsJson);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected final void doDispose()
  {
    PreviewWindow.clearAllHits(pvsQueriesTab);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Fetches this file type's stored data and builds the per-file hits JSON to
   * push to the viewer, or returns null if the data is unavailable.
   */
  abstract String buildHitsJson();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
