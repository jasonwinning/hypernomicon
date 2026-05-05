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

import java.util.List;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;

//---------------------------------------------------------------------------

/**
 * Highlight coordinator for native PDF files. Loads the PDF at the specified
 * start page, then pushes per-page hit data to the viewer; pdf.js applies
 * highlights lazily per page via its {@code _finishRendering} hook.
 */
final class PdfHitCoordinator extends PreloadedHitCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  PdfHitCoordinator(FTSResultRow row, FullTextIndexer indexer, List<PageMatch> matches, int startPage)
  {
    super(row, indexer, matches, startPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override String buildHitsJson()
  {
    int[] pageOffsets = indexer.getPageOffsets(path());

    return pageOffsets == null ? null : buildAllHitsJson(matches, pageOffsets);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
