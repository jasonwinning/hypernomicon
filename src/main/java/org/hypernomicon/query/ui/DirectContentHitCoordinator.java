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
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.query.ui.FTSQueryCtrlr.FTSResultRow;

//---------------------------------------------------------------------------

/**
 * Highlight coordinator for non-PDF content loaded directly into the browser
 * (HTML, plain text, XML, etc.). Extracts context windows around each match
 * from Lucene's stored text, then ships them to the viewer; JS walks the DOM
 * to find and wrap the matched portions.
 */
final class DirectContentHitCoordinator extends PreloadedHitCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  DirectContentHitCoordinator(FTSResultRow row, FullTextIndexer indexer, List<PageMatch> matches, int startPage)
  {
    super(row, indexer, matches, startPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override String buildHitsJson()
  {
    return nullSwitch(indexer.getStoredContent(path()), null, storedContent -> buildDirectContentHitsJson(matches, storedContent));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
