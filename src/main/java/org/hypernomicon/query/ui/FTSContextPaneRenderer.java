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

import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;

import org.hypernomicon.fts.FullTextIndexer.SearchResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile.WorkBoundary;

//---------------------------------------------------------------------------

/**
 * Renders the FTS context pane's HTML: the passage list shown below the
 * results table for the currently-selected file. Stateful across a single
 * file's render cycle; reset by the next {@link #renderInitial} call.
 * <p>
 * The rendered document contains inline CSS, a {@code selectPassage} JS
 * function, and (if more passages remain) an IntersectionObserver that
 * fires {@code alert('loadmore')} when the user scrolls to the sentinel.
 * The controller's {@code onAlert} handler is responsible for dispatching
 * those signals back to {@link #renderNextBatch}.
 */
class FTSContextPaneRenderer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final int PASSAGE_BATCH_SIZE = 30;

  private int passagesLoaded, boundaryNdx;
  private List<WorkBoundary> currentBoundaries = List.of(),
                             containmentStack = new ArrayList<>();
  private BitSet activeBoundarySet = new BitSet();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Renders the initial context pane HTML for a file. Resets all renderer
   * state and emits a full HTML document (CSS + JS + first batch of passages).
   * If more passages remain beyond the first batch, a sentinel element is
   * included whose IntersectionObserver triggers {@code alert('loadmore')}.
   *
   * @param path       relative path of the file being rendered (for the header)
   * @param matches    all page matches for the file
   * @param boundaries work-boundary list for the file (may be empty)
   * @return a complete HTML document string
   */
  String renderInitial(String path, List<PageMatch> matches, List<WorkBoundary> boundaries)
  {
    int end = Math.min(PASSAGE_BATCH_SIZE, matches.size());

    passagesLoaded = end;
    currentBoundaries = boundaries;
    boundaryNdx = 0;
    containmentStack = new ArrayList<>();

    // Pre-scan: determine which boundaries have at least one match within their range

    activeBoundarySet = new BitSet(currentBoundaries.size());

    for (int bNdx = 0; bNdx < currentBoundaries.size(); bNdx++)
    {
      WorkBoundary b = currentBoundaries.get(bNdx);

      boolean hasMatch = matches.stream().anyMatch(pm ->
      {
        int pg = pm.pageNumber();
        return (pg > 0) && (pg >= b.startPage()) && ((b.endPage() <= 0) || (pg <= b.endPage()));
      });

      if (hasMatch)
        activeBoundarySet.set(bNdx);
    }

    StringBuilder sb = new StringBuilder();

    sb.append("""
      <html><head><style>
        body { font-family: -apple-system, 'Segoe UI', sans-serif; font-size: 13px; margin: 12px; color: #222; }
        .file-path { font-size: 14px; font-weight: bold; color: #333; margin-bottom: 12px; word-break: break-all; }
        .match-count { font-size: 12px; color: #666; margin-bottom: 12px; }
        .passage { padding: 8px 10px; margin: 6px 0; border-left: 3px solid #ccc; background: #f8f8f8;
                    cursor: pointer; transition: border-left-color 0.15s; }
        .passage:hover { border-left-color: #4682B4; }
        .passage.selected { border-left-color: #4682B4; background: #eef4fa; }
        .page-label { font-size: 11px; color: #888; margin-bottom: 2px; }
        mark { background: #ffe066; padding: 0 1px; border-radius: 2px; }
        .snippet { line-height: 1.5; }
        .end-marker { text-align: center; color: #999; font-style: italic; padding: 12px; }
        .work-boundary { padding: 4px 10px; margin: 14px 0 2px; border-top: 1px solid #b0c4de;
                          cursor: pointer; font-size: 12px; color: #555; }
        .work-boundary:hover { color: #333; }
        .work-boundary img { vertical-align: middle; margin-right: 4px; }
        .work-boundary .author { color: #444; }
        .work-boundary .title { }
        .work-boundary .pages { color: #888; font-size: 11px; }
        .contained { margin-left: 18px; }
        #sentinel { height: 1px; }
      </style>
      <script>
        function selectPassage(ndx, page) {
          document.querySelectorAll('.passage.selected').forEach(function(el) { el.classList.remove('selected'); });
          document.getElementById('p-' + ndx).classList.add('selected');
          alert('page:' + page + ':' + ndx);
        }
      </script>
      </head><body>
      """);

    sb.append("<div class=\"file-path\">").append(htmlEscaper.escape(path)).append("</div>\n");

    int totalHits = matches.stream().mapToInt(pm -> nullSwitch(pm.hitRanges(), 0, List::size)).sum();

    sb.append("<div class=\"match-count\">").append(totalHits).append(totalHits == 1 ? " match" : " matches");

    if (matches.size() > 1)
      sb.append(" in ").append(matches.size()).append(" passages");

    sb.append("</div>\n");

    sb.append("<div id=\"passages\">\n");

    appendPassagesWithBoundaries(sb, matches, 0, end);

    sb.append("</div>\n");

    if (end < matches.size())
    {
      sb.append("<div id=\"sentinel\"></div>\n");
      sb.append("<script>\n");
      sb.append("new IntersectionObserver(function(entries) { if (entries[0].isIntersecting) alert('loadmore'); }, ");
      sb.append("{threshold: 0.1}).observe(document.getElementById('sentinel'));\n");
      sb.append("</script>\n");
    }
    else if (matches.isEmpty())
      sb.append("<div class=\"end-marker\">No highlighted passages found</div>\n");

    sb.append("</body></html>");
    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Renders the next batch of passages as HTML fragments (no document
   * chrome, no sentinel). Advances {@link #passagesLoaded}. Callers inject
   * the returned HTML into the existing document, then re-add their own
   * sentinel if {@link #hasMore} is still true.
   *
   * @param matches all page matches for the file (same list as the initial render)
   * @return HTML fragment containing the next batch of passage divs and
   *         any work-boundary markers that fall within them
   */
  String renderNextBatch(List<PageMatch> matches)
  {
    int end = Math.min(passagesLoaded + PASSAGE_BATCH_SIZE, matches.size());

    StringBuilder sb = new StringBuilder();
    appendPassagesWithBoundaries(sb, matches, passagesLoaded, end);

    passagesLoaded = end;
    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * @return whether a subsequent {@link #renderNextBatch} call would produce
   *         additional passages, given the match list used for the initial render
   */
  boolean hasMore(List<PageMatch> matches)
  {
    return passagesLoaded < matches.size();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Renders a stateless loading placeholder for a file whose highlights
   * are still being computed. Does not touch renderer state.
   */
  static String renderLoading(String path)
  {
    return "<html><head><style>" +
      "body { font-family: -apple-system, 'Segoe UI', sans-serif; font-size: 13px; margin: 12px; color: #222; }" +
      ".file-path { font-size: 14px; font-weight: bold; color: #333; margin-bottom: 12px; word-break: break-all; }" +
      ".loading { color: #999; font-style: italic; }" +
      "</style></head><body>" +
      "<div class=\"file-path\">" + htmlEscaper.escape(path) + "</div>" +
      "<div class=\"loading\">Loading passages…</div>" +
      "</body></html>";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Appends passages {@code [start, end)} from {@code matches} into {@code sb},
   * interleaving work-boundary markers and tracking containment. Advances
   * {@link #boundaryNdx} and mutates {@link #containmentStack}; both are
   * initialized by {@link #renderInitial} and carried across successive
   * {@link #renderNextBatch} calls.
   */
  private void appendPassagesWithBoundaries(StringBuilder sb, List<PageMatch> matches, int start, int end)
  {
    for (int ndx = start; ndx < end; ndx++)
    {
      int page = matches.get(ndx).pageNumber();

      while ((boundaryNdx < currentBoundaries.size()) && (currentBoundaries.get(boundaryNdx).startPage() <= page))
      {
        if (activeBoundarySet.get(boundaryNdx))
          appendWorkBoundaryHtml(sb, currentBoundaries.get(boundaryNdx));

        containmentStack.add(currentBoundaries.get(boundaryNdx));
        boundaryNdx++;
      }

      // Pop boundaries whose range this passage has exited

      containmentStack.removeIf(b -> (b.endPage() > 0) && (page > b.endPage()));

      boolean contained = (page > 0) && (containmentStack.isEmpty() == false);
      appendPassageHtml(sb, matches.get(ndx), ndx, contained);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendPassageHtml(StringBuilder sb, PageMatch pm, int ndx, boolean contained)
  {
    int page = pm.pageNumber();

    sb.append("<div id=\"p-").append(ndx).append("\" class=\"passage").append(contained ? " contained" : "")
      .append("\" onclick=\"selectPassage(").append(ndx).append(',').append(page > 0 ? page : 1).append(")\">\n");

    if (page > 0)
      sb.append("  <div class=\"page-label\">Page ").append(page).append("</div>\n");

    sb.append("  <div class=\"snippet\">").append(highlightSnippet(pm)).append("</div>\n");
    sb.append("</div>\n");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void appendWorkBoundaryHtml(StringBuilder sb, WorkBoundary boundary)
  {
    HDT_Work work = boundary.work();

    sb.append("<div class=\"work-boundary\" onclick=\"alert('work:").append(work.getID()).append("')\">\n")
      .append("  <img border=\"0\" width=\"16\" height=\"16\" src=\"").append(imgDataURIbyRecord(work)).append("\"> ");

    String authorStr = work.getShortAuthorsStr(true);

    if (strNotNullOrEmpty(authorStr))
      sb.append("<span class=\"author\">").append(htmlEscaper.escape(authorStr)).append("</span> ");

    String yearStr = work.getYearStr();

    if (strNotNullOrEmpty(yearStr))
      sb.append('(').append(htmlEscaper.escape(yearStr)).append(") ");

    sb.append("<span class=\"title\">").append(htmlEscaper.escape(work.name())).append("</span>");

    if (boundary.startPage() > 0)
      sb.append(" <span class=\"pages\">").append(formatPageRange(boundary.startPage(), boundary.endPage())).append("</span>");

    sb.append("\n</div>\n");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String highlightSnippet(PageMatch pm)
  {
    String snippet = pm.snippet();
    List<SearchResult.HitRange> ranges = pm.hitRanges();

    if (collEmpty(ranges))
      return htmlEscaper.escape(snippet);

    StringBuilder sb = new StringBuilder();
    int pos = 0;

    for (SearchResult.HitRange range : ranges)
    {
      int start = Math.max(range.start(), pos),
          end = Math.min(range.end(), snippet.length());

      if (start > pos)
        sb.append(htmlEscaper.escape(snippet.substring(pos, start)));

      if (end > start)
        sb.append("<mark>").append(htmlEscaper.escape(snippet.substring(start, end))).append("</mark>");

      pos = end;
    }

    if (pos < snippet.length())
      sb.append(htmlEscaper.escape(snippet.substring(pos)));

    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
