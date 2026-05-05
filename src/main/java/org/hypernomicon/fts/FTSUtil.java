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

import static org.hypernomicon.util.Util.*;

import java.util.*;

import org.hypernomicon.fts.FullTextIndexer.SearchResult.HitRange;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;

//---------------------------------------------------------------------------

/**
 * Utility methods for full-text search.
 */
public final class FTSUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FTSUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whitespace and Unicode-curly-quote normalization so server-side context
   * snippets can be located in the DOM's rendered text. Strips fancy quotes
   * and dashes that may have been encoded differently by the browser.
   */
  private static String normalizeForDomMatch(String s)
  {
    // Strip fancy quotes/dashes and the replacement char first, then convert
    // NBSP to space, then collapse whitespace. The order matters: collapsing
    // whitespace before stripping would leave a double space wherever a stripped
    // char sat between two spaces (e.g. " \u2014 "), and the JS-side normalizer
    // collapses on the fly, so contexts produced here would no longer match.

    return s.replace("\u2018", "").replace("\u2019", "")
            .replace("\u201C", "").replace("\u201D", "")
            .replace("\u2014", "").replace("\u2013", "")
            .replace("\uFFFD", "")
            .replace("\u00A0", " ")
            .replaceAll("\\s+", " ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the per-page hit JSON consumed by the pdf.js viewer to highlight
   * matches. Maps each {@link PageMatch} to its page number and converts
   * absolute character offsets within the document to page-relative offsets
   * using {@code pageOffsets}.
   *
   * @return the JSON object as a string ({@code {"page":[[start,end],...],...}}),
   *         or {@code null} if no usable ranges were produced
   */
  public static String buildAllHitsJson(List<PageMatch> matches, int[] pageOffsets)
  {
    StringBuilder sb = new StringBuilder("{");
    boolean firstPage = true;

    // Group hit ranges by page number

    Map<Integer, List<int[]>> pageToRanges = new LinkedHashMap<>();

    for (PageMatch pm : matches)
    {
      int pageNum = pm.pageNumber();
      if ((pageNum < 1) || (pageNum > pageOffsets.length)) continue;

      int pageStart = pageOffsets[pageNum - 1];

      for (HitRange hr : pm.hitRanges())
      {
        int relStart = (pm.startOffset() + hr.start()) - pageStart,
            relEnd   = (pm.startOffset() + hr.end  ()) - pageStart;

        pageToRanges.computeIfAbsent(pageNum, _ -> new ArrayList<>()).add(new int[] { relStart, relEnd });
      }
    }

    if (pageToRanges.isEmpty()) return null;

    for (Map.Entry<Integer, List<int[]>> entry : pageToRanges.entrySet())
    {
      if (firstPage == false) sb.append(',');
      sb.append('"').append(entry.getKey()).append("\":[");

      boolean firstRange = true;

      for (int[] range : entry.getValue())
      {
        if (firstRange == false) sb.append(',');
        sb.append('[').append(range[0]).append(',').append(range[1]).append(']');
        firstRange = false;
      }

      sb.append(']');
      firstPage = false;
    }

    sb.append('}');
    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the JSON consumed by the direct-content viewer: a list of context
   * windows (with embedded match offsets) extracted from {@code storedContent}.
   * The viewer's JS walks the DOM to locate each context and wraps the matched
   * portion. Returns {@code null} when no usable matches were produced.
   */
  public static String buildDirectContentHitsJson(List<PageMatch> matches, String storedContent)
  {
    StringBuilder sb = new StringBuilder("{\"matches\":[");
    boolean first = true;
    int contextPad = 20;

    for (PageMatch pm : matches)
    {
      for (HitRange hr : pm.hitRanges())
      {
        int matchStart = pm.startOffset() + hr.start(),
            matchEnd   = pm.startOffset() + hr.end();

        if ((matchStart < 0) || (matchEnd > storedContent.length()) || (matchStart >= matchEnd)) continue;

        int ctxStart = Math.max(0, matchStart - contextPad),
            ctxEnd   = Math.min(storedContent.length(), matchEnd + contextPad);

        String rawCtx = storedContent.substring(ctxStart, ctxEnd);

        // Normalize whitespace and special characters so context matches the DOM's text.
        // Characters that the browser may render as U+FFFD (replacement char) due to
        // encoding mismatches are stripped on both sides so the surrounding text matches.

        String ctx = normalizeForDomMatch(rawCtx);
        int relStart = normalizeForDomMatch(storedContent.substring(ctxStart, matchStart)).length(),
            relEnd   = relStart + normalizeForDomMatch(storedContent.substring(matchStart, matchEnd)).length();

        if (first == false) sb.append(',');
        sb.append("{\"ctx\":\"").append(ctx
          .replace("\\", "\\\\")
          .replace("\"", "\\\"")
          .replace("\n", "\\n")
          .replace("\r", "\\r")
          .replace("\t", "\\t"))
          .append("\",\"s\":").append(relStart)
          .append(",\"e\":").append(relEnd).append('}');
        first = false;
      }
    }

    sb.append("]}");
    return first ? null : sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns an HTML snippet for the given {@link PageMatch}, with each hit
   * range wrapped in a {@code <mark>} element. Surrounding text is HTML-escaped.
   */
  public static String highlightSnippet(PageMatch pm)
  {
    String snippet = pm.snippet();
    if (snippet == null) return "";

    List<HitRange> ranges = pm.hitRanges();
    if (collEmpty(ranges)) return htmlEscaper.escape(snippet);

    StringBuilder sb = new StringBuilder();
    int pos = 0;

    for (HitRange hr : ranges)
    {
      int start = hr.start(), end = hr.end();

      if (start > pos)
        sb.append(htmlEscaper.escape(snippet.substring(pos, start)));

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
