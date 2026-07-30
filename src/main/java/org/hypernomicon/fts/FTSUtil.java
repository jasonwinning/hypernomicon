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

import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.fts.FullTextIndexer.SearchResult.HitRange;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.searchKeys.*;
import org.hypernomicon.util.file.FilePath;

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
   * Normalizes text for matching between Tika and pdf.js extractions:
   * Unicode to ASCII via {@code convertToEnglishCharsWithMap}, lowercase, then
   * collapse whitespace. The position map tracks output positions back to original positions.
   *
   * @param text the raw text to normalize
   * @param posMap output parameter; on return, maps each normalized position to
   *               the corresponding position in the original text
   * @return the normalized text
   */
  public static String normalizeForMatching(String text, ArrayList<Integer> posMap)
  {
    return collapseWhitespace(convertToEnglishCharsWithMap(text, posMap).toLowerCase(), posMap);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Matches the SAVEDATE timestamp that ends each leaked page header
  // (HH:MM, optionally :SS, optionally AM/PM). Used to locate where the header ends.
  private static final Pattern HEADER_TIME = Pattern.compile("\\d{1,2}:\\d{2}(?::\\d{2})?(?:\\s*[ap]m\\b)?", Pattern.CASE_INSENSITIVE);

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Strips a leaked page-header artifact from converted-PDF text and adjusts page
   * offsets to match. The text is the source document's own header/footer field
   * codes ({@code FILENAME \p} + {@code PAGE} + {@code SAVEDATE}: full path, page
   * number, last-saved timestamp), not a banner added by LibreOffice or JodConverter.
   *
   * <p>The document holds an inactive/"stashed" header/footer (present in the DOCX
   * but not shown by Word). LibreOffice 24.x materialized it during headless PDF
   * export, so it leaked into the converted PDF; 26.2.0 stopped (most likely the
   * tdf#142785 stashed header/footer fix). The leaked text is absent from the Tika
   * extraction of the same file, so left in place it drifts passage/highlight
   * positions. On a version that doesn't materialize it this is a no-op (the db-root
   * path never appears in the extraction); kept as defense because the affected
   * versions (24.x/25.x, including the Mac minimum 24.2.6) are current.
   *
   * <p>Pattern: "...text C:\folder\...\file.docx - 3 Last saved: 2/20/2026 6:14:00 PM next page text..."
   *
   * @param pageOffsets if non-null, each entry is adjusted in-place to account
   *                    for removed header characters (must be a mutable copy)
   */
  public static String stripConvertedPdfHeaders(String text, String dbRoot, int[] pageOffsets)
  {
    if ((text == null) || strNullOrEmpty(dbRoot)) return text;

    StringBuilder sb = new StringBuilder(text.length());
    String textLower = text.toLowerCase(),
           rootLower = dbRoot.toLowerCase();

    int pos = 0, removedSoFar = 0, pageNdx = 0;

    while (pos < text.length())
    {
      int idx = textLower.indexOf(rootLower, pos);

      if (idx < 0)
      {
        sb.append(text, pos, text.length());
        break;
      }

      sb.append(text, pos, idx);

      // Find end of header: the materialized SAVEDATE field ends it with "<date> <time>", where <time> is
      // HH:MM[:SS] optionally followed by AM/PM. Anchor on that time token rather than a
      // bare "am"/"pm", so this handles 24-hour-clock locales and is not fooled by ordinary
      // words ending in "am"/"pm" (team, exam, diagram) that may follow the path.

      int searchEnd = Math.min(text.length(), idx + 500);

      Matcher matcher = HEADER_TIME.matcher(text).region(idx + rootLower.length(), searchEnd);

      int headerEnd = matcher.find() ? matcher.end() : (idx + rootLower.length()),
          headerLen = headerEnd - idx;

      // Adjust any page offsets that fall at or after this header

      if (pageOffsets != null)
      {
        while (pageNdx < pageOffsets.length)
        {
          if (pageOffsets[pageNdx] < idx)
          {
            // Before this header; adjust by cumulative removal so far

            pageOffsets[pageNdx] -= removedSoFar;
            pageNdx++;
          }
          else if (pageOffsets[pageNdx] < headerEnd)
          {
            // Inside this header; clamp to the header start in stripped text

            pageOffsets[pageNdx] = idx - removedSoFar;
            pageNdx++;
          }
          else
            break;  // Past this header; will be adjusted later
        }
      }

      removedSoFar += headerLen;
      pos = headerEnd;
    }

    // Adjust remaining page offsets (after the last header)

    if (pageOffsets != null)
    {
      while (pageNdx < pageOffsets.length)
      {
        pageOffsets[pageNdx] -= removedSoFar;
        pageNdx++;
      }
    }

    return sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns true if the file is an office document that OfficePreviewer converts to PDF
   * (as opposed to spreadsheets which convert to HTML).
   */
  public static boolean isOfficeDocConvertedToPdf(FilePath filePath)
  {
    String mime = getMediaType(filePath).toString();

    return (mime.contains("openxmlformats-officedocument") && (mime.contains("spreadsheet") == false))
      ||   "application/msword".equalsIgnoreCase(mime)
      ||   "application/rtf".equalsIgnoreCase(mime)
      ||   mime.contains("opendocument.text")
      ||   mime.contains("sun.xml.writer")
      ||   mime.contains("ms-powerpoint")
      ||   mime.contains("opendocument.presentation")
      ||   mime.contains("sun.xml.impress")
      ||   mime.contains("vnd.wordperfect");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds the per-page hit JSON consumed by the pdf.js viewer to highlight
   * matches, converting absolute character offsets within the document to
   * page-relative offsets using {@code pageOffsets} (page start offsets plus
   * a trailing total-length sentinel).
   * <p>
   * Each range is assigned its own page from its absolute offset rather than
   * inheriting {@link PageMatch#pageNumber()}: a PageMatch is a highlighter
   * passage whose page number comes from its first match, but a passage can
   * straddle a page boundary, putting later matches on the following page.
   * Offsets computed against the first match's page would then point past
   * that page's text, and the viewer clamps or drops such ranges (logged by
   * convertPageHits in javaapp.js).
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
      for (HitRange hr : pm.hitRanges())
      {
        int absStart = pm.startOffset() + hr.start(),
            absEnd   = pm.startOffset() + hr.end  ();

        int pageNum = pageForOffset(pageOffsets, absStart);
        if (pageNum < 1) continue;

        int pageStart = pageOffsets[pageNum - 1];

        // A range that itself crosses the page boundary (a wide phrase match)
        // keeps its full extent; the viewer clamps it to the page's text

        pageToRanges.computeIfAbsent(pageNum, _ -> new ArrayList<>()).add(new int[] { absStart - pageStart, absEnd - pageStart });
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
   * Largest 1-based page whose start offset is {@code <= charOffset}, or 0 for
   * an offset outside the document's pages. {@code pageOffsets} is
   * sentinel-terminated (page starts plus the total length), so offsets at or
   * beyond the sentinel return 0 rather than a phantom page.
   */
  private static int pageForOffset(int[] pageOffsets, int charOffset)
  {
    if ((pageOffsets.length < 2) || (charOffset < 0) || (charOffset >= pageOffsets[pageOffsets.length - 1]))
      return 0;

    int lo = 0, hi = pageOffsets.length - 2;

    while (lo <= hi)
    {
      int mid = (lo + hi) >>> 1;

      if (pageOffsets[mid] <= charOffset)
        lo = mid + 1;
      else
        hi = mid - 1;
    }

    return lo;
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

        String rawCtx = safeSubstring(storedContent, matchStart - contextPad, matchEnd + contextPad);

        // Normalize whitespace and special characters so context matches the DOM's text.
        // Characters that the browser may render as U+FFFD (replacement char) due to
        // encoding mismatches are stripped on both sides so the surrounding text matches.

        String ctx = normalizeForDomMatch(rawCtx);
        int relStart = normalizeForDomMatch(safeSubstring(storedContent, matchStart - contextPad, matchStart)).length(),
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
   * Maps a Tika-extracted PageMatch to a 1-based page number in the converted
   * PDF, by:
   * 1. Finding the absolute character offset of the first hit in Tika's text
   * 2. Extracting a context window from the normalized Tika text
   * 3. Searching for that context in the normalized converted PDF text
   * 4. Using the converted PDF's page offsets to determine the page number
   *
   * @param tikaMatch the Tika match to locate
   * @param tikaReverseMap input-to-output map for normalized Tika text
   * @param tikaNormText the normalized Tika text
   * @param convertedPdfNormText the normalized converted PDF text
   * @param convertedPdfPosMap output-to-input map for normalized PDF text
   * @param convertedPdfPageOffsets page boundary offsets in the converted-PDF text that
   *                                 {@code convertedPdfPosMap} maps into (header-stripped
   *                                 where stripping applied)
   * @return the 1-based page number, or -1 if not found
   */
  public static int findConvertedPdfPage(PageMatch tikaMatch, int[] tikaReverseMap, String tikaNormText,
                                         String convertedPdfNormText, ArrayList<Integer> convertedPdfPosMap,
                                         int[] convertedPdfPageOffsets)
  {
    if ((tikaReverseMap == null) || (tikaNormText == null) || (convertedPdfNormText == null) ||
        (convertedPdfPosMap == null) || (convertedPdfPageOffsets == null))
      return -1;

    // Get the absolute offset of the first hit in the Tika extraction

    int tikaAbsOffset = tikaMatch.startOffset();

    if ((tikaMatch.hitRanges() != null) && (tikaMatch.hitRanges().isEmpty() == false))
      tikaAbsOffset += tikaMatch.hitRanges().getFirst().start();

    // Convert to position in normalized Tika text

    if ((tikaAbsOffset < 0) || (tikaAbsOffset >= tikaReverseMap.length)) return -1;

    int tikaNormPos = tikaReverseMap[tikaAbsOffset];
    if (tikaNormPos < 0) return -1;

    int normHitPos = findPdfNormPos(tikaNormPos, tikaNormText, convertedPdfNormText, convertedPdfPosMap.size());

    if (normHitPos >= 0)
      return pageForPdfNormPos(normHitPos, convertedPdfPosMap, convertedPdfPageOffsets);

    // Last resort: estimate the page from the proportional position in the document

    int estimatedNormPos = (int) (((double) tikaNormPos / tikaNormText.length()) * convertedPdfPosMap.size());

    return ((estimatedNormPos >= 0) && (estimatedNormPos < convertedPdfPosMap.size()))
      ? pageForPdfNormPos(estimatedNormPos, convertedPdfPosMap, convertedPdfPageOffsets)
      : -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Finds the normalized-PDF position best matching a normalized-Tika position. Tries
   * progressively shorter context windows around {@code tikaNormPos} (the widest can span
   * footnote boundaries or injected page numbers where the two extractions diverge) and,
   * among multiple occurrences, picks the one closest to the proportional position in the
   * document. Returns a position in {@code [0, posMapSize)} (so it maps to a page via
   * {@link #pageForPdfNormPos}), or -1 if no mappable match is found.
   */
  public static int findPdfNormPos(int tikaNormPos, String tikaNormText, String pdfNormText, int posMapSize)
  {
    double proportionalPos = (double) tikaNormPos / tikaNormText.length();
    int expectedPdfNormPos = (int) (proportionalPos * pdfNormText.length());

    for (int halfWidth : new int[] { 40, 20, 10 })
    {
      int ctxStart = Math.max(0, tikaNormPos - halfWidth);

      String context = safeSubstring(tikaNormText, ctxStart, tikaNormPos + halfWidth);

      if (context.length() < 5) continue;

      // Among multiple occurrences, pick the one closest to the proportional position

      int bestPos = -1, bestDist = Integer.MAX_VALUE,
          searchFrom = 0;

      while (true)
      {
        int pos = pdfNormText.indexOf(context, searchFrom);
        if (pos < 0) break;

        int dist = Math.abs(pos - expectedPdfNormPos);
        if (dist < bestDist)
        {
          bestDist = dist;
          bestPos = pos;
        }

        searchFrom = pos + 1;
      }

      if (bestPos < 0) continue;

      int normHitPos = bestPos + (tikaNormPos - ctxStart);

      if ((normHitPos >= 0) && (normHitPos < posMapSize)) return normHitPos;
    }

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Maps a normalized-PDF position to a 1-based converted-PDF page via the normalized-to-original
   * position map and the page-boundary offsets.
   */
  public static int pageForPdfNormPos(int pdfNormPos, ArrayList<Integer> pdfPosMap, int[] pdfPageOffsets)
  {
    int origPdfPos = pdfPosMap.get(pdfNormPos);

    for (int pageNdx = pdfPageOffsets.length - 1; pageNdx >= 0; pageNdx--)
      if (origPdfPos >= pdfPageOffsets[pageNdx])
        return pageNdx + 1;  // 1-based page number

    return 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Recomputes each page match's hit ranges by scanning its snippet for occurrences of the
   * given search keys, replacing Lucene's per-term offsets with one span per keyword
   * occurrence (so a multi-word key reads as a single highlight rather than a duplicated
   * one). Scanning is separator-insensitive, so a key matches whichever way the text
   * punctuates it. Passages with no occurrence are dropped; if that empties the whole file
   * (a Lucene match the scanner can't pin down), the original matches are returned unchanged
   * so the result is never blank.
   *
   * @param matches   the Lucene-highlighted page matches for one file
   * @param keyLookup the ad-hoc keyword lookup (see {@code SearchKeys.buildAdHocLookup})
   * @return rescanned matches, or {@code matches} unchanged when the scanner finds nothing
   */
  public static List<PageMatch> rescanHitRanges(List<PageMatch> matches, Function<String, Iterable<Keyword>> keyLookup)
  {
    if (collEmpty(matches) || (keyLookup == null)) return matches;

    List<PageMatch> rescanned = new ArrayList<>();

    for (PageMatch pm : matches)
    {
      String snippet = pm.snippet();
      if (snippet == null) continue;

      List<HitRange> ranges = new ArrayList<>();

      for (KeywordLink link : KeywordLinkScanner.scan(snippet, keyLookup, true))
        ranges.add(new HitRange(link.getOffset(), link.getOffset() + link.getLength()));

      if (ranges.isEmpty() == false)
        rescanned.add(new PageMatch(pm.pageNumber(), pm.startOffset(), pm.endOffset(), snippet, pm.score(), ranges));
    }

    return rescanned.isEmpty() ? matches : rescanned;
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
