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

import org.hypernomicon.fts.FullTextIndexer.SearchResult.HitRange;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.model.searchKeys.Keyword;
import org.hypernomicon.model.searchKeys.SearchKeys;

import static org.hypernomicon.fts.FTSUtil.*;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;
import java.util.function.Function;

//---------------------------------------------------------------------------

/**
 * Unit tests for pure (no-database) {@link FTSUtil} helpers:
 * <ul>
 *   <li>{@link FTSUtil#rescanHitRanges}: the search-key highlight refinement that replaces
 *       Lucene's per-term offsets with one separator-insensitive span per keyword occurrence.</li>
 *   <li>{@link FTSUtil#stripConvertedPdfHeaders}: removal of LibreOffice page-header metadata
 *       (source path + date/time) from converted-PDF text, with page-offset adjustment.</li>
 * </ul>
 */
class FTSUtilTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static PageMatch passage(String snippet)
  {
    return new PageMatch(1, 0, snippet.length(), snippet, 1.0f, List.of());
  }

  private static String spanText(PageMatch pm, int rangeNdx)
  {
    HitRange hr = pm.hitRanges().get(rangeNdx);
    return pm.snippet().substring(hr.start(), hr.end());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRescanCollapsesMultiWordKeyToSingleSpan()
  {
    Function<String, Iterable<Keyword>> lookup = SearchKeys.buildAdHocLookup("Friedrich Nietzsche; ^Nietzsche");

    List<PageMatch> result = rescanHitRanges(List.of(passage("As Friedrich Nietzsche later argued, ...")), lookup);

    assertEquals(1, result.size());
    assertEquals(1, result.getFirst().hitRanges().size(), "the phrase is one span, not duplicated by ^Nietzsche");
    assertEquals("Friedrich Nietzsche", spanText(result.getFirst(), 0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRescanDropsPassageWithNoOccurrence()
  {
    Function<String, Iterable<Keyword>> lookup = SearchKeys.buildAdHocLookup("Nietzsche");

    List<PageMatch> result = rescanHitRanges(List.of(passage("Nietzsche wrote this."), passage("This passage is about cats.")), lookup);

    assertEquals(1, result.size(), "the passage with no occurrence is dropped");
    assertEquals("Nietzsche", spanText(result.getFirst(), 0));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRescanFallsBackToLuceneWhenFileHasNoOccurrence()
  {
    Function<String, Iterable<Keyword>> lookup = SearchKeys.buildAdHocLookup("Nietzsche");

    List<PageMatch> input = List.of(passage("Only cats here."), passage("And more cats."));

    assertSame(input, rescanHitRanges(input, lookup), "with no occurrence anywhere, the original Lucene matches are kept (no blank row)");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testRescanIsSeparatorInsensitive()
  {
    Function<String, Iterable<Keyword>> lookup = SearchKeys.buildAdHocLookup("dual-aspect");

    List<PageMatch> result = rescanHitRanges(List.of(passage("the dual aspect theory of mind")), lookup);

    assertEquals(1, result.size());
    assertEquals("dual aspect", spanText(result.getFirst(), 0), "hyphenated key matches the spaced text");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testStripsTwelveHourAmPmHeader()
  {
    // The original happy path: 12-hour timestamp with AM/PM. The match ends at "PM",
    // so the space that follows it (in "after") is preserved at the page boundary.

    String before = "Body before. ",
           header = "C:\\DB\\sub\\file.docx - 3 Last saved: 2/20/2026 6:14:00 PM",
           after  = " body after.";

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testStrips24HourHeaderWithoutMeridiem()
  {
    // 24-hour-clock locale: no AM/PM. The old bare-"am"/"pm" scan left the timestamp in
    // (or worse, ran into the body); anchoring on the HH:MM token strips it correctly.

    String before = "Body before. ",
           header = "C:\\DB\\sub\\file.docx - 3 Last saved: 2/20/2026 15:45",
           after  = " body after.";

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testDoesNotEatBodyWordEndingInMeridiem()
  {
    // Regression guard: with a 24-hour timestamp, the old code scanned for a bare "am "/"pm "
    // and would match the "am " inside "team", deleting "The team". The time-token anchor
    // stops at "15:45", leaving the body intact.

    String before = "Intro. ",
           header = "C:\\DB\\sub\\file.docx - 3 Last saved: 2/20/2026 15:45",
           after  = " The team gathered.";

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", null),
        "must not consume body text via the 'am' in 'team'");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testMeridiemMatchIsCaseInsensitive()
  {
    String before = "X ",
           header = "C:\\DB\\f.docx - 3 Last saved: 2/20/2026 6:14:00 pm",  // lowercase pm
           after  = " Y";

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testWordBoundaryGuardsAgainstMeridiemPrefixWord()
  {
    // "amazing" begins with "am" but is not a meridiem; the \b in the pattern keeps it from
    // being consumed, so only "15:45" is stripped and "amazing facts" survives.

    String before = "X ",
           header = "C:\\DB\\f.docx - 3 Last saved: 2/20/2026 15:45",
           after  = " amazing facts Y";

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFallsBackToRootWhenNoTimestamp()
  {
    // No time token within range: the unchanged fallback removes only the db-root literal.

    assertEquals("X \\f.docx no time Y", stripConvertedPdfHeaders("X C:\\DB\\f.docx no time Y", "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testReturnsTextUnchangedWhenRootAbsent()
  {
    String text = "No database path here.";
    assertEquals(text, stripConvertedPdfHeaders(text, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testGuardsNullTextAndBlankRoot()
  {
    assertNull  (stripConvertedPdfHeaders(null, "C:\\DB", null));
    assertEquals("text", stripConvertedPdfHeaders("text", "", null));
    assertEquals("text", stripConvertedPdfHeaders("text", null, null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testAdjustsPageOffsetsAroundHeader()
  {
    String before = "Body before. ",
           header = "C:\\DB\\sub\\file.docx - 3 Last saved: 2/20/2026 6:14:00 PM",
           after  = " body after.";

    int headerStart = before.length(),
        headerEnd   = before.length() + header.length();

    // One offset before the header, one inside it, one exactly at its end.

    int[] offsets = { 0, headerStart + 10, headerEnd };

    assertEquals(before + after, stripConvertedPdfHeaders(before + header + after, "C:\\DB", offsets));

    // Before stays; inside clamps to the header start; at-end shifts back by the removed length
    // (which equals the header length), all landing in stripped-text coordinates.

    assertArrayEquals(new int[] { 0, headerStart, headerStart }, offsets);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testHandlesMultipleHeadersCumulatively()
  {
    String h1   = "C:\\DB\\a.docx - 1 Last saved: 1/1/2026 9:00 AM",
           h2   = "C:\\DB\\a.docx - 2 Last saved: 1/1/2026 9:05 AM",
           text = "Page1. " + h1 + " Page2. " + h2 + " Page3.";

    assertEquals("Page1.  Page2.  Page3.", stripConvertedPdfHeaders(text, "C:\\DB", null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
