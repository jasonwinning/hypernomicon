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

import java.util.ArrayList;
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
 *   <li>{@link FTSUtil#findPdfNormPos}: locating a normalized-Tika position in the normalized
 *       pdf.js text via proportional, progressively-shorter context-window search.</li>
 *   <li>{@link FTSUtil#pageForPdfNormPos}: mapping a normalized-PDF position to a 1-based page.</li>
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

  private static ArrayList<Integer> posMap(int... values)
  {
    ArrayList<Integer> list = new ArrayList<>(values.length);
    for (int value : values) list.add(value);
    return list;
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

  @Test
  void testFindPdfNormPosIdenticalTextReturnsSamePosition()
  {
    String text = "the quick brown fox jumps over the lazy dog and then runs off";

    assertEquals(20, findPdfNormPos(20, text, text, text.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosAtStartOfText()
  {
    String text = "alphabet soup is quite delicious today indeed yes";

    assertEquals(0, findPdfNormPos(0, text, text, text.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosNearEndOfText()
  {
    String text = "alphabet soup is quite delicious today indeed yes really";
    int pos = text.length() - 3;

    assertEquals(pos, findPdfNormPos(pos, text, text, text.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosShiftedByPrefix()
  {
    // A pure prefix shifts every logical position by the prefix length.

    String tika   = "the quick brown fox jumps over the lazy dog",
           prefix = "PAGE 1 OF 3   ",
           pdf    = prefix + tika;

    int tikaPos = 16;  // start of "fox"

    assertEquals(tikaPos + prefix.length(), findPdfNormPos(tikaPos, tika, pdf, pdf.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosPicksNearOccurrenceForEarlyPosition()
  {
    // The context appears twice in the pdf text; an early Tika position resolves to the
    // occurrence closest to the proportional (early) expected position.

    String phrase = "the quick brown",
           filler = "0123456789012345678901234567890123456789012345678",  // 49 chars, no match
           pdf    = phrase + filler + phrase;

    assertEquals(pdf.indexOf(phrase) + 2, findPdfNormPos(2, phrase, pdf, pdf.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosPicksFarOccurrenceForLatePosition()
  {
    // Same two occurrences, but a late Tika position resolves to the later occurrence.

    String phrase = "the quick brown",
           filler = "0123456789012345678901234567890123456789012345678",
           pdf    = phrase + filler + phrase;

    assertEquals(pdf.lastIndexOf(phrase) + 12, findPdfNormPos(12, phrase, pdf, pdf.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosFallsBackToShorterWindow()
  {
    // Tika and pdf share only the immediate neighborhood (within 10 chars) around the unique
    // '@' marker; their wider surroundings differ, so the 40-char and 20-char context windows
    // fail to match and the search must fall through to the 10-char window.

    String core = "LLLLLLLLLL@RRRRRRRRRR",
           tika = "aaaaaaaaaaaaaaa" + core + "bbbbbbbbbbbbbbb",
           pdf  = "ppppppppppppppp" + core + "qqqqqqqqqqqqqqq";

    assertEquals(pdf.indexOf('@'), findPdfNormPos(tika.indexOf('@'), tika, pdf, pdf.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosReturnsMinusOneWhenNoMatch()
  {
    String tika = "aaaaa bbbbb ccccc ddddd eeeee",
           pdf  = "11111 22222 33333 44444 55555";  // disjoint alphabet, no shared context

    assertEquals(-1, findPdfNormPos(10, tika, pdf, pdf.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosReturnsMinusOneWhenContextTooShort()
  {
    // Whole text is under 5 chars, so every window yields a sub-5 context and is skipped.

    String text = "abcd";

    assertEquals(-1, findPdfNormPos(2, text, text, text.length()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testFindPdfNormPosReturnsMinusOneWhenPositionUnmappable()
  {
    // A match is found, but the resulting position is past posMapSize, so it is rejected.

    String text = "the quick brown fox jumps over the lazy dog";

    assertEquals(-1, findPdfNormPos(40, text, text, 5));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPageForPdfNormPosFirstPage()
  {
    ArrayList<Integer> map = posMap(0, 50, 99);
    int[] pageOffsets = { 0, 100, 200 };

    assertEquals(1, pageForPdfNormPos(0, map, pageOffsets));  // original pos 0
    assertEquals(1, pageForPdfNormPos(1, map, pageOffsets));  // original pos 50
    assertEquals(1, pageForPdfNormPos(2, map, pageOffsets));  // original pos 99
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPageForPdfNormPosBoundariesAndLaterPages()
  {
    ArrayList<Integer> map = posMap(100, 150, 199, 200, 350);
    int[] pageOffsets = { 0, 100, 200 };

    assertEquals(2, pageForPdfNormPos(0, map, pageOffsets));  // 100, on the page-2 boundary
    assertEquals(2, pageForPdfNormPos(1, map, pageOffsets));  // 150
    assertEquals(2, pageForPdfNormPos(2, map, pageOffsets));  // 199
    assertEquals(3, pageForPdfNormPos(3, map, pageOffsets));  // 200, on the page-3 boundary
    assertEquals(3, pageForPdfNormPos(4, map, pageOffsets));  // 350
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPageForPdfNormPosSinglePage()
  {
    ArrayList<Integer> map = posMap(0, 5000);
    int[] pageOffsets = { 0 };

    assertEquals(1, pageForPdfNormPos(0, map, pageOffsets));
    assertEquals(1, pageForPdfNormPos(1, map, pageOffsets));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPageForPdfNormPosUsesPosMapIndirection()
  {
    // The page is determined by the ORIGINAL position from the map, not the normalized index.

    ArrayList<Integer> map = posMap(250, 50);  // index 0 maps to orig 250 (page 3); index 1 to orig 50 (page 1)
    int[] pageOffsets = { 0, 100, 200 };

    assertEquals(3, pageForPdfNormPos(0, map, pageOffsets));
    assertEquals(1, pageForPdfNormPos(1, map, pageOffsets));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test
  void testPageForPdfNormPosFallsBackToOneBelowFirstOffset()
  {
    // Degenerate: the original position precedes the first page offset, so the backward scan
    // matches no page and it falls back to page 1.

    ArrayList<Integer> map = posMap(2);
    int[] pageOffsets = { 5, 10 };

    assertEquals(1, pageForPdfNormPos(0, map, pageOffsets));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
