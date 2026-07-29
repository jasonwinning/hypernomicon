/*
 * Copyright 2026 Jason Winning
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

import static org.junit.jupiter.api.Assertions.*;

import java.util.List;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.index.Term;
import org.apache.lucene.search.TermQuery;

import org.hypernomicon.fts.FullTextIndexer.ExtractionResult;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.HitRange;
import org.hypernomicon.fts.FullTextIndexer.SearchResult.PageMatch;
import org.hypernomicon.fts.HitSetService.*;
import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link HitSetService}'s hit computations, run against a
 * fake {@link TextSource} (no index, no pdf.js extractor pool, no JavaFX).
 * The converted-PDF pipeline is exercised end to end: the matching inside it
 * runs real in-memory Lucene via {@link FullTextIndexer#searchExtractedText},
 * so these tests pin the service's boundary contracts, most importantly that
 * the coordinate alignment is always part of the computed value and that hit
 * JSON is built in raw (unstripped) text coordinates.
 * <p>
 * The asynchronous request paths ({@code requestMatches},
 * {@code computeMatchesForBatch}) read the live indexer from the loaded
 * database and are exercised through the application, not here.
 */
class HitSetServiceTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Two-page converted-PDF text; the query term appears only on page 2

  private static final String PDF_PAGE_1 = "the quick brown fox jumps over the lazy dog near the river bank. ",
                              PDF_PAGE_2 = "meanwhile the machine hums quietly in the corner of the archive room. ",
                              PDF_TEXT = PDF_PAGE_1 + PDF_PAGE_2;

  // Page-offsets format: one entry per page start plus a total-length sentinel

  private static final int[] PDF_PAGE_OFFSETS = { 0, PDF_PAGE_1.length(), PDF_TEXT.length() };

  // The same content as Tika would extract it: line breaks instead of some
  // spaces, so the normalized texts align but the raw texts differ

  private static final String TIKA_TEXT = "the quick brown fox jumps\nover the lazy dog near the river bank.\n" +
                                          "meanwhile the machine hums quietly in the\ncorner of the archive room.\n";

  private static final FilePath CONVERTED_PATH = FilePath.of("converted.pdf");

  private static final String INDEX_PATH = "docs/chapter.docx";

//---------------------------------------------------------------------------

  /** Fake text source; per-field setup scripts each test's scenario. */
  private static final class FakeTextSource implements TextSource
  {
    private ExtractionResult extraction = null;
    private String storedContent = null;
    private int[] pageOffsets = null;

    private final Analyzer analyzer = FullTextIndexer.createAnalyzer();

    @Override public ExtractionResult extractPdfText(FilePath filePath) { return extraction; }
    @Override public String getStoredContent(String relativePath)       { return storedContent; }
    @Override public int[] getPageOffsets(String relativePath)          { return pageOffsets; }
    @Override public Analyzer getAnalyzer()                             { return analyzer; }
  }

  private final FakeTextSource source = new FakeTextSource();

  @AfterEach void tearDown()
  {
    source.analyzer.close();
  }

//---------------------------------------------------------------------------

  private static QueryDescriptor plainQuery(String queryStr)
  {
    return new QueryDescriptor(queryStr, null, null, null);
  }

  private void setupTwoPagePdf()
  {
    source.extraction = new ExtractionResult(PDF_TEXT, PDF_PAGE_OFFSETS.clone(), 2);
    source.storedContent = TIKA_TEXT;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void nullExtractionYieldsNull()
  {
    source.extraction = null;

    assertNull(HitSetService.computeConvertedPdfHits(source, plainQuery("machine"), INDEX_PATH, CONVERTED_PATH, null));
  }

//---------------------------------------------------------------------------

  @Test void blankExtractionYieldsNull()
  {
    source.extraction = new ExtractionResult("   ", new int[] { 0 }, 1);

    assertNull(HitSetService.computeConvertedPdfHits(source, plainQuery("machine"), INDEX_PATH, CONVERTED_PATH, null));
  }

//---------------------------------------------------------------------------

  @Test void matchProducesHitsJsonFirstMatchPageAndAlignment()
  {
    setupTwoPagePdf();

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, plainQuery("machine"), INDEX_PATH, CONVERTED_PATH, null);

    assertNotNull(hits);
    assertEquals(2, hits.firstMatchPage());
    assertNotNull(hits.alignment());

    // The hit range is page-relative: "machine" at its offset within page 2

    int relStart = PDF_PAGE_2.indexOf("machine");

    assertNotNull(hits.hitsJson());
    assertTrue(hits.hitsJson().contains("\"2\":[[" + relStart + ',' + (relStart + "machine".length()) + "]]"),
        "hit JSON should carry the page-relative range on page 2: " + hits.hitsJson());
  }

//---------------------------------------------------------------------------

  /** The core delivery-ungating contract: even with zero matches, the
   *  alignment is computed and returned, so passage-click navigation never
   *  depends on whether highlights were applied. */
  @Test void noMatchesStillProducesAlignment()
  {
    setupTwoPagePdf();

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, plainQuery("zeppelin"), INDEX_PATH, CONVERTED_PATH, null);

    assertNotNull(hits);
    assertNull(hits.hitsJson());
    assertEquals(1, hits.firstMatchPage());
    assertNotNull(hits.alignment());
  }

//---------------------------------------------------------------------------

  @Test void alignmentMapsTikaPassageToConvertedPage()
  {
    setupTwoPagePdf();

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, plainQuery("machine"), INDEX_PATH, CONVERTED_PATH, null);
    assertNotNull(hits);

    // A Tika-coordinate match (line-broken text, so raw offsets differ from
    // the converted PDF's) must map to the page the viewer displays

    PageMatch tikaMatch = new PageMatch(1, TIKA_TEXT.indexOf("machine"), TIKA_TEXT.indexOf("machine") + "machine".length(),
                                        "machine", 1.0f, List.of(new HitRange(0, "machine".length())));

    assertEquals(2, hits.alignment().pageForPassage(tikaMatch));
  }

//---------------------------------------------------------------------------

  /**
   * The regression this service exists to prevent: alignment publication must
   * never be gated on hit delivery. When the converted-side search finds
   * nothing to highlight, the alignment must still be complete enough to map a
   * passage to its page, because passage clicks navigate by it. (Real cause of
   * exactly that state: matches that live in content the office export does not
   * render, such as comments or tracked changes.) Asserting the alignment is
   * merely non-null would not catch a half-built one, so map a passage through
   * it here.
   */
  @Test void alignmentStaysUsableWhenThereAreNoHitsToDeliver()
  {
    setupTwoPagePdf();

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, plainQuery("zeppelin"), INDEX_PATH, CONVERTED_PATH, null);

    assertNotNull(hits);
    assertNull(hits.hitsJson());  // nothing matched, so nothing to deliver

    PageMatch tikaMatch = new PageMatch(1, TIKA_TEXT.indexOf("machine"), TIKA_TEXT.indexOf("machine") + "machine".length(),
                                        "machine", 1.0f, List.of(new HitRange(0, "machine".length())));

    assertEquals(2, hits.alignment().pageForPassage(tikaMatch));
  }

//---------------------------------------------------------------------------

  /**
   * The hit JSON must be built in RAW extraction-text coordinates (what the
   * viewer's text layer concatenates to), while the alignment gets the
   * header-stripped offsets (what the Tika-aligned normalized text uses).
   * Using stripped offsets for the JSON would shift every post-header hit.
   */
  @Test void hitsJsonUsesRawOffsetsWhenHeadersStripped()
  {
    String dbRoot = "C:\\myroot",
           header = dbRoot + "\\docs\\chapter.docx - 2 Last saved: 2/20/2026 6:14:00 PM ",
           page1 = header + PDF_PAGE_1,
           rawText = page1 + PDF_PAGE_2;

    int[] rawOffsets = { 0, page1.length(), rawText.length() };

    source.extraction = new ExtractionResult(rawText, rawOffsets.clone(), 2);
    source.storedContent = TIKA_TEXT;  // Tika never sees the leaked header

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, plainQuery("machine"), INDEX_PATH, CONVERTED_PATH, dbRoot);

    assertNotNull(hits);
    assertNotNull(hits.hitsJson());
    assertEquals(2, hits.firstMatchPage());

    // Stripping happened: the alignment's page-2 offset moved down by the removed header

    assertTrue(hits.alignment().convertedPdfPageOffsets()[1] < rawOffsets[1],
        "alignment offsets should be header-stripped");

    // The JSON's range is relative to the RAW page start, unaffected by the strip

    int relStart = PDF_PAGE_2.indexOf("machine");

    assertTrue(hits.hitsJson().contains("\"2\":[[" + relStart + ',' + (relStart + "machine".length()) + "]]"),
        "hit JSON should be in raw-text page coordinates: " + hits.hitsJson());
  }

//---------------------------------------------------------------------------

  /** Search-key mode passes a prebuilt query; the query string is null and
   *  must never be parsed. */
  @Test void prebuiltSearchKeyQueryBypassesParsing()
  {
    setupTwoPagePdf();

    QueryDescriptor query = new QueryDescriptor(null, new TermQuery(new Term("content", "machine")), null, null);

    PagedHits hits = HitSetService.computeConvertedPdfHits(source, query, INDEX_PATH, CONVERTED_PATH, null);

    assertNotNull(hits);
    assertNotNull(hits.hitsJson());
    assertEquals(2, hits.firstMatchPage());
  }

//---------------------------------------------------------------------------

  @Test void unparseableQueryStringThrows()
  {
    setupTwoPagePdf();

    assertThrows(RuntimeException.class, () ->
      HitSetService.computeConvertedPdfHits(source, plainQuery("AND OR ("), INDEX_PATH, CONVERTED_PATH, null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void pdfHitsBuildsPageRelativeJson()
  {
    source.pageOffsets = new int[] { 0, 50, 100 };

    List<PageMatch> matches = List.of(new PageMatch(2, 60, 80, "snippet", 1.0f, List.of(new HitRange(0, 5))));

    PagedHits hits = HitSetService.pdfHits(source, INDEX_PATH, matches);

    assertNotNull(hits);
    assertEquals("{\"2\":[[10,15]]}", hits.hitsJson());
    assertEquals(-1, hits.firstMatchPage());
    assertNull(hits.alignment());
  }

//---------------------------------------------------------------------------

  @Test void pdfHitsNullWhenIndexHasNoPageOffsets()
  {
    source.pageOffsets = null;

    List<PageMatch> matches = List.of(new PageMatch(1, 0, 5, "snippet", 1.0f, List.of(new HitRange(0, 5))));

    assertNull(HitSetService.pdfHits(source, INDEX_PATH, matches));
  }

//---------------------------------------------------------------------------

  @Test void directContentHitsBuildsContextJson()
  {
    source.storedContent = "some opening words before the needle sits here quietly among other words";

    int matchStart = source.storedContent.indexOf("needle");

    List<PageMatch> matches = List.of(new PageMatch(1, matchStart, matchStart + "needle".length(),
                                                    "needle", 1.0f, List.of(new HitRange(0, "needle".length()))));

    DirectHits hits = HitSetService.directContentHits(source, INDEX_PATH, matches);

    assertNotNull(hits);
    assertNotNull(hits.hitsJson());
    assertTrue(hits.hitsJson().contains("needle"), "context should contain the matched text: " + hits.hitsJson());
    assertTrue(hits.hitsJson().contains("\"s\":") && hits.hitsJson().contains("\"e\":"),
        "JSON should carry match offsets within the context: " + hits.hitsJson());
  }

//---------------------------------------------------------------------------

  @Test void directContentHitsNullWhenIndexHasNoStoredContent()
  {
    source.storedContent = null;

    List<PageMatch> matches = List.of(new PageMatch(1, 0, 5, "snippet", 1.0f, List.of(new HitRange(0, 5))));

    assertNull(HitSetService.directContentHits(source, INDEX_PATH, matches));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
