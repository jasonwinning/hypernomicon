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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.util.Util.*;

import static org.junit.jupiter.api.Assertions.*;

import java.util.*;
import java.util.concurrent.CompletableFuture;

import org.junit.jupiter.api.*;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.TestHyperDB;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.PopupRobot;
import org.hypernomicon.util.http.*;

import javafx.scene.control.Alert.AlertType;

//---------------------------------------------------------------------------

/**
 * Contract tests for the {@link BibDataRetriever} cascade, driven through the
 * {@link BibDataRetriever.Sources} seam with scripted results, so no network is
 * involved and the whole cascade runs synchronously on the test thread. Popups
 * are captured by {@link PopupRobot}, which {@link TestHyperDB} activates.
 * <p>
 * Not covered here: the PDF-derived stages (PDF metadata extraction is not
 * injectable; it runs only when real PDF files are passed in), and the sources'
 * own doHttpRequest internals, which have their own tests.
 */
class BibDataRetrieverTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String VALID_ISBN = "9780140449266";

  private FakeSources fakeSources;
  private AsyncHttpClient httpClient;

  /** A new empty dummy BibData to play the role of something's being found by the retriever */
  private static GUIBibData hit() { return new GUIBibData(); }

//---------------------------------------------------------------------------

  /** One recorded query operation: which source operation ran, with which inputs,
   *  and which per-source dedupe set it was handed. */
  private record Call(String op, List<String> isbns, String title, Set<String> checkedIDs) { }

//---------------------------------------------------------------------------

  private static final class FakeSources implements BibDataRetriever.Sources
  {
    private final List<Call> calls = new ArrayList<>();
    private final Deque<CompletableFuture<BibDataStandalone>> script = new ArrayDeque<>();

    private List<String> ops() { return calls.stream().map(Call::op).toList(); }

    /** Responses are consumed in cascade order, one per operation; when the
     *  script runs out, every further operation reports a clean miss. */
    private CompletableFuture<BibDataStandalone> next()
    {
      return script.isEmpty() ? CompletableFuture.completedFuture(null) : script.poll();
    }

    private CompletableFuture<BibDataStandalone> record(String op, Iterator<String> isbnIt, String title, Set<String> checkedIDs)
    {
      List<String> isbns = new ArrayList<>();

      if (isbnIt != null)
        isbnIt.forEachRemaining(isbns::add);

      calls.add(new Call(op, isbns, title, checkedIDs));

      return next();
    }

    @Override public CompletableFuture<BibDataStandalone> crossrefByDoi(AsyncHttpClient httpClient, String doi, Set<String> checkedIDs)
    { return record("crossrefDoi", null, doi, checkedIDs); }

    @Override public CompletableFuture<BibDataStandalone> crossrefByTitle(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper, BibAuthors authors, Set<String> checkedIDs)
    { return record("crossrefTitle", null, title, checkedIDs); }

    @Override public CompletableFuture<BibDataStandalone> locByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> checkedIDs)
    { return record("locIsbn", isbnIt, null, checkedIDs); }

    @Override public CompletableFuture<BibDataStandalone> locByTitle(AsyncHttpClient httpClient, String title, String yearStr, BibAuthors authors, Set<String> checkedIDs)
    { return record("locTitle", null, title, checkedIDs); }

    @Override public CompletableFuture<BibDataStandalone> googleByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> checkedIDs)
    { return record("googleIsbn", isbnIt, null, checkedIDs); }

    @Override public CompletableFuture<BibDataStandalone> googleByTitle(AsyncHttpClient httpClient, String title, BibAuthors authors, Set<String> checkedIDs)
    { return record("googleTitle", null, title, checkedIDs); }
  }

//---------------------------------------------------------------------------

  /** Captures what the retriever reported when it finished. */
  private static final class Result
  {
    private PDFBibData pdfBD;
    private BibDataStandalone queryBD, supplementBD;
    private boolean messageShown;
    private int timesCalled = 0;

    private void handle(PDFBibData pdfBD, BibDataStandalone queryBD, BibDataStandalone supplementBD, boolean messageShown)
    {
      this.pdfBD = pdfBD;
      this.queryBD = queryBD;
      this.supplementBD = supplementBD;
      this.messageShown = messageShown;
      timesCalled++;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @BeforeAll
  static void setUpOnce()
  {
    TestHyperDB.instance();  // Provides the work-type lookups and activates PopupRobot
  }

//---------------------------------------------------------------------------

  @BeforeEach
  void setUp()
  {
    PopupRobot.clear();

    fakeSources = new FakeSources();
    BibDataRetriever.setSourcesForTesting(fakeSources);

    GoogleBibData.setApiKeyForTesting("test-key");  // Keyless Google stages are skipped; most tests want them to run

    httpClient = new AsyncHttpClient();
  }

//---------------------------------------------------------------------------

  @AfterEach
  void tearDown()
  {
    BibDataRetriever.setSourcesForTesting(null);
    GoogleBibData.setApiKeyForTesting(null);
  }

//---------------------------------------------------------------------------

  /** A BibData seed for a book with a DOI, an ISBN, and a title. */
  private static GUIBibData bookBD()
  {
    GUIBibData bd = new GUIBibData();

    bd.setEntryType(etBook);
    bd.setTitle("Naming and Necessity");
    bd.setStr(bfDOI, "10.1234/abc123");
    bd.setMultiStr(bfISBNs, List.of(VALID_ISBN));

    return bd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void doiHitStopsTheCascade()
  {
    GUIBibData found = hit();
    fakeSources.script.add(CompletableFuture.completedFuture(found));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn", "locTitle"), fakeSources.ops(),
                 "a hit must prevent every later stage; for a Crossref book win, the LC supplement still runs (ISBN, then title on a miss)");
    assertEquals(1, result.timesCalled, "the handler runs exactly once, synchronously here");
    assertSame(found, result.queryBD);
    assertNull(result.supplementBD, "LC supplement missed here (default scripted misses)");
    assertFalse(result.messageShown);
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  @Test void fullMissRunsTheStagesInOrderThenWarns()
  {
    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    // No year on the seed, so the early Crossref title stage is gated out and
    // the final one runs instead

    assertEquals(List.of("crossrefDoi", "locIsbn", "googleIsbn", "googleTitle", "locTitle", "crossrefTitle"), fakeSources.ops());

    assertNull(result.queryBD);
    assertTrue(result.messageShown, "the nothing-found warning counts as a shown message");
    assertEquals(1, PopupRobot.getInvocationCount());
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** The dedupe sets must be per-source: a shared set would let whichever source
   *  ran first consume every identifier and leave the next one nothing to query. */
  @Test void dedupeSetsArePerSourceAndStablePerSource()
  {
    noOp(new BibDataRetriever(httpClient, bookBD(), null, (pdfBD, queryBD, supplementBD, ms) -> { }));

    Map<String, Set<String>> opToSet = new HashMap<>();

    for (Call call : fakeSources.calls)
    {
      String source = call.op().startsWith("loc") ? "loc" : (call.op().startsWith("google") ? "google" : "crossref");

      Set<String> existing = opToSet.putIfAbsent(source, call.checkedIDs());

      if (existing != null)
        assertSame(existing, call.checkedIDs(), "every stage of one source shares that source's set");
    }

    assertEquals(3, opToSet.size());
    assertNotSame(opToSet.get("loc"), opToSet.get("google"));
    assertNotSame(opToSet.get("loc"), opToSet.get("crossref"));
    assertNotSame(opToSet.get("google"), opToSet.get("crossref"));
  }

//---------------------------------------------------------------------------

  /** The advance-on-error policy: a failed book source is logged and the
   *  cascade continues, with no popup. */
  @Test void bookSourceFailureAdvancesToTheNextStage()
  {
    GUIBibData found = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(null));                                        // crossrefDoi: miss
    fakeSources.script.add(CompletableFuture.failedFuture(new HttpResponseException(429, "http://test")));  // locIsbn: quota-style failure
    fakeSources.script.add(CompletableFuture.completedFuture(found));                                       // googleIsbn: hit

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn", "googleIsbn"), fakeSources.ops());
    assertSame(found, result.queryBD);
    assertFalse(result.messageShown);
    assertEquals(0, PopupRobot.getInvocationCount(), "an advanced-past failure must not pop anything up");
  }

//---------------------------------------------------------------------------

  /** An LoC miss (not an error) must still leave Google its turn with the same
   *  ISBNs; this is the regression shape for the shared-dedupe-set bug. */
  @Test void locMissFallsThroughToGoogleWithTheSameIsbns()
  {
    GUIBibData found = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(null));   // crossrefDoi: miss
    fakeSources.script.add(CompletableFuture.completedFuture(null));   // locIsbn: miss
    fakeSources.script.add(CompletableFuture.completedFuture(found));  // googleIsbn: hit

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertSame(found, result.queryBD);

    assertEquals(List.of(VALID_ISBN), fakeSources.calls.get(1).isbns());
    assertEquals(List.of(VALID_ISBN), fakeSources.calls.get(2).isbns(), "Google must be offered the same ISBNs LoC missed on");
  }

//---------------------------------------------------------------------------

  /** A Crossref DOI failure is fatal: error popup, no later stages. */
  @Test void crossrefDoiFailureEndsTheCascadeWithAnError()
  {
    fakeSources.script.add(CompletableFuture.failedFuture(new HttpResponseException(500, "http://test")));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi"), fakeSources.ops());
    assertNull(result.queryBD);
    assertTrue(result.messageShown);
    assertEquals(1, PopupRobot.getInvocationCount());
    assertEquals(AlertType.ERROR, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** Cancellation ends the cascade quietly from any stage: results nulled,
   *  message counted as shown (the cancellation itself), nothing popped up. */
  @Test void cancellationEndsTheCascadeQuietly()
  {
    fakeSources.script.add(CompletableFuture.completedFuture(null));                       // crossrefDoi: miss
    fakeSources.script.add(CompletableFuture.failedFuture(new CancelledTaskException()));  // locIsbn: cancelled

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn"), fakeSources.ops());
    assertNull(result.queryBD);
    assertNull(result.pdfBD);
    assertTrue(result.messageShown);
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  /** With no title anywhere, the cascade ends after the ISBN stages instead of
   *  running the title stages with an empty query. (This corner once fell
   *  through into the next stage for lack of a return statement.) */
  @Test void blankTitleEndsTheCascadeAfterTheIsbnStages()
  {
    GUIBibData bd = new GUIBibData();
    bd.setEntryType(etBook);
    bd.setMultiStr(bfISBNs, List.of(VALID_ISBN));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bd, null, result::handle));

    assertEquals(List.of("locIsbn", "googleIsbn"), fakeSources.ops(), "no DOI stage without a DOI; no title stages without a title");
    assertNull(result.queryBD);
    assertTrue(result.messageShown);
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** Crossref 503 on the early title stage: reported, counted as searched (so
   *  the final Crossref stage is skipped), and the cascade continues. */
  @Test void crossrefServiceUnavailableIsReportedAndTheCascadeContinues()
  {
    GUIBibData bd = new GUIBibData();
    bd.setEntryType(etBook);
    bd.setTitle("Naming and Necessity");
    bd.setDate(BibliographicDate.fromUserStr("2000"));

    fakeSources.script.add(CompletableFuture.failedFuture(new HttpResponseException(HttpStatusCode.SC_SERVICE_UNAVAILABLE, "http://test")));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bd, null, result::handle));

    assertEquals(List.of("crossrefTitle", "googleTitle", "locTitle"), fakeSources.ops(),
                 "no second crossrefTitle: the 503 counts as having searched Crossref");

    assertNull(result.queryBD);
    assertEquals(2, PopupRobot.getInvocationCount(), "the 503 error, then the nothing-found warning");
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** The Library of Congress answering with its block page: reported once, and
   *  every later LoC stage skipped, since each would get the same page. The
   *  other sources still get their turns. */
  @Test void locBlockPageIsReportedOnceAndSkipsLaterLocStages()
  {
    fakeSources.script.add(CompletableFuture.completedFuture(null));                                                 // crossrefDoi: miss
    fakeSources.script.add(CompletableFuture.failedFuture(new LibraryOfCongressBibData.AccessBlockedException()));   // locIsbn: blocked

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn", "googleIsbn", "googleTitle", "crossrefTitle"), fakeSources.ops(),
                 "no locTitle: once blocked, LoC is not asked again in this retrieval");

    assertNull(result.queryBD);
    assertEquals(2, PopupRobot.getInvocationCount(), "the block message, then the nothing-found warning");
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** Any other failure of the early Crossref title stage ends the cascade
   *  without an error popup; longstanding behavior the migration preserved. */
  @Test void otherEarlyCrossrefTitleFailureEndsTheCascadeSilently()
  {
    GUIBibData bd = new GUIBibData();
    bd.setEntryType(etBook);
    bd.setTitle("Naming and Necessity");
    bd.setDate(BibliographicDate.fromUserStr("2000"));

    fakeSources.script.add(CompletableFuture.failedFuture(new HttpResponseException(500, "http://test")));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bd, null, result::handle));

    assertEquals(List.of("crossrefTitle"), fakeSources.ops());
    assertEquals(1, PopupRobot.getInvocationCount(), "only the nothing-found warning; the failure itself stays silent");
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** The book supplement: a Crossref win for a book pulls the LC record in alongside it,
   *  because the two are complementary (Crossref has the DOI, LC the cataloged fields). */
  @Test void crossrefBookWinFetchesTheLocSupplement()
  {
    GUIBibData crossrefHit = hit(), locHit = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(crossrefHit));  // crossrefDoi: hit
    fakeSources.script.add(CompletableFuture.completedFuture(locHit));       // locIsbn supplement: hit

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn"), fakeSources.ops());
    assertSame(crossrefHit, result.queryBD, "the supplement must not displace the winner");
    assertSame(locHit, result.supplementBD);
    assertFalse(result.messageShown);
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  /** The files-only retriever: no source is queried, and finding nothing is not
   *  reported, since nothing online was asked for. (With no files passed in,
   *  nothing is found; the PDF stage itself is not injectable here.) */
  @Test void pdfFilesOnlyFactoryQueriesNothingAndStaysQuiet()
  {
    Result result = new Result();
    noOp(BibDataRetriever.forPdfFilesOnly(httpClient, null, result::handle));

    assertEquals(List.of(), fakeSources.ops(), "no online source may be consulted");
    assertEquals(1, result.timesCalled);
    assertNull(result.queryBD);
    assertFalse(result.messageShown, "no nothing-found warning when no source was enabled");
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  /** The supplement's ISBN lookup draws on the Crossref record's own ISBNs too,
   *  which raises LoC's hit rate when the work record lacks one. */
  @Test void supplementIsbnsIncludeTheCrossrefRecords()
  {
    GUIBibData crossrefHit = hit();
    crossrefHit.setMultiStr(bfISBNs, List.of("9780975229804"));

    fakeSources.script.add(CompletableFuture.completedFuture(crossrefHit));

    noOp(new BibDataRetriever(httpClient, bookBD(), null, (pdfBD, queryBD, supplementBD, ms) -> { }));

    assertEquals("locIsbn", fakeSources.calls.get(1).op());
    assertEquals(List.of(VALID_ISBN, "9780975229804"), fakeSources.calls.get(1).isbns(),
                 "work-record ISBNs first, then the Crossref record's");
  }

//---------------------------------------------------------------------------

  /** The supplement falls back to an LC title search when the ISBN lookup misses. */
  @Test void supplementFallsBackToTitleSearch()
  {
    GUIBibData crossrefHit = hit(), locHit = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(crossrefHit));  // crossrefDoi: hit
    fakeSources.script.add(CompletableFuture.completedFuture(null));         // locIsbn supplement: miss
    fakeSources.script.add(CompletableFuture.completedFuture(locHit));       // locTitle supplement: hit

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn", "locTitle"), fakeSources.ops());
    assertSame(crossrefHit, result.queryBD);
    assertSame(locHit, result.supplementBD);
  }

//---------------------------------------------------------------------------

  /** No supplement for non-books: LC is a book source, and the complementarity
   *  argument is about book metadata. */
  @Test void noSupplementForPapers()
  {
    GUIBibData bd = new GUIBibData();
    bd.setWorkType(HDT_WorkType.get(wtPaper));
    bd.setTitle("Some Paper");
    bd.setStr(bfDOI, "10.1234/abc123");

    fakeSources.script.add(CompletableFuture.completedFuture(hit()));

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bd, null, result::handle));

    assertEquals(List.of("crossrefDoi"), fakeSources.ops());
    assertNull(result.supplementBD);
  }

//---------------------------------------------------------------------------

  /** No supplement when LC itself won: it only exists to compensate for the
   *  Crossref stages running first. */
  @Test void noSupplementWhenLocWasTheWinner()
  {
    GUIBibData locHit = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(null));    // crossrefDoi: miss
    fakeSources.script.add(CompletableFuture.completedFuture(locHit));  // locIsbn: hit (the winner)

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn"), fakeSources.ops());
    assertSame(locHit, result.queryBD);
    assertNull(result.supplementBD);
  }

//---------------------------------------------------------------------------

  /** A supplement failure is logged and ignored; it must never disturb the winner. */
  @Test void supplementFailureKeepsTheWinner()
  {
    GUIBibData crossrefHit = hit();

    fakeSources.script.add(CompletableFuture.completedFuture(crossrefHit));                                 // crossrefDoi: hit
    fakeSources.script.add(CompletableFuture.failedFuture(new HttpResponseException(500, "http://test")));  // locIsbn supplement: fails

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertSame(crossrefHit, result.queryBD);
    assertNull(result.supplementBD);
    assertFalse(result.messageShown);
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  /** The single-source Crossref factory has LC disabled, so no supplement. */
  @Test void singleSourceCrossrefDoesNotSupplement()
  {
    List<BibDataStandalone> results = new ArrayList<>();

    fakeSources.script.add(CompletableFuture.completedFuture(hit()));

    BibDataRetriever.forCrossref(httpClient, bookBD(), results::add);

    assertEquals(List.of("crossrefDoi"), fakeSources.ops());
    assertEquals(1, results.size());
    assertNotNull(results.getFirst());
  }

//---------------------------------------------------------------------------

  /** With no Google Books API key, the Google stages are skipped entirely (a keyless
   *  query always fails with HTTP 429), but the cascade otherwise runs in full and the
   *  nothing-found warning still fires: Google counts as covered, not as disabled. */
  @Test void keylessGoogleStagesAreSkipped()
  {
    GoogleBibData.setApiKeyForTesting("");

    Result result = new Result();
    noOp(new BibDataRetriever(httpClient, bookBD(), null, result::handle));

    assertEquals(List.of("crossrefDoi", "locIsbn", "locTitle", "crossrefTitle"), fakeSources.ops());
    assertNull(result.queryBD);
    assertTrue(result.messageShown);
    assertEquals(AlertType.WARNING, PopupRobot.getLastType());
  }

//---------------------------------------------------------------------------

  /** A single-source factory must query only its source, and must not show the
   *  nothing-found warning, which is reserved for the full cascade. */
  @Test void singleSourceFactoryQueriesOnlyThatSource()
  {
    List<BibDataStandalone> results = new ArrayList<>();

    BibDataRetriever.forGoogleBooks(httpClient, bookBD(), results::add);

    assertEquals(List.of("googleIsbn", "googleTitle"), fakeSources.ops());
    assertEquals(1, results.size());
    assertNull(results.getFirst());
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------

  /** After stop(), a late completion from an in-flight stage must not reach the
   *  handler; the canceller has already moved on. */
  @Test void stopSuppressesLateCompletions()
  {
    CompletableFuture<BibDataStandalone> pending = new CompletableFuture<>();

    fakeSources.script.add(CompletableFuture.completedFuture(null));  // crossrefDoi: miss
    fakeSources.script.add(pending);                                  // locIsbn: still in flight

    Result result = new Result();
    BibDataRetriever retriever = new BibDataRetriever(httpClient, bookBD(), null, result::handle);

    assertEquals(0, result.timesCalled, "cascade is suspended awaiting the pending stage");

    retriever.stop();
    pending.complete(hit());

    assertEquals(0, result.timesCalled, "a completion after stop must not invoke the handler");
    assertEquals(0, PopupRobot.getInvocationCount());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
