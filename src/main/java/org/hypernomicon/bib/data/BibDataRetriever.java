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
import static org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum.*;
import static org.hypernomicon.util.TestContext.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.Serial;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.http.*;

import org.json.simple.parser.ParseException;

//---------------------------------------------------------------------------

/**
 * Queries the online bibliographic sources for the best available metadata,
 * one source at a time, stopping at the first hit: a chain of stages, each a
 * {@link CompletableFuture} that completes with a result (ending the cascade),
 * with {@code null} (advancing to the next stage), or exceptionally (ending or
 * advancing according to that stage's error policy).
 * <p>
 * Threading: the constructor runs on the JavaFX thread, and every stage's
 * future is completed on the JavaFX thread (the HTTP clients marshal their
 * callbacks there), so chain continuations and all mutable state stay
 * FX-confined. Under unit tests the substituted {@link Sources} complete their
 * futures on the test thread and the whole cascade runs there instead.
 */
public class BibDataRetriever
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The online sources this retriever is allowed to query. */
  public enum BibSource { crossref, googleBooks, libraryOfCongress }

//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface RetrieveHandler { void handle(PDFBibData pdfBD, BibDataStandalone queryBD, boolean messageShown); }

//---------------------------------------------------------------------------

  /**
   * The query operations the cascade performs, as futures: completes with the
   * source's result, with {@code null} for a clean miss, or exceptionally.
   * Package-private and substitutable so the cascade itself is unit-testable
   * without the network; the production implementation adapts each source's
   * callback-based {@code doHttpRequest}.
   */
  interface Sources
  {
    CompletableFuture<BibDataStandalone> crossrefByDoi(AsyncHttpClient httpClient, String doi, Set<String> alreadyCheckedIDs);

    CompletableFuture<BibDataStandalone> crossrefByTitle(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper,
                                                         BibAuthors authors, Set<String> alreadyCheckedIDs);

    CompletableFuture<BibDataStandalone> locByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs);

    CompletableFuture<BibDataStandalone> locByTitle(AsyncHttpClient httpClient, String title, String yearStr, BibAuthors authors, Set<String> alreadyCheckedIDs);

    CompletableFuture<BibDataStandalone> googleByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs);

    CompletableFuture<BibDataStandalone> googleByTitle(AsyncHttpClient httpClient, String title, BibAuthors authors, Set<String> alreadyCheckedIDs);
  }

//---------------------------------------------------------------------------

  /**
   * Ends the cascade with no result and no error: the terminal handler treats
   * this exactly like the last stage completing empty. Thrown, never shown;
   * carries no stack trace.
   */
  private static final class TerminateCascade extends RuntimeException
  {
    @Serial private static final long serialVersionUID = 1L;

    private TerminateCascade() { super(null, null, false, false); }
  }

//---------------------------------------------------------------------------

  private static final Sources PRODUCTION_SOURCES = new Sources()
  {
    @Override public CompletableFuture<BibDataStandalone> crossrefByDoi(AsyncHttpClient httpClient, String doi, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      CrossrefBibData.doHttpRequest(httpClient, doi, alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }

    @Override public CompletableFuture<BibDataStandalone> crossrefByTitle(AsyncHttpClient httpClient, String title, String yearStr, boolean isPaper,
                                                                          BibAuthors authors, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      CrossrefBibData.doHttpRequest(httpClient, title, yearStr, isPaper, authors, "", alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }

    @Override public CompletableFuture<BibDataStandalone> locByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      LibraryOfCongressBibData.doHttpRequest(httpClient, isbnIt, alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }

    @Override public CompletableFuture<BibDataStandalone> locByTitle(AsyncHttpClient httpClient, String title, String yearStr, BibAuthors authors, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      LibraryOfCongressBibData.doHttpRequest(httpClient, title, yearStr, authors, null, alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }

    @Override public CompletableFuture<BibDataStandalone> googleByIsbns(AsyncHttpClient httpClient, Iterator<String> isbnIt, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      GoogleBibData.doHttpRequest(httpClient, isbnIt, alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }

    @Override public CompletableFuture<BibDataStandalone> googleByTitle(AsyncHttpClient httpClient, String title, BibAuthors authors, Set<String> alreadyCheckedIDs)
    {
      CompletableFuture<BibDataStandalone> future = new CompletableFuture<>();
      GoogleBibData.doHttpRequest(httpClient, title, authors, null, alreadyCheckedIDs, future::complete, future::completeExceptionally);
      return future;
    }
  };

//---------------------------------------------------------------------------

  private static Sources sourcesOverride = null;

  private BibData workBD = null;
  private BibDataStandalone queryBD = null;
  private PDFBibData pdfBD = null;
  private boolean stopped = false, searchedCrossref = false, locBlocked = false;

  private final AsyncHttpClient httpClient;
  private final WorkTypeEnum workTypeEnum;
  private final List<FilePath> pdfFiles;
  private final RetrieveHandler doneHndlr;
  private final EnumSet<BibSource> enabledSources;
  private final Sources sources;

  /**
   * Identifiers already queried, tracked separately per source.
   * <p>
   * This must not be shared across sources: each source's doHttpRequest skips identifiers
   * already in the set and adds every one it tries, so a shared set would let whichever
   * source ran first consume every ISBN and leave the next one with nothing to query.
   * </p>
   */
  private final Map<BibSource, Set<String>> alreadyCheckedIDs = new EnumMap<>(BibSource.class);

//---------------------------------------------------------------------------

  public BibDataRetriever(AsyncHttpClient httpClient, BibData workBD, List<FilePath> pdfFiles, RetrieveHandler doneHndlr)
  {
    this(httpClient, workBD, pdfFiles, EnumSet.allOf(BibSource.class), doneHndlr);
  }

//---------------------------------------------------------------------------

  private BibDataRetriever(AsyncHttpClient httpClient, BibData workBD, List<FilePath> pdfFiles,
                           EnumSet<BibSource> enabledSources, RetrieveHandler doneHndlr)
  {
    this.pdfFiles = pdfFiles;

    if (collEmpty(pdfFiles) == false) try
    {
      pdfBD = PDFBibData.createFromFiles(pdfFiles);

      if (BibData.isEmpty(pdfBD))
        pdfBD = null;
      else if ((workBD == null) || (((workBD instanceof WorkBibData) == false) && BibData.isEmpty(workBD)))
        workBD = pdfBD;
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while extracting metadata: " + getThrowableMessage(e));
    }

    this.workBD = workBD;

    workTypeEnum = HDT_WorkType.getEnumVal(workBD == null ? null : workBD.getWorkType());

    this.httpClient = httpClient;
    this.doneHndlr = doneHndlr;
    this.enabledSources = enabledSources;

    sources = sourcesOverride != null ? sourcesOverride : PRODUCTION_SOURCES;

    runCascade();
  }

//---------------------------------------------------------------------------

  private Set<String> checkedIDs(BibSource source) { return alreadyCheckedIDs.computeIfAbsent(source, src -> new HashSet<>()); }
  private boolean bookOrUnknownType()              { return (workTypeEnum == wtNone) || (workTypeEnum == wtBook); }

  public void stop()                               { httpClient.stop(); stopped = true; }

  static void setSourcesForTesting(Sources sources) { assertThatThisIsUnitTestThread(); sourcesOverride = sources; }

  private static Throwable causeOf(Throwable throwable) { return ((throwable instanceof CompletionException) && (throwable.getCause() != null)) ? throwable.getCause() : throwable; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static BibDataRetriever forSources(AsyncHttpClient httpClient, BibData workBD, EnumSet<BibSource> sources, Consumer<BibDataStandalone> doneHndlr)
  {
    return new BibDataRetriever(httpClient, workBD, null, sources, (pdfBD, queryBD, ms) -> doneHndlr.accept(queryBD));
  }

//---------------------------------------------------------------------------

  public static BibDataRetriever forCrossref(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return forSources(httpClient, workBD, EnumSet.of(BibSource.crossref), doneHndlr);
  }

//---------------------------------------------------------------------------

  public static BibDataRetriever forGoogleBooks(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return forSources(httpClient, workBD, EnumSet.of(BibSource.googleBooks), doneHndlr);
  }

//---------------------------------------------------------------------------

  public static BibDataRetriever forLibraryOfCongress(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return forSources(httpClient, workBD, EnumSet.of(BibSource.libraryOfCongress), doneHndlr);
  }

//---------------------------------------------------------------------------

  /**
   * Queries the book sources: Library of Congress first, then Google Books
   */
  public static BibDataRetriever forBooks(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return forSources(httpClient, workBD, EnumSet.of(BibSource.libraryOfCongress, BibSource.googleBooks), doneHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void finish(Exception e)
  {
    if (stopped) return;
    stop();

    boolean messageShown = false;

    if (e != null)
    {
      if (e instanceof ParseException)
        noOp();
      else if (e instanceof CancelledTaskException)
      {
        pdfBD = null;
        queryBD = null;
        messageShown = true;
      }
      else
      {
        errorPopup("Error: " + getThrowableMessage(e));
        messageShown = true;
      }
    }

    if ((queryBD == null) && (pdfBD == null) && (messageShown == false) && enabledSources.containsAll(EnumSet.allOf(BibSource.class)))
    {
      warningPopup("Unable to find bibliographic information in " +
                   (collEmpty(pdfFiles) ? "" : "work file(s) or ") +
                   "online sources.\n\nIt might work to add more information manually and then click Auto-Fill.");

      messageShown = true;
    }

    doneHndlr.handle(pdfBD, queryBD, messageShown);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Builds and runs the whole cascade. Each stage advances by completing with
   * {@code null}; {@link #chain} skips the remaining stages once a stage has
   * produced a result (or the retriever has been stopped). The stage order and
   * every gate are the cascade's contract; see the individual stage methods.
   */
  private void runCascade()
  {
    String workTitle = workBD == null ? "" : workBD.getStr(bfTitle).strip(),

           title = workTitle.isBlank() ? (pdfBD == null ? "" : pdfBD.getStr(bfTitle).strip()) : workTitle;

    BibAuthors authors = workBD == null ? null : workBD.getAuthors();

    CompletableFuture<BibDataStandalone> future = chain(stageCrossrefByWorkDoi(),

      this::stageCrossrefByPdfDoi,
      () -> stageCrossrefByTitleEarly(title, authors),
      () -> stageIsbns(BibSource.libraryOfCongress, workBD),
      () -> stageIsbns(BibSource.googleBooks      , workBD),
      () -> stageIsbns(BibSource.libraryOfCongress, pdfBD),
      () -> stageIsbns(BibSource.googleBooks      , pdfBD),

    // With no title there is nothing left to search by; end the cascade here
    // rather than running the title stages with an empty query

      () -> title.isBlank() ? CompletableFuture.failedFuture(new TerminateCascade()) : CompletableFuture.completedFuture(null),

      () -> stageTitle(BibSource.googleBooks      , title, authors),
      () -> stageTitle(BibSource.libraryOfCongress, title, authors),
      () -> stageCrossrefByTitleFinal(authors));

    future.whenComplete((bd, throwable) ->
    {
      if (throwable == null)
      {
        queryBD = bd;
        finish(null);
        return;
      }

      Throwable cause = causeOf(throwable);

      if (cause instanceof TerminateCascade)
        finish(null);
      else if (cause instanceof Exception e)
        finish(e);
      else
        finish(new RuntimeException(cause));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Runs each stage in turn, only while the chain so far has produced no result and the
   *  retriever has not been stopped. (After a stop, completing with {@code null} suffices:
   *  {@link #finish} ignores everything once stopped.) */
  @SafeVarargs
  private final CompletableFuture<BibDataStandalone> chain(CompletableFuture<BibDataStandalone> future, Supplier<CompletableFuture<BibDataStandalone>>... stages)
  {
    for (Supplier<CompletableFuture<BibDataStandalone>> stage : stages)
      future = future.thenCompose(bd -> ((bd != null) || stopped) ? CompletableFuture.completedFuture(bd) : stage.get());

    return future;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether a stage for this source should run: the source must be enabled for this
   * retrieval, and not known in advance to be pointless. The enabled-sources set itself is
   * left alone in either case: for the warning in finish(), a skipped source still counts as
   * having been covered by the cascade.
   */
  private boolean query(BibSource source)
  {
    if (enabledSources.contains(source) == false) return false;

    return switch (source)
    {
      // Keyless Google queries always fail (HTTP 429 by policy; see advanceOnError), so with
      // no API key configured the Google stages are skipped rather than sending doomed requests

      case googleBooks       -> GoogleBibData.apiKeyConfigured();

      // Once the Library of Congress has answered with its block page, every later LC stage
      // would get the same page (see advanceOnError, which reports it once)

      case libraryOfCongress -> locBlocked == false;

      default                -> true;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //   if there is a DOI
  //     if can get bib info from DOI
  //       exit

  private CompletableFuture<BibDataStandalone> stageCrossrefByWorkDoi()
  {
    if (query(BibSource.crossref) == false)
      return CompletableFuture.completedFuture(null);

    String doi = workBD == null ? "" : workBD.getStr(bfDOI);

    if (doi.isEmpty())
      return CompletableFuture.completedFuture(null);

    // No error handler: a Crossref DOI failure ends the cascade with an error popup

    return sources.crossrefByDoi(httpClient, doi, checkedIDs(BibSource.crossref));
  }

//---------------------------------------------------------------------------

  //   if have PDF bib info
  //     if PDF bib info has DOI
  //       if can get bib info from DOI
  //         exit

  private CompletableFuture<BibDataStandalone> stageCrossrefByPdfDoi()
  {
    if (query(BibSource.crossref) == false)
      return CompletableFuture.completedFuture(null);

    String doi = pdfBD == null ? "" : pdfBD.getStr(bfDOI);

    if (doi.isEmpty())
      return CompletableFuture.completedFuture(null);

    return sources.crossrefByDoi(httpClient, doi, checkedIDs(BibSource.crossref)).thenApply(bd ->
    {
      // A DOI scraped out of a PDF that resolves to a whole book, when the user
      // said this work is a chapter or paper, is almost certainly the containing
      // volume's DOI rather than this item's; discard it and keep searching

      if ((HDT_WorkType.getEnumVal(bd == null ? null : bd.getWorkType()) == wtBook) && ((workTypeEnum == wtChapter) || (workTypeEnum == wtPaper)))
        return null;

      return bd;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //   if this is a newer book or a non-book
  //     use title, year, and authors to query Crossref for DOI and bib info
  //     if got bib info
  //       exit

  private CompletableFuture<BibDataStandalone> stageCrossrefByTitleEarly(String title, BibAuthors authors)
  {
    int year = workBD == null ? 0 : workBD.getDate().year.numericValueWhereMinusOneEqualsOneBC();

    if ((year <= 0) || (query(BibSource.crossref) == false) || title.isEmpty() || ((workTypeEnum == wtBook) && (year < 1995)))
      return CompletableFuture.completedFuture(null);

    return sources.crossrefByTitle(httpClient, title, workBD.getYearStr(), workTypeEnum == wtPaper, authors, checkedIDs(BibSource.crossref))
      .thenApply(bd ->
      {
        searchedCrossref = true;
        return bd;
      })
      .exceptionallyCompose(throwable ->
      {
        Throwable cause = causeOf(throwable);

        if ((cause instanceof HttpResponseException hre) && (hre.getStatusCode() == HttpStatusCode.SC_SERVICE_UNAVAILABLE))
        {
          // Crossref is down for the moment; report it, count Crossref as searched
          // so the final stage does not retry it, and let the other sources run

          searchedCrossref = true;
          errorPopup(hre);
          return CompletableFuture.completedFuture(null);
        }

        // Any other failure here ends the cascade without an error popup

        return CompletableFuture.failedFuture(new TerminateCascade());
      });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //   if this is a book or there is no work type
  //     if there are 1 or more ISBNs (from the work record, then from the PDF)
  //       try them against the Library of Congress, then against Google Books,
  //       which holds more than LoC does

  private CompletableFuture<BibDataStandalone> stageIsbns(BibSource source, BibData bd)
  {
    if ((query(source) == false) || (bookOrUnknownType() == false))
      return CompletableFuture.completedFuture(null);

    List<String> isbns = bd == null ? null : bd.getMultiStr(bfISBNs);

    if (collEmpty(isbns))
      return CompletableFuture.completedFuture(null);

    CompletableFuture<BibDataStandalone> future = source == BibSource.libraryOfCongress ?
      sources.locByIsbns   (httpClient, isbns.iterator(), checkedIDs(source))
    :
      sources.googleByIsbns(httpClient, isbns.iterator(), checkedIDs(source));

    return advanceOnError(future);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //     use title and authors to query for ISBN and bib info
  //     if got bib info
  //       exit
  //
  //     Google goes before LoC for title searches: the candidate scoring is tuned
  //     against Google's relevance ranking, while LoC's title index is sensitive to
  //     subtitles and punctuation.

  private CompletableFuture<BibDataStandalone> stageTitle(BibSource source, String title, BibAuthors authors)
  {
    if ((query(source) == false) || (bookOrUnknownType() == false))
      return CompletableFuture.completedFuture(null);

    CompletableFuture<BibDataStandalone> future = source == BibSource.libraryOfCongress ?
      sources.locByTitle   (httpClient, title, workBD == null ? "" : workBD.getYearStr(), authors, checkedIDs(source))
    :
      sources.googleByTitle(httpClient, title, authors, checkedIDs(source));

    return advanceOnError(future);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  //   if didn't try to do so earlier,
  //     use title, year, and authors to query Crossref for DOI and bib info

  private CompletableFuture<BibDataStandalone> stageCrossrefByTitleFinal(BibAuthors authors)
  {
    if ((query(BibSource.crossref) == false) || searchedCrossref)
      return CompletableFuture.completedFuture(null);

    // Deliberately the work record's own title (no PDF fallback)

    String title   = workBD == null ? "" : workBD.getStr(bfTitle).strip(),
           yearStr = workBD == null ? "" : workBD.getYearStr();

    return sources.crossrefByTitle(httpClient, title, yearStr, workTypeEnum == wtPaper, authors, checkedIDs(BibSource.crossref));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Error policy for stages that have a later stage to fall back to: the error is logged and
   * the cascade advances, rather than ending with a popup. A cancellation still ends it,
   * since that came from the user.
   * <p>
   * Every book source needs this, for its own reason. The Library of Congress SRU server is
   * plain HTTP on a nonstandard port, which some networks block outright. Google Books retired
   * anonymous access in 2026: an unkeyed request is billed to Google's shared fallback consumer
   * project, whose daily quota is now configured as zero, so every keyless request fails with
   * HTTP 429 by policy. A Google failure must therefore fall through to the stages after it
   * instead of taking the whole cascade down with it.
   * </p><p>
   * One failure is worth telling the user about, once: the Library of Congress answering with
   * its block page. Every later Library of Congress stage would get the same page, so they are
   * skipped for the rest of this retrieval.
   * </p>
   */
  private CompletableFuture<BibDataStandalone> advanceOnError(CompletableFuture<BibDataStandalone> future)
  {
    return future.exceptionallyCompose(throwable ->
    {
      Throwable cause = causeOf(throwable);

      if (cause instanceof CancelledTaskException)
        return CompletableFuture.failedFuture(cause);

      if (cause instanceof LibraryOfCongressBibData.AccessBlockedException)
      {
        if (locBlocked == false)
        {
          locBlocked = true;
          errorPopup(cause);
        }
      }
      else
        logThrowable(cause);

      return CompletableFuture.completedFuture(null);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
