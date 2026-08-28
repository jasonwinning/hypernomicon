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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.*;
import java.util.function.Consumer;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.http.*;

import org.json.simple.parser.ParseException;

//---------------------------------------------------------------------------

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

  private BibData workBD = null;
  private BibDataStandalone queryBD = null;
  private PDFBibData pdfBD = null;
  private boolean stopped = false, searchedCrossref = false, locBlocked = false;

  private final AsyncHttpClient httpClient;
  private final WorkTypeEnum workTypeEnum;
  private final List<FilePath> pdfFiles;
  private final RetrieveHandler doneHndlr;
  private final EnumSet<BibSource> sources;

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
                           EnumSet<BibSource> sources, RetrieveHandler doneHndlr)
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
    this.sources = sources;

    doStage(1);
  }

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

  private Set<String> checkedIDs(BibSource source) { return alreadyCheckedIDs.computeIfAbsent(source, src -> new HashSet<>()); }
  private boolean query(BibSource source)          { return sources.contains(source) && ((source != BibSource.libraryOfCongress) || (locBlocked == false)); }
  private boolean bookOrUnknownType()              { return (workTypeEnum == wtNone) || (workTypeEnum == wtBook); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void stop()
  {
    httpClient.stop();
    stopped = true;
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

    if ((queryBD == null) && (pdfBD == null) && (messageShown == false) && sources.containsAll(EnumSet.allOf(BibSource.class)))
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

  private void doStage(int stage)
  {
    if (queryBD != null)
    {
      finish(null);
      return;
    }

    if (stage < 2)
    {
      //   if there is a DOI
      //     if can get bib info from DOI
      //       exit

      if (query(BibSource.crossref))
      {
        String doi = workBD == null ? "" : workBD.getStr(bfDOI);
        if (doi.length() > 0)
        {
          if (stopped) return;

          CrossrefBibData.doHttpRequest(httpClient, doi, checkedIDs(BibSource.crossref), bd ->
          {
            queryBD = bd;
            doStage(2);
          }, this::finish);

          return;
        }
      }
    }

    if (stage < 3)
    {
      //   if have PDF bib info
      //     if PDF bib info has DOI
      //       if can get bib info from DOI
      //         exit

      if (query(BibSource.crossref))
      {
        String doi = pdfBD == null ? "" : pdfBD.getStr(bfDOI);
        if (doi.length() > 0)
        {
          if (stopped) return;

          CrossrefBibData.doHttpRequest(httpClient, doi, checkedIDs(BibSource.crossref), bd ->
          {
            if ((HDT_WorkType.getEnumVal(bd == null ? null : bd.getWorkType()) != wtBook) || ((workTypeEnum != wtChapter) && (workTypeEnum != wtPaper)))
              queryBD = bd;

            doStage(3);
          }, this::finish);

          return;
        }
      }
    }

    String title = workBD == null ? "" : workBD.getStr(bfTitle).strip();
    if (title.isBlank())
      title = pdfBD == null ? "" : pdfBD.getStr(bfTitle).strip();

    BibAuthors authors = workBD == null ? null : workBD.getAuthors();

    if (stage < 4)
    {
      //   if this is a newer book or a non-book
      //     use title, year, and authors to query Crossref for DOI and bib info
      //     if got bib info
      //       exit

      int year = workBD == null ? 0 : workBD.getDate().year.numericValueWhereMinusOneEqualsOneBC();

      if ((year > 0) && query(BibSource.crossref) && (title.length() > 0) && ((workTypeEnum != wtBook) || (year >= 1995)))
      {
        if (stopped) return;

        CrossrefBibData.doHttpRequest(httpClient, title, workBD.getYearStr(), workTypeEnum == wtPaper, authors, "", checkedIDs(BibSource.crossref), bd ->
        {
          searchedCrossref = true;
          queryBD = bd;
          doStage(4);

        }, e ->
        {
          if ((e instanceof HttpResponseException hre) && (hre.getStatusCode() == HttpStatusCode.SC_SERVICE_UNAVAILABLE))
          {
            searchedCrossref = true;
            errorPopup(e);
            doStage(4);
          }
          else
            finish(null);
        });

        return;
      }
    }

    if (stage < 5)
    {
      //   if this is a book or there is no work type
      //     if there are 1 or more ISBNs
      //       if can use existing ISBNs to get bib info from the Library of Congress
      //         exit

      if (query(BibSource.libraryOfCongress) && bookOrUnknownType())
      {
        List<String> isbns = workBD == null ? null : workBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          LibraryOfCongressBibData.doHttpRequest(httpClient, isbns.iterator(), checkedIDs(BibSource.libraryOfCongress), bd ->
          {
            queryBD = bd;
            doStage(5);
          }, advanceOnError(5));

          return;
        }
      }
    }

    if (stage < 6)
    {
      //       otherwise try the same ISBNs against Google Books

      if (query(BibSource.googleBooks) && bookOrUnknownType())
      {
        List<String> isbns = workBD == null ? null : workBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          GoogleBibData.doHttpRequest(httpClient, isbns.iterator(), checkedIDs(BibSource.googleBooks), bd ->
          {
            queryBD = bd;
            doStage(6);
          }, advanceOnError(6));

          return;
        }
      }
    }

    if (stage < 7)
    {
      //     if have PDF bib info
      //       if PDF bib info has ISBN(s)
      //         if can use existing ISBNs to get bib info
      //           exit

      if (query(BibSource.libraryOfCongress) && bookOrUnknownType())
      {
        List<String> isbns = pdfBD == null ? null : pdfBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          LibraryOfCongressBibData.doHttpRequest(httpClient, isbns.iterator(), checkedIDs(BibSource.libraryOfCongress), bd ->
          {
            queryBD = bd;
            doStage(7);
          }, advanceOnError(7));

          return;
        }
      }
    }

    if (stage < 8)
    {
      if (query(BibSource.googleBooks) && bookOrUnknownType())
      {
        List<String> isbns = pdfBD == null ? null : pdfBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          GoogleBibData.doHttpRequest(httpClient, isbns.iterator(), checkedIDs(BibSource.googleBooks), bd ->
          {
            queryBD = bd;
            doStage(8);
          }, advanceOnError(8));

          return;
        }
      }
    }

    if (title.isBlank())
    {
      finish(null);
      return;
    }

    if (stage < 9)
    {
      //     use title and authors to query Google for ISBN and bib info
      //     if got bib info
      //       exit
      //
      //     Google goes before LoC for title searches: the candidate scoring is tuned against
      //     Google's relevance ranking, while LoC's title index is sensitive to subtitles and
      //     punctuation.

      if (query(BibSource.googleBooks) && bookOrUnknownType())
      {
        if (stopped) return;

        GoogleBibData.doHttpRequest(httpClient, title, authors, null, checkedIDs(BibSource.googleBooks), bd ->
        {
          queryBD = bd;
          doStage(9);
        }, advanceOnError(9));

        return;
      }
    }

    if (stage < 10)
    {
      if (query(BibSource.libraryOfCongress) && bookOrUnknownType())
      {
        if (stopped) return;

        LibraryOfCongressBibData.doHttpRequest(httpClient, title, workBD == null ? "" : workBD.getYearStr(), authors, null, checkedIDs(BibSource.libraryOfCongress), bd ->
        {
          queryBD = bd;
          doStage(10);
        }, advanceOnError(10));

        return;
      }
    }

    //   if didn't try to do so earlier,
    //     use title, year, and authors to query Crossref for DOI and bib info

    if (query(BibSource.crossref) && (searchedCrossref == false))
    {
      if (stopped) return;

      title = workBD == null ? "" : workBD.getStr(bfTitle).strip();
      String yearStr = workBD == null ? "" : workBD.getYearStr();

      CrossrefBibData.doHttpRequest(httpClient, title, yearStr, workTypeEnum == wtPaper, authors, "", checkedIDs(BibSource.crossref), bd ->
      {
        queryBD = bd;
        finish(null);
      }, this::finish);

      return;
    }

    finish(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Failure handler for stages that have a later stage to fall back to: the error is logged and
   * the next stage runs, rather than aborting the whole cascade with a popup. A cancellation is
   * still honored, since that came from the user.
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
  private Consumer<Exception> advanceOnError(int nextStage)
  {
    return e ->
    {
      if (e instanceof CancelledTaskException)
      {
        finish(e);
        return;
      }

      if (e instanceof LibraryOfCongressBibData.AccessBlockedException)
      {
        if (locBlocked == false)
        {
          locBlocked = true;
          errorPopup(e);
        }
      }
      else
        logThrowable(e);

      doStage(nextStage);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
