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

  private BibData workBD = null;
  private BibDataStandalone queryBD = null;
  private PDFBibData pdfBD = null;
  private boolean stopped = false, searchedCrossref = false;

  private final AsyncHttpClient httpClient;
  private final WorkTypeEnum workTypeEnum;
  private final List<FilePath> pdfFiles;
  private final RetrieveHandler doneHndlr;
  private final boolean queryCrossref, queryGoogle;
  private final Set<String> alreadyCheckedIDs = new HashSet<>();

//---------------------------------------------------------------------------

  @FunctionalInterface
  public interface RetrieveHandler { void handle(PDFBibData pdfBD, BibDataStandalone queryBD, boolean messageShown); }

//---------------------------------------------------------------------------

  public BibDataRetriever(AsyncHttpClient httpClient, BibData workBD, List<FilePath> pdfFiles, RetrieveHandler doneHndlr)
  {
    this(httpClient, workBD, pdfFiles, true, true, doneHndlr);
  }

//---------------------------------------------------------------------------

  private BibDataRetriever(AsyncHttpClient httpClient, BibData workBD, List<FilePath> pdfFiles,
                           boolean queryCrossref, boolean queryGoogle, RetrieveHandler doneHndlr)
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
    this.queryCrossref = queryCrossref;
    this.queryGoogle = queryGoogle;

    doStage(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibDataRetriever forCrossref(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return new BibDataRetriever(httpClient, workBD, null, true, false, (pdfBD, queryBD, ms) -> doneHndlr.accept(queryBD));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibDataRetriever forGoogleBooks(AsyncHttpClient httpClient, BibData workBD, Consumer<BibDataStandalone> doneHndlr)
  {
    return new BibDataRetriever(httpClient, workBD, null, false, true, (pdfBD, queryBD, ms) -> doneHndlr.accept(queryBD));
  }

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

    if ((queryBD == null) && (pdfBD == null) && (messageShown == false) && queryCrossref && queryGoogle)
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

      if (queryCrossref)
      {
        String doi = workBD == null ? "" : workBD.getStr(bfDOI);
        if (doi.length() > 0)
        {
          if (stopped) return;

          CrossrefBibData.doHttpRequest(httpClient, doi, alreadyCheckedIDs, bd ->
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

      if (queryCrossref)
      {
        String doi = pdfBD == null ? "" : pdfBD.getStr(bfDOI);
        if (doi.length() > 0)
        {
          if (stopped) return;

          CrossrefBibData.doHttpRequest(httpClient, doi, alreadyCheckedIDs, bd ->
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

      if ((year > 0) && queryCrossref && (title.length() > 0) && ((workTypeEnum != wtBook) || (year >= 1995)))
      {
        if (stopped) return;

        CrossrefBibData.doHttpRequest(httpClient, title, workBD.getYearStr(), workTypeEnum == wtPaper, authors, "", alreadyCheckedIDs, bd ->
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
      //       if can use existing ISBNs to get bib info
      //         exit

      if (queryGoogle && ((workTypeEnum == wtNone) || (workTypeEnum == wtBook)))
      {
        List<String> isbns = workBD == null ? null : workBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          GoogleBibData.doHttpRequest(httpClient, isbns.iterator(), alreadyCheckedIDs, bd ->
          {
            queryBD = bd;
            doStage(5);
          }, this::finish);

          return;
        }
      }
    }

    if (stage < 6)
    {
      //     if have PDF bib info
      //       if PDF bib info has ISBN(s)
      //         if can use existing ISBNs to get bib info
      //           exit

      if (queryGoogle && ((workTypeEnum == wtNone) || (workTypeEnum == wtBook)))
      {
        List<String> isbns = pdfBD == null ? null : pdfBD.getMultiStr(bfISBNs);
        if (collEmpty(isbns) == false)
        {
          if (stopped) return;

          GoogleBibData.doHttpRequest(httpClient, isbns.iterator(), alreadyCheckedIDs, bd ->
          {
            queryBD = bd;
            doStage(6);
          }, this::finish);

          return;
        }
      }
    }

    if (title.isBlank())
      finish(null);

    if (stage < 7)
    {
      //     use title and authors to query Google for ISBN and bib info
      //     if got bib info
      //       exit

      if (queryGoogle && ((workTypeEnum == wtNone) || (workTypeEnum == wtBook)))
      {
        if (stopped) return;

        GoogleBibData.doHttpRequest(httpClient, title, authors, null, alreadyCheckedIDs, bd ->
        {
          queryBD = bd;
          doStage(7);
        }, this::finish);

        return;
      }
    }

    //   if didn't try to do so earlier,
    //     use title, year, and authors to query Crossref for DOI and bib info

    if (queryCrossref && (searchedCrossref == false))
    {
      if (stopped) return;

      title = workBD == null ? "" : workBD.getStr(bfTitle).strip();
      String yearStr = workBD == null ? "" : workBD.getYearStr();

      CrossrefBibData.doHttpRequest(httpClient, title, yearStr, workTypeEnum == wtPaper, authors, "", alreadyCheckedIDs, bd ->
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

}
