/*
 * Copyright 2015-2025 Jason Winning
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

package org.hypernomicon.query;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import javafx.concurrent.Worker.State;

import static org.hypernomicon.App.app;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.bib.data.PDFBibData;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.query.Query.WorkQuery;

//---------------------------------------------------------------------------

public final class WorkQueries
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private WorkQueries() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  private static final int QUERY_LIKELY_EDITED_VOLS        = 2001,  // "likely edited volumes"
                           QUERY_ANALYZE_METADATA          = 2003,  // "analyze pdf metadata"
                           QUERY_WORK_NEEDING_PAGE_NUMBERS = 2004;  // "in a PDF with one or more other works, missing page number(s)"
  public static final int  QUERY_WHERE_BIB_FIELD           = 2005;  // "where bibliographic field"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(List<Query<?>> allQueries)
  {
    allQueries.add(new WorkQuery(QUERY_LIKELY_EDITED_VOLS, "likely edited volumes")
    {
      @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        if (work.authorRecords.isEmpty()) return false;
        if (work.getWorkTypeEnum() == WorkTypeEnum.wtPaper) return false;

        if (work.authorRecords.stream().allMatch(author -> work.personIsEditor(author) || work.personIsTranslator(author)))
          return false;

        for (HDT_Work subWork : work.subWorks)
          for (HDT_Person subAuthor : subWork.authorRecords)
            if (work.authorRecords.contains(subAuthor) == false)
            {
//              for (HDT_Person author : work.authors)
//                work.setPersonAsEditor(author, true);

              return true;
            }

        return false;
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return false; }
    });

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    allQueries.add(new WorkQuery(QUERY_WORK_NEEDING_PAGE_NUMBERS, "in a PDF with one or more other works, missing page number(s)")
    {
      @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        for (HDT_WorkFile workFile : work.workFiles)
          if ((workFile.works.size() > 1) && "pdf".equalsIgnoreCase(workFile.filePath().getExtensionOnly()))
            if ((work.getStartPageNum(workFile) == -1) || (work.getEndPageNum(workFile) == -1))
              for (HDT_Work otherWork : workFile.works)
                if ((otherWork != work) && (otherWork.largerWork.get() != work))
                  return true;

        return false;
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return false; }
    });

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    if (app.debugging) allQueries.add(new WorkQuery(QUERY_ANALYZE_METADATA, "analyze pdf metadata")
    {
      private List<List<String>> csvFile;

      @Override public void init(HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        FilePath filePath = homeDir().resolve("data.csv");
        if (filePath.exists()) try
        {
          Files.delete(filePath.toPath());
        }
        catch (IOException e)
        {
          logThrowable(e);
        }

        csvFile = new ArrayList<>();
      }

      @Override public boolean evaluate(HDT_Work work, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        work.workFiles.forEach(workFile ->
        {
          if (workFile.pathNotEmpty() && workFile.filePath().exists() && getMediaType(workFile.filePath()).toString().contains("pdf"))
          {
            try
            {
              new PDFBibData(workFile.filePath()).addCsvLines(csvFile);
            }
            catch (IOException e)
            {
              logThrowable(e);
            }
          }
        });

        return true;
      }

      @Override public void cleanup(State state)
      {
        if (state != State.SUCCEEDED) return;

        FilePath filePath = homeDir().resolve("data.csv");

        try
        {
          writeCsvFile(filePath, csvFile.stream().distinct());
        }
        catch (IOException e)
        {
          logThrowable(e);
        }
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return false; }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
