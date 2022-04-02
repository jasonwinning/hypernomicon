/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.query.engines;

import org.hypernomicon.App;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.query.QueryTabCtrlr.*;
import static org.hypernomicon.util.MediaUtil.*;

import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.io.FileUtils;

import org.hypernomicon.bib.data.PDFBibData;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.QuerySource;

public class WorkQueryEngine extends QueryEngine<HDT_Work>
{
  private static final int QUERY_LIKELY_EDITED_VOLS        = QUERY_FIRST_NDX + 1,
                           QUERY_4_OR_MORE_AUTHORS         = QUERY_FIRST_NDX + 2,
                           QUERY_ANALYZE_METADATA          = QUERY_FIRST_NDX + 3,
                           QUERY_WORK_NEEDING_PAGE_NUMBERS = QUERY_FIRST_NDX + 4;

  private static List<String> csvFile;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    pop.addEntry(row, QUERY_LIKELY_EDITED_VOLS,        "likely edited volumes");
    pop.addEntry(row, QUERY_4_OR_MORE_AUTHORS,         "with 4 or more authors");
    pop.addEntry(row, QUERY_WORK_NEEDING_PAGE_NUMBERS, "in a PDF with one or more other works, missing page number(s)");

    if (App.debugging())
      pop.addEntry(row, QUERY_ANALYZE_METADATA, "analyze pdf metadata");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    switch (query)
    {
      default :
        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(int curQuery, HDT_Work work, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3, boolean firstCall, boolean lastCall)
  {
    switch (curQuery)
    {
      case QUERY_LIKELY_EDITED_VOLS :

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

        break;

      case QUERY_4_OR_MORE_AUTHORS :

        return work.authorRecords.size() >= 4;

      case QUERY_WORK_NEEDING_PAGE_NUMBERS :

        for (HDT_WorkFile workFile : work.workFiles)
          if ((workFile.works.size() > 1) && "pdf".equalsIgnoreCase(workFile.filePath().getExtensionOnly()))
            if ((work.getStartPageNum(workFile) == -1) || (work.getEndPageNum(workFile) == -1))
              for (HDT_Work otherWork : workFile.works)
                if ((otherWork != work) && (otherWork.largerWork.get() != work))
                  return true;

        return false;

      case QUERY_ANALYZE_METADATA :

        if (firstCall)
        {
          FilePath filePath = DesktopUtil.homeDir().resolve("data.csv");
          if (filePath.exists()) try
          {
            Files.delete(filePath.toPath());
          }
          catch (IOException e)
          {
            e.printStackTrace();
          }

          csvFile = new ArrayList<>();
        }

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
              e.printStackTrace();
            }
          }
        });

        if (lastCall)
        {
          FilePath filePath = DesktopUtil.homeDir().resolve("data.csv");

          try
          {
            FileUtils.writeLines(filePath.toFile(), csvFile);
          }
          catch (IOException e)
          {
            e.printStackTrace();
          }
        }

        return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QueryType getQueryType()
  {
    return QueryType.qtWorks;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    switch (query)
    {
      default :
        break;
    }

    return new DatasetQuerySource(hdtWork);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean needsMentionsIndex(int query)
  {
    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasOperand(int query, int opNum, HyperTableCell prevOp)
  {
    switch (query)
    {
      case QUERY_LIKELY_EDITED_VOLS        :
      case QUERY_4_OR_MORE_AUTHORS         :
      case QUERY_ANALYZE_METADATA          :
      case QUERY_WORK_NEEDING_PAGE_NUMBERS :
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
