/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.query.QueryTabCtrlr.QUERY_FIRST_NDX;
import static org.hypernomicon.query.QueryTabCtrlr.curQuery;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.query.sources.DatasetQuerySource;
import org.hypernomicon.query.sources.FilteredQuerySource;
import org.hypernomicon.query.sources.QuerySource;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.populators.QueryPopulator;
import org.hypernomicon.view.populators.VariablePopulator;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public class FolderQueryEngine extends QueryEngine<HDT_Folder>
{
  private static final int QUERY_DUPLICATE_FOLDERS = QUERY_FIRST_NDX + 1;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QueryType getQueryType()
  {
    return QueryType.qtFolders;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void addQueries(QueryPopulator pop, HyperTableRow row)
  {
    if (app.debugging())
      pop.addEntry(row, QUERY_DUPLICATE_FOLDERS, "that are duplicate folders");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void queryChange(int query, HyperTableRow row, VariablePopulator vp1, VariablePopulator vp2, VariablePopulator vp3)
  {
    switch (query)
    {

    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean evaluate(HDT_Folder folder, boolean firstCall, boolean lastCall)
  {
    switch (curQuery)
    {
      case QUERY_DUPLICATE_FOLDERS :

        return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public QuerySource getSource(int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    switch (query)
    {
      case QUERY_DUPLICATE_FOLDERS :
        return new FilteredQuerySource(getQueryType(), query)
        {
          @Override protected void runFilter()
          {
            Map<FilePath, HDT_Folder> map = new HashMap<>();
            Set<HDT_Folder> set = new HashSet<>();

            db.folders.forEach(folder ->
            {
              FilePath filePath = folder.filePath();

              if (map.containsKey(filePath))
              {
                if (set.contains(map.get(filePath)) == false)
                {
                  set.add(map.get(filePath));
                  list.add(map.get(filePath));
                }

                set.add(folder);
                list.add(folder);
              }
              else
                map.put(filePath, folder);
            });
          }
        };

      default :
        break;
    }

    return new DatasetQuerySource(hdtFolder);
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
      case QUERY_DUPLICATE_FOLDERS :
        return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
