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

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.db;

import java.util.*;

import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.query.Query.FilteredFolderQuery;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableRow;

public final class FolderQueries
{

//---------------------------------------------------------------------------

  private FolderQueries() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  private static final int QUERY_DUPLICATE_FOLDERS = 3001;  // "that are duplicate folders"

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void addQueries(List<Query<?>> allQueries)
  {
    if (app.debugging) allQueries.add(new FilteredFolderQuery(QUERY_DUPLICATE_FOLDERS, "that are duplicate folders")
    {
      @Override public boolean evaluate(HDT_Folder folder, HyperTableRow row, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        return true;
      }

      @Override protected void runFilter(LinkedHashSet<HDT_Folder> folders, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
      {
        Map<FilePath, HDT_Folder> map = new HashMap<>();

        db.folders.forEach(folder ->
        {
          FilePath filePath = folder.filePath();

          if (map.containsKey(filePath))
          {
            folders.add(map.get(filePath));
            folders.add(folder);
          }
          else
            map.put(filePath, folder);
        });
      }

      @Override public boolean hasOperand(int opNum, HyperTableCell op1, HyperTableCell op2) { return false; }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
