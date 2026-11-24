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

package org.hypernomicon.query;

import java.util.ArrayList;
import java.util.List;

//---------------------------------------------------------------------------

/**
 * The record queries, built once on first use. A query tab offers them as choices, and a query
 * favorite looks up the query each of its rows was saved with, to ask which operands hold records.
 */
public final class QueryRegistry
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private QueryRegistry() { throw new UnsupportedOperationException("Instantiation is not allowed."); }

  private static final List<Query<?>> queries = buildList();

//---------------------------------------------------------------------------

  public static List<Query<?>> all() { return queries; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Returns the query with the given ID, or null if there is none: the ID can be a report's, or that
   * of a query that is only available while debugging.
   */
  public static Query<?> withID(int queryID)
  {
    return queries.stream().filter(query -> query.getID() == queryID).findFirst().orElse(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<Query<?>> buildList()
  {
    List<Query<?>> list = new ArrayList<>();

    GeneralQueries .addQueries(list);
    FolderQueries  .addQueries(list);
    PersonQueries  .addQueries(list);
    ArgumentQueries.addQueries(list);
    WorkQueries    .addQueries(list);

    return List.copyOf(list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
