/*
 * Copyright 2015-2021 Jason Winning
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

package org.hypernomicon.query.sources;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.query.engines.QueryEngine.QueryType;

public abstract class FilteredQuerySource implements QuerySource
{
  protected final List<HDT_Record> list = new ArrayList<>();
  private Iterator<HDT_Record> it;
  protected final HyperTableCell op1, op2, op3;
  protected final int query;
  protected final QueryType queryType;

  private boolean generated = false;

  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1, HyperTableCell op2) { this(queryType, query, op1, op2, null); }
  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1)                     { this(queryType, query, op1, null, null); }
  public FilteredQuerySource(QueryType queryType, int query)                                         { this(queryType, query, null, null, null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilteredQuerySource(QueryType queryType, int query, HyperTableCell op1, HyperTableCell op2, HyperTableCell op3)
  {
    this.queryType = queryType;
    this.query = query;
    this.op1 = op1;
    this.op2 = op2;
    this.op3 = op3;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int count()                               { ensureGenerated(); return list.size(); }
  @Override public boolean hasNext()                         { ensureGenerated(); return it.hasNext(); }
  @Override public HDT_Record next()                         { ensureGenerated(); return it.next(); }
  @Override public QuerySourceType sourceType()              { return QuerySourceType.QST_filteredRecords; }
  @Override public boolean containsRecord(HDT_Record record) { ensureGenerated(); return list.contains(record); }

  protected abstract void runFilter();

  public RecordType recordType()                        { return queryType.getRecordType(); }
  public boolean containsCell(HyperTableCell cell)      { ensureGenerated(); return list.contains(HyperTableCell.getRecord(cell)); }
  public void addAllTo(Set<HDT_Record> filteredRecords) { ensureGenerated(); filteredRecords.addAll(list); }
  protected void ensureGenerated()                      { if (!generated) { runFilter(); it = list.iterator(); generated = true; }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
