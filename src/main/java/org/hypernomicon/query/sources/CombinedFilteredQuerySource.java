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

import java.util.Iterator;
import java.util.List;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Record;

public class CombinedFilteredQuerySource implements QuerySource
{
  private final List<HDT_Record> records;
  private final Iterator<HDT_Record> it;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CombinedFilteredQuerySource(Set<HDT_Record> recordSet)
  {
    records = List.copyOf(recordSet);
    it = records.iterator();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int count()                               { return records.size(); }
  @Override public QuerySourceType sourceType()              { return QuerySourceType.QST_combinedFilteredRecords; }
  @Override public boolean containsRecord(HDT_Record record) { return records.contains(record); }
  @Override public boolean hasNext()                         { return it.hasNext(); }
  @Override public HDT_Record next()                         { return it.next(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
