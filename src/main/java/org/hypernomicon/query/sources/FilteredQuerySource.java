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

package org.hypernomicon.query.sources;

import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public class FilteredQuerySource extends QuerySource
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final LinkedHashSet<HDT_Record> records;
  private final RecordType recordType;

//---------------------------------------------------------------------------

  public FilteredQuerySource(QuerySource origSource, LinkedHashSet<HDT_Record> records)
  {
    recordType = origSource.recordType();
    this.records = records;

    records.removeIf(Predicate.not(origSource::contains));
  }

//---------------------------------------------------------------------------

  @Override public int size()                      { return records.size(); }
  @Override public Iterator<HDT_Record> iterator() { return records.iterator(); }
  @Override public QuerySourceType sourceType()    { return QuerySourceType.QST_filteredRecords; }
  @Override public boolean contains(Object record) { return records.contains(record); }
  @Override public RecordType recordType()         { return recordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
