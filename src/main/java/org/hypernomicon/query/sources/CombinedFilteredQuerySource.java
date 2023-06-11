/*
 * Copyright 2015-2023 Jason Winning
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
import java.util.Set;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

import static org.hypernomicon.model.records.RecordType.*;

//---------------------------------------------------------------------------

public class CombinedFilteredQuerySource extends QuerySource
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Set<HDT_Record> records;
  private final RecordType recordType;

//---------------------------------------------------------------------------

  public CombinedFilteredQuerySource(Iterable<QuerySource> sources)
  {
    records = new LinkedHashSet<>();
    RecordType singleType = null;

    for (QuerySource src : sources)
      if (src.sourceType() == QuerySourceType.QST_filteredRecords)
      {
        FilteredQuerySource fqs = (FilteredQuerySource) src;

        if (singleType == null)
          singleType = fqs.recordType();
        else if ((singleType != hdtNone) && (singleType != fqs.recordType()))
          singleType = hdtNone;

        records.addAll(fqs);
      }

    this.recordType = singleType == null ? hdtNone : singleType;
  }

//---------------------------------------------------------------------------

  @Override public int size()                      { return records.size(); }
  @Override public QuerySourceType sourceType()    { return QuerySourceType.QST_combinedFilteredRecords; }
  @Override public Iterator<HDT_Record> iterator() { return records.iterator(); }
  @Override public boolean contains(Object record) { return records.contains(record); }
  @Override public RecordType recordType()         { return recordType; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
