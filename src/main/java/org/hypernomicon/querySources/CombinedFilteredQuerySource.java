/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.querySources;

import java.util.Set;

import org.hypernomicon.model.records.HDT_Base;

public class CombinedFilteredQuerySource implements QuerySource
{
  private final HDT_Base[] records;
  private final Set<HDT_Base> recordSet;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CombinedFilteredQuerySource(Set<HDT_Base> records)
  {
    this.recordSet = records;
    this.records = records.toArray(new HDT_Base[0]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int count()                             { return records.length; }
  @Override public QuerySourceType sourceType()            { return QuerySourceType.QST_combinedFilteredRecords; }
  @Override public boolean containsRecord(HDT_Base record) { return recordSet.contains(record); }
  @Override public HDT_Base getRecord(int ndx)             { return records[ndx]; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
