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

package org.hypernomicon.query.sources;

import java.util.EnumSet;
import java.util.Iterator;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_RecordType;

import com.google.common.collect.Iterators;

import static org.hypernomicon.model.HyperDB.*;

public class CombinedUnfilteredQuerySource implements QuerySource
{
  private final Set<HDT_RecordType> types;
  private final Iterator<? extends HDT_Record> it;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public CombinedUnfilteredQuerySource(EnumSet<HDT_RecordType> types)
  {
    this.types = types;

    it = Iterators.concat(types.stream().map(type -> db.records(type).iterator()).iterator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int count()                          { return types.stream().map(type -> db.records(type).size()).reduce(0, (i1, i2) -> i1 + i2); }
  @Override public QuerySourceType sourceType()         { return QuerySourceType.QST_combinedUnfilteredRecords; }
  @Override public boolean containsRecord(HDT_Record r) { return types.contains(r.getType()); }
  @Override public boolean hasNext()                    { return it.hasNext(); }
  @Override public HDT_Record next()                    { return it.next(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
