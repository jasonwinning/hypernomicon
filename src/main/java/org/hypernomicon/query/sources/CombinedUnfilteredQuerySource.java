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

package org.hypernomicon.query.sources;

import java.util.Collection;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.Set;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

import com.google.common.collect.Iterators;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;

//---------------------------------------------------------------------------

public class CombinedUnfilteredQuerySource extends QuerySource
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Set<RecordType> types;

//---------------------------------------------------------------------------

  public CombinedUnfilteredQuerySource(Set<RecordType> newTypes)
  {
    if (newTypes == null)
      types = EnumSet.noneOf(RecordType.class);
    else if (newTypes.contains(hdtNone))
    {
      types = EnumSet.allOf(RecordType.class);
      types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
    }
    else
      types = newTypes;
  }

//---------------------------------------------------------------------------

  @Override public int size()                       { return types.stream().map(db::records).mapToInt(Collection::size).sum(); }
  @Override public QuerySourceType sourceType()     { return QuerySourceType.QST_combinedUnfilteredRecords; }
  @Override public Iterator<HDT_Record> iterator()  { return Iterators.concat(types.stream().map(type -> db.records(type).iterator()).iterator()); }
  @Override public RecordType recordType()          { return types.size() == 1 ? (RecordType) types.toArray()[0] : hdtNone; }
  @Override public boolean contains(Object element) { return (element instanceof HDT_Record record) && types.contains(record.getType()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
