/*
 * Copyright 2015-2022 Jason Winning
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

import java.util.AbstractCollection;
import java.util.Collection;
import java.util.function.Predicate;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

public abstract class QuerySource extends AbstractCollection<HDT_Record>
{
  private static UnsupportedOperationException uoe() { return new UnsupportedOperationException(); }

  public enum QuerySourceType
  {
    QST_allRecords,
    QST_recordsByType,
    QST_filteredRecords,
    QST_combinedFilteredRecords,
    QST_combinedUnfilteredRecords
  }

  public abstract QuerySourceType sourceType();
  public abstract RecordType recordType();

  @Override public boolean add(HDT_Record e)                              { throw uoe(); }
  @Override public boolean addAll(Collection<? extends HDT_Record> c)     { throw uoe(); }
  @Override public void    clear()                                        { throw uoe(); }
  @Override public boolean remove(Object o)                               { throw uoe(); }
  @Override public boolean removeAll(Collection<?> c)                     { throw uoe(); }
  @Override public boolean removeIf(Predicate<? super HDT_Record> filter) { throw uoe(); }
  @Override public boolean retainAll(Collection<?> c)                     { throw uoe(); }
}
