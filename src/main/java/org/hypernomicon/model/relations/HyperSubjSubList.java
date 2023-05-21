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

package org.hypernomicon.model.relations;

import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class HyperSubjSubList<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> extends HyperSubjList<HDT_SubjType, HDT_ObjType>
{
  private final HyperSubjList<HDT_SubjType, HDT_ObjType> parentList;
  private final int startNdx, endNdx;

  public HyperSubjSubList(HyperSubjList<HDT_SubjType, HDT_ObjType> parentList, int startNdx, int endNdx)
  {
    super(parentList.relSet, parentList.obj);

    this.parentList = parentList;
    this.startNdx = startNdx;
    this.endNdx = endNdx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                           { return endNdx - startNdx; }
  @Override public boolean isEmpty()                    { return size() > 0; }
  @Override public HDT_SubjType get(int index)          { return parentList.get(startNdx + index); }
  @Override public boolean containsAll(Collection<?> c) { return c.stream().allMatch(this::contains); }
  @Override public Object[] toArray()                   { return relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx).toArray(); }
  @Override public <T> T[] toArray(T[] a)               { return relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx).toArray(a); }
  @Override public boolean contains(Object o)           { return IntStream.range(startNdx, endNdx).anyMatch(ndx -> parentList.get(ndx) == o); }
  @Override public int indexOf(Object o)                { return IntStream.range(startNdx, endNdx).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }
  @Override public int lastIndexOf(Object o)            { return IntStream.iterate(endNdx - 1, ndx -> ndx >= startNdx, ndx -> ndx - 1).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }

  @Override public HDT_SubjType set(int index, HDT_SubjType element)      { throw uoe(); }
  @Override public HDT_SubjType remove(int index)                         { throw uoe(); }

  @Override public List<HDT_SubjType> subList(int fromIndex, int toIndex) { return new HyperSubjSubList<>(parentList, startNdx + fromIndex, startNdx + toIndex); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
