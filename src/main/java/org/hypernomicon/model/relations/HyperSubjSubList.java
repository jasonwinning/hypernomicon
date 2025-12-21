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

package org.hypernomicon.model.relations;

import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class HyperSubjSubList<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> extends HyperSubjList<HDT_SubjType, HDT_ObjType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperSubjList<HDT_SubjType, HDT_ObjType> parentList;
  private final int startNdx, endNdx;

//---------------------------------------------------------------------------

  public HyperSubjSubList(HyperSubjList<HDT_SubjType, HDT_ObjType> parentList, int startNdx, int endNdx)
  {
    super(parentList.relSet, parentList.obj);

    if ((startNdx < 0) || (endNdx > parentList.size()) || (startNdx > endNdx))
      throw new IndexOutOfBoundsException("Invalid sublist range: startNdx=" + startNdx + ", endNdx=" + endNdx);

    this.parentList = parentList;
    this.startNdx = startNdx;
    this.endNdx = endNdx;
  }

//---------------------------------------------------------------------------

  @Override public int size()                           { return endNdx - startNdx; }
  @Override public boolean isEmpty()                    { return size() == 0; }
  @Override public boolean containsAll(Collection<?> c) { return c.stream().allMatch(this::contains); }
  @Override public Object[] toArray()                   { return relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx).toArray(); }
  @Override public <T> T[] toArray(T[] a)               { return relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx).toArray(a); }
  @Override public boolean contains(Object o)           { return IntStream.range(startNdx, endNdx).anyMatch(ndx -> parentList.get(ndx) == o); }
  @Override public int indexOf(Object o)                { return IntStream.range(0, size()).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }
  @Override public int lastIndexOf(Object o)            { return IntStream.iterate(size() - 1, ndx -> ndx >= 0, ndx -> ndx - 1).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HDT_SubjType> subList(int fromIndex, int toIndex)
  {
    if ((fromIndex < 0) || (toIndex > size()) || (fromIndex > toIndex))
      throw new IndexOutOfBoundsException("Invalid subList range: from=" + fromIndex + ", to=" + toIndex);

    return new HyperSubjSubList<>(parentList, startNdx + fromIndex, startNdx + toIndex);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_SubjType get(int index)
  {
    if ((index < 0) || (index >= size()))
      throw new IndexOutOfBoundsException();

    return parentList.get(startNdx + index);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
