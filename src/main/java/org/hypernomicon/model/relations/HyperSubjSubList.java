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

package org.hypernomicon.model.relations;

import java.util.Collection;
import java.util.List;

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

  @Override public boolean add(HDT_SubjType e)                                     { throw uoe(); }
  @Override public boolean remove(Object o)                                        { throw uoe(); }
  @Override public boolean addAll(Collection<? extends HDT_SubjType> c)            { throw uoe(); }
  @Override public boolean addAll(int index, Collection<? extends HDT_SubjType> c) { throw uoe(); }
  @Override public boolean retainAll(Collection<?> c)                              { throw uoe(); }
  @Override public void clear()                                                    { throw uoe(); }
  @Override public HDT_SubjType set(int index, HDT_SubjType element)               { throw uoe(); }
  @Override public void add(int index, HDT_SubjType element)                       { throw uoe(); }
  @Override public HDT_SubjType remove(int index)                                  { throw uoe(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean contains(Object o)
  {
    for (int ndx = startNdx; ndx < endNdx; ndx++)
      if (parentList.get(ndx) == o) return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Object[] toArray()
  {
    List<HDT_SubjType> subjList = relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx);

    Object[] array = new Object[subjList.size()];

    for (int ndx = 0; ndx < subjList.size(); ndx++)
      array[ndx] = subjList.get(ndx);

    return array;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <T> T[] toArray(T[] a)
  {
    List<HDT_SubjType> subjList = relSet.getUnmodifiableSubjectList(obj).subList(startNdx, endNdx);

    if (a.length < subjList.size())
      a = (T[]) new HDT_Record[subjList.size()];

    for (int ndx = 0; ndx < subjList.size(); ndx++)
      a[ndx] = (T) subjList.get(ndx);

    if (a.length > subjList.size())
      a[subjList.size()] = null;

    return a;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int indexOf(Object o)
  {
    for (int ndx = startNdx; ndx < endNdx; ndx++)
      if (get(ndx) == o)
        return ndx;

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int lastIndexOf(Object o)
  {
    for (int ndx = endNdx - 1; ndx >= startNdx; ndx++)
      if (get(ndx) == o)
        return ndx;

    return -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HDT_SubjType> subList(int fromIndex, int toIndex)
  {
    return new HyperSubjSubList<>(parentList, startNdx + fromIndex, startNdx + toIndex);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
}
