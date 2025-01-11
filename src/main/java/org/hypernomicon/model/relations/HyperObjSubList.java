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

package org.hypernomicon.model.relations;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.IntStream;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class HyperObjSubList<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> extends HyperObjList<HDT_SubjType, HDT_ObjType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperObjList<HDT_SubjType, HDT_ObjType> parentList;
  private final int startNdx;
  private int endNdx;

//---------------------------------------------------------------------------

  public HyperObjSubList(HyperObjList<HDT_SubjType, HDT_ObjType> parentList, int startNdx, int endNdx)
  {
    super(parentList.relSet, parentList.subj, parentList.modTracking);

    if ((startNdx < 0) || (endNdx > parentList.size()) || (startNdx > endNdx))
      throw new IndexOutOfBoundsException("Invalid sublist range: startNdx=" + startNdx + ", endNdx=" + endNdx);

    this.parentList = parentList;
    this.startNdx = startNdx;
    this.endNdx = endNdx;
  }

//---------------------------------------------------------------------------

  @Override public int size()                { return endNdx - startNdx; }
  @Override public boolean isEmpty()         { return size() == 0; }
  @Override public void clear()              { while (endNdx > startNdx) remove(0); }
  @Override public Object[] toArray()        { return relSet.getUnmodifiableObjectList(subj).subList(startNdx, endNdx).toArray(); }
  @Override public <T> T[] toArray(T[] a)    { return relSet.getUnmodifiableObjectList(subj).subList(startNdx, endNdx).toArray(a); }
  @Override public int indexOf(Object o)     { return IntStream.range(0, size()).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }
  @Override public int lastIndexOf(Object o) { return IntStream.iterate(size() - 1, ndx -> ndx >= 0, ndx -> ndx - 1).filter(ndx -> get(ndx) == o).findFirst().orElse(-1); }

  @Override public boolean contains(Object o)            { return IntStream.range(startNdx, endNdx).anyMatch(ndx -> parentList.get(ndx) == o); }
  @Override public boolean containsAll(Collection<?> c)  { return c.stream().allMatch(this::contains); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HDT_ObjType> subList(int from, int to)
  {
    if ((from < 0) || (to > size()) || (from > to))
      throw new IndexOutOfBoundsException("Invalid subList range: from=" + from + ", to=" + to);

    return new HyperObjSubList<>(parentList, startNdx + from, startNdx + to);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType get(int ndx)
  {
    if ((ndx < 0) || (ndx >= size()))
      throw new IndexOutOfBoundsException();

    return parentList.get(startNdx + ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType set(int ndx, HDT_ObjType element)
  {
    if ((ndx < 0) || (ndx >= size()))
      throw new IndexOutOfBoundsException();

    return parentList.set(startNdx + ndx, element);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean add(HDT_ObjType e)
  {
    if (parentList.contains(e)) return false;

    parentList.add(endNdx, e);

    if (parentList.lastException == null)
    {
      endNdx++;
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean remove(Object o)
  {
    if (contains(o))
    {
      parentList.remove(o);
      endNdx--;
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean addAll(Collection<? extends HDT_ObjType> c)
  {
    List<HDT_ObjType> added = new ArrayList<>();

    for (HDT_ObjType record : c)
    {
      if (add(record))
        added.add(record);
      else
      {
        if (parentList.lastException != null)
        {
          Exception e = parentList.lastException;

          added.forEach(this::remove);

          parentList.lastException = e;
          return false;
        }
      }
    }

    return added.size() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean addAll(int index, Collection<? extends HDT_ObjType> c)
  {
    List<HDT_ObjType> added = new ArrayList<>();

    for (HDT_ObjType record : c)
    {
      if (parentList.contains(record) == false)
      {
        add(index, record);
        if (parentList.lastException == null)
        {
          index++;
          added.add(record);
        }
        else
        {
          Exception e = parentList.lastException;

          added.forEach(this::remove);

          parentList.lastException = e;
          return false;
        }
      }
    }

    return added.size() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean retainAll(Collection<?> c)
  {
    boolean removedAny = false;

    for (int ndx = startNdx; ndx < endNdx; ndx++)
    {
      if (c.contains(relSet.getObject(subj, ndx)) == false)
      {
        remove(ndx - startNdx);
        removedAny = true;
        ndx--;
      }
    }

    return removedAny;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void add(int index, HDT_ObjType element)
  {
    if ((index < 0) || (index > size()))
      throw new IndexOutOfBoundsException();

    int oldSize = parentList.size();

    parentList.add(startNdx + index, element);

    endNdx = endNdx + (parentList.size() - oldSize);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType remove(int index)
  {
    if ((index < 0) || (index >= size()))
      throw new IndexOutOfBoundsException();

    int oldSize = parentList.size();

    HDT_ObjType record = parentList.remove(startNdx + index);

    endNdx = endNdx + (parentList.size() - oldSize);

    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
