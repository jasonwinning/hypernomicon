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

package org.hypernomicon.model.relations;

import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.records.HDT_Base;

public class HyperObjListIterator<HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base> implements ListIterator<HDT_ObjType>
{
  private final HyperObjList<HDT_SubjType, HDT_ObjType> list;
  private int nextNdx;
  private int lastNdx = -1;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperObjListIterator(HyperObjList<HDT_SubjType, HDT_ObjType> list, int startNdx)
  {
    this.list = list;
    nextNdx = startNdx;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasNext()       { return nextNdx < list.size(); }
  @Override public boolean hasPrevious()   { return nextNdx > 0; }
  @Override public int nextIndex()         { return nextNdx; }
  @Override public int previousIndex()     { return nextNdx - 1; }
  @Override public void add(HDT_ObjType e) { list.add(nextNdx, e); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType next()
  {
    if (hasNext())
    {
      HDT_ObjType record = list.get(nextNdx);
      lastNdx = nextNdx;
      nextNdx++;
      return record;
    }

    throw new NoSuchElementException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType previous()
  {
    if (hasPrevious())
    {
      nextNdx--;
      HDT_ObjType record = list.get(nextNdx);
      lastNdx = nextNdx;

      return record;
    }

    throw new NoSuchElementException();

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void remove()
  {
    if (lastNdx < 0)
      throw new IllegalStateException();

    if (lastNdx >= list.size())
      throw new IllegalStateException();

    list.remove(lastNdx);

    if (nextNdx > lastNdx) nextNdx--;

    lastNdx = -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void set(HDT_ObjType e)
  {
    if (lastNdx < 0)
      throw new IllegalStateException();

    if (lastNdx >= list.size())
      throw new IllegalStateException();

    list.set(lastNdx, e);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
