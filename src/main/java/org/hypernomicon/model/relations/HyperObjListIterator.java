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

import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

class HyperObjListIterator<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> implements ListIterator<HDT_ObjType>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperObjList<HDT_SubjType, HDT_ObjType> list;
  private int nextNdx, lastNdx = -1;
  private long expectedSizeModCount;

//---------------------------------------------------------------------------

  HyperObjListIterator(HyperObjList<HDT_SubjType, HDT_ObjType> list, int startNdx)
  {
    if ((startNdx < 0) || (startNdx > list.size()))
      throw new IndexOutOfBoundsException("Invalid index: " + startNdx);

    this.list = list;
    nextNdx = startNdx;

    expectedSizeModCount = list.getSizeModCount();
  }

//---------------------------------------------------------------------------

  @Override public boolean hasNext()       { return nextNdx < list.size(); }
  @Override public boolean hasPrevious()   { return nextNdx > 0; }
  @Override public int nextIndex()         { return nextNdx; }
  @Override public int previousIndex()     { return nextNdx - 1; }

  private void checkForComodification()    { list.checkForComodification(expectedSizeModCount); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType next()
  {
    if (hasNext() == false)
      throw new NoSuchElementException();

    checkForComodification();

    HDT_ObjType record = list.get(nextNdx);
    lastNdx = nextNdx++;
    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType previous()
  {
    if (hasPrevious() == false)
      throw new NoSuchElementException();

    checkForComodification();

    HDT_ObjType record = list.get(--nextNdx);
    lastNdx = nextNdx;
    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void add(HDT_ObjType e)
  {
    globalLock.lock();

    try
    {
      checkForComodification();

      list.add(nextNdx, e);

      if (list.lastException == null)
      {
        lastNdx = nextNdx++;

        expectedSizeModCount = list.getSizeModCount();
      }
    }
    finally
    {
      globalLock.unlock();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void remove()
  {
    if (lastNdx < 0)
      throw new IllegalStateException("No element to remove");

    if (lastNdx >= list.size())
      throw new IllegalStateException("Index out of bounds");

    globalLock.lock();

    try
    {
      checkForComodification();

      list.remove(lastNdx);

      if (nextNdx > lastNdx) nextNdx--;

      lastNdx = -1;

      expectedSizeModCount = list.getSizeModCount();
    }
    finally
    {
      globalLock.unlock();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void set(HDT_ObjType e)
  {
    if (lastNdx < 0)
      throw new IllegalStateException("No element to set");

    if (lastNdx >= list.size())
      throw new IllegalStateException("Index out of bounds");

    globalLock.lock();

    try
    {
      checkForComodification();

      list.set(lastNdx, e);

      if (list.lastException == null)
        expectedSizeModCount = list.getSizeModCount();
    }
    finally
    {
      globalLock.unlock();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
