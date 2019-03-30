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

import java.util.Iterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.records.HDT_Record;

public class HyperObjIterator<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> implements Iterator<HDT_ObjType>
{
  private final HyperObjList<HDT_SubjType, HDT_ObjType> list;
  private int nextNdx = 0, lastNdx = -1;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperObjIterator(HyperObjList<HDT_SubjType, HDT_ObjType> list)
  {
    this.list = list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasNext() { return nextNdx < list.size();  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType next()
  {
    if (hasNext())
    {
      lastNdx = nextNdx;
      nextNdx++;
      return list.get(lastNdx);
    }

    throw new NoSuchElementException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void remove()
  {
    if (lastNdx == -1)
      throw new IllegalStateException();

    list.remove(lastNdx);
    nextNdx--;
    lastNdx = -1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
