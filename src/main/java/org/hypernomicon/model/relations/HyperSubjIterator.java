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
import static org.hypernomicon.model.relations.HyperSubjList.*;

public class HyperSubjIterator<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> implements Iterator<HDT_SubjType>
{
  private final HyperSubjList<HDT_SubjType, HDT_ObjType> list;
  int nextNdx = 0;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperSubjIterator(HyperSubjList<HDT_SubjType, HDT_ObjType> list)
  {
    this.list = list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasNext() { return nextNdx < list.size(); }
  @Override public void remove()     { throw new UnsupportedOperationException(modErrMsg); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_SubjType next()
  {
    if (hasNext())
      return list.get(nextNdx++);

    throw new NoSuchElementException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
