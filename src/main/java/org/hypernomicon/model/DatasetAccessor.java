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

package org.hypernomicon.model;

import java.util.Collection;
import java.util.Iterator;

import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public interface DatasetAccessor<HDT_DT extends HDT_Record> extends Collection<HDT_DT>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  String getKeyByID(int id);
  int getKeyNdxByID(int id);
  int getIDNdxByID(int id);
  HDT_DT getByID(int id);
  HDT_DT getByKeyNdx(int ndx);
  void setKey(int id, String newKey);
  Iterable<HDT_DT> keyIterable();
  Iterator<HDT_DT> keyIterator();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
