/*
 * Copyright 2015-2024 Jason Winning
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

import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.Util.*;

import java.util.Objects;

//---------------------------------------------------------------------------

public class HyperSubjPointer<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  private final HDT_ObjType obj;

//---------------------------------------------------------------------------

  public HyperSubjPointer(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_ObjType obj)
  {
    this.relSet = relSet;
    this.obj = obj;
  }

//---------------------------------------------------------------------------

  public HDT_SubjType get()  { return relSet.getSubjectCount(obj) == 0 ? null : relSet.getSubject(obj, 0); }
  public boolean isNull()    { return get() == null; }
  public boolean isNotNull() { return get() != null; }
  public int getID()         { return nullSwitch(get(), -1, HDT_Record::getID); }

  @Override public int hashCode()           { return Objects.hash(get()); }
  @Override public boolean equals(Object o) { return (o instanceof HyperSubjPointer<?, ?>) && (((HyperSubjPointer<?, ?>) o).get() == get()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
