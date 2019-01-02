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

import org.hypernomicon.model.records.HDT_Base;

import static org.hypernomicon.util.Util.*;

public class HyperSubjPointer<HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base>
{
  final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  final HDT_ObjType obj;

  public HyperSubjPointer(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_ObjType obj)
  {
    this.relSet = relSet;
    this.obj = obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_SubjType get()       { return relSet.getSubjectCount(obj) == 0 ? null : relSet.getSubject(obj, 0); }
  public boolean isNull()         { return get() == null; }
  public boolean isNotNull()      { return get() != null; }
  public int getID()              { return nullSwitch(get(), -1, HDT_Base::getID); }

  @Override public int hashCode()           { return super.hashCode(); }  
  @Override public boolean equals(Object o) { return o instanceof HyperSubjPointer<?, ?> ? ((HyperSubjPointer<?, ?>) o).get() == get() : false; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
