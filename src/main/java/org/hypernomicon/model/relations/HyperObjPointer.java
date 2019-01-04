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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Base;

public class HyperObjPointer<HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base>
{
  final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  final HDT_SubjType subj;
  final protected boolean modTracking;
  private Exception lastException;

  public HyperObjPointer(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_SubjType subj, boolean modTracking)
  {
    this.relSet = relSet;
    this.subj = subj;
    this.modTracking = modTracking;

    lastException = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_ObjType get()            { return relSet.getObjectCount(subj) == 0 ? null : relSet.getObject(subj, 0); }
  public int getID()                  { return nullSwitch(get(), -1, HDT_Base::getID); }
  public boolean isNull()             { return get() == null; }
  public boolean isNotNull()          { return get() != null; }
  public Exception getLastException() { return lastException; }

  @SuppressWarnings("unchecked")
  public boolean setID(int newID)     { return set(newID < 1 ? null : (HDT_ObjType) db.records(relSet.getObjType()).getByID(newID)); }

  @Override public int hashCode()           { return super.hashCode(); }
  @Override public boolean equals(Object o) { return o instanceof HyperObjPointer<?, ?> ? ((HyperObjPointer<?, ?>) o).get() == get() : false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean set(HDT_ObjType obj)
  {
    if (get() == obj) return true;

    relSet.clearObjects(subj);

    try
    {
      if (obj != null)
      {
        if (obj.getType() != relSet.getObjType())
        {
          messageDialog("Interal error #02055", mtError);
          return false;
        }
        relSet.setObject(subj, obj, -1, true);
      }

      if (modTracking) subj.modifyNow();
    }
    catch (RelationCycleException e)
    {
      messageDialog(e.getMessage(), mtError);

      lastException = e;
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void throwLastException() throws RelationCycleException
  {
    if (getLastException() instanceof RelationCycleException)
      throw (RelationCycleException) getLastException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
