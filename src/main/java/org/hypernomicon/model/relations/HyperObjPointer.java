/*
 * Copyright 2015-2026 Jason Winning
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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Objects;

import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class HyperObjPointer<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  private final HDT_SubjType subj;
  private final boolean modTracking;
  private Exception lastException;

//---------------------------------------------------------------------------

  public HyperObjPointer(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_SubjType subj, boolean modTracking)
  {
    this.relSet = relSet;
    this.subj = subj;
    this.modTracking = modTracking;

    lastException = null;
  }

//---------------------------------------------------------------------------

  public HDT_ObjType get()        { return relSet.getObjectCount(subj) == 0 ? null : relSet.getObject(subj, 0); }
  public int getID()              { return HDT_Record.getIDSafe(get()); }
  public boolean isNull()         { return get() == null; }
  public boolean isNotNull()      { return get() != null; }

  @SuppressWarnings("unchecked")
  public boolean setID(int newID) { return set(newID < 1 ? null : (HDT_ObjType) db.records(relSet.getObjType()).getByID(newID)); }

  @Override public int hashCode()           { return Objects.hash(get()); }
  @Override public boolean equals(Object o) { return (o instanceof HyperObjPointer<?, ?> ptr) && (ptr.get() == get()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean set(HDT_ObjType obj)
  {
    if (get() == obj) return true;

    try
    {
      globalLock.lock();

      try
      {
        if (obj != null)
        {
          if (obj.getType() != relSet.getObjType())
            throw new HDB_InternalError(2055);

          relSet.cycleCheck(subj, obj);

          relSet.clearObjects(subj);

          relSet.setObjectSkipCycleCheck(subj, obj, -1);
        }
        else
          relSet.clearObjects(subj);

        if (modTracking) subj.modifyNow();
      }
      catch (HyperDataException e)
      {
        lastException = e;
        throw e;
      }
      finally
      {
        globalLock.unlock();
      }
    }
    catch (HyperDataException e)
    {
      if (e instanceof RelationCycleException)
        errorPopup(e);

      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void throwLastException() throws RelationCycleException
  {
    if (lastException instanceof RelationCycleException rce)
      throw rce;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
