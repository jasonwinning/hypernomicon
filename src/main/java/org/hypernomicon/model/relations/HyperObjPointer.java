/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Record;

public class HyperObjPointer<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record>
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

  public HDT_ObjType get()        { return relSet.getObjectCount(subj) == 0 ? null : relSet.getObject(subj, 0); }
  public int getID()              { return nullSwitch(get(), -1, HDT_Record::getID); }
  public boolean isNull()         { return get() == null; }
  public boolean isNotNull()      { return get() != null; }

  @SuppressWarnings("unchecked")
  public boolean setID(int newID) { return set(newID < 1 ? null : (HDT_ObjType) db.records(relSet.getObjType()).getByID(newID)); }

  @Override public int hashCode()           { return super.hashCode(); }
  @Override public boolean equals(Object o) { return (o instanceof HyperObjPointer<?, ?>) && (((HyperObjPointer<?, ?>) o).get() == get()); }

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
          return falseWithErrorMessage("Interal error #02055");

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
    if (lastException instanceof RelationCycleException)
      throw (RelationCycleException) lastException;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
