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

package org.hypernomicon.model;

import static java.util.Collections.*;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.HDT_Record;

final class HyperCore<HDT_DT extends HDT_Record>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record KeyIDpair(int id, String key) implements Comparable<KeyIDpair>, Cloneable
  {
    @Override public KeyIDpair clone()
    {
      try { return (KeyIDpair) super.clone(); } catch (CloneNotSupportedException ex) { throw new AssertionError(ex); }
    }

    //---------------------------------------------------------------------------

    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;

      KeyIDpair otherPair = (KeyIDpair) obj;
      return (otherPair.id == id) && otherPair.key.equals(key);
    }

    //---------------------------------------------------------------------------

    @Override public int compareTo(KeyIDpair otherPair)
    {
      int result = key.compareTo(otherPair.key);
      return result != 0 ? result : (id - otherPair.id);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<KeyIDpair>      sortedKeys = new ArrayList<>();
  private final List<Integer>        sortedIDs  = new ArrayList<>();
  private final Map<Integer, String> idToKey    = new HashMap<>();
  private final Map<Integer, HDT_DT> idToRecord = new HashMap<>();

  int size()                   { return sortedIDs.size(); }
  Stream<HDT_DT> stream()      { return sortedIDs.stream().map(idToRecord::get); }
  String getKeyByID(int id)    { return idToKey.get(id); }
  int getIDbyIDNdx(int ndx)    { return sortedIDs.get(ndx); }
  int getIDbyKeyNdx(int ndx)   { return sortedKeys.get(ndx).id(); }
  boolean containsID(int id)   { return idToRecord.containsKey(id); }
  HDT_DT getRecordByID(int id) { return idToRecord.get(id); }
  int getIDNdxByID(int id)     { return Math.max(-1, binarySearch(sortedIDs, id)); }
  int getKeyNdxByID(int id)    { return Math.max(-1, binarySearch(sortedKeys, new KeyIDpair(id, idToKey.get(id)))); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clear()
  {
    idToRecord.clear();
    idToKey   .clear();
    sortedIDs .clear();
    sortedKeys.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void changeRecordID(int oldID, int newID) throws HDB_InternalError
  {
    HDT_DT record = getRecordByID(oldID);
    String key = getKeyByID(oldID);

    if (record.getID() != newID)          // The record ID should have been changed to the new one already
      throw new HDB_InternalError(35468); // This function should only be called from HDT_Record.changeID

    remove(oldID);
    add(newID, key, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void resolvePointers() throws HDB_InternalError
  {
    Iterator<Entry<Integer, HDT_DT>> it = idToRecord.entrySet().iterator();

    while (it.hasNext())
    {
      Entry<Integer, HDT_DT> entry = it.next();
      HDT_DT record = entry.getValue();

      if (record.isExpired() == false)
      {
        if (record.getID() < 1)
        {
          record.expire();
          throw new HDB_InternalError(88388);
        }

        record.resolvePointers();
      }

      if (record.isExpired()) // See HDI_OnlineHubSpokes.resolvePointers
      {
        int id = entry.getKey();

        it.remove();

        sortedIDs.remove(getIDNdxByID(id));

        if (idToKey.containsKey(id))
        {
          sortedKeys.remove(getKeyNdxByID(id));
          idToKey   .remove(id);
        }
      }
      else if (record.getID() < 1)
      {
        throw new HDB_InternalError(88389);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void add(int id, String key, HDT_DT record)
  {
    addToSortedList(sortedIDs, id);
    setKey(id, key);
    idToRecord.put(id, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void remove(int id)
  {
    sortedIDs .remove(getIDNdxByID (id));
    sortedKeys.remove(getKeyNdxByID(id));
    idToKey   .remove(id);
    idToRecord.remove(id);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setKey(int id, String newKey)
  {
    String oldKey = idToKey.get(id);

    if (oldKey != null)
    {
      if (oldKey.equals(newKey)) return;

      sortedKeys.remove(getKeyNdxByID(id));
    }

    idToKey.put(id, newKey);
    addToSortedList(sortedKeys, new KeyIDpair(id, newKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
