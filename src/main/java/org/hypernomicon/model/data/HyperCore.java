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

package org.hypernomicon.model.data;

import static java.util.Collections.*;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Stream;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

final class HyperCore<HDT_DT extends HDT_Record> implements DatasetAccessor<HDT_DT>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record KeyIDpair(int id, String key) implements Comparable<KeyIDpair>
  {
    @Override public boolean equals(Object obj)
    {
      if (this == obj) return true;
      if (obj == null) return false;
      if (getClass() != obj.getClass()) return false;

      KeyIDpair otherPair = (KeyIDpair) obj;
      return (id == otherPair.id) && key.equals(otherPair.key);
    }

    //---------------------------------------------------------------------------

    @Override public int compareTo(KeyIDpair otherPair)
    {
      int result = key.compareTo(otherPair.key);
      return result != 0 ? result : Integer.compare(id, otherPair.id);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<KeyIDpair>      sortedKeys = new ArrayList<>();
  private final List<Integer>        sortedIDs  = new ArrayList<>();
  private final Map<Integer, String> idToKey    = new HashMap<>();
  private final Map<Integer, HDT_DT> idToRecord = new HashMap<>();

  private final RecordType type;

  private int modCount = 0;

//---------------------------------------------------------------------------

  HyperCore(RecordType type)
  {
    this.type = type;
  }

//---------------------------------------------------------------------------

  private int getIDbyIDNdx(int ndx)    { return sortedIDs.get(ndx); }
  private int getIDbyKeyNdx(int ndx)   { return sortedKeys.get(ndx).id(); }
  private HDT_DT getByIDNdx(int ndx)   { return getByID(getIDbyIDNdx(ndx)); }

  boolean containsID(int id)           { return idToRecord.containsKey(id); }

  @Override public int size()                           { return sortedIDs.size(); }
  @Override public Stream<HDT_DT> stream()              { return sortedIDs.stream().map(idToRecord::get); }
  @Override public int getIDNdxByID(int id)             { return Math.max(-1, binarySearch(sortedIDs, id)); }
  @Override public int getKeyNdxByID(int id)            { return Math.max(-1, binarySearch(sortedKeys, new KeyIDpair(id, idToKey.get(id)))); }
  @Override public int getRandomUsedID(Random random)   { return isEmpty() ? -1 : getIDbyIDNdx(random.nextInt(size())); }

  @Override public String getKeyByID(int id)            { return idToKey.get(id); }
  @Override public HDT_DT getByID(int id)               { return idToRecord.get(id); }
  @Override public HDT_DT getByKeyNdx(int ndx)          { return getByID(getIDbyKeyNdx(ndx)); }

  @Override public Iterable<HDT_DT> keyIterable()       { return this::keyIterator; }
  @Override public Iterator<HDT_DT> keyIterator()       { return new CoreIterator(true); }

  @Override public Iterator<HDT_DT> iterator()          { return new CoreIterator(false); }

  @Override public boolean isEmpty()                    { return size() == 0; }
  @Override public boolean containsAll(Collection<?> c) { return c.stream().allMatch(this::contains); }
  @Override public Object[] toArray()                   { return stream().toArray(); }

  //---------------------------------------------------------------------------

  @Override public boolean contains(Object o)
  {
    if (o instanceof HDT_Record record)
    {
      if ((record.getType() != type) || (record.getID() < 1))
        return false;

      return getByID(record.getID()) == record;
    }

    return false;
  }

//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <T> T[] toArray(T[] a)
  {
    Objects.requireNonNull(a, "The provided array is null");

    int size = size();
    T[] result = a.length >= size ? a : (T[]) java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), size);

    int ndx = 0;
    for (HDT_DT record : this)
      result[ndx++] = (T) record;

    if (result.length > size)
      result[size] = null;  // Add ending marker to fulfill contract. This is done in case the passed-in array was full of data.

    return result;
  }

//---------------------------------------------------------------------------

  @Override public boolean add(HDT_DT e)                          { throw new UnsupportedOperationException("Add operation is not supported."      ); }
  @Override public boolean remove(Object o)                       { throw new UnsupportedOperationException("Remove operation is not supported."   ); }
  @Override public boolean addAll(Collection<? extends HDT_DT> c) { throw new UnsupportedOperationException("AddAll operation is not supported."   ); }
  @Override public boolean removeAll(Collection<?> c)             { throw new UnsupportedOperationException("RemoveAll operation is not supported."); }
  @Override public boolean retainAll(Collection<?> c)             { throw new UnsupportedOperationException("RetainAll operation is not supported."); }
  @Override public void clear()                                   { throw new UnsupportedOperationException("Clear operation is not supported."    ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class CoreIterator implements Iterator<HDT_DT>
  {
    private final boolean byKey;
    private final int expectedModCount;

    private int nextNdx = 0;

    @Override public boolean hasNext() { return nextNdx < size(); }
    @Override public void remove()     { throw new UnsupportedOperationException("Remove operation is not supported."); }

    //---------------------------------------------------------------------------

    private CoreIterator(boolean byKey)
    {
      this.byKey = byKey;
      this.expectedModCount = modCount;
    }

  //---------------------------------------------------------------------------

    @Override public HDT_DT next()
    {
      if (modCount != expectedModCount) throw new ConcurrentModificationException();
      if (hasNext() == false) throw new NoSuchElementException();

      return byKey ? getByKeyNdx(nextNdx++) : getByIDNdx(nextNdx++);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearInternal()
  {
    idToRecord.clear();
    idToKey   .clear();
    sortedIDs .clear();
    sortedKeys.clear();
    modCount++;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void changeRecordID(int oldID, int newID) throws HDB_InternalError
  {
    HDT_DT record = getByID(oldID);

    if (record.getID() != newID)          // The record ID should have been changed to the new one already
      throw new HDB_InternalError(35468);

    String key = getKeyByID(oldID);
    remove(oldID);
    add(newID, key, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * For each record, calls its resolvePointers method (which may cause the
   * record to become expired), and removes ("deletes") the record if it
   * became expired.<br>
   * HDT_Record.expire is where delete handlers get called.
   * @throws HDB_InternalError If the record somehow entered the inconsistent
   * state of its ID being -1 but the expired flag was not set.
   */
  void resolvePointers() throws HDB_InternalError
  {
    Iterator<Entry<Integer, HDT_DT>> it = idToRecord.entrySet().iterator();

    while (it.hasNext())
    {
      Entry<Integer, HDT_DT> entry = it.next();
      HDT_DT record = entry.getValue();

      boolean wasExpired = record.isExpired();

      if (wasExpired == false)
      {
        if (record.getID() < 1)
        {
          record.expire();
          throw new HDB_InternalError(88388);
        }

        record.resolvePointers();
      }

      if (record.isExpired())
      {
        if ((wasExpired == false) && (type != RecordType.hdtHub))
        {
          // Currently, the only case where a record can get expired
          // during pointer resolution is if a hub has less than 2
          // spokes when loading the database from XML.
          // If other integrity checks are added so that other
          // record types can get expired during pointer resolution,
          // this check should be updated.
          //
          // See HDT_RecordBase.expire and HDI_OnlineHubSpokes.resolvePointers

          throw new HDB_InternalError(88387);
        }

        int id = entry.getKey();

        it.remove();

        sortedIDs.remove(getIDNdxByID(id));

        if (idToKey.containsKey(id))
        {
          sortedKeys.remove(getKeyNdxByID(id));
          idToKey   .remove(id);
        }

        modCount++;
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
    modCount++;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void remove(int id)
  {
    sortedIDs .remove(getIDNdxByID (id));
    sortedKeys.remove(getKeyNdxByID(id));
    idToKey   .remove(id);
    idToRecord.remove(id);
    modCount++;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setKey(int id, String newKey)
  {
    String oldKey = idToKey.get(id);

    if (oldKey != null)
    {
      if (oldKey.equals(newKey)) return;

      sortedKeys.remove(getKeyNdxByID(id));
    }

    idToKey.put(id, newKey);
    addToSortedList(sortedKeys, new KeyIDpair(id, newKey));

    // Note: setKey doesn't increment modCount because it reorders but doesn't add/remove elements.
    // The CoreIterator iterates by index, so reordering is safe during iteration.
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
