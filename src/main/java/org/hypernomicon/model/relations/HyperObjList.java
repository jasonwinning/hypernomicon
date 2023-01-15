/*
 * Copyright 2015-2023 Jason Winning
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.stream.IntStream;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.Util.*;

public class HyperObjList<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> implements List<HDT_ObjType>
{
  final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  final HDT_SubjType subj;
  Exception lastException;
  protected final boolean modTracking;
  private final List<HDT_ObjType> before = new ArrayList<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperObjList(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_SubjType subj, boolean modTracking)
  {
    this.relSet = relSet;
    this.subj = subj;
    this.modTracking = modTracking;

    lastException = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()
  {
    lastException = null;
    return relSet.getObjectCount(subj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isEmpty()
  {
    lastException = null;
    return size() == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean contains(Object o)
  {
    lastException = null;

    return (o instanceof HDT_Record) && (((HDT_Record) o).getType() == relSet.getObjType()) && relSet.alreadyHasAsObject(subj, (HDT_ObjType) o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Object[] toArray()
  {
    lastException = null;

    List<HDT_ObjType> objList = relSet.getUnmodifiableObjectList(subj);

    Object[] array = new Object[objList.size()];

    for (int ndx = 0; ndx < objList.size(); ndx++)
      array[ndx] = objList.get(ndx);

    return array;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <T> T[] toArray(T[] a)
  {
    lastException = null;

    List<HDT_ObjType> objList = relSet.getUnmodifiableObjectList(subj);

    if (a.length < objList.size())
      a = (T[]) new HDT_Record[objList.size()];

    for (int ndx = 0; ndx < objList.size(); ndx++)
      a[ndx] = (T) objList.get(ndx);

    if (a.length > objList.size())
      a[objList.size()] = null;

    return a;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a record the object list if it is not already present.
   * <p>
   * <i>(Violation)</i> The {@code List} interface requires that this
   * method returns {@code true} always. However this class may return
   * {@code false} because of the {@code Set} behavior, or because a
   * cycle would result.
   *
   * @param obj  the record to add
   * @return true if record was added
   */
  @Override public boolean add(HDT_ObjType obj)
  {
    lastException = null;
    if (relSet.alreadyHasAsObject(subj, obj)) return false;
    modStart();

    try
    {
      relSet.setObject(subj, obj, -1, true);
    }
    catch (RelationCycleException e)
    {
      lastException = e;
      return false;
    }

    modEnd();
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void initObjWithSubjOrd(HDT_ObjType obj, int subjOrd) throws RelationCycleException, HDB_InternalError
  {
    if (db.isLoaded())
      throw new HDB_InternalError(71634);

    relSet.setObject(subj, obj, -1, subjOrd, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void modEnd()
  {
    if (modTracking == false) return;

    List<HDT_ObjType> after = relSet.getUnmodifiableObjectList(subj);

    if ((before.size() != after.size()) || (before.equals(after) == false))
      subj.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void modStart()
  {
    if (modTracking == false) return;

    before.clear();
    before.addAll(relSet.getUnmodifiableObjectList(subj));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean remove(Object o)
  {
    lastException = null;

    if ((o instanceof HDT_Record) == false)
      return false;

    if (((HDT_Record)o).getType() != relSet.getObjType())
      return false;

    HDT_ObjType obj = (HDT_ObjType)o;

    if (relSet.alreadyHasAsObject(subj, obj) == false) return false;

    modStart();

    try { relSet.setObject(subj, obj, -1, false); }
    catch (RelationCycleException e) { noOp(); }

    modEnd();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean containsAll(Collection<?> c)
  {
    lastException = null;

    for (Object o : c)
    {
      if ((o instanceof HDT_Record) == false)
        return false;

      if (((HDT_Record)o).getType() != relSet.getObjType())
        return false;

      HDT_ObjType obj = (HDT_ObjType)o;

      if (relSet.alreadyHasAsObject(subj, obj) == false) return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    modStart();

    lastException = null;
    relSet.clearObjects(subj);

    modEnd();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType get(int ndx)
  {
    lastException = null;
    return relSet.getObject(subj, ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a collection of records to the end of the list avoiding duplicates.
   * <p>
   * Only records that are not already in this list will be added, and
   * duplicates, as well as records that would result in a cycle, from
   * the specified collection will be ignored.
   * <p>
   * <i>(Violation)</i> The {@code List} interface makes the assumption
   * that the elements are always inserted. This may not happen with this
   * implementation.
   *
   * @param c  the collection to add in iterator order
   * @return true if this collection changed
   */
  @Override public boolean addAll(Collection<? extends HDT_ObjType> c)
  {
    lastException = null;
    List<HDT_ObjType> added = new ArrayList<>();

    modStart();

    for (HDT_ObjType record : c)
    {
      if (add(record))
        added.add(record);
      else
      {
        if (lastException != null)
        {
          Exception e = lastException;

          added.forEach(this::remove);

          lastException = e;

          modEnd();
          return false;
        }
      }
    }

    modEnd();
    return added.size() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a collection of records at a specific index in the list avoiding
   * duplicates.
   * <p>
   * Only elements that are not already in this list will be added, and
   * duplicates, as well as records that would result in a cycle, from
   * the specified collection will be ignored.
   * <p>
   * <i>(Violation)</i> The {@code List} interface makes the assumption
   * that the elements are always inserted. This may not happen with this
   * implementation.
   *
   * @param index  the index to insert at
   * @param c  the collection to add in iterator order
   * @return true if this collection changed
   */
  @Override public boolean addAll(int index, Collection<? extends HDT_ObjType> c)
  {
    lastException = null;
    List<HDT_ObjType> added = new ArrayList<>();

    modStart();

    for (HDT_ObjType record : c)
    {
      if (contains(record) == false)
      {
        add(index, record);
        if (lastException == null)
        {
          index++;
          added.add(record);
        }
        else
        {
          Exception e = lastException;

          added.forEach(this::remove);

          lastException = e;
          modEnd();
          return false;
        }
      }
    }

    modEnd();
    return added.size() > 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean removeAll(Collection<?> c)
  {
    lastException = null;
    boolean removedAny = false;

    modStart();

    for (Object o : c)
      while (contains(o))
        if (remove(o))
          removedAny = true;

    modEnd();
    return removedAny;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean retainAll(Collection<?> c)
  {
    lastException = null;
    boolean removedAny = false;

    modStart();

    for (int cnt = relSet.getObjectCount(subj), ndx = 0; ndx < cnt; ndx++)
    {
      if (c.contains(relSet.getObject(subj, ndx)) == false)
      {
        remove(ndx--);
        removedAny = true;
        cnt--;
      }
    }

    modEnd();
    return removedAny;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType set(int index, HDT_ObjType element)
  {
    lastException = null;

    modStart();

    HDT_ObjType record = remove(index);
    add(index, element);

    modEnd();
    return record;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Adds a record to a specific index in the list if it is not already
   * present.
   * <p>
   * <i>(Violation)</i> The {@code List} interface makes the assumption
   * that the element is always inserted. This may not happen with this
   * implementation.
   *
   * @param index  the index to insert at
   * @param obj  the record to add
   */
  @Override public void add(int index, HDT_ObjType obj)
  {
    lastException = null;
    if (relSet.alreadyHasAsObject(subj, obj)) return;

    modStart();

    try
    {
      relSet.setObject(subj, obj, index, true);
    }
    catch (RelationCycleException e)
    {
      lastException = e;
    }

    modEnd();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_ObjType remove(int index)
  {
    lastException = null;
    HDT_ObjType obj = get(index);

    modStart();

    try { relSet.setObject(subj, obj, index, false); }
    catch (RelationCycleException e) { noOp(); }

    modEnd();

    return obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public int indexOf(Object o)
  {
    lastException = null;

    if ((o instanceof HDT_Record) == false) return -1;

    if (((HDT_Record)o).getType() != relSet.getObjType()) return -1;

    return relSet.getObjectNdx(subj, (HDT_ObjType)o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public int lastIndexOf(Object o)
  {
    lastException = null;

    if ((o instanceof HDT_Record) == false) return -1;

    if (((HDT_Record)o).getType() != relSet.getObjType()) return -1;

    return relSet.getObjectNdx(subj, (HDT_ObjType)o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reorder(List<HDT_ObjType> list)
  {
    boolean changed = false;

    if (size() != list.size()) throw new NoSuchElementException();

    for (HDT_ObjType record : list)
      if (contains(record) == false) throw new NoSuchElementException();

    for (int ndx = 0; ndx < size(); ndx++)
    {
      if (list.contains(get(ndx)) == false) throw new NoSuchElementException();
      if (list.get(ndx) != get(ndx)) changed = true;
    }

    relSet.reorderObjects(subj, list);
    if (modTracking && changed) subj.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<HDT_ObjType> subList(int fromIndex, int toIndex)
  {
    lastException = null;
    return new HyperObjSubList<>(this, fromIndex, toIndex);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<HDT_ObjType> iterator()
  {
    lastException = null;
    return new HyperObjIterator<>(this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ListIterator<HDT_ObjType> listIterator()
  {
    lastException = null;
    return new HyperObjListIterator<>(this, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ListIterator<HDT_ObjType> listIterator(int index)
  {
    lastException = null;
    return new HyperObjListIterator<>(this, index);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object o)
  {
    if ((o instanceof List) == false) return false;

    List<?> list = (List<?>)o;

    if (list.size() != size()) return false;

    return IntStream.range(0, list.size()).noneMatch(ndx -> list.get(ndx) != get(ndx));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    int hashCode = 1;

    for (HDT_ObjType obj : this)
      hashCode = (31 * hashCode) + obj.hashCode();

    return hashCode;
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
