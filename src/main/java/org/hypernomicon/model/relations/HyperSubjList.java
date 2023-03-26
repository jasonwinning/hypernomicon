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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.records.HDT_Record;

public class HyperSubjList<HDT_SubjType extends HDT_Record, HDT_ObjType extends HDT_Record> implements List<HDT_SubjType>
{
  final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  final HDT_ObjType obj;

  public HyperSubjList(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_ObjType obj)
  {
    this.relSet = relSet;
    this.obj = obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static UnsupportedOperationException uoe() { return new UnsupportedOperationException("Internal error: An attempt was made to modify a subject list."); }

  @Override public int size()                                             { return relSet.getSubjectCount(obj); }
  @Override public boolean isEmpty()                                      { return size() == 0; }
  @Override public HDT_SubjType get(int ndx)                              { return relSet.getSubject(obj, ndx); }
  @Override public int lastIndexOf(Object o)                              { return indexOf(o); }
  @Override public List<HDT_SubjType> subList(int fromIndex, int toIndex) { return new HyperSubjSubList<>(this, fromIndex, toIndex); }
  @Override public Iterator<HDT_SubjType> iterator()                      { return new HyperSubjIterator<>(this); }
  @Override public ListIterator<HDT_SubjType> listIterator()              { return new HyperSubjListIterator<>(this, 0); }
  @Override public ListIterator<HDT_SubjType> listIterator(int index)     { return new HyperSubjListIterator<>(this, index); }
  @Override public Object[] toArray()                                     { return relSet.getUnmodifiableSubjectList(obj).toArray(); }
  @Override public <T> T[] toArray(T[] a)                                 { return relSet.getUnmodifiableSubjectList(obj).toArray(a); }

  @Override public boolean add(HDT_SubjType subj)                                  { throw uoe(); }
  @Override public boolean remove(Object o)                                        { throw uoe(); }
  @Override public void clear()                                                    { throw uoe(); }
  @Override public boolean addAll(Collection<? extends HDT_SubjType> c)            { throw uoe(); }
  @Override public boolean addAll(int index, Collection<? extends HDT_SubjType> c) { throw uoe(); }
  @Override public boolean removeAll(Collection<?> c)                              { throw uoe(); }
  @Override public boolean retainAll(Collection<?> c)                              { throw uoe(); }
  @Override public HDT_SubjType set(int index, HDT_SubjType element)               { throw uoe(); }
  @Override public void add(int index, HDT_SubjType subj)                          { throw uoe(); }
  @Override public HDT_SubjType remove(int index)                                  { throw uoe(); }

  public int getOrd(HDT_SubjType subj)   { return relSet.getSubjectOrd(obj, subj); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean contains(Object o)
  {
    return (o instanceof HDT_Record) && (((HDT_Record)o).getType() == relSet.getSubjType()) ?
      relSet.alreadyHasAsObject((HDT_SubjType)o, obj)
    :
      false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean containsAll(Collection<?> c)
  {
    for (Object o : c)
    {
      if ((o instanceof HDT_Record) == false)
        return false;

      if (((HDT_Record)o).getType() != relSet.getSubjType())
        return false;

      HDT_SubjType subj = (HDT_SubjType)o;

      if (relSet.alreadyHasAsSubject(obj, subj) == false) return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public int indexOf(Object o)
  {
    if ((o instanceof HDT_Record) == false) return -1;

    if (((HDT_Record)o).getType() != relSet.getSubjType()) return -1;

    return relSet.getSubjectNdx(obj, (HDT_SubjType)o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    int hashCode = 1;

    for (HDT_SubjType subj : this)
      hashCode = (31 * hashCode) + subj.hashCode();

    return hashCode;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object o)
  {
    if ((o instanceof List) == false) return false;

    List<?> list = (List<?>)o;

    if (list.size() != size()) return false;

    for (int ndx = 0; ndx < list.size(); ndx++)
      if (list.get(ndx) != get(ndx)) return false;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reorder(List<HDT_SubjType> list, boolean modTracking)
  {
    boolean changed = false;

    if (size() != list.size()) throw new NoSuchElementException();

    if (list.stream().allMatch(this::contains) == false)
      throw new NoSuchElementException();

    for (int ndx = 0; ndx < size(); ndx++)
    {
      if (list.contains(get(ndx)) == false) throw new NoSuchElementException();
      if (list.get(ndx) != get(ndx)) changed = true;
    }

    relSet.reorderSubjects(obj, list);
    if (modTracking && changed) obj.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
