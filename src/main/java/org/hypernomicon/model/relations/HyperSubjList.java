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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import org.hypernomicon.model.records.HDT_Base;

public class HyperSubjList<HDT_SubjType extends HDT_Base, HDT_ObjType extends HDT_Base> implements List<HDT_SubjType>
{
  final RelationSet<HDT_SubjType, HDT_ObjType> relSet;
  final HDT_ObjType obj;

  static final String modErrMsg = "Internal error: An attempt was made to modify a subject list.";
  
  public HyperSubjList(RelationSet<HDT_SubjType, HDT_ObjType> relSet, HDT_ObjType obj)
  {
    this.relSet = relSet;
    this.obj = obj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                                             { return relSet.getSubjectCount(obj); }
  @Override public boolean isEmpty()                                      { return size() == 0; }
  @Override public HDT_SubjType get(int ndx)                              { return relSet.getSubject(obj, ndx); }
  @Override public int lastIndexOf(Object o)                              { return indexOf(o); }
  @Override public List<HDT_SubjType> subList(int fromIndex, int toIndex) { return new HyperSubjSubList<HDT_SubjType, HDT_ObjType>(this, fromIndex, toIndex); }
  @Override public Iterator<HDT_SubjType> iterator()                      { return new HyperSubjIterator<HDT_SubjType, HDT_ObjType>(this); }
  @Override public ListIterator<HDT_SubjType> listIterator()              { return new HyperSubjListIterator<HDT_SubjType, HDT_ObjType>(this, 0); }
  @Override public ListIterator<HDT_SubjType> listIterator(int index)     { return new HyperSubjListIterator<HDT_SubjType, HDT_ObjType>(this, index); }
  @Override public int hashCode()                                         { return super.hashCode(); }

  @Override public boolean add(HDT_SubjType subj)                                  { throw new UnsupportedOperationException(modErrMsg); }
  @Override public boolean remove(Object o)                                        { throw new UnsupportedOperationException(modErrMsg); }
  @Override public void clear()                                                    { throw new UnsupportedOperationException(modErrMsg); }
  @Override public boolean addAll(Collection<? extends HDT_SubjType> c)            { throw new UnsupportedOperationException(modErrMsg); }
  @Override public boolean addAll(int index, Collection<? extends HDT_SubjType> c) { throw new UnsupportedOperationException(modErrMsg); }
  @Override public boolean removeAll(Collection<?> c)                              { throw new UnsupportedOperationException(modErrMsg); }
  @Override public boolean retainAll(Collection<?> c)                              { throw new UnsupportedOperationException(modErrMsg); }
  @Override public HDT_SubjType set(int index, HDT_SubjType element)               { throw new UnsupportedOperationException(modErrMsg); }
  @Override public void add(int index, HDT_SubjType subj)                          { throw new UnsupportedOperationException(modErrMsg); }
  @Override public HDT_SubjType remove(int index)                                  { throw new UnsupportedOperationException(modErrMsg); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean contains(Object o)
  {
    if (o instanceof HDT_Base)
      if (HDT_Base.class.cast(o).getType() == relSet.getSubjType())
        return relSet.alreadyHasAsObject((HDT_SubjType)o, obj);

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Object[] toArray()
  {
    List<HDT_SubjType> subjList = relSet.getUnmodifiableSubjectList(obj);
    
    Object[] array = new Object[subjList.size()];
    
    for (int ndx = 0; ndx < subjList.size(); ndx++)
      array[ndx] = subjList.get(ndx);
    
    return array;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <T> T[] toArray(T[] a)
  {
    List<HDT_SubjType> subjList = relSet.getUnmodifiableSubjectList(obj);
    
    if (a.length < subjList.size())
      a = (T[]) new HDT_Base[subjList.size()];
        
    for (int ndx = 0; ndx < subjList.size(); ndx++)
      a[ndx] = (T) subjList.get(ndx);
    
    if (a.length > subjList.size())
      a[subjList.size()] = null;
    
    return a;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public boolean containsAll(Collection<?> c)
  {
    for (Object o : c)
    {
      if ((o instanceof HDT_Base) == false)
        return false;
      
      if (HDT_Base.class.cast(o).getType() != relSet.getSubjType())
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
    if ((o instanceof HDT_Base) == false) return -1;
    
    if (HDT_Base.class.cast(o).getType() != relSet.getSubjType()) return -1;
    
    return relSet.getSubjectNdx(obj, (HDT_SubjType)o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object o)
  {
    if ((o instanceof List) == false) return false;

    List<?> list = List.class.cast(o);
    
    if (list.size() != size()) return false;
    
    for (int ndx = 0; ndx < list.size(); ndx++)
      if (list.get(ndx) != get(ndx)) return false;
   
    return true;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void reorder(ArrayList<HDT_SubjType> list, boolean modTracking)
  {
    boolean changed = false;
    
    if (size() != list.size()) throw new NoSuchElementException();
    
    for (HDT_SubjType record : list)
      if (contains(record) == false) throw new NoSuchElementException();
    
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
