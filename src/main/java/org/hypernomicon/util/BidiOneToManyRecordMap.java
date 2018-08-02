/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.util;

import static org.hypernomicon.model.records.HDT_RecordType.*;

import static java.util.Objects.*;

import java.util.Iterator;
import java.util.Map;

import org.hypernomicon.model.records.HDT_Hub;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.HDT_Base;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.collect.Sets;

public class BidiOneToManyRecordMap
{
  private Map<HDT_Base, Set<HDT_Base>> forwardMap, reverseMap;
  
  public BidiOneToManyRecordMap()
  {
    forwardMap = new ConcurrentHashMap<>();
    reverseMap = new ConcurrentHashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear() { forwardMap.clear(); reverseMap.clear(); }
  
  @SuppressWarnings("unchecked") public <HDT_T extends HDT_Base> Set<HDT_T> getForwardSet(HDT_Base fromRecord) { return (Set<HDT_T>) getSet(forwardMap, fromRecord); }
  @SuppressWarnings("unchecked") public <HDT_T extends HDT_Base> Set<HDT_T> getReverseSet(HDT_Base fromRecord) { return (Set<HDT_T>) getSet(reverseMap, fromRecord); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void addForward(HDT_Base fromRecord, HDT_Base toRecord)
  {
    if (fromRecord.getType() == hdtHub)
    {
      StrongLink link = ((HDT_Hub) fromRecord).getLink();
      
      if (nonNull(link.getNote()))     addForwardMapping(link.getNote(), toRecord);
      if (nonNull(link.getLabel()))    addForwardMapping(link.getLabel(), toRecord);
      if (nonNull(link.getDebate()))   addForwardMapping(link.getDebate(), toRecord);
      if (nonNull(link.getPosition())) addForwardMapping(link.getPosition(), toRecord);
      if (nonNull(link.getConcept()))  addForwardMapping(link.getConcept(), toRecord);
    }
          
    addForwardMapping(fromRecord, toRecord);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addForwardMapping(HDT_Base fromRecord, HDT_Base toRecord)
  {
    getSet(forwardMap, fromRecord).add(toRecord);
    getSet(reverseMap, toRecord).add(fromRecord);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void removeForward(HDT_Base fromRecord, HDT_Base toRecord)
  {
    getSet(forwardMap, fromRecord).remove(toRecord);
    getSet(reverseMap, toRecord).remove(fromRecord);
  }
    
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  private Set<HDT_Base> getSet(Map<HDT_Base, Set<HDT_Base>> map1, HDT_Base record1)
  {
    if (map1.containsKey(record1)) return map1.get(record1);
    
    Set<HDT_Base> set = Sets.newConcurrentHashSet();
    
    map1.put(record1, set);
    return set;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeRecord(HDT_Base record)
  {
    removeForwardKey(record);
    removeReverseKey(record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeForwardKey(HDT_Base key)
  {
    if (forwardMap.containsKey(key) == false) return;
    
    Iterator<HDT_Base> it = forwardMap.get(key).iterator();
    
    while (it.hasNext())
    {
      HDT_Base target = it.next();
      getSet(reverseMap, target).remove(key);
      it.remove();
    }    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeReverseKey(HDT_Base key)
  {
    if (reverseMap.containsKey(key) == false) return;
          
    Iterator<HDT_Base> it = reverseMap.get(key).iterator();
    
    while (it.hasNext())
    {
      HDT_Base target = it.next();
      getSet(forwardMap, target).remove(key);
      it.remove();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Base> getAllHeads()
  {
    Set<HDT_Base> recordSet = Sets.newConcurrentHashSet();
    
    forwardMap.entrySet().forEach(entry ->
    {
      HDT_Base head = entry.getKey();
      
      if (head.getID() != -1)
        if (entry.getValue().isEmpty() == false)
          recordSet.add(head);
    });
    
    return recordSet;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
