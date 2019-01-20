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

package org.hypernomicon.util;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.Map;

import org.hypernomicon.model.records.HDT_Hub;
import org.hypernomicon.model.items.StrongLink;
import org.hypernomicon.model.records.HDT_Base;

import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import com.google.common.collect.Sets;

public class BidiOneToManyRecordMap
{
  private final Map<HDT_Base, Set<HDT_Base>> forwardMap = new ConcurrentHashMap<>(),
                                             reverseMap = new ConcurrentHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear() { forwardMap.clear(); reverseMap.clear(); }

  @SuppressWarnings("unchecked") public <HDT_T extends HDT_Base> Set<HDT_T> getForwardSet(HDT_Base fromRecord) { return (Set<HDT_T>) getSet(forwardMap, fromRecord); }
  @SuppressWarnings("unchecked") public <HDT_T extends HDT_Base> Set<HDT_T> getReverseSet(HDT_Base fromRecord) { return (Set<HDT_T>) getSet(reverseMap, fromRecord); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addForward(HDT_Base fromRecord, HDT_Base toRecord)
  {
    if (toRecord.getType() == hdtHub)
    {
      StrongLink link = ((HDT_Hub) toRecord).getLink();

      nullSwitch(link.getNote    (), note    -> addForward(fromRecord, note   ));
      nullSwitch(link.getLabel   (), label   -> addForward(fromRecord, label  ));
      nullSwitch(link.getDebate  (), debate  -> addForward(fromRecord, debate ));
      nullSwitch(link.getPosition(), pos     -> addForward(fromRecord, pos    ));
      nullSwitch(link.getConcept (), concept -> addForward(fromRecord, concept));
    }
    else if (fromRecord.getType() == hdtHub)
    {
      StrongLink link = ((HDT_Hub) fromRecord).getLink();

      nullSwitch(link.getNote    (), note    -> addForward(note   , toRecord));
      nullSwitch(link.getLabel   (), label   -> addForward(label  , toRecord));
      nullSwitch(link.getDebate  (), debate  -> addForward(debate , toRecord));
      nullSwitch(link.getPosition(), pos     -> addForward(pos    , toRecord));
      nullSwitch(link.getConcept (), concept -> addForward(concept, toRecord));
    }
    else
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

  private void removeForwardKey(HDT_Base key)
  {
    if (forwardMap.containsKey(key) == false) return;

    forwardMap.get(key).removeIf(target ->
    {
      getSet(reverseMap, target).remove(key);
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeReverseKey(HDT_Base key)
  {
    if (reverseMap.containsKey(key) == false) return;

    reverseMap.get(key).removeIf(target ->
    {
      getSet(forwardMap, target).remove(key);
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<HDT_Base> getAllHeads()
  {
    Set<HDT_Base> heads = Sets.newConcurrentHashSet();

    forwardMap.forEach((head, set) ->
    {
      if ((head.getID() != -1) && (set.isEmpty() == false))
        heads.add(head);
    });

    return heads;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
