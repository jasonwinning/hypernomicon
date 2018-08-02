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

import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import com.google.common.collect.Sets;
import org.hypernomicon.model.items.MainText;

public class BidiOneToManyMainTextMap
{
  private Map<MainText, Set<MainText>> forwardMap, reverseMap;
  
  public BidiOneToManyMainTextMap()
  {
    forwardMap = new ConcurrentHashMap<>();
    reverseMap = new ConcurrentHashMap<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                                       { forwardMap.clear(); reverseMap.clear(); } 
  public Set<MainText> getForwardSet(MainText fromMainText) { return getSet(forwardMap, fromMainText); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void addForward(MainText fromMainText, MainText toMainText)
  {
    getSet(forwardMap, fromMainText).add(toMainText);
    getSet(reverseMap, toMainText).add(fromMainText);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public void removeForward(MainText fromMainText, MainText toMainText)
  {
    getSet(forwardMap, fromMainText).remove(toMainText);
    getSet(reverseMap, toMainText).remove(fromMainText);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void replaceItem(MainText oldItem, MainText newItem)
  {
    if ((oldItem == null) || (newItem == null)) return;
    if (oldItem == newItem) return;
    
    Set<MainText> oldSet, newSet;
    
    if (forwardMap.containsKey(oldItem))
    {
      oldSet = forwardMap.remove(oldItem);
      
      oldSet.forEach(mt ->
      {
        reverseMap.get(mt).remove(oldItem);
        reverseMap.get(mt).add(newItem);
      });
      
      if (forwardMap.containsKey(newItem))
      {
        newSet = forwardMap.get(newItem);
        newSet.addAll(oldSet);
      }
      else
        forwardMap.put(newItem, oldSet);
    }
    
    if (reverseMap.containsKey(oldItem))
    {
      oldSet = reverseMap.remove(oldItem);

      oldSet.forEach(mt ->
      {
        forwardMap.get(mt).remove(oldItem);
        forwardMap.get(mt).add(newItem);
      });
      
      if (reverseMap.containsKey(newItem))
      {
        newSet = reverseMap.get(newItem);
        newSet.addAll(oldSet);
      }
      else
        reverseMap.put(newItem, oldSet);
    }
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  private Set<MainText> getSet(Map<MainText, Set<MainText>> map1, MainText mainText1)
  {
    if (map1.containsKey(mainText1)) return map1.get(mainText1);
    
    Set<MainText> set = Sets.newConcurrentHashSet();
    
    map1.put(mainText1, set);
    return set;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
