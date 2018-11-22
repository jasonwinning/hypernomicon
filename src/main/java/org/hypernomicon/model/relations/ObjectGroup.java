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

package org.hypernomicon.model.relations;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Base;

public final class ObjectGroup
{
  private final HDT_Base primary;
  private final String primaryStr;
  private final Map<Tag, NestedValue> map = new HashMap<>(); 
  
  public ObjectGroup(HDT_Base primary)  { this.primary = primary; this.primaryStr = null;       }
  public ObjectGroup(String primaryStr) { this.primary = null;    this.primaryStr = primaryStr; }
  
  public final void addNestedEntry(Tag tag, NestedValue val) { map.put(tag, val); }
  
  @SuppressWarnings("unchecked")
  public final <HDT_T extends HDT_Base> HDT_T getPrimary()   { return (HDT_T)primary; }
  public final String getPrimaryStr()                        { return primaryStr; }
  public final NestedValue getValue(Tag tag)                 { return map.get(tag); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (map == null ? 0 : map.hashCode());
    result = prime * result + (primary == null ? 0 : primary.hashCode());
    result = prime * result + (primaryStr == null ? 0 : primaryStr.hashCode());
    return result;
  }
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  @Override public final boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    
    ObjectGroup other = (ObjectGroup) obj;
    
    if (primary != other.primary) return false;
    if (safeStr(primaryStr).equals(safeStr(other.primaryStr)) == false) return false;
    
    for (Entry<Tag, NestedValue> entry : map.entrySet())
    {
      NestedValue otherVal = other.map.get(entry.getKey());
      
      if (otherVal == null) 
      {
        if (entry.getValue().isEmpty() == false) return false;
      }
      else
      {
        if (otherVal.equals(entry.getValue()) == false) return false;
      }
    }
    
    for (Entry<Tag, NestedValue> entry : other.map.entrySet())
    {
      NestedValue val = map.get(entry.getKey());
      
      if (val == null) 
      {
        if (entry.getValue().isEmpty() == false) return false;
      }
      else
      {
        if (val.equals(entry.getValue()) == false) return false;
      }
    }
    
    return true;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}