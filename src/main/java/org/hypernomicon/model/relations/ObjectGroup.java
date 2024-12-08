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

package org.hypernomicon.model.relations;

import java.util.EnumMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public final class ObjectGroup
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HDT_Record primary;
  private final String primaryStr;
  private final Map<Tag, NestedValue> map = new EnumMap<>(Tag.class);

//---------------------------------------------------------------------------

  public ObjectGroup(HDT_Record primary) { this.primary = primary; primaryStr = null;       }
  public ObjectGroup(String primaryStr)  { primary = null;    this.primaryStr = primaryStr; }

//---------------------------------------------------------------------------

  public void addNestedEntry(Tag tag, NestedValue val) { map.put(tag, val); }

  @SuppressWarnings("unchecked")
  public <HDT_T extends HDT_Record> HDT_T getPrimary() { return (HDT_T)primary; }
  public String getPrimaryStr()                        { return primaryStr; }
  public NestedValue getValue(Tag tag)                 { return map.get(tag); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    int result = primary != null ? primary.hashCode() : 0;
    result = 31 * result + (primaryStr != null ? primaryStr.hashCode() : 0);
    result = 31 * result + mapHashCode(map);
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int mapHashCode(Map<Tag, NestedValue> map)
  {
    int result = 0;

    for (Entry<Tag, NestedValue> entry : map.entrySet())
    {
      NestedValue value = entry.getValue();

      if ((value != null) && (value.isEmpty() == false))
        result += entry.getKey().hashCode() * value.hashCode();
    }

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if ((obj == null) || (getClass() != obj.getClass())) return false;

    ObjectGroup other = (ObjectGroup) obj;

    if (primary != other.primary) return false;
    if (safeStr(primaryStr).equals(safeStr(other.primaryStr)) == false) return false;

    return Stream.concat(map.keySet().stream(), other.map.keySet().stream())
                 .distinct()
                 .allMatch(key ->
    {
      NestedValue value1 =       map.get(key),
                  value2 = other.map.get(key);

      return value1 == null ?
        ((value2 == null) || value2.isEmpty())
      :
        (value1.equals(value2) || ((value2 == null) && value1.isEmpty()));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
