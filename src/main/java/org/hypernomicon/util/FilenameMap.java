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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.UnaryOperator;

import static org.hypernomicon.util.Util.*;

import org.apache.commons.io.FilenameUtils;

public class FilenameMap<T> implements Map<String, T>
{
  private final Map<String, T> nameToObject = new ConcurrentHashMap<>();
  private final Map<String, List<String>> lowerToList = new ConcurrentHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                                        { return nameToObject.size(); }
  @Override public boolean isEmpty()                                 { return nameToObject.isEmpty(); }
  @Override public boolean containsValue(Object value)               { return nameToObject.containsValue(value); }
  @Override public Set<String> keySet()                              { return nameToObject.keySet(); }
  @Override public Collection<T> values()                            { return nameToObject.values(); }
  @Override public Set<Entry<String, T>> entrySet()                  { return nameToObject.entrySet(); }
  @Override public void clear()                                      { lowerToList.clear(); nameToObject.clear(); }
  @Override public void putAll(Map<? extends String, ? extends T> m) { m.forEach(this::put); }
  @Override public boolean containsKey(Object key)                   { return key instanceof String ? findKey((String) key).length() > 0 : false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String findKey(String query)
  {
    List<String> list = lowerToList.get(query.toLowerCase());

    if (list == null) return "";

    return findFirst(list, entry -> FilenameUtils.equalsNormalizedOnSystem(entry, query), "", UnaryOperator.identity());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T get(Object key)
  {
    if ((key instanceof String) == false) return null;

    String realKey = findKey((String) key);

    return realKey.length() == 0 ? null : nameToObject.get(realKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T put(String key, T value)
  {
    if ((value == null) || (safeStr(key).length() == 0)) return null;

    T oldVal = remove(key);

    lowerToList.get(key.toLowerCase()).add(key);
    nameToObject.put(key, value);

    return oldVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T remove(Object key)
  {
    if ((key instanceof String) == false) return null;

    String strKey = (String) key;
    T oldVal = get(strKey);

    List<String> list = lowerToList.get(strKey.toLowerCase());
    if (list == null)
    {
      list = new ArrayList<>();
      lowerToList.put(strKey.toLowerCase(), list);
    }

    String realKey = findKey(strKey);

    if (realKey.length() > 0)
    {
      list.remove(realKey);
      nameToObject.remove(realKey);
    }

    return oldVal;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
