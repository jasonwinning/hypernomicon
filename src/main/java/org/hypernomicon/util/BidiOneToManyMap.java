/*
 * Copyright 2015-2025 Jason Winning
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

import java.util.Collections;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

//---------------------------------------------------------------------------

public class BidiOneToManyMap<T>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Map<T, Set<T>> forwardMap = new ConcurrentHashMap<>(),
                               reverseMap = new ConcurrentHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                      { forwardMap.clear(); reverseMap.clear();  }
  public Stream<T> getForwardStream(T key) { return getSet(forwardMap, key).stream(); }

  public Stream<Entry<T, Set<T>>> getForwardStream() { return forwardMap.entrySet().stream(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void addForward(T key, T value)
  {
    getSet(forwardMap, key  ).add(value);
    getSet(reverseMap, value).add(key  );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public synchronized void removeForward(T key, T value)
  {
    getSet(forwardMap, key  ).remove(value);
    getSet(reverseMap, value).remove(key  );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeItem(T item)
  {
    removeKey(forwardMap, reverseMap, item);
    removeKey(reverseMap, forwardMap, item);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeReverseKey(T key)
  {
    removeKey(reverseMap, forwardMap, key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeKey(Map<T, Set<T>> keyMap, Map<T, Set<T>> valueMap, T key)
  {
    if (keyMap.containsKey(key) == false) return;

    keyMap.get(key).removeIf(target ->
    {
      getSet(valueMap, target).remove(key);
      return true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Replaces an old key with a new key in both maps.
   *
   * @param oldItem the old key
   * @param newItem the new key
   */
  public void replaceItem(T oldItem, T newItem)
  {
    if ((oldItem == null) || (newItem == null) || (oldItem == newItem))
      return;

    synchronized (this)
    {
      replaceInMap(forwardMap, reverseMap, oldItem, newItem);
      replaceInMap(reverseMap, forwardMap, oldItem, newItem);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <T> void replaceInMap(Map<T, Set<T>> keyToValueMap, Map<T, Set<T>> valueToKeyMap, T oldKey, T newKey)
  {
    if (keyToValueMap.containsKey(oldKey) == false)
      return;

    Set<T> oldSet = keyToValueMap.remove(oldKey);

    oldSet.forEach(relatedMT ->
    {
      valueToKeyMap.get(relatedMT).remove(oldKey);
      valueToKeyMap.get(relatedMT).add   (newKey);
    });

    keyToValueMap.merge(newKey, oldSet, (newSet, oldSet_) -> { newSet.addAll(oldSet_); return newSet; });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <T> Set<T> getSet(Map<T, Set<T>> map, T key)
  {
    return map.computeIfAbsent(key, _ -> ConcurrentHashMap.newKeySet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<T> getForwardSet(T key)
  {
    return Collections.unmodifiableSet(getSet(forwardMap, key));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Set<T> getReverseSet(T value)
  {
    return Collections.unmodifiableSet(getSet(reverseMap, value));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
