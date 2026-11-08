/*
 * Copyright 2015-2026 Jason Winning
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

import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

//---------------------------------------------------------------------------

/**
 * A one-to-many map readable in both directions: each key maps to a set of values, and each value
 * maps to the set of keys it appears under. Mutations are serialized. Reads take no lock, see the
 * sets as they are at the time of the call, and never add entries; a key or value whose set becomes
 * empty is dropped.
 *
 * @param <K> the key type
 * @param <V> the value type
 */
public class BidiOneToManyMap<K, V>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Object lock = new Object();
  private final Map<K, Set<V>> forwardMap = new ConcurrentHashMap<>();
  private final Map<V, Set<K>> reverseMap = new ConcurrentHashMap<>();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()                      { forwardMap.clear(); reverseMap.clear(); }
  public Stream<V> getForwardStream(K key) { return stream(forwardMap, key); }
  public Set<V> getForwardSet(K key)       { return unmodifiableSet(forwardMap, key); }
  public Set<K> getReverseSet(V value)     { return unmodifiableSet(reverseMap, value); }

  public Stream<Entry<K, Set<V>>> getForwardStream() { return forwardMap.entrySet().stream(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addForward(K key, V value)
  {
    synchronized (lock)
    {
      getSet(forwardMap, key  ).add(value);
      getSet(reverseMap, value).add(key  );
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void removeForward(K key, V value)
  {
    synchronized (lock)
    {
      removeFromSet(forwardMap, key  , value);
      removeFromSet(reverseMap, value, key  );
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes a key together with every pair it is part of.
   *
   * @param key the key
   */
  public void removeForwardKey(K key)
  {
    synchronized (lock)
    {
      removeKey(forwardMap, reverseMap, key);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Removes a value together with every pair it is part of.
   *
   * @param value the value
   */
  public void removeReverseKey(V value)
  {
    synchronized (lock)
    {
      removeKey(reverseMap, forwardMap, value);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <K1, V1> void removeKey(Map<K1, Set<V1>> keyMap, Map<V1, Set<K1>> valueMap, K1 key)
  {
    Set<V1> set = keyMap.remove(key);

    if (set != null)
      set.forEach(target -> removeFromSet(valueMap, target, key));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Replaces a value with another one in every pair the old value is part of; the keys stay as they
   * are, and pairs the new value is already part of are kept.
   *
   * @param oldValue the value being replaced
   * @param newValue the value taking its place
   */
  public void replaceValue(V oldValue, V newValue)
  {
    if ((oldValue == null) || (newValue == null) || (oldValue == newValue))
      return;

    synchronized (lock)
    {
      Set<K> keys = reverseMap.remove(oldValue);

      if (keys == null) return;

      keys.forEach(key ->
      {
        Set<V> values = getSet(forwardMap, key);

        values.remove(oldValue);
        values.add   (newValue);
      });

      getSet(reverseMap, newValue).addAll(keys);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <K1, V1> void removeFromSet(Map<K1, Set<V1>> map, K1 key, V1 value)
  {
    Set<V1> set = map.get(key);

    if (set == null) return;

    set.remove(value);

    if (set.isEmpty())
      map.remove(key);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static <K1, V1> Set<V1> getSet(Map<K1, Set<V1>> map, K1 key)
  {
    return map.computeIfAbsent(key, _ -> ConcurrentHashMap.newKeySet());
  }

  private static <K1, V1> Stream<V1> stream(Map<K1, Set<V1>> map, K1 key)
  {
    return nullSwitch(map.get(key), Stream.empty(), Set::stream);
  }

  private static <K1, V1> Set<V1> unmodifiableSet(Map<K1, Set<V1>> map, K1 key)
  {
    return nullSwitch(map.get(key), Collections.emptySet(), Collections::unmodifiableSet);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
