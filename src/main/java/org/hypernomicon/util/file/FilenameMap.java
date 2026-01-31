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

package org.hypernomicon.util.file;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.stream.Collectors;

//---------------------------------------------------------------------------

/**
 * A thread-safe ConcurrentMap implementation with OS-appropriate filename key lookup.
 * <p>
 * Keys are filenames (strings) that are matched using the configured key normalization
 * strategy. By default, the system's native filename comparison rules are used:
 * on Windows, "File.txt" and "FILE.TXT" are considered equivalent; on Unix, they are distinct.
 * <p>
 * Keys in the map are stored in normalized form. When you call {@link #keySet()},
 * {@link #entrySet()}, or iterate over the map, keys will be returned as normalized strings
 * (e.g., lowercase on case-insensitive systems).
 *
 * <h2>Concurrency Model</h2>
 * This implementation delegates to {@link ConcurrentHashMap} for thread safety.
 * All {@link ConcurrentMap} atomic operations (putIfAbsent, remove, replace, compute, merge)
 * are fully supported.
 *
 * <h2>Null Handling</h2>
 * This map does not permit null keys or null values. Mutating operations ({@code put},
 * {@code compute}, {@code merge}, {@code replace}) throw {@link NullPointerException}
 * for null keys or values. Query operations ({@code get}, {@code containsKey}, {@code remove})
 * return null or false for null keys, consistent with {@link ConcurrentHashMap}.
 *
 * <h2>View Methods</h2>
 * The {@link #keySet()}, {@link #values()}, and {@link #entrySet()} methods return
 * snapshot copies, not live views. Modifications to the map after calling these methods
 * will not be reflected in the returned collections.
 *
 * <h2>Testability</h2>
 * The key normalization strategy can be customized via the {@link #FilenameMap(FilenameRules)}
 * constructor. This allows unit tests to use deterministic rules (e.g., {@link FilenameRules#WINDOWS_HEURISTIC})
 * instead of the filesystem-probed default.
 *
 * @param <T> the type of mapped values
 */
public class FilenameMap<T> implements ConcurrentMap<String, T>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final ConcurrentHashMap<String, T> entries = new ConcurrentHashMap<>();
  private final FilenameRules rules;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates a new FilenameMap using filesystem-probed filename comparison rules.
   * <p>
   * The comparison rules are determined by {@link FilenameRules}, which detects actual
   * filesystem behavior (case sensitivity, Unicode normalization, etc.) rather than
   * relying on OS-based heuristics. This correctly handles edge cases like case-sensitive
   * APFS volumes on macOS.
   */
  public FilenameMap()
  {
    this(FilenameRules.current());
  }

//---------------------------------------------------------------------------

  /**
   * Creates a new FilenameMap with the specified filename equivalence rules.
   * <p>
   * This constructor allows creating a map with specific rules independent of the
   * global {@link FilenameRules} state. Useful for testing or handling files from
   * multiple volumes with different rules.
   *
   * @param rules the filename equivalence rules to use
   */
  FilenameMap(FilenameRules rules)
  {
    this.rules = rules;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int size()                                        { return entries.size(); }
  @Override public boolean isEmpty()                                 { return entries.isEmpty(); }
  @Override public void clear()                                      { entries.clear(); }
  @Override public Set<String> keySet()                              { return Set.copyOf(entries.keySet()); }
  @Override public Collection<T> values()                            { return List.copyOf(entries.values()); }
  @Override public boolean containsValue(Object value)               { return (value != null) && entries.containsValue(value); }
  @Override public void putAll(Map<? extends String, ? extends T> m) { m.forEach(this::put); }

//---------------------------------------------------------------------------

  @Override public Set<Map.Entry<String, T>> entrySet()
  {
    return entries.entrySet().stream()
      .map(e -> Map.entry(e.getKey(), e.getValue()))
      .collect(Collectors.toUnmodifiableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean containsKey(Object key)
  {
    return (key instanceof String strKey) && entries.containsKey(rules.normalize(strKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T get(Object key)
  {
    return (key instanceof String strKey) ? entries.get(rules.normalize(strKey)) : null;
  }

//---------------------------------------------------------------------------

  @Override public T getOrDefault(Object key, T defaultValue)
  {
    return (key instanceof String strKey) ? entries.getOrDefault(rules.normalize(strKey), defaultValue) : defaultValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T put(String key, T value)
  {
    Objects.requireNonNull(key, "key");

    return entries.put(rules.normalize(key), value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T putIfAbsent(String key, T value)
  {
    Objects.requireNonNull(key, "key");

    return entries.putIfAbsent(rules.normalize(key), value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T computeIfAbsent(String key, Function<? super String, ? extends T> mappingFunction)
  {
    Objects.requireNonNull(key, "key");

    return entries.computeIfAbsent(rules.normalize(key), mappingFunction);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T computeIfPresent(String key, BiFunction<? super String, ? super T, ? extends T> remappingFunction)
  {
    Objects.requireNonNull(key, "key");

    return entries.computeIfPresent(rules.normalize(key), remappingFunction);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T compute(String key, BiFunction<? super String, ? super T, ? extends T> remappingFunction)
  {
    Objects.requireNonNull(key, "key");

    return entries.compute(rules.normalize(key), remappingFunction);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T merge(String key, T value, BiFunction<? super T, ? super T, ? extends T> remappingFunction)
  {
    Objects.requireNonNull(key, "key");

    return entries.merge(rules.normalize(key), value, remappingFunction);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public T remove(Object key)
  {
    return (key instanceof String strKey) ? entries.remove(rules.normalize(strKey)) : null;
  }

//---------------------------------------------------------------------------

  @Override public boolean remove(Object key, Object value)
  {
    return (key instanceof String strKey) && (value != null) && entries.remove(rules.normalize(strKey), value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean replace(String key, T oldValue, T newValue)
  {
    Objects.requireNonNull(key, "key");

    return entries.replace(rules.normalize(key), oldValue, newValue);
  }

//---------------------------------------------------------------------------

  @Override public T replace(String key, T value)
  {
    Objects.requireNonNull(key, "key");

    return entries.replace(rules.normalize(key), value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
