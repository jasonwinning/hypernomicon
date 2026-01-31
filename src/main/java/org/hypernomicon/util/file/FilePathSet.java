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

import java.io.File;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Predicate;

//---------------------------------------------------------------------------

/**
 * A thread-safe Set implementation for FilePath objects, organized by filename.
 * <p>
 * Internally uses a {@link FilenameMap} with OS-appropriate filename key matching, where each
 * key maps to a concurrent set of FilePath objects sharing that filename. This allows
 * efficient lookups by filename while supporting multiple paths with the same name
 * (in different directories).
 *
 * <h2>Thread Safety</h2>
 * Individual operations ({@link #add}, {@link #remove}, {@link #contains}) are thread-safe
 * and can be called concurrently from multiple threads. The underlying data structures use
 * {@link java.util.concurrent.ConcurrentHashMap} for safe concurrent access.
 * <p>
 * Empty internal buckets are cleaned up after removal to prevent memory leaks. The {@link #add}
 * method uses a retry mechanism to handle the rare race condition where a concurrent
 * {@link #remove} cleans up the bucket during the add operation.
 *
 * <h2>Scalability</h2>
 * This implementation scales well with large datasets:
 * <ul>
 *   <li>O(1) average time for {@link #add}, {@link #remove}, and {@link #contains}</li>
 *   <li>Memory usage is proportional to the number of distinct filenames</li>
 *   <li>Empty buckets are cleaned up immediately, preventing memory leaks during high churn</li>
 *   <li>Operations on different filenames run fully concurrently</li>
 * </ul>
 */
public class FilePathSet implements Set<FilePath>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final FilenameMap<Set<FilePath>> nameToPaths = new FilenameMap<>();

//---------------------------------------------------------------------------

  public FilePathSet() { }

  public FilePathSet(Collection<? extends FilePath> c) { addAll(c); }

//---------------------------------------------------------------------------

  @Override public void clear()                         { nameToPaths.clear(); }
  @Override public Iterator<FilePath> iterator()        { return nameToPaths.values().stream().flatMap(Set::stream).iterator(); }
  @Override public boolean isEmpty()                    { return size() == 0; }
  @Override public boolean retainAll(Collection<?> c)   { return removeIf(Predicate.not(c::contains)); }
  @Override public int size()                           { return nameToPaths.values().stream().map(Set::size).reduce(0, Math::addExact); }
  @Override public boolean containsAll(Collection<?> c) { return c.stream().allMatch(this::contains); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean contains(Object o)
  {
    FilePath filePath;

    switch (o)
    {
      case String   str       -> filePath = new FilePath(str);
      case Path     path      -> filePath = new FilePath(path);
      case File     file      -> filePath = new FilePath(file);
      case FilePath oFilePath -> filePath = oFilePath;

      case null, default -> { return false; }
    }

    Set<FilePath> set = nameToPaths.get(filePath.getNameOnly().toString());

    return (set != null) && set.contains(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Object[] toArray()
  {
    Object[] array = new Object[size()];

    int ndx = 0;

    for (Set<FilePath> pathSet : nameToPaths.values())
      for (FilePath filePath : pathSet)
        array[ndx++] = filePath;

    return array;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  @Override public <T> T[] toArray(T[] a)
  {
    int size = size();

    if (a.length < size)
      a = (T[]) java.lang.reflect.Array.newInstance(a.getClass().getComponentType(), size);

    int ndx = 0;

    for (Set<FilePath> pathSet : nameToPaths.values())
      for (FilePath filePath : pathSet)
        a[ndx++] = (T) filePath;

    if (a.length > size)
      a[size] = null;  // Null terminator to fulfill Collection.toArray contract

    return a;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean add(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
      throw new IllegalArgumentException("Unable to add null or empty path to FilePathSet.");

    String key = filePath.getNameOnly().toString();

    // Retry loop handles the rare race where our set gets cleaned up during add

    while (true)
    {
      Set<FilePath> set = nameToPaths.computeIfAbsent(key, _ -> ConcurrentHashMap.newKeySet());
      boolean added = set.add(filePath);

      // Verify our set is still in the map (concurrent cleanup may have removed it)

      if (nameToPaths.get(key) == set)
        return added;

      // Our set was removed - the add went to a detached set.
      // If the item is already in the current map, we're done (another thread added it)

      if (contains(filePath))
        return false;

      // Otherwise retry with a fresh set
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean remove(Object o)
  {
    if ((o instanceof FilePath) == false) return false;

    FilePath filePath = (FilePath) o;
    String key = filePath.getNameOnly().toString();
    Set<FilePath> set = nameToPaths.get(key);

    if (set == null) return false;

    boolean removed = set.remove(filePath);

    // Clean up empty buckets to prevent memory leaks with high churn

    if (removed && set.isEmpty())
      nameToPaths.remove(key);

    return removed;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean addAll(Collection<? extends FilePath> c)
  {
    boolean changed = false;

    for (FilePath filePath : c)
      if (add(filePath))
        changed = true;

    return changed;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean removeAll(Collection<?> c)
  {
    boolean changed = false;

    for (Object o : c)
      if (remove(o))
        changed = true;

    return changed;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Overridden because the stream-based iterator doesn't support remove(),
   * and we need to use {@link #remove(Object)} for proper empty bucket cleanup.
   */
  @Override public boolean removeIf(Predicate<? super FilePath> filter)
  {
    boolean changed = false;

    for (Set<FilePath> pathSet : nameToPaths.values())
      for (FilePath filePath : List.copyOf(pathSet))
        if (filter.test(filePath) && remove(filePath))
          changed = true;

    return changed;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
