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
 * A thread-safe Set implementation for FilePath objects.
 * <p>
 * Backed by a {@link ConcurrentHashMap}-based key set. Thread-safe for concurrent
 * access from multiple threads. {@link FilePath#equals(Object)} and
 * {@link FilePath#hashCode()} handle OS-appropriate path comparison (case folding,
 * real path resolution, etc.).
 */
public class FilePathSet implements Set<FilePath>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final Set<FilePath> inner = ConcurrentHashMap.newKeySet();

//---------------------------------------------------------------------------

  public FilePathSet() { }

  public FilePathSet(Collection<? extends FilePath> c) { addAll(c); }

//---------------------------------------------------------------------------

  @Override public void clear()                         { inner.clear(); }
  @Override public Iterator<FilePath> iterator()        { return inner.iterator(); }
  @Override public boolean isEmpty()                    { return inner.isEmpty(); }
  @Override public int size()                           { return inner.size(); }
  @Override public boolean containsAll(Collection<?> c) { return inner.containsAll(c); }
  @Override public boolean retainAll(Collection<?> c)   { return inner.retainAll(c); }
  @Override public Object[] toArray()                   { return inner.toArray(); }
  @Override public <T> T[] toArray(T[] a)               { return inner.toArray(a); }

  @Override public boolean removeIf(Predicate<? super FilePath> filter) { return inner.removeIf(filter); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean contains(Object o)
  {
    return switch (o)
    {
      case FilePath filePath -> inner.contains(filePath);
      case String   str      -> inner.contains(new FilePath(str));
      case Path     path     -> inner.contains(new FilePath(path));
      case File     file     -> inner.contains(new FilePath(file));

      case null, default -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean add(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
      throw new IllegalArgumentException("Unable to add null or empty path to FilePathSet.");

    return inner.add(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean remove(Object o)
  {
    if ((o instanceof FilePath) == false) return false;

    return inner.remove(o);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean addAll(Collection<? extends FilePath> c)
  {
    return inner.addAll(c);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean removeAll(Collection<?> c)
  {
    return inner.removeAll(c);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
