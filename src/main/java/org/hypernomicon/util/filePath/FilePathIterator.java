/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.util.filePath;

import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;

import org.hypernomicon.util.FilenameMap;

class FilePathIterator implements Iterator<FilePath>
{
  private final Iterator<Set<FilePath>> nameIterator;
  private Iterator<FilePath> pathIterator;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FilePathIterator(FilenameMap<Set<FilePath>> nameToPaths)
  {
    nameIterator = nameToPaths.values().iterator();
    pathIterator = nameIterator.hasNext() ? nameIterator.next().iterator() : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean hasNext()
  {
    if ((pathIterator != null) && pathIterator.hasNext()) return true;

    while (nameIterator.hasNext())
    {
      pathIterator = nameIterator.next().iterator();
      if (pathIterator.hasNext()) return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public FilePath next()
  {
    if (hasNext()) return pathIterator.next();

    throw new NoSuchElementException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void remove()
  {
    if (pathIterator == null) throw new IllegalStateException();

    pathIterator.remove();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
