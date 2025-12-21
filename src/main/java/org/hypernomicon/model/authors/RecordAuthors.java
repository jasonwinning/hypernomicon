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

package org.hypernomicon.model.authors;

import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Person;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public abstract class RecordAuthors implements Iterable<RecordAuthor>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class AuthorIterator implements Iterator<RecordAuthor>
  {
    private int nextNdx = 0;

    @Override public boolean hasNext() { return nextNdx < size(); }

    @Override public RecordAuthor next()
    {
      if (hasNext()) return get(nextNdx++);
      throw new NoSuchElementException("No more elements in AuthorIterator");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  abstract int size();
  abstract boolean containsPerson(HDT_Person person);
  abstract void resolvePointers() throws HDB_InternalError;
  abstract RecordAuthor get(int ndx);
  abstract void clearNoMod();
  abstract void clear();
  abstract void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException, HDB_InternalError;

  public final boolean isEmpty()                     { return size() == 0; }
  public final Stream<RecordAuthor> stream()         { return iterableToStream(this); }
  final void expire()                                { clearNoMod(); }

  @Override public Iterator<RecordAuthor> iterator() { return new AuthorIterator(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final boolean contains(RecordAuthor author)
  {
    for (RecordAuthor existingAuthor : this)
      if (existingAuthor.equals(author))
        return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
