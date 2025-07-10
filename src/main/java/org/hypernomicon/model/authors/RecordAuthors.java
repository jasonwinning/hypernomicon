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

package org.hypernomicon.model.authors;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.HDI_OfflineBase;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Person;

import static org.hypernomicon.util.Util.*;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Sets;

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
      throw new NoSuchElementException();
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

  public boolean isEmpty()                       { return size() == 0; }
  public Collection<RecordAuthor> asCollection() { return Sets.newLinkedHashSet(this); }
  public List<RecordAuthor> asList()             { return ImmutableList.copyOf(this); }
  public Stream<RecordAuthor> stream()           { return iterableToStream(this); }
  void expire()                                  { clearNoMod(); }

  @Override public Iterator<RecordAuthor> iterator() { return new AuthorIterator(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getShortAuthorsStr(Stream<? extends Author> authors, boolean sort, boolean fullNameIfSingleton, boolean includeEds)
  {
    return getAuthorsStr(authors, ',', true, true, sort, fullNameIfSingleton, includeEds);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getLongAuthorsStr(Stream<? extends Author> authors, boolean fullNameIfSingleton, boolean includeEds)
  {
    return getAuthorsStr(authors, ';', false, false, false, fullNameIfSingleton, includeEds);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAuthorsStr(Stream<? extends Author> authorStream, char delimiter, boolean amp, boolean firstInitials,
                                      boolean sort, boolean fullNameIfSingleton, boolean includeEds)
  {
    List<Author> authors = authorStream.collect(Collectors.toCollection(ArrayList::new));

    if (authors.isEmpty())
      return "";

    String eds = "";

    if (includeEds && authors.stream().allMatch(Author::getIsEditor))
      eds = authors.size() > 1 ? " (Eds.)" : " (Ed.)";

    if (authors.size() == 1)
      return (firstInitials && (fullNameIfSingleton == false) ? authors.get(0).getBibName() : authors.get(0).nameLastFirst()) + eds;

    if (sort)
      authors.sort(null);

    int num = Math.min(6, authors.size());
    String peopleStr = "";

    for (int ndx = 0; ndx < num; ndx++)
    {
      if (ndx != 0)
      {
        peopleStr = peopleStr + delimiter + ' ';

        if ((ndx == (authors.size() - 1)) && amp)
          peopleStr = peopleStr.strip() + " & ";
      }

      peopleStr = peopleStr + (firstInitials ? authors.get(ndx).getBibName() : authors.get(ndx).nameLastFirst());
    }

    if (num < authors.size())
      peopleStr = peopleStr + " et al.";

    return peopleStr + eds;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
