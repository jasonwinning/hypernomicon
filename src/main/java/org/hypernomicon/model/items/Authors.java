/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.model.items;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.hypernomicon.model.records.HDT_Person;

import com.google.common.collect.Sets;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Tag;

public abstract class Authors implements Iterable<Author>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final class AuthorIterator implements Iterator<Author>
  {
    private int nextNdx = 0;

    @Override public boolean hasNext() { return nextNdx < size(); }

    @Override public Author next()
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
  abstract Author get(int ndx);
  abstract void clearNoMod();
  abstract void clear();
  abstract void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException, HDB_InternalError;

  public boolean isEmpty()                     { return size() == 0; }
  public Collection<Author> asCollection()     { return Sets.newLinkedHashSet(this); }
  public Stream<Author> stream()               { return StreamSupport.stream(spliterator(), false); }
  void expire()                                { clearNoMod(); }

  @Override public Iterator<Author> iterator() { return new AuthorIterator(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getShortAuthorsStr(Stream<Author> authors, boolean sort, boolean fullNameIfSingleton, boolean includeEds)
  {
    return getAuthorsStr(authors, ',', true, true, sort, fullNameIfSingleton, includeEds);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getLongAuthorsStr(Stream<Author> authors, boolean fullNameIfSingleton, boolean includeEds)
  {
    return getAuthorsStr(authors, ';', false, false, false, fullNameIfSingleton, includeEds);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAuthorsStr(Stream<Author> authorStream, char delimiter, boolean amp, boolean firstInitials,
                                      boolean sort, boolean fullNameIfSingleton, boolean includeEds)
  {
    List<Author> authors = authorStream.collect(Collectors.toCollection(ArrayList::new));

    if (authors.isEmpty())
      return "";

    String eds = "";

    if (includeEds && authors.stream().allMatch(Author::getIsEditor))
      eds = authors.size() > 1 ? " (Eds.)" : " (Ed.)";

    if (authors.size() == 1)
      return (firstInitials && (fullNameIfSingleton == false) ? authors.get(0).getBibName() : authors.get(0).getNameLastFirst()) + eds;

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
          peopleStr = peopleStr.trim() + " & ";
      }

      peopleStr = peopleStr + (firstInitials ? authors.get(ndx).getBibName() : authors.get(ndx).getNameLastFirst());
    }

    if (num < authors.size())
      peopleStr = peopleStr + " et al.";

    return peopleStr + eds;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
