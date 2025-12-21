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

package org.hypernomicon.bib.authors;

import java.util.*;

import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.util.StringUtil.*;

import com.google.common.collect.Iterators;

import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.authors.Author.AuthorType;
import org.hypernomicon.model.authors.AuthorStandalone;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.SplitString;

//---------------------------------------------------------------------------

public class BibAuthorsStandalone extends BibAuthors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<Author> authors = new ArrayList<>();
  private String oneLiner; // Sometimes all the authors appear in one line, in various formats and with various delimiting characters

//---------------------------------------------------------------------------

  @Override public boolean isEmpty()  { return authors.isEmpty() && strNullOrBlank(oneLiner); }

  public void add(Author author)      { authors.add(author); }
  public void setOneLiner(String str) { oneLiner = convertToSingleLine(safeStr(str)).strip(); }

  public final void add(AuthorType authorType, PersonName name) { add(new AuthorStandalone(authorType, name)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void clear()
  {
    authors.clear();
    oneLiner = "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr()
  {
    return authors.isEmpty() && strNotNullOrEmpty(oneLiner) ? oneLiner : super.getStr();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(AuthorType authorType)
  {
    return (authorType == author) && authors.isEmpty() && strNotNullOrEmpty(oneLiner) ?
      oneLiner
    :
      super.getStr(authorType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<Author> getOneLinerAsList()
  {
    oneLiner = safeStr(oneLiner);

    List<Author> list = new ArrayList<>();

    if (oneLiner.length() > 1)
      new SplitString(oneLiner, ';').forEach(authorStr -> list.add(new AuthorStandalone(author, new PersonName(authorStr))));

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<Author> iterator()
  {
    return authors.isEmpty() ?
      getOneLinerAsList().iterator()
    :
      Iterators.unmodifiableIterator(authors.iterator());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
