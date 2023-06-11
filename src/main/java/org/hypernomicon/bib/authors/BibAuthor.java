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

package org.hypernomicon.bib.authors;

import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;

import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public final class BibAuthor implements Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum AuthorType { author, editor, translator }

//---------------------------------------------------------------------------

  private final PersonName name;
  private final HDT_Person person;
  private final AuthorType type;

//---------------------------------------------------------------------------

  private BibAuthor(AuthorType type, HDT_Person person, PersonName name)
  {
    this.type = type;
    this.person = person;
    this.name = name;
  }

//---------------------------------------------------------------------------

  public BibAuthor(AuthorType type, HDT_Person person) { this(type, person, null); }
  public BibAuthor(AuthorType type, PersonName name)   { this(type, null, name);   }

//---------------------------------------------------------------------------

  public BibAuthor(AuthorType type, Author author)
  {
    this.type = type;
    person = author.getPerson();
    name = person == null ? author.getName() : null;
  }

//---------------------------------------------------------------------------

  public AuthorType getType()   { return type; }
  public String getGiven()      { return getName().getFirst(); }
  public String getFamily()     { return getName().getLast(); }
  public PersonName getName()   { return person == null ? name : person.getName(); }
  public HDT_Person getPerson() { return person; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibAuthor clone()
  { try { return (BibAuthor) super.clone(); } catch (CloneNotSupportedException ex) { throw new AssertionError(ex); }}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;

    String first = "", last = "";

    if (getName() != null)
    {
      first = getName().getFirst();
      last  = getName().getLast();
    }

    result = prime * result + safeStr(last).hashCode();
    result = prime * result + safeStr(first).hashCode();
    result = prime * result + (person == null ? 0 : person.hashCode());
    result = prime * result + (type == null ? 0 : type.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    BibAuthor other = (BibAuthor) obj;

    if (type != other.type) return false;

    if (person != null) return person.equals(other.person);

    if (other.person != null) return false;

    String first = "", last = "", otherFirst = "", otherLast = "";

    if (name != null)
    {
      first = name.getFirst();
      last  = name.getLast();
    }

    if (other.name != null)
    {
      otherFirst = other.name.getFirst();
      otherLast  = other.name.getLast();
    }

    return (safeStr(first).equals(safeStr(otherFirst)) &&
            safeStr(last ).equals(safeStr(otherLast )));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
