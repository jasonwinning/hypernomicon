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

import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.relations.ObjectGroup;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.Objects;
import java.util.stream.Stream;

//---------------------------------------------------------------------------

public final class AuthorStandalone extends Author implements Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final PersonName name;
  private final HDT_Person person;
  private final Roles roles;
  private final Ternary inFileName;

//---------------------------------------------------------------------------

  public AuthorStandalone(AuthorType type, PersonName name)                { this(name, null, Roles.of(type)); }
  public AuthorStandalone(Roles roles, PersonName name)                    { this(name, null, roles); }
  public AuthorStandalone(Author srcAuthor)                                { this(srcAuthor.getName(), srcAuthor.getPerson(), srcAuthor.getRoles()); }
  public AuthorStandalone(PersonName name, HDT_Person person, Roles roles) { this(name, person, roles, Ternary.Unset); }

//---------------------------------------------------------------------------

  private AuthorStandalone(PersonName name, HDT_Person person, Roles roles, Ternary inFileName)
  {
    this.person = person;
    this.name = person != null ? null : name;

    this.roles      = Objects.requireNonNull(roles);
    this.inFileName = inFileName;
  }

//---------------------------------------------------------------------------

  @Override public PersonName getName   () { return person == null ? name : person.getName(); }
  @Override public HDT_Person getPerson () { return person; }
  @Override public Roles getRoles       () { return roles; }
  @Override public Ternary getInFileName() { return inFileName; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public AuthorStandalone clone()
  { try { return (AuthorStandalone) super.clone(); } catch (CloneNotSupportedException e) { throw newAssertionError(e); }}

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
    result = prime * result + roles.hashCode();

    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (Author.class.isAssignableFrom(obj.getClass()) == false) return false;

    return areEqual(this, (Author) obj, false, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean areEqual(Author author1, Author author2, boolean ignorePersonRecords, boolean ignoreParenthetical)
  {
    if (author1 == author2) return true;
    if ((author1 == null) != (author2 == null)) return false;

    if (author1.getRoles().equals(author2.getRoles()) == false) return false;

    if ((ignorePersonRecords == false) && (author1.getPerson() != author2.getPerson())) return false;

    PersonName name1 = nullSwitch(author1.getName(), PersonName.EMPTY),
               name2 = nullSwitch(author2.getName(), PersonName.EMPTY);

    if (ignoreParenthetical)
      return name1.equalsExceptParenthetical(name2);

    String first1 = author1.getName().getFirst(),
           last1  = author1.getName().getLast (),
           first2 = author2.getName().getFirst(),
           last2  = author2.getName().getLast ();

    return (safeStr(first1).equals(safeStr(first2)) &&
            safeStr(last1 ).equals(safeStr(last2 )));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Stream<Author> getAuthorsFromObjectGroups(Iterable<ObjectGroup> objGroups)
  {
    return iterableToStream(objGroups).map(objGroup -> new AuthorStandalone
    (
      new PersonName(objGroup.getPrimaryStr()),
      objGroup.getPrimary(),
      Roles.of(objGroup),
      objGroup.getValue(tagInFileName).ternary)
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
