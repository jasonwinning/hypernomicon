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
  private final boolean isEditor, isTrans;
  private final Ternary inFileName;

//---------------------------------------------------------------------------

  private AuthorStandalone(AuthorType type, HDT_Person person, PersonName name)
  {
    Objects.requireNonNull(type);

    this.person = person;
    this.name = name;

    inFileName = Ternary.Unset;

    switch (type)
    {
      case author:

        isEditor = false;
        isTrans  = false;
        break;

      case editor:

        isEditor = true;
        isTrans  = false;
        break;

      case translator:

        isTrans  = true;
        isEditor = false;
        break;

      default:

        throw new AssertionError("Invalid author type");
    }
  }

//---------------------------------------------------------------------------

  public AuthorStandalone(AuthorType type, PersonName name)   { this(type, null, name);   }

//---------------------------------------------------------------------------

  public AuthorStandalone(Author srcAuthor)
  {
    this(srcAuthor.getName(), srcAuthor.getPerson(), srcAuthor.getIsEditor(), srcAuthor.getIsTrans());
  }

//---------------------------------------------------------------------------

  public AuthorStandalone(PersonName name, HDT_Person person, boolean isEditor, boolean isTrans)
  {
    this(name, person, isEditor, isTrans, Ternary.Unset);
  }

//---------------------------------------------------------------------------

  private AuthorStandalone(PersonName name, HDT_Person person, boolean isEditor, boolean isTrans, Ternary inFileName)
  {
    this.person = person;
    this.name = person != null ? null : name;

    this.isEditor   = isEditor;
    this.isTrans    = isTrans;
    this.inFileName = inFileName;
  }

//---------------------------------------------------------------------------

  @Override public PersonName getName   () { return person == null ? name : person.getName(); }
  @Override public HDT_Person getPerson () { return person; }
  @Override public boolean getIsEditor  () { return isEditor; }
  @Override public boolean getIsTrans   () { return isTrans; }
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
    result = prime * result + (isEditor ? 1231 : 1237);
    result = prime * result + (isTrans ? 1231 : 1237);

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

    if (author1.getIsEditor() != author2.getIsEditor()) return false;
    if (author1.getIsTrans () != author2.getIsTrans ()) return false;

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
      objGroup.getValue(tagEditor    ).bool,
      objGroup.getValue(tagTranslator).bool,
      objGroup.getValue(tagInFileName).ternary)
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
