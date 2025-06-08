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

package org.hypernomicon.bib.authors;

import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;

import static org.hypernomicon.util.Util.*;

import java.util.Objects;

import org.hypernomicon.bib.data.BibField.BibFieldEnum;

//---------------------------------------------------------------------------

public final class BibAuthor implements Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum AuthorType
  {
    author,
    editor,
    translator;

    public static AuthorType fromBibFieldEnum(BibFieldEnum bibFieldEnum)
    {
      return switch (bibFieldEnum)
      {
        case bfAuthors     -> author;
        case bfEditors     -> editor;
        case bfTranslators -> translator;

        default -> throw new IllegalArgumentException("Unexpected value: " + bibFieldEnum);
      };
    }
  }

//---------------------------------------------------------------------------

  private final PersonName name;
  private final HDT_Person person;
  private final boolean isEditor, isTrans;

//---------------------------------------------------------------------------

  private BibAuthor(AuthorType type, HDT_Person person, PersonName name)
  {
    Objects.requireNonNull(type);

    this.person = person;
    this.name = name;

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

  public BibAuthor(AuthorType type, HDT_Person person) { this(type, person, null); }
  public BibAuthor(AuthorType type, PersonName name)   { this(type, null, name);   }

//---------------------------------------------------------------------------

  public BibAuthor(Author author)
  {
    this(author.getName(), author.getPerson(), author.getIsEditor(), author.getIsTrans());
  }

//---------------------------------------------------------------------------

  public BibAuthor(PersonName name, HDT_Person person, boolean isEditor, boolean isTrans)
  {
    this.person = person;
    this.name = person != null ? null : name;

    this.isEditor = isEditor;
    this.isTrans = isTrans;
  }

//---------------------------------------------------------------------------

  public String getGiven()      { return getName().getFirst(); }
  public String getFamily()     { return getName().getLast(); }
  public PersonName getName()   { return person == null ? name : person.getName(); }
  public HDT_Person getPerson() { return person; }
  public boolean getIsAuthor()  { return (isEditor == false) && (isTrans == false); }
  public boolean getIsEditor()  { return isEditor; }
  public boolean getIsTrans ()  { return isTrans; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibAuthor clone()
  { try { return (BibAuthor) super.clone(); } catch (CloneNotSupportedException e) { throw newAssertionError(e); }}

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
    if (getClass() != obj.getClass()) return false;

    return areEqual(this, (BibAuthor) obj, false, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean areEqual(BibAuthor bibAuthor1, BibAuthor bibAuthor2, boolean ignorePersonRecords, boolean ignoreParenthetical)
  {
    if (bibAuthor1 == bibAuthor2) return true;
    if ((bibAuthor1 == null) != (bibAuthor2 == null)) return false;

    if (bibAuthor1.isEditor != bibAuthor2.isEditor) return false;
    if (bibAuthor1.isTrans  != bibAuthor2.isTrans ) return false;

    if ((ignorePersonRecords == false) && (bibAuthor1.person != bibAuthor2.person)) return false;

    PersonName name1 = nullSwitch(bibAuthor1.getName(), PersonName.EMPTY),
               name2 = nullSwitch(bibAuthor2.getName(), PersonName.EMPTY);

    if (ignoreParenthetical)
      return name1.equalsExceptParenthetical(name2);

    String first1 = bibAuthor1.getName().getFirst(),
           last1  = bibAuthor1.getName().getLast (),
           first2 = bibAuthor2.getName().getFirst(),
           last2  = bibAuthor2.getName().getLast ();

    return (safeStr(first1).equals(safeStr(first2)) &&
            safeStr(last1 ).equals(safeStr(last2 )));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
