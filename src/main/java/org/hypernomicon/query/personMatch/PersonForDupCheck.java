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

package org.hypernomicon.query.personMatch;

import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Person.PotentialKeySet;
import org.hypernomicon.model.records.HDT_Work;

//---------------------------------------------------------------------------

public class PersonForDupCheck
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  final Author author;
  final String fullLCNameEngChar;

  private final PersonName name;
  private final PotentialKeySet keySet, keySetNoNicknames;

  public PersonForDupCheck(PersonName name)                { this(new Author(name)); }
  public PersonForDupCheck(HDT_Person person)              { this(new Author(person)); }
  public PersonForDupCheck(Author author)                  { this(author, author.getName(), author.fullName(true)); }
  public PersonForDupCheck(Author author, PersonName name) { this(author, name, convertToEnglishChars(name.getFull())); }

//---------------------------------------------------------------------------

  private PersonForDupCheck(Author author, PersonName name, String newFullNameEngChar)
  {
    this.author = author == null ? new Author(name) : author;
    this.name = name;

    keySet            = HDT_Person.makeSearchKeySet(name, true, false);
    keySetNoNicknames = HDT_Person.makeSearchKeySet(name, true, true );

    newFullNameEngChar = removeAllParentheticals(newFullNameEngChar.toLowerCase());

    while (newFullNameEngChar.contains("  "))
      newFullNameEngChar = newFullNameEngChar.replaceAll("  ", " ");

    fullLCNameEngChar = newFullNameEngChar.strip().replaceAll("[.,;]", "");
  }

//---------------------------------------------------------------------------

  public HDT_Person getPerson()         { return nullSwitch(author, null, Author::getPerson); }
  public HDT_Work getWork()             { return nullSwitch(author, null, Author::getWork); }
  public Author getAuthor()             { return author; }
  public PersonName getName()           { return name; }
  public boolean startsWith(String str) { return keySetNoNicknames.startsWith(str.replaceAll("[.,;]", "")); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean matches(PersonForDupCheck person2)
  {
    return fullLCNameEngChar.equals(person2.fullLCNameEngChar) ||
           keySetNoNicknames.isSubsetOf(person2.keySet)        ||
           person2.keySetNoNicknames.isSubsetOf(keySet);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
