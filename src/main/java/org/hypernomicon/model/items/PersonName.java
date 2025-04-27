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

package org.hypernomicon.model.items;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.util.SplitString;

//---------------------------------------------------------------------------

public final class PersonName implements Comparable<PersonName>, Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final PersonName EMPTY = new PersonName("", "");

  private final String first, last;

//---------------------------------------------------------------------------

  public PersonName(String first, String last)
  {
    this.first = stripSafe(first); this.last = stripSafe(last);
  }

  public PersonName(String name)
  {
    name = convertToSingleLine(safeStr(name)).strip();
    while (name.contains("  "))
      name = name.replaceAll("  ", " ");

    if ((name.matches(".*[A-Z].*") == false) || (name.matches(".*[a-z].*") == false))
      name = titleCase(name);

    while (name.matches(".*\\h\\..*"))
      name = name.replaceFirst("\\h\\.", ".");   // remove space before periods

    int ndx = name.indexOf(',');

    if (ndx > 0)
    {
      first = name.substring(ndx + 1).strip();
      last = name.substring(0, ndx).strip();
      return;
    }

    if (ndx == 0)
    {
      first = name.substring(ndx + 1).strip();
      last = "";
      return;
    }

    if (name.matches("[Vv][ao]n\\h.*"))
    {
      first = "";
      last = name.strip();
      return;
    }

    ndx = name.length();
    while (ndx > 0)
    {
      if (name.substring(--ndx).matches("[^A-Za-z][A-Z]\\..*"))  // Parses "John B. X. James"
      {
        ndx = ndx + 3;
        first = name.substring(0, ndx).strip();
        last = name.substring(ndx).strip();
        return;
      }

      if (name.substring(ndx).matches("[^A-Za-z][A-Za-z]+\\..*"))  // Parses "John B. St. James"
      {
        ndx = ndx + 1;
        first = name.substring(0, ndx).strip();
        last = name.substring(ndx).strip();
        return;
      }

      if (name.substring(ndx).matches("[^A-Za-z][Vv][ao]n\\h.*"))
      {
        ndx = ndx + 1;
        first = name.substring(0, ndx).strip();
        last = name.substring(ndx).strip();
        return;
      }
    }

    ndx = name.lastIndexOf(' ');

    if (ndx < 0)
    {
      first = ""; last = name.strip();
    }
    else if (ndx > 0)
    {
      first = name.substring(0, ndx + 1).strip();
      last = name.substring(ndx + 1).strip();
    }
    else
    {
      first = "";
      last = name.substring(ndx + 1).strip();
    }
  }

//---------------------------------------------------------------------------

  public String getFirst()        { return safeStr(first); }
  public String getLast()         { return safeStr(last); }
  public String getLastFirst()    { return (first.length() > 0) && (last.length() > 0) ? (last + ", " + first) : (last + first); }
  public boolean isEmpty()        { return (getLast().length() + getFirst().length()) == 0; }
  public PersonName toLowerCase() { return new PersonName(first.toLowerCase(), last.toLowerCase()); }
  public String getFull()         { return (first + ' ' + last).strip(); }
  public String getSingle()       { return getLast().length() > 0 ? getLast() : getFirst(); }
  public PersonName toEngChar()   { return new PersonName(convertToEnglishChars(first), convertToEnglishChars(last)); }
  public String getSortKey()      { return (last.isEmpty() || first.isEmpty() ? (last + first) : (last + '\u0000' + first)).toLowerCase(); }

  @Override public PersonName clone()
  { try { return (PersonName) super.clone(); } catch (CloneNotSupportedException e) { throw new AssertionError(e); }}

  @Override public int compareTo(PersonName o) { return getSortKey().compareTo(o.getSortKey()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + getFirst().hashCode();
    result = prime * result + getLast().hashCode();
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean equalsExceptParenthetical(PersonName other)
  {
    PersonName this1 = new PersonName(removeFirstParenthetical(getFirst()), getLast());
    PersonName other1 = new PersonName(removeFirstParenthetical(other.getFirst()), other.getLast());

    return this1.equals(other1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;

    return compareTo((PersonName) obj) == 0;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  String getBibName()
  {
    List<String> initialList = new ArrayList<>();

    SplitString splitStr = new SplitString(removeAllParentheticals(getFirst()), ' ');

    while (splitStr.hasNext())
    {
      String nameStr = splitStr.next();
      if (nameStr.isEmpty()) continue;

      if (nameStr.endsWith("."))
      {
        if ((nameStr.length() == 2) && nameStr.equals(nameStr.toUpperCase()))  // true if it is an initial
        {
          initialList.add(nameStr.substring(0, 1));
        }
        else
        {
          nameStr = nameStr + ' ' + splitStr.next();
          initialList.add(nameStr.substring(0, 1));
        }
      }
      else
      {
        initialList.add(nameStr.substring(0, 1));
      }
    }

    if (getLast().isEmpty())
      return getFirst();

    if (initialList.isEmpty())
      return getLast();

    String bibName = getLast() + ", ";

    for (String initial : initialList)
      bibName = bibName + initial + ". ";

    return bibName.strip();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
