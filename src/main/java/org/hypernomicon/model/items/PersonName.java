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

package org.hypernomicon.model.items;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.util.SplitString;

//---------------------------------------------------------------------------

/**
 * A person's name in two parts, first and last, each kept in canonical composition (NFC) form.
 *
 * <h2>When two names count as the same</h2>
 *
 * There are four levels, each with its own purpose. The first three are successively looser: names
 * that are the same at one of them are the same at the next.
 * <ol>
 *   <li><b>Exactly as written</b>: the parts compared as strings ({@link #getFirst()},
 *       {@link #getLast()}, {@link #getLastFirst()}). For telling whether a name was changed at all,
 *       as {@code AuthorStandalone.equals} and {@code RecordAuthor.equalsObjGroup} do.</li>
 *   <li><b>Equal, ignoring case</b>: {@link #equals}, {@link #hashCode} and the natural order
 *       ({@link #compareTo}) all go through {@link #getSortKey()}: the last part, then the first, in
 *       lower case. A name with only one part has that part as its whole key, so it equals the same
 *       text in the other part ("Plato" as a first name equals "Plato" as a last name). For sorting,
 *       and for authors kept in collections.</li>
 *   <li><b>The same name</b>: {@link #sameNameKey()} also ignores diacritics and removes every
 *       parenthetical from both parts. Names with equal keys are treated as one name wherever the
 *       application decides that without asking: when it consolidates an author list
 *       ({@code BibAuthors.normalizeAuthors}) and when it finds the person record for a name
 *       ({@code HDT_Person.lookUpByName}). It is a key for equality, not an order, so
 *       {@link #compareTo} does not use it.</li>
 *   <li><b>Similar names</b>: the duplicate check in {@code org.hypernomicon.query.personMatch}
 *       ({@code PersonForDupCheck}, {@code NameMatcher}) allows for initials, omitted given names and
 *       a nickname in parentheses. It only suggests matches for the user to confirm.</li>
 * </ol>
 * {@link #equalsExceptParenthetical} lies between the second and third levels and serves one purpose:
 * telling whether a reference manager entry's authors changed since the last sync.
 */
public final class PersonName implements Comparable<PersonName>, Cloneable
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final PersonName EMPTY = new PersonName("", "");

  private final String first, last;

  private static final Pattern SINGLE_INITIAL_PATTERN       = Pattern.compile("\\P{L}\\p{Lu}\\..*", Pattern.UNICODE_CHARACTER_CLASS),
                               FINAL_INITIAL_PATTERN        = Pattern.compile("^(?<prefix>.*\\P{L})(?<final>\\p{L}(\\.|$))$", Pattern.UNICODE_CHARACTER_CLASS),
                               ABBREVIATION_PATTERN         = Pattern.compile("^(.*?\\P{L})(\\p{L}+\\..*)", Pattern.UNICODE_CHARACTER_CLASS),
                               START_PARTICLE_PATTERN       = Pattern.compile("[Vv][ao]n\\h.*"),
                               MID_PARTICLE_PATTERN         = Pattern.compile("\\P{L}[Vv][ao]n\\h.*", Pattern.UNICODE_CHARACTER_CLASS),
                               NORMALIZE_WHITESPACE_PATTERN = Pattern.compile("\\s+"),
                               TRAILING_COMMA_PATTERN       = Pattern.compile("[,\\h]+$"),
                               SPACE_BEFORE_DOT_PATTERN     = Pattern.compile("\\h+\\."),
                               MULTI_DOTS_PATTERN           = Pattern.compile("\\.{2,}");

//  Todo: Maybe recognize more particles
//
//  private static final Set<String> SURNAME_PARTICLES = Set.of
//  (
//    "de", "del", "della", "di", "la", "le", "van", "von", "der", "den",
//    "ter", "ten", "bin", "al", "da", "dos"
//  );

//---------------------------------------------------------------------------

  // Names are kept in canonical composition (NFC) form like all bibliographic text (see BibData.setStr):
  // they are compared as strings when deciding whether a reference manager entry's authors changed, and
  // the parsing below looks for letters, which a combining mark is not.

  public PersonName(String first, String last)
  {
    this.first = toNFC(stripSafe(first)); this.last = toNFC(stripSafe(last));
  }

  public PersonName(String name)
  {
    name = toNFC(convertToSingleLine(safeStr(name)).strip());

    name = NORMALIZE_WHITESPACE_PATTERN.matcher(name).replaceAll(" ");

    if (lacksMixedCase(name))
      name = titleCase(name);

    // Get rid of spaces before periods and repeat periods

    name = SPACE_BEFORE_DOT_PATTERN.matcher(name).replaceAll(".");
    name = MULTI_DOTS_PATTERN      .matcher(name).replaceAll(".");

    // A trailing comma is never part of the name; without this, "Dumas, Alexandre," would
    // yield a first name of "Alexandre," because the split below keeps everything after the
    // first comma. Sources that terminate names this way include MARC-derived records and
    // hand-typed semicolon-delimited author lists.

    name = TRAILING_COMMA_PATTERN.matcher(name).replaceAll("");

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

    if (START_PARTICLE_PATTERN.matcher(name).matches())
    {
      first = "";
      last = name.strip();
      return;
    }

    Matcher m = FINAL_INITIAL_PATTERN.matcher(name);
    if (m.matches())
    {
      first = m.group("prefix").strip();
      last = m.group("final").strip();
      return;
    }

    ndx = name.length();
    while (ndx > 0)
    {
      if (SINGLE_INITIAL_PATTERN.matcher(name.substring(--ndx)).matches())  // Parses "John B. X. James"
      {
        ndx = ndx + 3;
        first = name.substring(0, ndx).strip();
        last = name.substring(ndx).strip();
        return;
      }

      if (ABBREVIATION_PATTERN.matcher(name.substring(ndx)).matches())  // Parses "John B. St. James"
      {
        ndx = ndx + 1;
        first = name.substring(0, ndx).strip();
        last = name.substring(ndx).strip();
        return;
      }

      if (MID_PARTICLE_PATTERN.matcher(name.substring(ndx)).matches())
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
  public PersonName toLowerCase() { return new PersonName(first.toLowerCase(Locale.ROOT), last.toLowerCase(Locale.ROOT)); }
  public String getFull()         { return (first + ' ' + last).strip(); }
  public String getSingle()       { return getLast().length() > 0 ? getLast() : getFirst(); }
  public PersonName toEngChar()   { return new PersonName(convertToEnglishChars(first), convertToEnglishChars(last)); }
  public String getSortKey()      { return (last.isEmpty() || first.isEmpty() ? (last + first) : (last + '\u0000' + first)).toLowerCase(Locale.ROOT); }

  @Override public PersonName clone()
  { try { return (PersonName) super.clone(); } catch (CloneNotSupportedException e) { throw newAssertionError(e); }}

  @Override public int compareTo(PersonName o) { return getSortKey().compareTo(o.getSortKey()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    return getSortKey().hashCode();  // equals compares sort keys, so names it finds equal must hash alike
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether the names are {@linkplain #equals equal} once the first parenthetical of each first name
   * is removed. Used to tell whether a reference manager entry's authors changed since the last sync:
   * parentheticals are removed from first names when authors are sent, so they must not count as a
   * change. It removes only the first one, though sending removes them all.
   */
  public boolean equalsExceptParenthetical(PersonName other)
  {
    PersonName this1  = new PersonName(removeFirstParenthetical(getFirst()), getLast()),
               other1 = new PersonName(removeFirstParenthetical(other.getFirst()), other.getLast());

    return this1.equals(other1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The key that decides whether two names are the same name (see the class description): the
   * {@linkplain #getSortKey() sort key} of this name in English characters, with every parenthetical
   * removed from both parts whatever it holds. Compare keys with {@code equals}, or keep them in a
   * hash set.
   */
  public String sameNameKey()
  {
    return new PersonName(removeAllParentheticals(first), removeAllParentheticals(last)).toEngChar().getSortKey();
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

  public String getBibName()
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
