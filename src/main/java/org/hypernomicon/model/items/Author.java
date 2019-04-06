/*
 * Copyright 2015-2019 Jason Winning
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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.ObjectGroup;

public final class Author implements Cloneable, Comparable<Author>
{
  private final HDT_Person person;
  private final HDT_Work work;
  private final PersonName name, nameEngChar;
  private final boolean isEditor, isTrans;
  private final Ternary inFileName;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author(HDT_Work work, HDT_Person person)
  { this(work, person, null, false, false, Ternary.Unset); }

  public Author(HDT_Work work, PersonName name, boolean isEditor, boolean isTrans, Ternary inFileName)
  { this(work, null, name, isEditor, isTrans, inFileName); }

  public Author(HDT_Person person)
  { this(person.works.isEmpty() ? null : person.works.get(0), person, null, false, false, Ternary.Unset); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private Author(HDT_Work work, HDT_Person person, PersonName name, boolean isEditor, boolean isTrans, Ternary inFileName)
  {
    this.work = work;
    this.person = person;
    this.name = name;
    nameEngChar = name == null ? null : name.toEngChar();
    this.isEditor = isEditor;
    this.isTrans = isTrans;
    this.inFileName = inFileName;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public PersonName getName()                     { return getName(false); }
  public String getLastName()                     { return getLastName(false); }
  public String getLastName(boolean engChar)      { return getName(engChar).getLast(); }
  public String getFirstName()                    { return getFirstName(false); }
  public String getFirstName(boolean engChar)     { return getName(engChar).getFirst(); }
  public String getNameLastFirst(boolean engChar) { return getName(engChar).getLastFirst(); }
  public String getFullName(boolean engChar)      { return getName(engChar).getFull(); }
  public String singleName()                      { return getName().getSingle(); }
  public String getNameLastFirst()                { return getNameLastFirst(false); }
  public HDT_Person getPerson()                   { return person; }
  public HDT_Work getWork()                       { return work; }
  public boolean outOfDate()                      { return work.getAuthors().stream().noneMatch(this::equals); }
  String getBibName()                             { return getName().getBibName(); }
  private PersonName getName(boolean engChar)     { return person == null ? (engChar ? nameEngChar : name) : person.getName(engChar); }
  private String getSortKey()                     { return person == null ? nameEngChar.getSortKey() : person.getSortKey(); }

  @Override public int compareTo(Author o)        { return getSortKey().compareTo(o.getSortKey()); }

  @Override public Author clone()
  { try { return (Author) super.clone(); } catch (CloneNotSupportedException ex) { throw new RuntimeException(ex); }}

  public boolean getIsEditor()   { return person == null ? isEditor   : (work == null ? false         : db.getNestedBoolean(work, person, tagEditor)); }
  public boolean getIsTrans()    { return person == null ? isTrans    : (work == null ? false         : db.getNestedBoolean(work, person, tagTranslator)); }
  public Ternary getInFileName() { return person == null ? inFileName : (work == null ? Ternary.Unset : db.getNestedTernary(work, person, tagInFileName)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((inFileName == null) ? 0 : inFileName.hashCode());
    result = prime * result + (isEditor ? 1231 : 1237);
    result = prime * result + (isTrans ? 1231 : 1237);
    result = prime * result + ((name == null) ? 0 : name.hashCode());
    result = prime * result + ((person == null) ? 0 : person.hashCode());
    result = prime * result + ((work == null) ? 0 : work.hashCode());
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if ((obj == null) || ((obj instanceof Author) == false)) return false;

    Author other = (Author)obj;

    if ((person != other.person) || (work != other.work)) return false;

    if ((person == null) && (other.person == null))
      if (name.equals(other.name) == false)
        return false;

    return (getInFileName() == other.getInFileName()) &&
           (getIsEditor  () == other.getIsEditor  ()) &&
           (getIsTrans   () == other.getIsTrans   ());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean equalsObjGroup(ObjectGroup objGroup)
  {
    if (person != objGroup.getPrimary()) return false;

    if ((person == null) && (name.equalsExceptParenthetical(new PersonName(objGroup.getPrimaryStr())) == false))
      return false;

    return nullSwitch(objGroup.getValue(tagInFileName), true, val -> val.ternary == getInFileName()) &&
           nullSwitch(objGroup.getValue(tagEditor    ), true, val -> val.bool    == getIsEditor  ()) &&
           nullSwitch(objGroup.getValue(tagTranslator), true, val -> val.bool    == getIsTrans   ());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}