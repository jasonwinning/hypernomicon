/*
 * Copyright 2015-2018 Jason Winning
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
import static java.util.Objects.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;

public final class Author implements Cloneable
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
    this.nameEngChar = name == null ? null : name.toEngChar();
    this.isEditor = isEditor;
    this.isTrans = isTrans;
    this.inFileName = inFileName;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public final Author clone() 
  { try { return (Author) super.clone(); } catch (CloneNotSupportedException ex) { throw new RuntimeException(ex); }}
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public final PersonName getName()                     { return getName(false); }
  public final String getLastName()                     { return getLastName(false); }       
  public final String getLastName(boolean engChar)      { return getName(engChar).getLast(); }
  public final String getFirstName()                    { return getFirstName(false); }  
  public final String getFirstName(boolean engChar)     { return getName(engChar).getFirst(); }
  public final String getNameLastFirst(boolean engChar) { return getName(engChar).getLastFirst(); }
  public final String getFullName(boolean engChar)      { return getName(engChar).getFull(); }
  public final String getBibName()                      { return getName().getBibName(); }
  public final String singleName()                      { return getName().getSingle(); }
  public final String getNameLastFirst()                { return getNameLastFirst(false); }  
  public final HDT_Person getPerson()                   { return person; }
  public final HDT_Work getWork()                       { return work; }
  public final PersonName getName(boolean engChar)      { return nullSwitch(person, engChar ? nameEngChar : name, () -> person.getName(engChar)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public final boolean getIsEditor()   { return isNull(person) ? isEditor :   (work == null ? false         : db.getNestedBoolean(work, person, tagEditor)); }
  public final boolean getIsTrans()    { return isNull(person) ? isTrans :    (work == null ? false         : db.getNestedBoolean(work, person, tagTranslator)); }
  public final Ternary getInFileName() { return isNull(person) ? inFileName : (work == null ? Ternary.Unset : db.getNestedTernary(work, person, tagInFileName)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------


  public final boolean equalsObjGroup(ObjectGroup objGroup)
  {
    if (person != objGroup.getPrimary()) return false;
    
    if ((person == null) && (objGroup.getPrimary() == null))
      if (name.equalsExceptParenthetical(new PersonName(objGroup.getPrimaryStr())) == false)
        return false;
    
    NestedValue val = objGroup.getValue(tagInFileName);
    
    if ((val != null) && (val.ternary != getInFileName()))
      return false;          
    
    val = objGroup.getValue(tagEditor);
    
    if ((val != null) && (val.bool != getIsEditor()))
      return false;
    
    val = objGroup.getValue(tagTranslator);
    
    if ((val != null) && (val.bool != getIsTrans()))
      return false;
    
    return true;
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

  public final String getSortKey()
  {
    if (nonNull(person)) return person.getSortKey();
    
    String last = getLastName(true), first = getFirstName(true);
    
    if ((last.length() == 0) || (first.length() == 0))
      return last + first;
    
    return last + '\u0000' + first;  
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}