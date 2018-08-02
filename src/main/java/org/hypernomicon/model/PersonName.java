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

package org.hypernomicon.model;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.mutable.MutableInt;

public final class PersonName implements Comparable<PersonName>, Cloneable
{
  public static final PersonName EMPTY = new PersonName("", "");
  
  private String first, last;
  
  public PersonName(String first, String last) { init(first, last); } 
  public PersonName(String name)               { parseStr(name); }
  
  private void init(String first, String last) { this.first = ultraTrim(safeStr(first)); this.last = ultraTrim(safeStr(last)); }
  
  public String getFirst()                     { return safeStr(first); }
  public String getLast()                      { return safeStr(last); }
  public boolean isEmpty()                     { return (getLast().length() + getFirst().length()) == 0; }    
  public PersonName toLowerCase()              { return new PersonName(first.toLowerCase(), last.toLowerCase()); }
  public String getFull()                      { return String.valueOf(first + " " + last).trim(); }
  public String getSingle()                    { return getLast().length() > 0 ? getLast() : getFirst(); }
  public PersonName toEngChar()                { return new PersonName(convertToEnglishChars(first), convertToEnglishChars(last)); }
 
  @Override public PersonName clone() 
  { try { return (PersonName) super.clone(); } catch (CloneNotSupportedException ex) { throw new RuntimeException(ex); }}
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int compareTo(PersonName o)
  {
    return getSortKey().compareTo(o.getSortKey());
  }    
    
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------    
    
  private String getSortKey()
  {
    if ((last.length() == 0) || (first.length() == 0))
      return last + first;
    
    return last + '\u0000' + first;  
  }

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
    PersonName other = (PersonName) obj;
    
    return compareTo(other) == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getLastFirst()
  {
    if ((first.length() > 0) && (last.length() > 0))
      return last + ", " + first;
    
    return last + first;
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void parseStr(String name)
  {
    name = ultraTrim(convertToSingleLine(name));
    
    if ((name.matches(".*[A-Z].*") == false) || (name.matches(".*[a-z].*") == false))
      name = titleCase(name);
    
    while (name.matches(".*\\h\\..*"))
      name = name.replaceFirst("\\h\\.", ".");   // remove space before periods   
    
    int ndx = name.indexOf(',');
    
    if (ndx > 0)
    {
      init(ultraTrim(name.substring(ndx + 1)), ultraTrim(name.substring(0, ndx)));
      return;
    }
    else if (ndx == 0)
    {
      init(ultraTrim(name.substring(ndx + 1)), "");
      return;
    }
    
    if (name.matches("[Vv][ao]n\\h.*"))
    {
      init("", ultraTrim(name));
      return;
    }
    
    ndx = name.length();
    while (ndx > 0)
    {
      ndx--;
      
      if (name.substring(ndx).matches("[^A-Za-z][A-Z]\\..*"))  // Parses "John B. X. James"
      {
        ndx = ndx + 3;
        init(ultraTrim(name.substring(0, ndx)), ultraTrim(name.substring(ndx)));
        return;
      }
      
      if (name.substring(ndx).matches("[^A-Za-z][A-Za-z]+\\..*"))  // Parses "John B. St. James"
      {
        ndx = ndx + 1;
        init(ultraTrim(name.substring(0, ndx)), ultraTrim(name.substring(ndx)));
        return;
      }
      
      if (name.substring(ndx).matches("[^A-Za-z][Vv][ao]n\\h.*"))
      {
        ndx = ndx + 1;
        init(ultraTrim(name.substring(0, ndx)), ultraTrim(name.substring(ndx)));
        return;
      }
    }
    
    ndx = name.lastIndexOf(' ');

    if      (ndx < 0) init("", name);
    else if (ndx > 0) init(ultraTrim(name.substring(0, ndx + 1)), ultraTrim(name.substring(ndx + 1))); 
    else              init("", ultraTrim(name.substring(ndx + 1)));           
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  public String getBibName()
  {
    List<String> initialList = new ArrayList<>();
    String nameStr, firstName = getFirst();
    MutableInt pos = new MutableInt(0);
    
    while (firstName.contains("("))
      firstName = removeFirstParenthetical(firstName);
        
    while (pos.intValue() != -1)
    {
      nameStr = nextSubString(firstName, " ", pos);
      if (nameStr.length() == 0) continue;
      
      if (nameStr.endsWith("."))
      {
        if ((nameStr.length() == 2) && (nameStr.equals(nameStr.toUpperCase())))  // true if it is an initial
        {
          initialList.add(nameStr.substring(0, 1));
        }
        else
        {
          nameStr = nameStr + " " + nextSubString(firstName, " ", pos);
          initialList.add(nameStr.substring(0, 1));
        }
      }
      else
      {
        initialList.add(nameStr.substring(0, 1));
      }
    }
    
    if (initialList.size() == 0)
    {
      if (getLast().length() > 0)
        return getLast();
      else
        return getFirst();
    }
    
    if (getLast().length() == 0)
      return getFirst();
    
    String bibName = getLast() + ", ";
    
    for (String initial : initialList)
      bibName = bibName + initial + ". ";
    
    return bibName.trim();
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}