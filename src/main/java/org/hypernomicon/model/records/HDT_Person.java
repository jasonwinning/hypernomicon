/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.model.records;

import org.hypernomicon.model.HyperDataset;
import org.hypernomicon.model.SearchKeys;
import org.hypernomicon.model.SearchKeys.SearchKeyword;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Field;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_PersonStatus;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_Rank;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.util.SplitString;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javafx.geometry.Rectangle2D;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

//---------------------------------------------------------------------------

public class HDT_Person extends HDT_RecordWithConnector implements HDT_RecordWithPath
{
  public final List<HDT_Institution> institutions;
  public final List<HDT_Work> works;
  public final List<HDT_MiscFile> miscFiles;
  public final List<HDT_Investigation> investigations;

  public final HyperObjPointer<HDT_Person, HDT_Rank> rank;
  public final HyperObjPointer<HDT_Person, HDT_PersonStatus> status;
  public final HyperObjPointer<HDT_Person, HDT_Field> field;
  public final HyperObjPointer<HDT_Person, HDT_Subfield> subfield;

  private final HyperPath picture;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDT_Person(RecordState xmlState, HyperDataset<HDT_Person> dataset)
  {
    super(xmlState, dataset, tagNone);

    setNameInternal("|", false);

    institutions = getObjList(rtInstOfPerson);

    works = getSubjList(rtAuthorOfWork);
    miscFiles = getSubjList(rtAuthorOfFile);
    investigations = getSubjList(rtPersonOfInv);

    rank = getObjPointer(rtRankOfPerson);
    status = getObjPointer(rtStatusOfPerson);
    field = getObjPointer(rtFieldOfPerson);
    subfield = getObjPointer(rtSubfieldOfPerson);

    picture = new HyperPath(getObjPointer(rtPictureFolderOfPerson), this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getWebURL()                                { return getTagString(tagWebURL); }
  public String getOrcID()                                 { return getTagString(tagORCID); }
  public PersonName getName()                              { return getName(false); }
  public String getNameLastFirst(boolean engChar)          { return getName(engChar).getLastFirst(); }
  public String getFullName(boolean engChar)               { return getName(engChar).getFull(); }
  public boolean instIsPast(HDT_Institution inst)          { return db.getNestedBoolean(this, inst, tagPast); }
  public void setWebURL(String newStr)                     { updateTagString(tagWebURL, newStr); }
  public void setORCID(String newOrcid)                    { updateTagString(tagORCID, newOrcid); }
  void setFirstNameInternal(String newStr, boolean update) { setNameInternal(getLastName() + "|" + newStr.replace("|", ""), update); }
  void setLastNameInternal(String newStr, boolean update)  { setNameInternal(newStr.replace("|", "") + "|" + getFirstName(), update); }

  @Override public void setName(String str) { messageDialog("Internal error #19982", mtError); }
  @Override public String listName()        { return getNameLastFirst(false); }
  @Override public HyperPath getPath()      { return picture; }

//---------------------------------------------------------------------------

  public PersonName getName(boolean engChar)
  {
    return engChar ?
      new PersonName(getFirstNameEngChar(), getLastNameEngChar())
    :
      new PersonName(getFirstName(), getLastName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setName(PersonName personName)
  {
    setFirstNameInternal(personName.getFirst(), true);
    setLastNameInternal(personName.getLast(), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getLastName()
  {
    String s = name();
    return s.substring(0, s.indexOf('|'));
  }

  private String getLastNameEngChar()
  {
    String s = getNameEngChar();
    return s.substring(0, s.indexOf('|'));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getFirstName()
  {
    String s = name();
    return s.substring(s.indexOf('|') + 1);
  }

  private String getFirstNameEngChar()
  {
    String s = getNameEngChar();
    return s.substring(s.indexOf('|') + 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void expire()
  {
    while (investigations.isEmpty() == false)
      db.deleteRecord(investigations.get(0));

    picture.clear();

    nullSwitch(subfield.get(), oldSubfield ->
    {
      subfield.setID(-1);
      if (oldSubfield.persons.isEmpty())
        db.deleteRecord(oldSubfield);
    });

    super.expire();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static HDT_Person addSearchKey(StringBuilder keys, String key, HDT_Person person)
  {
    key = key.trim();
    if (key.length() < 3) return null;

    SearchKeyword hyperKey = db.getKeyByKeyword(key);

    if ((hyperKey != null) && (hyperKey.record != person))
      return hyperKey.record.getType() == hdtPerson ? HDT_Person.class.cast(hyperKey.record) : null;

    for (String val : new SplitString(keys.toString(), ';'))
      if (val.trim().equalsIgnoreCase(key))
        return null;

    if (keys.length() > 0) keys.append("; ");
    keys.append(key);

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_Person lookUpByName(PersonName name)
  {
    StringBuilder searchKeySB = new StringBuilder();

    HDT_Person person = makeSearchKey(name, null, searchKeySB);

    if (searchKeySB.toString().isEmpty())
      return person;

    String sortKey = removeAllParentheticals(name.toEngChar().getSortKey());

    return findFirst(db.persons, p -> removeAllParentheticals(p.getSortKey()).equalsIgnoreCase(sortKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class PotentialKeySet
  {
    private final Map<String, Boolean> keys = new HashMap<>();
    private final boolean lowerCase;

    private PotentialKeySet(boolean lowerCase)
    {
      this.lowerCase = lowerCase;
    }

    private boolean containsKey(String key)           { return keys.containsKey(key); }
    public boolean isSubsetOf(PotentialKeySet keySet) { return keys.keySet().stream().allMatch(keySet::containsKey); }

  //---------------------------------------------------------------------------

    private void add(String newKey, boolean newUseForDupCheck)
    {
      if (newKey.isEmpty() == false)
        keys.put(lowerCase ? newKey.toLowerCase() : newKey, newUseForDupCheck);
    }

  //---------------------------------------------------------------------------

    public boolean startsWith(String str)
    {
      for (String key : keys.keySet())
        if (key.replaceAll("[.,;]", "").startsWith(str))
          return true;

      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Pass person=null if making a search key for a new record

  public static HDT_Person makeSearchKey(PersonName name, HDT_Person person, StringBuilder newSearchKey)
  {
    HDT_Person rv, otherPerson = null;
    StringBuilder keys = new StringBuilder();

    PotentialKeySet keySet = makeSearchKeySet(name, false, false, false);

    for (Entry<String, Boolean> entry : keySet.keys.entrySet())
    {
      rv = addSearchKey(keys, entry.getKey(), person);

      if (rv != null)
      {
        if (entry.getValue())
          otherPerson = rv;
        else
        {
          String fullNameEngChar = name.toEngChar().getFull();
          if ((fullNameEngChar.contains(" ") == false) && fullNameEngChar.equals(rv.getFullName(true)))
            otherPerson = rv;
        }
      }
    }

    assignSB(newSearchKey, keys.toString());
    return otherPerson;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Returns last name, prepared for search key

  private static String getSearchKeyComponents(String first, String last, List<String> nameList, List<String> initialList, StringBuilder nickNames)
  {
    String name;

    first = first.replace(".", ". ");

    first = SearchKeys.prepSearchKey(first);
    last = SearchKeys.prepSearchKey(last);

    if (first.contains("("))
    {
      int paren1 = first.indexOf('(');

      if (first.contains(")"))
      {
        int paren2 = first.indexOf(')');

        assignSB(nickNames, first.substring(paren1, paren2 + 1));
        first = first.substring(0, paren1) + first.substring(paren2 + 1);
      }
      else
        first = first.substring(0, paren1);

      first = first.trim();
    }

    SplitString splitStr = new SplitString(first, ' ');

    while (splitStr.hasNext())
    {
      name = splitStr.next();
      if (name.isEmpty()) continue;

      if (name.endsWith("."))
      {
        if ((name.length() == 2) && name.equals(name.toUpperCase()))  // true if it is an initial
        {
          initialList.add(name.substring(0, 1));
          nameList.add("");
        }
        else
        {
          name = name + " " + splitStr.next();
          initialList.add(name.substring(0, 1));
          nameList.add(name);
        }
      }
      else if ((name.length() == 1) && name.equals(name.toUpperCase())) // initial without period
      {
        initialList.add(name);
        nameList.add("");
      }
      else
      {
        initialList.add(name.substring(0, 1));
        nameList.add(name);
      }
    }

    return last;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static PotentialKeySet makeSearchKeySet(PersonName personName, boolean useAllInitials, boolean lowerCase, boolean noNicknames)
  {
    PotentialKeySet keySet = new PotentialKeySet(lowerCase);
    String first = personName.getFirst(), last = personName.getLast();
    StringBuilder nickNames = new StringBuilder();

    List<String> nameList    = new ArrayList<>(),
                 initialList = new ArrayList<>();

    last = getSearchKeyComponents(first, last, nameList, initialList, nickNames);

    keySet.add(last, false);

    if (nameList.size() > 0)
    {
      if (nameList.get(0).length() > 0)
      {
        if (useAllInitials)
          keySet.add(initialList.get(0) + ". " + last, false);

        keySet.add(nameList.get(0) + " " + last, true);
      }
      else
      {
        keySet.add(initialList.get(0) + ". " + last, false);

        for (int ndx = 1; ndx < nameList.size(); ndx++)
          if (nameList.get(ndx).length() > 0)
          {
            keySet.add(nameList.get(ndx) + " " + last, false);
            break;
          }
      }

      if (initialList.size() > 1)
      {
        String name = "";
        for (String initial : initialList)
          name = name + initial + ". ";

        keySet.add(name + last, false);

        if (useAllInitials)
        {
          for (int ndx = 1; ndx < initialList.size(); ndx++)
            keySet.add(initialList.get(ndx) + ". " + last, false);
        }

        if (nameList.get(0).length() > 0)
        {
          name = nameList.get(0) + " ";
          for (int ndx = 1; ndx < initialList.size(); ndx++)
            name = name + initialList.get(ndx) + ". ";

          keySet.add(name + last, true);
        }
      }

      if (nameList.size() > 1)
      {
        String name = "";
        for (int ndx = 0; ndx < nameList.size(); ndx++)
          name = name + (nameList.get(ndx).length() > 0 ? (nameList.get(ndx) + " ") : (initialList.get(ndx) + ". "));

        keySet.add(name + last, true);

        if (useAllInitials)
        {
          String middleNames = "", middleInits = "";
          for (int ndx = 1; ndx < nameList.size(); ndx++)
          {
            middleNames = middleNames + (nameList.get(ndx).length() > 0 ? (nameList.get(ndx) + " ") : (initialList.get(ndx) + ". "));
            middleInits = middleInits + initialList.get(ndx) + ". ";
          }

          keySet.add(initialList.get(0) + ". " + middleNames + last, true);
          keySet.add(middleNames + last, true);
          keySet.add(middleInits + last, true);
        }
      }
    }

    if ((noNicknames == false) && (nickNames.length() > 0))
    {
      String nickName = "";
      List<String> nickNameList = new ArrayList<>();

      for (int ndx = 0; ndx < nickNames.length(); ndx++)
      {
        char c = nickNames.charAt(ndx);

        if ((c == ')') || (c == '(') || (c == ' ') || (c == ',') || (c == ';'))
        {
          if (nickName.length() > 0)
          {
            nickNameList.add(nickName);
            nickName = "";
          }
        }
        else
          nickName = nickName + c;
      }

      if (nickName.length() > 0)
        nickNameList.add(nickName);

      for (String nName : nickNameList)
        keySet.add(nName + " " + last, false);
    }

    return keySet;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Rectangle2D getViewPort()
  {
    String str = getTagString(tagPictureCrop);

    if (str.length() < 7) return null;

    String[] vals = str.split(";");
    if (vals.length != 4) return null;

    int x      = parseInt(vals[0], -1),
        y      = parseInt(vals[1], -1),
        width  = parseInt(vals[2], -1),
        height = parseInt(vals[3], -1);

    return new Rectangle2D(x, y, width, height);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void setViewPort(Rectangle2D viewPort)
  {
    if (viewPort == null)
    {
      updateTagString(tagPictureCrop, "");
      return;
    }

    int x      = (int) viewPort.getMinX(),
        y      = (int) viewPort.getMinY(),
        width  = (int) viewPort.getWidth(),
        height = (int) viewPort.getHeight();

    updateTagString(tagPictureCrop, x + ";" + y + ";" + width + ";" + height);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}