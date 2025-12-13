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

package org.hypernomicon.model.records;

import org.hypernomicon.model.DatasetAccessor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.SimpleRecordTypes.*;
import org.hypernomicon.model.relations.HyperObjPointer;
import org.hypernomicon.model.relations.HyperSubjList;
import org.hypernomicon.model.searchKeys.*;
import org.hypernomicon.model.unities.HDT_RecordWithMainText;
import org.hypernomicon.util.SplitString;


import java.util.*;
import java.util.Map.Entry;

import javafx.geometry.Rectangle2D;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class HDT_Person extends HDT_RecordWithMainText implements HDT_RecordWithPath
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final HyperSubjList<HDT_Investigation, HDT_Person> investigations;

  public final List<HDT_Institution> institutions;
  public final List<HDT_Work> works;
  public final List<HDT_MiscFile> miscFiles;

  public final HyperObjPointer<HDT_Person, HDT_Rank> rank;
  public final HyperObjPointer<HDT_Person, HDT_PersonStatus> status;
  public final HyperObjPointer<HDT_Person, HDT_Field> field;
  public final HyperObjPointer<HDT_Person, HDT_Subfield> subfield;

  private final HyperPath picture;

//---------------------------------------------------------------------------

  public HDT_Person(RecordState xmlState, DatasetAccessor<HDT_Person> dataset)
  {
    super(xmlState, dataset);

    setNameInternal("|", false);

    institutions = getObjList(rtInstOfPerson);

    works          = getSubjList(rtAuthorOfWork);
    miscFiles      = getSubjList(rtAuthorOfFile);
    investigations = getSubjList(rtPersonOfInv);

    rank     = getObjPointer(rtRankOfPerson);
    status   = getObjPointer(rtStatusOfPerson);
    field    = getObjPointer(rtFieldOfPerson);
    subfield = getObjPointer(rtSubfieldOfPerson);

    picture = new HyperPath(getObjPointer(rtPictureFolderOfPerson), this);
  }

//---------------------------------------------------------------------------

  public String getWebURL()                                { return getTagString(tagWebURL); }
  public String getOrcID()                                 { return getTagString(tagORCID); }
  public PersonName getName()                              { return getName(false); }
  public String getNameLastFirst(boolean engChar)          { return getName(engChar).getLastFirst(); }
  public String getFullName(boolean engChar)               { return getName(engChar).getFull(); }
  public boolean instIsPast(HDT_Institution inst)          { return db.getNestedBoolean(this, inst, tagPast); }
  public void setWebURL(String newStr)                     { updateTagString(tagWebURL, newStr); }
  public void setORCID(String newOrcid)                    { updateTagString(tagORCID, newOrcid); }
  void setFirstNameInternal(String newStr, boolean update) { setNameInternal(getLastName() + '|' + newStr.replace("|", ""), update); }
  void setLastNameInternal(String newStr, boolean update)  { setNameInternal(newStr.replace("|", "") + '|' + getFirstName(), update); }

  @Override public void setName(String str)  { internalErrorPopup(19982); }

  /**
   * {@inheritDoc}
   */
  @Override public String defaultCellText()   { return getNameLastFirst(false); }

  /**
   * {@inheritDoc}
   */
  @Override public String defaultChoiceText() { return defaultCellText(); }
  @Override public String getXMLObjectName()  { return defaultCellText(); }
  @Override public HyperPath getPath()        { return picture; }

//---------------------------------------------------------------------------
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
      db.deleteRecord(investigations.getFirst());

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
    KeywordBinding binding = new KeywordBinding(key, person);
    if (binding.getUserText().length() < 3) return null;

    Keyword existingKeyObj = db.getKeyByKeyword(binding.getNormalizedText());

    if ((existingKeyObj != null) && (existingKeyObj.getAllRecords().contains(person) == false))
      return (HDT_Person) findFirst(existingKeyObj.getAllRecords(), record -> record.getType() == hdtPerson);

    for (String val : new SplitString(keys.toString(), ';'))
      if (val.strip().equalsIgnoreCase(binding.getUserText()))
        return null;

    if (keys.length() > 0) keys.append("; ");
    keys.append(binding.getUserText());

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

  private static final class PotentialKeySet
  {
    private final Map<String, Boolean> keys = new HashMap<>();

    private void add(String newKey, boolean newUseForDupCheck)
    {
      if (newKey.isEmpty() == false)
        keys.put(newKey, newUseForDupCheck);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Pass person=null if making a search key for a new record

  public static HDT_Person makeSearchKey(PersonName name, HDT_Person person, StringBuilder newSearchKey)
  {
    HDT_Person otherPerson = null;
    StringBuilder keys = new StringBuilder();

    PotentialKeySet keySet = makeSearchKeySet(name);

    for (Entry<String, Boolean> entry : keySet.keys.entrySet())
    {
      HDT_Person rv = addSearchKey(keys, entry.getKey(), person);

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
    if (first.matches("[A-Z]+"))                        // If first name is all caps, treat the letters as initials
      first = String.join(".", first.split("")) + '.';  // E.g., "EG Marshall" -> "E. G. Marshall"

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

      first = first.strip();
    }

    SplitString splitStr = new SplitString(first, ' ');

    while (splitStr.hasNext())
    {
      String name = splitStr.next();
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
          name = name + ' ' + splitStr.next();
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

  private static PotentialKeySet makeSearchKeySet(PersonName personName)
  {
    PotentialKeySet keySet = new PotentialKeySet();
    String first = personName.getFirst(), last = personName.getLast();
    StringBuilder nickNames = new StringBuilder();

    List<String> nameList    = new ArrayList<>(),
                 initialList = new ArrayList<>();

    last = getSearchKeyComponents(first, last, nameList, initialList, nickNames);

    keySet.add('^' + last, false);

    if (nameList.size() > 0)
    {
      String name1 = nameList.getFirst();

      if (name1.length() > 0)
      {
        keySet.add(name1 + ' ' + last, true);
      }
      else
      {
        keySet.add(initialList.getFirst() + ". " + last, false);

        for (int ndx = 1; ndx < nameList.size(); ndx++)
          if (nameList.get(ndx).length() > 0)
          {
            keySet.add(nameList.get(ndx) + ' ' + last, false);
            break;
          }
      }

      if (initialList.size() > 1)
      {
        String name = "";
        for (String initial : initialList)
          name = name + initial + ". ";

        keySet.add(name + last, false);

        if (nameList.getFirst().length() > 0)
        {
          name = nameList.getFirst() + ' ';
          for (int ndx = 1; ndx < initialList.size(); ndx++)
            name = name + initialList.get(ndx) + ". ";

          keySet.add(name + last, true);
        }
      }

      if (nameList.size() > 1)
      {
        String name = "";
        for (int ndx = 0; ndx < nameList.size(); ndx++)
          name = name + (nameList.get(ndx).length() > 0 ? (nameList.get(ndx) + ' ') : (initialList.get(ndx) + ". "));

        keySet.add(name + last, true);
      }
    }

    if (nickNames.length() > 0)
      for (String nickName : parseNickNames(nickNames))
        keySet.add(nickName + ' ' + last, false);

    return keySet;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<String> parseNickNames(CharSequence nickNames)
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

    return nickNameList;
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

    updateTagString(tagPictureCrop, x + ";" + y + ';' + width + ';' + height);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
