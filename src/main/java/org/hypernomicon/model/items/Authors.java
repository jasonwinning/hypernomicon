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
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Map.Entry;

import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.bib.BibAuthors;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.PersonName;

public class Authors implements Iterable<Author>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private class AuthorIterator implements Iterator<Author>
  {
    private int nextNdx = 0;
    
    @Override public boolean hasNext() { return nextNdx < size(); }

    @Override public Author next()
    {
      if (hasNext()) return get(nextNdx++);      
      throw new NoSuchElementException();
    }    
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
  
  private HyperObjList<HDT_Work, HDT_Person> objList, objListNoMod;
  private HDT_Work work;
  private boolean allRecords = true; // if this is true, ignore authorList and treat as pointer multi
  private List<Author> authorList = null;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Authors(HyperObjList<HDT_Work, HDT_Person> objList, HDT_Work work)
  {
    this.objList = objList;
    this.work = work;
    
    objListNoMod = db.getObjectList(rtAuthorOfWork, work, false);
  }
  
  public final int size()  { return allRecords ? objList.size() : authorList.size(); }
  public boolean isEmpty() { return size() == 0; }
  final void expire()      { clearNoMod(); }

  @Override public Iterator<Author> iterator() { return new AuthorIterator(); }

  final void resolvePointers() throws HDB_InternalError { db.resolvePointersByRelation(rtAuthorOfWork, work); }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author get(int ndx)
  {
    if (allRecords) return new Author(work, objList.get(ndx));
    else            return authorList.get(ndx);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Collection<Author> asCollection()
  {
    Collection<Author> set = new LinkedHashSet<>();
    
    forEach(set::add);

    return set;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException
  {
    objListNoMod.add(person);
    objListNoMod.throwLastException();

    if (tagToNestedItem != null)
      for (Entry<Tag, HDI_OfflineBase> entry : tagToNestedItem.entrySet())
        db.setNestedItemFromOfflineValue(work, person, entry.getKey(), entry.getValue());

    if (allRecords)
      return;

    authorList.add(new Author(work, person));    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void addNoMod(PersonName name, Map<Tag, HDI_OfflineBase> tagToNestedItem)
  {
    if (allRecords)
      initAuthorList();
    
    boolean editor = false, translator = false;
    Ternary inFileName = Ternary.Unset;
    
    if (tagToNestedItem != null)
    {
      for (Entry<Tag, HDI_OfflineBase> entry : tagToNestedItem.entrySet())
      {
        switch (entry.getKey())
        {
          case tagInFileName: inFileName = HDI_OfflineTernary.class.cast(entry.getValue()).get(); break;                 
          case tagEditor:     editor = HDI_OfflineBoolean.class.cast(entry.getValue()).get(); break;
          case tagTranslator: translator = HDI_OfflineBoolean.class.cast(entry.getValue()).get(); break;
          default : break;
        }
      }
    }
    
    authorList.add(new Author(work, name, editor, translator, inFileName));    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void initAuthorList()
  {
    authorList = new ArrayList<>();
    allRecords = false;            

    objListNoMod.forEach(person -> authorList.add(new Author(work, person)));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------
    
  void clearNoMod()
  {
    objListNoMod.clear();
    authorList = null;
    allRecords = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void clear()
  {
    objList.clear();
    
    if ((authorList != null) && (authorList.isEmpty() == false))
    {
      authorList = null;
      work.modifyNow();
    }
      
    allRecords = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author getAuthor(PersonName personName)
  {
    if (allRecords) return null;
    
    for (Author author : authorList)
      if (author.getPerson() == null)
        if (author.getName().equals(personName))
          return author;
    
    return null;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void update(List<ObjectGroup> objGroups)
  {    
    db.updateObjectGroups(rtAuthorOfWork, work, objGroups);
    allRecords = true;
    
    for (ObjectGroup objGroup : objGroups)
    {
      if (objGroup.getPrimary() == null)
      {
        allRecords = false;
        
        ensurePresent(objGroup, tagInFileName);
        ensurePresent(objGroup, tagEditor);
        ensurePresent(objGroup, tagTranslator);
      }
    }
    
    authorList = allRecords ? null : getListFromObjectGroups(objGroups, work);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static List<Author> getListFromObjectGroups(List<ObjectGroup> objGroups, HDT_Work work)
  {
    List<Author> authorList = new ArrayList<>();
    
    objGroups.forEach(objGroup ->
    {
      if (objGroup.getPrimary() != null)
        authorList.add(new Author(work, (HDT_Person) objGroup.getPrimary()));
      else
        authorList.add(new Author(work, new PersonName(objGroup.getPrimaryStr()), 
                                        objGroup.getValue(tagEditor).bool, 
                                        objGroup.getValue(tagTranslator).bool, 
                                        objGroup.getValue(tagInFileName).ternary));
    });
    
    return authorList;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void ensurePresent(ObjectGroup objGroup, Tag tag)
  {
    if (objGroup.getValue(tag) == null)
    {
      Author author = getAuthor(new PersonName(objGroup.getPrimaryStr()));
      if (author != null)
      {
        NestedValue val = new NestedValue(db.getNestedSchema(rtAuthorOfWork, tag).getCategory());
        
        switch (tag)
        {
          case tagInFileName : val.ternary = author.getInFileName(); break;
          case tagEditor : val.bool = author.getIsEditor(); break;
          case tagTranslator : val.bool = author.getIsTrans(); break;
          default : break;
        }

        objGroup.addNestedEntry(tagInFileName, val);
      }            
    }    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void add(HDT_Person person)
  {
    add(person, false, false, Ternary.Unset); 
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void add(Author author)
  {   
    if (author.getPerson() == null)
      add(author.getName(), author.getIsEditor(), author.getIsTrans(), author.getInFileName());
    else
      add(author.getPerson(), author.getIsEditor(), author.getIsTrans(), author.getInFileName());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void add(PersonName name, boolean isEditor, boolean isTrans, Ternary inFileName)
  {
    if (allRecords)
      initAuthorList();
    
    authorList.add(new Author(work, name, isEditor, isTrans, inFileName));
    work.modifyNow();    
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void add(HDT_Person person, boolean isEditor, boolean isTrans, Ternary inFileName)
  {
    objList.add(person);
    
    if (isEditor) work.setPersonIsEditor(person, true);
    if (isTrans) work.setPersonIsTranslator(person, true);
    if (inFileName != Ternary.Unset) work.setPersonIsInFileName(person, inFileName);
    
    if (allRecords)
      return;

    authorList.add(new Author(work, person));    
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void setAuthorRecord(Author oldAuthor, HDT_Person person)
  {
    if (oldAuthor.getPerson() != null)      
    {
      messageDialog("Internal error #73222", mtError);
      return;
    }
    
    HDT_Person insertAfter = null;
    
    for (Author author : authorList)
    {
      HDT_Person curPerson = author.getPerson();
      if (curPerson != null) insertAfter = curPerson;
      
      if (author == oldAuthor)
      {
        if (insertAfter == null)
          objListNoMod.add(0, person);
        else
          objListNoMod.add(objListNoMod.indexOf(insertAfter) + 1, person);
        
        work.setPersonIsEditor(person, author.getIsEditor());
        work.setPersonIsTranslator(person, author.getIsTrans());
        work.setPersonIsInFileName(person, author.getInFileName());
        
        Author newAuthor = new Author(work, person);
        
        authorList.set(authorList.indexOf(oldAuthor), newAuthor);
        
        return;
      }
    }
    
    messageDialog("Internal error #73223", mtError);
  }

//---------------------------------------------------------------------------
//--------------------------------------------------------------------------- 
  
  public static String getShortAuthorsStr(Collection<Author> authors, boolean sort, boolean fullNameIfSingleton)
  {
    return getAuthorsStr(authors, ',', true, true, sort, fullNameIfSingleton);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getLongAuthorsStr(Collection<Author> authors, boolean fullNameIfSingleton)
  {   
    return getAuthorsStr(authors, ';', false, false, false, fullNameIfSingleton);
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getAuthorsStr(Collection<Author> authorCol, char delimiter, boolean amp, boolean firstInitials, boolean sort, boolean fullNameIfSingleton)
  {
    if (authorCol.size() == 0)
      return "";
    
    List<Author> authors = new ArrayList<>(authorCol);
    String peopleStr = "";
    
    if (authors.size() == 1)
    {
      if (firstInitials && (fullNameIfSingleton == false))
        return authors.get(0).getBibName();
      
      return authors.get(0).getNameLastFirst();
    }
    
    if (sort)
      authors.sort((x, y) -> x.getNameLastFirst(true).compareTo(y.getNameLastFirst(true)));
    
    for (int ndx = 0; ndx < authors.size(); ndx++)
    {
      if (ndx != 0)
      {
        peopleStr = peopleStr + delimiter + " ";
        
        if ((ndx == (authors.size() - 1)) && amp)
          peopleStr = peopleStr.trim() + " & ";
      }
      
      if (firstInitials)
        peopleStr = peopleStr + authors.get(ndx).getBibName();
      else
        peopleStr = peopleStr + authors.get(ndx).getNameLastFirst();
    }
    
    return peopleStr;
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void setAll(BibAuthors bibAuthors)
  {
    clear();
    
    if (bibAuthors == null) return;
    if (bibAuthors.isEmpty()) return;

    ArrayList<PersonName> nameList = new ArrayList<>();
    ArrayList<HDT_Person> personList = new ArrayList<>();
    HashMap<PersonName, Boolean> nameToEd = new HashMap<>();
    HashMap<PersonName, Boolean> nameToTr = new HashMap<>();

    bibAuthors.getListsForWorkMerge(nameList, personList, nameToEd, nameToTr);
    
    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      PersonName name = nameList.get(ndx);
      HDT_Person person = personList.get(ndx);
      
      if (person != null)
        add(person, nameToEd.get(name), nameToTr.get(name), Ternary.Unset);
      else
        add(name, nameToEd.get(name), nameToTr.get(name), Ternary.Unset);
    }
  }
  
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

}
