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

package org.hypernomicon.bib;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Iterators;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.BibData.AuthorType;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.dialogs.NewPersonDialogController;

import static org.hypernomicon.model.HyperDB.Tag.tagEditor;
import static org.hypernomicon.model.HyperDB.Tag.tagTranslator;
import static org.hypernomicon.util.Util.*;

public abstract class BibAuthors implements Iterable<BibAuthor>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract void getLists(ArrayList<BibAuthor> authorList, ArrayList<BibAuthor> editorList, ArrayList<BibAuthor> translatorList);
    
  @SuppressWarnings("unused")
  public void add(BibAuthor author) { throw new UnsupportedOperationException("add"); }
  public void clear()               { throw new UnsupportedOperationException("clear"); }  

  public boolean isEmpty()          { return iterator().hasNext() == false; }
   
  public final void add(AuthorType authorType, HDT_Person person) { add(new BibAuthor(authorType, person)); }  
  public final void add(AuthorType authorType, PersonName name)   { add(new BibAuthor(authorType, name)); }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setFromCrossRefJson(JsonArray jsonArr, AuthorType aType)
  {
    if (jsonArr == null) return;
    
    Iterator<JsonObj> it = jsonArr.objIterator();
           
    while (it.hasNext())
    {
      JsonObj author = it.next();
      add(new BibAuthor(aType, new PersonName(author.getStrSafe("given"), author.getStrSafe("family"))));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    ArrayList<BibAuthor> authList = new ArrayList<>();
    iterator().forEachRemaining(bibAuthor -> authList.add(bibAuthor));
    
    authList.sort((a1, a2) -> a1.getName().compareTo(a2.getName()));
    
    String auth = "", auths = "";
               
    for (int ndx = 0; ndx < authList.size(); ndx++)
    {
      auth = authList.get(ndx).getName().getLastFirst();
      
      switch (authList.get(ndx).getType())
      {
        case author: break;
        case editor: auth = auth + " (ed)"; break;
        case translator: auth = auth + " (tr)"; break;
        default: break;        
      }
      
      if (auths.length() > 0) auths = auths + "; ";
      auths = auths + auth;
    }
    
    return auths;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(AuthorType authorType)
  {
    ArrayList<BibAuthor> authList = new ArrayList<>();
    iterator().forEachRemaining(bibAuthor -> 
    {
      if (bibAuthor.getType() == authorType)
        authList.add(bibAuthor);  
    });
    
    String auth = "";
           
    for (int ndx = 0; ndx < authList.size(); ndx++)
    {
      if (auth.length() > 0) auth = auth + "; ";
      
      auth = auth + authList.get(ndx).getName().getLastFirst();
    }
    
    return auth;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<BibAuthor> iterator()
  {
    ArrayList<BibAuthor> authorList = new ArrayList<>(),
                         editorList = new ArrayList<>(),
                         translatorList = new ArrayList<>();
    
    getLists(authorList, editorList, translatorList);
    
    return readOnlyIterator(Iterators.concat(authorList.iterator(), editorList.iterator(), translatorList.iterator()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAllFromTable(List<ObjectGroup> authGroups)
  {
    clear();
    
    for (ObjectGroup authGroup : authGroups)
    {
      boolean ed = authGroup.getValue(tagEditor).bool,
              tr = authGroup.getValue(tagTranslator).bool;
      
      ArrayList<AuthorType> authorTypes = new ArrayList<>();
      
      if ((ed == false) && (tr == false))
        authorTypes.add(AuthorType.author);
      
      if (ed) authorTypes.add(AuthorType.editor);
      if (tr) authorTypes.add(AuthorType.translator);
      
      HDT_Person person = authGroup.getPrimary();
      
      for (AuthorType authorType : authorTypes)
      {
        if (person == null)
          add(authorType, new PersonName(authGroup.getPrimaryStr()));
        else
          add(authorType, person);
      }
    }
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void getListsForWorkMerge(List<PersonName> nameList, List<HDT_Person> personList, 
                                   Map<PersonName, Boolean> nameToEd, Map<PersonName, Boolean> nameToTr)
  {
    if (isEmpty()) return;

    ArrayList<BibAuthor> authorList = new ArrayList<>(), editorList = new ArrayList<>(), translatorList = new ArrayList<>();

    getLists(authorList, editorList, translatorList);
    
    for (BibAuthor bibAuthor : translatorList)
    {
      PersonName name = bibAuthor.getName();
      HDT_Person person = bibAuthor.getPerson();
      
      if (name.isEmpty() == false)
      {
        if (nameList.contains(name) == false)
        {
          nameList.add(name);
          personList.add(HDT_Person.lookUpByName(name));
          nameToTr.put(name, true);
          nameToEd.put(name, false);
        }
        
        if (person != null)
          personList.set(nameList.indexOf(name), person);
      }
    }
 
    for (BibAuthor bibAuthor : editorList)
    {
      PersonName name = bibAuthor.getName();
      HDT_Person person = bibAuthor.getPerson();
      
      if (name.isEmpty() == false)
      {
        if (nameList.contains(name) == false)
        {
          nameList.add(name);
          personList.add(HDT_Person.lookUpByName(name));
        }
        
        nameToTr.putIfAbsent(name, false);
        nameToEd.put(name, true);
        
        if (person != null)
          personList.set(nameList.indexOf(name), person);
      }
    }
    
    for (BibAuthor bibAuthor : authorList)
    {
      PersonName name = bibAuthor.getName();
      HDT_Person person = bibAuthor.getPerson();
      
      if (name.isEmpty() == false)
      {
        if (nameList.contains(name) == false)
        {
          nameList.add(name);
          personList.add(HDT_Person.lookUpByName(name));
        }
        
        nameToTr.put(name, false);
        nameToEd.put(name, false);
        
        if (person != null)
          personList.set(nameList.indexOf(name), person);
      }
    }

    ArrayList<PersonName> nonRecordNames = new ArrayList<>();
    ArrayList<Integer> nameListIndices = new ArrayList<>();
    
    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      if (personList.get(ndx) == null)
      {
        nonRecordNames.add(nameList.get(ndx)); 
        nameListIndices.add(ndx);
      }
    }
    
    if (nonRecordNames.size() > 0)
    {
      List<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();
      
      HyperTask task = NewPersonDialogController.createDupCheckTask(nonRecordNames, null, null, matchedAuthorsList , null);
      
      if (!HyperTask.performTaskWithProgressDialog(task)) return;
      
      for (int ndx = 0; ndx < matchedAuthorsList.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = matchedAuthorsList.get(ndx);
        
        if (matchedAuthors.size() > 0)
        {
          int nameListNdx = nameListIndices.get(ndx);
          
          NewPersonDialogController npdc = NewPersonDialogController.create(nonRecordNames.get(ndx), null, false, null, null, matchedAuthors);
          
          if (npdc.showModal() == false) return;
          
          if (npdc.getPerson() != null)                                
            personList.set(nameListNdx, npdc.getPerson());
          else                     
            nameList.set(nameListNdx, npdc.getName());
        }
      }
    }
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
