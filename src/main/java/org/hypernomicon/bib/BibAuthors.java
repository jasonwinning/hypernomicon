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

package org.hypernomicon.bib;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.google.common.collect.Iterators;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.BibData.AuthorType;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.json.JsonArray;
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

    jsonArr.getObjs().forEach(author -> add(new BibAuthor(aType, new PersonName(author.getStrSafe("given"), author.getStrSafe("family")))));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    StringBuilder auths = new StringBuilder();

    forEach(bibAuthor ->
    {
      String auth = bibAuthor.getName().getLastFirst();

      switch (bibAuthor.getType())
      {
        case author: break;
        case editor: auth = auth + " (ed)"; break;
        case translator: auth = auth + " (tr)"; break;
        default: break;
      }

      if (auths.length() > 0) auths.append("; ");
      auths.append(auth);
    });

    return auths.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(AuthorType authorType)
  {
    StringBuilder auths = new StringBuilder();

    forEach(bibAuthor ->
    {
      if (bibAuthor.getType() == authorType)
      {
        if (auths.length() > 0) auths.append("; ");
        auths.append(bibAuthor.getName().getLastFirst());
      }
    });

    return auths.toString();
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
    ArrayList<AuthorType> authorTypes = new ArrayList<>();

    authGroups.forEach(authGroup ->
    {
      boolean ed = authGroup.getValue(tagEditor).bool,
              tr = authGroup.getValue(tagTranslator).bool;

      authorTypes.clear();

      if ((ed == false) && (tr == false))
        authorTypes.add(AuthorType.author);

      if (ed) authorTypes.add(AuthorType.editor);
      if (tr) authorTypes.add(AuthorType.translator);

      HDT_Person person = authGroup.getPrimary();

      if (person == null)
        authorTypes.forEach(authorType -> add(authorType, new PersonName(authGroup.getPrimaryStr())));
      else
        authorTypes.forEach(authorType -> add(authorType, person));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getListsForWorkMerge(List<PersonName> nameList, List<HDT_Person> personList,
                                   Map<PersonName, Boolean> nameToEd, Map<PersonName, Boolean> nameToTr, HDT_Work destWork)
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

    removeDupPersonRecordsFromLists(nameList, personList);

    ArrayList<PersonName> nonRecordNames = new ArrayList<>();
    ArrayList<Author> nonRecordAuthors = new ArrayList<>();
    ArrayList<Integer> nameListIndices = new ArrayList<>();

    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      if (personList.get(ndx) == null)
      {
        PersonName name = nameList.get(ndx);

        nonRecordNames.add(name);
        nonRecordAuthors.add(new Author(destWork, name, nameToEd.get(name), nameToTr.get(name), Ternary.Unset));
        nameListIndices.add(ndx);
      }
    }

    if (nonRecordNames.size() > 0)
    {
      List<ArrayList<Author>> matchedAuthorsList = new ArrayList<>();

      HyperTask task = NewPersonDialogController.createDupCheckTask(nonRecordNames, nonRecordAuthors, matchedAuthorsList, null);

      if (!HyperTask.performTaskWithProgressDialog(task)) return;

      for (int ndx = 0; ndx < matchedAuthorsList.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = matchedAuthorsList.get(ndx);

        if (matchedAuthors.size() > 0)
        {
          int nameListNdx = nameListIndices.get(ndx);

          NewPersonDialogController npdc = NewPersonDialogController.create(nonRecordAuthors.get(ndx).getName(), null, false, null, null, matchedAuthors);

          if (npdc.showModal() == false) return;

          if (npdc.getPerson() != null)
            personList.set(nameListNdx, npdc.getPerson());
          else
            nameList.set(nameListNdx, npdc.getName());
        }
      }
    }

    removeDupPersonRecordsFromLists(nameList, personList);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void removeDupPersonRecordsFromLists(List<PersonName> nameList, List<HDT_Person> personList)
  {
    HashSet<Integer> indicesToRemove = new HashSet<>();

    for (int ndx = 0; ndx < nameList.size(); ndx++)
    {
      if (indicesToRemove.contains(ndx)) continue;

      HDT_Person person1 = personList.get(ndx);
      if (person1 == null) continue;

      for (int ndx2 = ndx + 1; ndx2 < nameList.size(); ndx2++)
        if (person1 == personList.get(ndx2))
          indicesToRemove.add(ndx2);
    }

    indicesToRemove.forEach(ndx ->
    {
      personList.remove(ndx.intValue());
      nameList.remove(ndx.intValue());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
