/*
 * Copyright 2015-2023 Jason Winning
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

package org.hypernomicon.bib.authors;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import com.google.common.collect.Iterators;

import javafx.concurrent.Worker.State;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.dialogs.NewPersonDlgCtrlr;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.ObjectGroup;

import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.util.Util.*;

public abstract class BibAuthors implements Iterable<BibAuthor>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public abstract void getLists(List<BibAuthor> authorList, List<BibAuthor> editorList, List<BibAuthor> translatorList);

  @SuppressWarnings("unused")
  public void add(BibAuthor author) { throw new UnsupportedOperationException("add"); }
  public void clear()               { throw new UnsupportedOperationException("clear"); }

  public boolean isEmpty()          { return iterator().hasNext() == false; }
  public Stream<BibAuthor> stream() { return StreamSupport.stream(spliterator(), false); }

  public final void add(AuthorType authorType, HDT_Person person) { add(new BibAuthor(authorType, person)); }
  public final void add(AuthorType authorType, PersonName name)   { add(new BibAuthor(authorType, name)); }

  public static boolean isEmpty(BibAuthors bibAuthors) { return (bibAuthors == null) || bibAuthors.isEmpty(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr()
  {
    Function<? super BibAuthor, String> mapper = bibAuthor ->
    {
      String auth = bibAuthor.getName().getLastFirst();

      switch (bibAuthor.getType())
      {
        case editor:     return auth + " (ed)";
        case translator: return auth + " (tr)";
        default:         return auth;
      }
    };

    return stream().map(mapper)
                   .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String getStr(AuthorType authorType)
  {
    return stream().filter(bibAuthor -> bibAuthor.getType() == authorType)
                   .map(bibAuthor -> bibAuthor.getName().getLastFirst())
                   .reduce((s1, s2) -> s1 + "; " + s2).orElse("");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<BibAuthor> iterator()
  {
    List<BibAuthor> authorList = new ArrayList<>(),
                    editorList = new ArrayList<>(),
                    translatorList = new ArrayList<>();

    getLists(authorList, editorList, translatorList);

    return Iterators.unmodifiableIterator(Iterators.concat(authorList.iterator(), editorList.iterator(), translatorList.iterator()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAllFromTable(List<ObjectGroup> authGroups)
  {
    clear();
    List<AuthorType> authorTypes = new ArrayList<>();

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

      authorTypes.forEach(person == null ?
        authorType -> add(authorType, new PersonName(authGroup.getPrimaryStr()))
      :
        authorType -> add(authorType, person));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getListsForWorkMerge(List<PersonName> nameList,         // list of names that will be shown in work merge dialog table
                                   List<HDT_Person> personList,       // person records that will be populated in work merge dialog table
                                   Map<PersonName, Boolean> nameToEd,
                                   Map<PersonName, Boolean> nameToTr,
                                   HDT_Work destWork)
  {
    if (isEmpty()) return;

    List<BibAuthor> authorList     = new ArrayList<>(),  // list of authors for this bib entry
                    editorList     = new ArrayList<>(),  // list of editors for this bib entry
                    translatorList = new ArrayList<>();  // list of translators for this bib entry

    getLists(authorList, editorList, translatorList);

    translatorList.forEach(bibAuthor ->
    {
      PersonName name = bibAuthor.getName();
      if (name.isEmpty()) return;

      if (nameList.contains(name) == false)
      {
        nameList.add(name);
        personList.add(HDT_Person.lookUpByName(name));
        nameToTr.put(name, true);
        nameToEd.put(name, false);
      }

      nullSwitch(bibAuthor.getPerson(), person -> personList.set(nameList.indexOf(name), person));
    });

    editorList.forEach(bibAuthor ->
    {
      PersonName name = bibAuthor.getName();
      if (name.isEmpty()) return;

      if (nameList.contains(name) == false)
      {
        nameList.add(name);
        personList.add(HDT_Person.lookUpByName(name));
      }

      nameToTr.putIfAbsent(name, false);
      nameToEd.put(name, true);

      nullSwitch(bibAuthor.getPerson(), person -> personList.set(nameList.indexOf(name), person));
    });

    authorList.forEach(bibAuthor ->
    {
      PersonName name = bibAuthor.getName();
      if (name.isEmpty()) return;

      if (nameList.contains(name) == false)
      {
        nameList.add(name);
        personList.add(HDT_Person.lookUpByName(name));
      }

      nameToTr.put(name, false);
      nameToEd.put(name, false);

      nullSwitch(bibAuthor.getPerson(), person -> personList.set(nameList.indexOf(name), person));
    });

    removeDupPersonRecordsFromLists(nameList, personList);

    List<PersonName> nonRecordNames   = new ArrayList<>(); // list containing only non-record authors
    List<Author>     nonRecordAuthors = new ArrayList<>(); // author objects for each non-record author
    List<Integer>    nameListIndices  = new ArrayList<>(); // index into nameList for this non-record author

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

    int startNdx = 0;

    while (startNdx < nonRecordNames.size())
    {
      ArrayList<ArrayList<Author>> matchedAuthorsList = new ArrayList<>(); // List of matches for each non-record author

      HyperTask task = NewPersonDlgCtrlr.createDupCheckTask(nonRecordNames  .subList(startNdx, nonRecordNames.size()),
                                                            nonRecordAuthors.subList(startNdx, nonRecordNames.size()),
                                                            matchedAuthorsList,
                                                            null);

      if (task.runWithProgressDialog() != State.SUCCEEDED) return;

      int ndx;

      for (ndx = startNdx; ndx < nonRecordNames.size(); ndx++)
      {
        ArrayList<Author> matchedAuthors = matchedAuthorsList.get(ndx - startNdx);

        if (matchedAuthors.size() > 0)
        {
          int nameListNdx = nameListIndices.get(ndx);
          PersonName name = nonRecordAuthors.get(ndx).getName();
          boolean ed = nameToEd.get(name), tr = nameToTr.get(name);
          NewPersonDlgCtrlr npdc = new NewPersonDlgCtrlr(name, null, false, null, null, matchedAuthors);

          if (npdc.showModal() == false) return;

          if (npdc.getPerson() != null)
          {
            HDT_Person person = npdc.getPerson();
            personList.set(nameListNdx, person);
            name = person.getName();
          }
          else
          {
            personList.set(nameListNdx, null);
            name = npdc.getName();
          }

          nameList.set(nameListNdx, name);
          nameToEd.put(name, ed);
          nameToTr.put(name, tr);

          startNdx = ndx + 1;
          break;
        }
      }

      if (ndx == nonRecordNames.size())
        startNdx = ndx;
    }

    removeDupPersonRecordsFromLists(nameList, personList);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void removeDupPersonRecordsFromLists(List<PersonName> nameList, List<HDT_Person> personList)
  {
    Set<Integer> indicesToRemove = new HashSet<>();

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
      nameList  .remove(ndx.intValue());
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean notAllEngCharLastNames()
  {
    for (BibAuthor author : this)
      if (author.getName().toEngChar().getLast().equals(author.getName().getLast()) == false)
        return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
