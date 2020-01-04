/*
 * Copyright 2015-2020 Jason Winning
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

import static java.util.Objects.*;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;
import java.util.Map.Entry;

import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;

import com.google.common.collect.Sets;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB.Tag;

public final class Authors implements Iterable<Author>
{
  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final class AuthorIterator implements Iterator<Author>
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

  private final HyperObjList<HDT_Work, HDT_Person> objList, objListNoMod;
  private final HDT_Work work;
  private final List<Author> authorList;
  private boolean allRecords = true; // if this is true, ignore authorList and treat as pointer multi

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Authors(HyperObjList<HDT_Work, HDT_Person> objList, HDT_Work work)
  {
    this.objList = objList;
    this.work = work;
    authorList = new ArrayList<>();

    objListNoMod = db.getObjectList(rtAuthorOfWork, work, false);
  }

  public int size()        { return allRecords ? objList.size() : authorList.size(); }
  public boolean isEmpty() { return size() == 0; }
  void expire()            { clearNoMod(); }

  public boolean containsPerson(HDT_Person person) { return objListNoMod.contains(person); }
  public Collection<Author> asCollection()         { return Sets.newLinkedHashSet(this); }
  void resolvePointers() throws HDB_InternalError  { db.resolvePointersByRelation(rtAuthorOfWork, work); }
  public Stream<Author> stream()                   { return StreamSupport.stream(spliterator(), false); }

  @Override public Iterator<Author> iterator()     { return new AuthorIterator(); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author get(int ndx)
  {
    if (allRecords) return new Author(work, objList.get(ndx));
    else            return authorList.get(ndx);
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
          case tagInFileName : inFileName = HDI_OfflineTernary.class.cast(entry.getValue()).get(); break;
          case tagEditor     : editor     = HDI_OfflineBoolean.class.cast(entry.getValue()).get(); break;
          case tagTranslator : translator = HDI_OfflineBoolean.class.cast(entry.getValue()).get(); break;
          default            :                                                                     break;
        }
      }
    }

    authorList.add(new Author(work, name, editor, translator, inFileName));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void initAuthorList()
  {
    authorList.clear();
    allRecords = false;

    objListNoMod.forEach(person -> authorList.add(new Author(work, person)));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void clearNoMod()
  {
    objListNoMod.clear();
    authorList.clear();
    allRecords = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void clear()
  {
    objList.clear();

    if (authorList.isEmpty() == false)
    {
      authorList.clear();
      work.modifyNow();
    }

    allRecords = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author getAuthor(PersonName personName)
  {
    return allRecords ? null : findFirst(authorList, author -> isNull(author.getPerson()) && author.getName().equals(personName));
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

    authorList.clear();

    if (allRecords == false)
      setListFromObjectGroups(authorList, objGroups, work);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void update(Author oldAuthor, Author newAuthor)
  {
    if (oldAuthor.equals(newAuthor)) return;

    int ndx = authorList.indexOf(oldAuthor);

    if ((ndx == -1) || (oldAuthor.getPerson() != null) || (newAuthor.getPerson() != null) || (oldAuthor.getWork() != work) || (newAuthor.getWork() != work))
    {
      messageDialog("Internal error #38891", mtError);
      return;
    }

    authorList.set(ndx, newAuthor);

    work.modifyNow();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private static void setListFromObjectGroups(List<Author> authorList, List<ObjectGroup> objGroups, HDT_Work work)
  {
    authorList.clear();

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
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static List<Author> getListFromObjectGroups(List<ObjectGroup> objGroups, HDT_Work work)
  {
    List<Author> authorList = new ArrayList<>();

    setListFromObjectGroups(authorList, objGroups, work);

    return authorList;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void ensurePresent(ObjectGroup objGroup, Tag tag)
  {
    if (objGroup.getValue(tag) != null) return;

    Author author = getAuthor(new PersonName(objGroup.getPrimaryStr()));
    if (author == null) return;

    NestedValue val = new NestedValue(db.getNestedSchema(rtAuthorOfWork, tag).getCategory());

    switch (tag)
    {
      case tagInFileName : val.ternary = author.getInFileName(); break;
      case tagEditor     : val.bool    = author.getIsEditor  (); break;
      case tagTranslator : val.bool    = author.getIsTrans   (); break;
      default            :                                       break;
    }

    objGroup.addNestedEntry(tagInFileName, val);
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

    if (allRecords == false)
      authorList.add(new Author(work, person));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public Author setAuthorRecord(Author oldAuthor, HDT_Person person)
  {
    if (oldAuthor.getPerson() != null)
    {
      messageDialog("Internal error #73222", mtError);
      return oldAuthor;
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

        work.setPersonIsEditor    (person, author.getIsEditor  ());
        work.setPersonIsTranslator(person, author.getIsTrans   ());
        work.setPersonIsInFileName(person, author.getInFileName());

        Author newAuthor = new Author(work, person);

        authorList.set(authorList.indexOf(oldAuthor), newAuthor);

        return newAuthor;
      }
    }

    messageDialog("Internal error #73223", mtError);
    return oldAuthor;
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
      authors.sort(null);

    int num = Math.min(6, authors.size());

    for (int ndx = 0; ndx < num; ndx++)
    {
      if (ndx != 0)
      {
        peopleStr = peopleStr + delimiter + " ";

        if ((ndx == (authors.size() - 1)) && amp)
          peopleStr = peopleStr.trim() + " & ";
      }

      peopleStr = peopleStr + (firstInitials ? authors.get(ndx).getBibName() : authors.get(ndx).getNameLastFirst());
    }

    if (num < authors.size())
      peopleStr = peopleStr + " et al.";

    return peopleStr;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public void setAll(BibAuthors bibAuthors)
  {
    clear();

    if (BibAuthors.isEmpty(bibAuthors)) return;

    List<PersonName> nameList = new ArrayList<>();
    List<HDT_Person> personList = new ArrayList<>();
    Map<PersonName, Boolean> nameToEd = new HashMap<>(), nameToTr = new HashMap<>();

    bibAuthors.getListsForWorkMerge(nameList, personList, nameToEd, nameToTr, work);

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
