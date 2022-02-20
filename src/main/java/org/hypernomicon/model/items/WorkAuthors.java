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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static java.util.Objects.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.HyperObjList;
import org.hypernomicon.model.relations.NestedValue;
import org.hypernomicon.model.relations.ObjectGroup;

public class WorkAuthors extends Authors
{

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private final HyperObjList<HDT_Work, HDT_Person> objList, objListNoMod;
  private final HDT_Work work;
  private final List<Author> authorList;
  private boolean allRecords = true; // if this is true, ignore authorList and treat as pointer multi

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public WorkAuthors(HyperObjList<HDT_Work, HDT_Person> objList, HDT_Work work)
  {
    this.objList = objList;
    this.work = work;
    authorList = new ArrayList<>();

    objListNoMod = db.getObjectList(rtAuthorOfWork, work, false);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public int size()        { return allRecords ? objList.size() : authorList.size(); }

  @Override public boolean containsPerson(HDT_Person person) { return objListNoMod.contains(person); }
  @Override void resolvePointers() throws HDB_InternalError  { db.resolvePointersByRelation(rtAuthorOfWork, work); }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override public Author get(int ndx)
  {
    if (allRecords) return new Author(work, objList.get(ndx));
    else            return authorList.get(ndx);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException
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

  @Override void clearNoMod()
  {
    objListNoMod.clear();
    authorList.clear();
    allRecords = true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @Override void clear()
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
