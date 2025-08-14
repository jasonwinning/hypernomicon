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

package org.hypernomicon.model.authors;

import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import static java.util.Objects.*;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.items.*;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.relations.*;

//---------------------------------------------------------------------------

public class WorkAuthors extends RecordAuthors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperObjList<HDT_Work, HDT_Person> objList, objListNoMod;
  private final HDT_Work work;
  private final List<RecordAuthor> authorList;
  private boolean allRecords = true; // if this is true, ignore authorList and treat as pointer multi

//---------------------------------------------------------------------------

  public WorkAuthors(HyperObjList<HDT_Work, HDT_Person> objList, HDT_Work work)
  {
    this.objList = objList;
    this.work = work;
    authorList = new ArrayList<>();

    objListNoMod = db.getObjectList(rtAuthorOfWork, work, false);
  }

//---------------------------------------------------------------------------

  @Override public int size()        { return allRecords ? objList.size() : authorList.size(); }

  @Override public boolean containsPerson(HDT_Person person) { return objListNoMod.contains(person); }
  @Override void resolvePointers() throws HDB_InternalError  { db.resolvePointersByRelation(rtAuthorOfWork, work); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public RecordAuthor get(int ndx)
  {
    return allRecords ? new RecordAuthor(work, objList.get(ndx)) : authorList.get(ndx);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override void addNoMod(HDT_Person person, Map<Tag, HDI_OfflineBase> tagToNestedItem) throws RelationCycleException, HDB_InternalError
  {
    objListNoMod.add(person);
    objListNoMod.throwLastException();

    if (tagToNestedItem != null)
      for (Entry<Tag, HDI_OfflineBase> entry : tagToNestedItem.entrySet())
        db.setNestedItemFromOfflineValue(work, person, entry.getKey(), entry.getValue());

    if (allRecords)
      return;

    authorList.add(new RecordAuthor(work, person));
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
          case tagInFileName : inFileName = ((HDI_OfflineTernary) entry.getValue()).get(); break;
          case tagEditor     : editor     = ((HDI_OfflineBoolean) entry.getValue()).get(); break;
          case tagTranslator : translator = ((HDI_OfflineBoolean) entry.getValue()).get(); break;
          default            :                                                             break;
        }
      }
    }

    authorList.add(new RecordAuthor(work, name, editor, translator, inFileName));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initAuthorList()
  {
    authorList.clear();
    allRecords = false;

    objListNoMod.forEach(person -> authorList.add(new RecordAuthor(work, person)));
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

  public RecordAuthor getAuthor(PersonName personName)
  {
    return allRecords ? null : findFirst(authorList, author -> isNull(author.getPerson()) && author.getName().equals(personName));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update(List<ObjectGroup> objGroups)
  {
    try { db.updateObjectGroups(rtAuthorOfWork, work, objGroups); } catch (RelationCycleException e) { throw newAssertionError(e); }
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
      getAuthorsFromObjectGroups(objGroups, work).forEach(authorList::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void update(RecordAuthor oldAuthor, RecordAuthor newAuthor)
  {
    if (oldAuthor.equals(newAuthor)) return;

    int ndx = authorList.indexOf(oldAuthor);

    if ((ndx == -1) || (oldAuthor.getPerson() != null) || (newAuthor.getPerson() != null) || (oldAuthor.getWork() != work) || (newAuthor.getWork() != work))
    {
      internalErrorPopup(38891);
      return;
    }

    authorList.set(ndx, newAuthor);

    work.modifyNow();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Stream<RecordAuthor> getAuthorsFromObjectGroups(Iterable<ObjectGroup> objGroups, HDT_Work work)
  {
    return iterableToStream(objGroups).map(objGroup ->
      objGroup.getPrimary() != null ?
        new RecordAuthor
        (
          work,
          objGroup.getPrimary()
        )
      :
        new RecordAuthor
        (
          work,
          new PersonName(objGroup.getPrimaryStr()),
          objGroup.getValue(tagEditor).bool,
          objGroup.getValue(tagTranslator).bool,
          objGroup.getValue(tagInFileName).ternary
        ));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void ensurePresent(ObjectGroup objGroup, Tag tag)
  {
    if (objGroup.getValue(tag) != null) return;

    RecordAuthor author = getAuthor(new PersonName(objGroup.getPrimaryStr()));
    if (author == null) return;

    NestedValue val = new NestedValue(db.getNestedSchema(rtAuthorOfWork, tag).category());

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

  public void add(RecordAuthor author)
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

    authorList.add(new RecordAuthor(work, name, isEditor, isTrans, inFileName));
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
      authorList.add(new RecordAuthor(work, person));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAuthorRecord(RecordAuthor oldAuthor, HDT_Person person)
  {
    if (oldAuthor.getPerson() != null)
    {
      internalErrorPopup(73222);
      return;
    }

    HDT_Person insertAfter = null;

    for (RecordAuthor author : authorList)
    {
      HDT_Person curPerson = author.getPerson();
      if (curPerson != null) insertAfter = curPerson;

      if (author == oldAuthor)
      {
        if (objListNoMod.contains(person))
        {
          authorList.remove(oldAuthor);
          return;
        }

        objListNoMod.add(insertAfter == null ? 0 : objListNoMod.indexOf(insertAfter) + 1, person);

        work.setPersonIsEditor    (person, author.getIsEditor  ());
        work.setPersonIsTranslator(person, author.getIsTrans   ());
        work.setPersonIsInFileName(person, author.getInFileName());

        RecordAuthor newAuthor = new RecordAuthor(work, person);

        authorList.set(authorList.indexOf(oldAuthor), newAuthor);

        return;
      }
    }

    internalErrorPopup(73223);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setAll(BibAuthors bibAuthors)
  {
    clear();

    if (BibAuthors.isEmpty(bibAuthors)) return;

    bibAuthors.getListForWorkMerge(work).forEach(bibAuthor ->
    {
      if (bibAuthor.getPerson() == null)
        add(bibAuthor.getName(), bibAuthor.getIsEditor(), bibAuthor.getIsTrans(), Ternary.Unset);
      else
        add(bibAuthor.getPerson(), bibAuthor.getIsEditor(), bibAuthor.getIsTrans(), Ternary.Unset);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
