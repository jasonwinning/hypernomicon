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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.Exceptions.RelationCycleException;
import org.hypernomicon.model.items.HDI_OfflineAuthors.OfflineAuthor;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.records.HDT_RecordWithAuthors;
import org.hypernomicon.model.relations.NestedValue;

//---------------------------------------------------------------------------

public class HDI_OnlineAuthors extends HDI_OnlineBase<HDI_OfflineAuthors>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HDI_OnlineAuthors(HDI_Schema schema, HDT_RecordWithAuthors<? extends Authors> record)
  {
    super(schema, record);
  }

//---------------------------------------------------------------------------

  private Authors getAuthors() { return ((HDT_RecordWithAuthors<?>) record).getAuthors(); }

  @Override public void expire()                                   { getAuthors().expire(); }
  @Override public void resolvePointers() throws HDB_InternalError { getAuthors().resolvePointers(); }
  @Override public int getResultCount(Tag tag)                     { return getAuthors().size(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (searchLinkedRecords)
      getAuthors().forEach(author -> list.add(author.getNameLastFirst()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    return getAuthors().stream().map(Author::getNameLastFirst)
                                .filter(name -> name.length() > 0)
                                .collect(Collectors.joining("; "));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineAuthors val, Tag tag) throws RelationCycleException, HDB_InternalError
  {
    Authors authors = getAuthors();

    if ((record.getType() == hdtMiscFile) && (authors instanceof WorkAuthors))
      return;

    authors.clearNoMod();

    for (OfflineAuthor offlineAuthor : val.authors)
    {
      int personID = offlineAuthor.personID;

      if (personID > 0)
      {
        HDT_Person person = db.persons.getByID(personID);
        if (person != null)
          authors.addNoMod(person, offlineAuthor.nestedItems);
      }
      else
      {
        if (authors instanceof WorkAuthors workAuthors)
          workAuthors.addNoMod(offlineAuthor.name, offlineAuthor.nestedItems);
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineAuthors val, Tag tag)
  {
    val.authors.clear();

    Authors authors = getAuthors();

    if ((record.getType() == hdtMiscFile) && (authors instanceof WorkAuthors))
      return;

    authors.forEach(author ->
    {
      OfflineAuthor offlineAuthor = new OfflineAuthor();

      offlineAuthor.name = author.getName();
      offlineAuthor.nestedItems = new LinkedHashMap<>();

      HDT_Person person = author.getPerson();

      if (person == null)
      {
        boolean editor = author.getIsEditor();
        if (NestedValue.isEmpty(editor) == false)
        {
          HDI_OfflineBoolean editorItem = new HDI_OfflineBoolean(db.getNestedSchema(rtAuthorOfWork, tagEditor), val.recordState);
          editorItem.boolValue = editor;
          offlineAuthor.nestedItems.put(tagEditor, editorItem);
        }

        boolean trans = author.getIsTrans();
        if (NestedValue.isEmpty(trans) == false)
        {
          HDI_OfflineBoolean transItem = new HDI_OfflineBoolean(db.getNestedSchema(rtAuthorOfWork, tagTranslator), val.recordState);
          transItem.boolValue = trans;
          offlineAuthor.nestedItems.put(tagTranslator, transItem);
        }

        Ternary inFileName = author.getInFileName();
        if (NestedValue.isEmpty(inFileName) == false)
        {
          HDI_OfflineTernary inFileNameItem = new HDI_OfflineTernary(db.getNestedSchema(rtAuthorOfWork, tagInFileName), val.recordState);
          inFileNameItem.value = inFileName;
          offlineAuthor.nestedItems.put(tagInFileName, inFileNameItem);
        }
      }
      else
      {
        offlineAuthor.personID = person.getID();
        db.saveNestedValuesToOfflineMap(record, person, offlineAuthor.nestedItems, val.recordState);
      }

      val.authors.add(offlineAuthor);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
