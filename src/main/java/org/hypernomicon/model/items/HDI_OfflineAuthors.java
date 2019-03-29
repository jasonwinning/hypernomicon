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

package org.hypernomicon.model.items;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.HDT_RecordState.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_RecordState;
import org.hypernomicon.model.records.HDT_RecordType;

public class HDI_OfflineAuthors extends HDI_OfflineBase
{
  public static class OfflineAuthor
  {
    public int personID = -1;
    public PersonName name = PersonName.EMPTY;
    public Map<Tag, HDI_OfflineBase> nestedItems = null;
  }

  List<OfflineAuthor> authors = new ArrayList<>();

  public HDI_OfflineAuthors(HDI_Schema newSchema, HDT_RecordState recordState)
  {
    super(newSchema, recordState);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromXml(Tag tag, String nodeText, HDT_RecordType objType, int objID, LinkedHashMap<Tag, HDI_OfflineBase> nestedItems)
  {
    if (objID < 1)
    {
      nodeText = ultraTrim(convertToSingleLine(nodeText));
      if (nodeText.length() == 0) return;
    }

    OfflineAuthor author = new OfflineAuthor();

    if (objID > 0)
      author.personID = objID;
    else
      author.name = new PersonName(nodeText);

    if (nestedItems != null)
      if (nestedItems.isEmpty() == false)
        author.nestedItems = nestedItems;

    authors.add(author);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void writeToXml(Tag tag, StringBuilder xml)
  {
    authors.forEach(author ->
    {
      String name;

      if (author.personID > 0)
        name = db.persons.getByID(author.personID).getName().getLastFirst();
      else
        name = author.name.getLastFirst();

      if (author.nestedItems != null)
        writePointerTagWithNestedPointers(xml, tag, author.personID, name, author.nestedItems, true);
      else
        writePointerTag(xml, tag, author.personID, hdtNone, name, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
