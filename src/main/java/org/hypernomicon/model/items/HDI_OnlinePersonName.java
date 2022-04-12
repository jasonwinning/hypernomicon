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

import static org.hypernomicon.model.Tag.*;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Person;

public class HDI_OnlinePersonName extends HDI_OnlineBase<HDI_OfflinePersonName>
{
  private final HDT_Person person;

  public HDI_OnlinePersonName(HDI_Schema schema, HDT_Person person)
  {
    super(schema, person);

    this.person = person;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflinePersonName val, Tag tag) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    if (tag == tagLastName)
      list.add(person.listName());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    return tag == tagFirstName ? person.getFirstName() : person.getLastName();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflinePersonName val, Tag tag)
  {
    if (tag == tagFirstName) val.firstName = person.getFirstName();
    else                     val.lastName  = person.getLastName();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
