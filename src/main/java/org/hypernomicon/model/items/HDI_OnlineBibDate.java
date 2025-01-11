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

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;

//---------------------------------------------------------------------------

public class HDI_OnlineBibDate extends HDI_OnlineBase<HDI_OfflineBibDate>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibliographicDate value = BibliographicDate.EMPTY_DATE;

//---------------------------------------------------------------------------

  public HDI_OnlineBibDate(HDI_Schema schema, HDT_Record record)
  {
    super(schema, record);
  }

  public BibliographicDate get()            { return value; }
  public void set(BibliographicDate newVal) { value = newVal; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineBibDate val, Tag tag)
  {
    value = val.get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineBibDate val, Tag tag)
  {
    val.set(value);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords)
  {
    list.add(value.displayToUser());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getResultTextForTag(Tag tag)
  {
    return value.displayToUser();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getResultCount(Tag tag)
  {
    return BibliographicDate.isEmpty(value) ? 0 : 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
