/*
 * Copyright 2015-2026 Jason Winning
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
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Concept;

//---------------------------------------------------------------------------

public class HDI_OnlineString extends HDI_OnlineBase<HDI_OfflineString>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String strValue = "";
  private final Tag tag;

//---------------------------------------------------------------------------

  public HDI_OnlineString(HDI_Schema schema, HDT_Record record)
  {
    super(schema, record);
    tag = schema.tags().getFirst();
  }

//---------------------------------------------------------------------------

  public void set(String strValue) { this.strValue = strValue; }

  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items) { return convertToSingleLine(get()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * {@inheritDoc}
   */
  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords, boolean engChar)
  {
    list.add(engChar ? convertToEnglishChars(get()) : get());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int getResultCount(Tag tag)
  {
    return (tag == tagISBN) ?
      matchISBN(get()).size()
    :
      (strNullOrBlank(get()) ? 0 : 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String get()
  {
    return (record.getType() == hdtConcept) && (tag == hdtConcept.getNameTag()) ?
      ((HDT_Concept)record).term.get().name()
    :
      strValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineString val, Tag tag)
  {
    strValue = val.get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineString val, Tag tag)
  {
    if (tag == record.getType().getNameTag()) val.set(record.name());
    else                                      val.set(strValue);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
