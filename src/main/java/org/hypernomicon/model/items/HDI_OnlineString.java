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

import static org.hypernomicon.model.HyperDB.Tag.tagListName;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.HyperDB.Tag;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.HDT_Concept;

public class HDI_OnlineString extends HDI_OnlineBase<HDI_OfflineString>
{
  private String strValue = "";

  public HDI_OnlineString(HDI_Schema newSchema, HDT_Record newRecord)
  {
    super(newSchema, newRecord);
  }

  public void set(String strValue) { this.strValue = strValue; }

  @Override public void getStrings(ArrayList<String> list, Tag tag, boolean searchLinkedRecords) { list.add(get()); }

  @Override public String getResultTextForTag(Tag tag) { return get(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String get()
  {
    if ((record.getType() == hdtConcept) && (mainTag == record.getNameTag()))
      return HDT_Concept.class.cast(record).term.get().name();

    return strValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineString val, Tag tag)
  {
    if (tag == tagListName)  // This is used by HDT_PositionVerdict and HDT_ArgumentVerdict
      strValue = safeStr(val.recordState.listName);
    else
      strValue = val.get();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineString val, Tag tag)
  {
    if (tag == record.getNameTag()) val.strValue = record.name();
    else if (tag == tagListName)    val.recordState.listName = strValue;
    else                            val.strValue = strValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
