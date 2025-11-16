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

import java.util.List;

import org.hypernomicon.model.HDI_Schema;
import org.hypernomicon.model.Tag;
import org.hypernomicon.model.records.HDT_Record;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class HDI_OnlineNestedPointer extends HDI_OnlineBase<HDI_OfflineNestedPointer>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private HDT_Record target;

//---------------------------------------------------------------------------

  public HDI_OnlineNestedPointer(HDI_Schema schema, HDT_Record record)
  {
    super(schema, record);
    target = null;
  }

//---------------------------------------------------------------------------

  public HDT_Record get()            { return target; }
  public void set(HDT_Record target) { this.target = target; }

  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items) { return nullSwitch(target, "", HDT_Record::defaultCellText); }
  @Override public int getResultCount(Tag tag)                                 { return target == null ? 0 : 1; }
  @Override public void expire()                                               { target = null; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void resolvePointers()
  {
    if (HDT_Record.isEmpty(target, false))
      target = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setFromOfflineValue(HDI_OfflineNestedPointer val, Tag tag)
  {
    target = val.objID < 0 ? null : db.records(nestedTargetType()).getByID(val.objID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getToOfflineValue(HDI_OfflineNestedPointer val, Tag tag)
  {
    val.objID = -1;

    if (HDT_Record.isEmpty(target, false) == false)
      val.objID = target.getID();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * {@inheritDoc}
   */
  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords, boolean engChar)
  {
    if ((searchLinkedRecords == false) || (target == null))
      return;

    String str = target.defaultCellText();

    if (strNotNullOrEmpty(str))
      list.add(engChar ? convertToEnglishChars(str) : str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
