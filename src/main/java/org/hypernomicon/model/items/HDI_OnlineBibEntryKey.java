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
import org.hypernomicon.model.records.HDT_Work;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class HDI_OnlineBibEntryKey extends HDI_OnlineBase<HDI_OfflineString>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String keyStr = "";
  private final HDT_Work work;

//---------------------------------------------------------------------------

  public HDI_OnlineBibEntryKey(HDI_Schema schema, HDT_Work work)
  {
    super(schema, work);

    this.work = work;
  }

//---------------------------------------------------------------------------

  public String get() { return keyStr; }

  @Override public void setFromOfflineValue(HDI_OfflineString val, Tag tag)    { set(val.get()); }
  @Override public void getToOfflineValue  (HDI_OfflineString val, Tag tag)    { val.set(keyStr); }

  @Override public void expire()                                               { set(""); }
  @Override public String getResultTextForTag(Tag tag, boolean limitTo20Items) { return keyStr; }
  @Override public int getResultCount(Tag tag)                                 { return strNullOrBlank(keyStr) ? 0 : 1; }

  @Override public void getStrings(List<String> list, Tag tag, boolean searchLinkedRecords) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void set(String newKeyStr)
  {
    newKeyStr = safeStr(newKeyStr);

    if (newKeyStr.equals(keyStr)) return;

    if (keyStr.length() > 0)
      db.handleBibEntryKeyAssocation(keyStr, work, false);

    keyStr = newKeyStr;

    if (keyStr.length() > 0)
      db.handleBibEntryKeyAssocation(keyStr, work, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
