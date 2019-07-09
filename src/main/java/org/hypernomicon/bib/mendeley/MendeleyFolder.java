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

package org.hypernomicon.bib.mendeley;

import java.time.Instant;

import org.hypernomicon.bib.BibCollection;
import org.hypernomicon.util.Util;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public class MendeleyFolder implements MendeleyEntity, BibCollection
{
  private boolean synced;
  private JsonObj jObj;

  public MendeleyFolder(JsonObj jObj)
  {
    update(jObj, false, false);
  }

  @Override public MendeleyEntityType getType() { return MendeleyEntityType.mendeleyFolder; }
  @Override public boolean isSynced()           { return synced; }
  @Override public String getCollectionKey()    { return getKey(); }
  @Override public String getName()             { return jObj.getStr("name"); }
  @Override public String getKey()              { return jObj.getStr("id"); }
  @Override public Instant lastModified()       { return Util.parseIso8601(jObj.getStr("modified")); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getParentKey()
  {
    String parentKey = jObj.getStrSafe("parent_id");
    return parentKey.length() > 0 ? parentKey : null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge)
  {
    this.jObj = jObj;
    if (jObj.containsKey("synced") == false) return;

    synced = jObj.getBoolean("synced", false);
    jObj.remove("synced");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToDisk(JsonArray jArr)
  {
    JsonObj jDiskObj = jObj.clone();
    jDiskObj.put("synced", (isSynced() ? "true" : "false"));
    jArr.add(jDiskObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
