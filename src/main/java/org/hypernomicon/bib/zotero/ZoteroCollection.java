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

package org.hypernomicon.bib.zotero;

import org.hypernomicon.bib.BibCollection;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public class ZoteroCollection implements ZoteroEntity, BibCollection
{
  private boolean synced;
  private JsonObj jObj;

  public ZoteroCollection(JsonObj jObj)
  {
    update(jObj, false, false);
  }

  @Override public ZoteroEntityType getType() { return ZoteroEntityType.zoteroCollection; }
  @Override public boolean isSynced()         { return synced; }
  @Override public String getCollectionKey()  { return getKey(); }
  @Override public String getName()           { return jObj.getObj("data").getStr("name"); }
  @Override public String getKey()            { return jObj.getStr("key"); }
  @Override public long getVersion()          { return jObj.getLong("version", 0); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getParentKey()
  {
    String parentKey = jObj.getObj("data").getStrSafe("parentCollection");
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
