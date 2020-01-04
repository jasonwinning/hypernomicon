/*
 * Copyright 2015-2020 Jason Winning
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
import org.hypernomicon.util.json.JsonObj;

public class ZoteroCollection extends BibCollection implements ZoteroEntity
{
  public ZoteroCollection(JsonObj jObj)
  {
    update(jObj, false, false);
  }

  @Override public boolean isSynced() { return true; }
  @Override public String getName()   { return jObj.getObj("data").getStr("name"); }
  @Override public String getKey()    { return jObj.getStr("key"); }
  @Override public long getVersion()  { return jObj.getLong("version", 0); }

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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
