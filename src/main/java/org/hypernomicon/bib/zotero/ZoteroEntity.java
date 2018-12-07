/*
 * Copyright 2015-2018 Jason Winning
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

import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public interface ZoteroEntity
{
  public static enum ZoteroEntityType
  {
    zoteroItem,
    zoteroTag,
    zoteroCollection
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public ZoteroEntityType getType();
  public void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge);
  public void saveToDisk(JsonArray jArr);
  public boolean isSynced(); 
  public long getVersion();
  public String getKey();
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public static ZoteroEntity create(ZoteroWrapper zWrapper, JsonObj jObj)
  {    
    JsonObj subObj = jObj.getObj("data");
    
    if (subObj.getStrSafe("itemType").equals("attachment") == false)
      return new ZoteroItem(zWrapper, jObj, false);
    
    if (subObj.containsKey("parentCollection"))
      return new ZoteroCollection(jObj);      
    
    return null;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
