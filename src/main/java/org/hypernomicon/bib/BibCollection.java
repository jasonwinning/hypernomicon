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

package org.hypernomicon.bib;

import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.mendeley.MendeleyFolder;
import org.hypernomicon.bib.zotero.ZoteroCollection;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public abstract class BibCollection implements BibEntity
{
  protected JsonObj jObj;

  public abstract String getName();
  public abstract String getParentKey();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToDisk(JsonArray jArr) { jArr.add(jObj.clone()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <Collection_T extends BibCollection> Collection_T create(LibraryType lType, JsonObj jObj)
  {
    switch (lType)
    {
      case ltMendeley : return (Collection_T) new MendeleyFolder  (jObj);
      case ltZotero   : return (Collection_T) new ZoteroCollection(jObj);
      default         : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
