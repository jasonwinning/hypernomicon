/*
 * Copyright 2015-2023 Jason Winning
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

import java.util.List;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.util.Util.*;

public class MendeleyAuthors extends BibAuthors
{
  private final JsonObj jsonObj;
  private final EntryType entryType;

  MendeleyAuthors(JsonObj jsonObj, EntryType entryType)
  {
    this.jsonObj = jsonObj;
    this.entryType = entryType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    JsonArray authorsArr = jsonObj.getArray("authors"),
              editorsArr = jsonObj.getArray("editors"),
              transArr   = jsonObj.getArray("translators");

    if (authorsArr != null) authorsArr.clear();
    if (transArr   != null) transArr  .clear();

    if (ignoreEditors()) return;

    if (editorsArr != null) editorsArr.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean ignoreEditors()
  {
    switch (entryType)
    {
      case etBookChapter : case etEncyclopediaArticle :

        return true; // Mendeley stores the editor of work X's parent as the editor of X. Hypernomicon doesn't do that.
                     // The best way to avoid most problems that can result from this is probably for Hypernomicon to just ignore
                     // editors for work types where the parent's editor will often appear in the child's bibliography entry.
      default :

        return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void getList(JsonArray arr, List<BibAuthor> list, AuthorType aType)
  {
    if ((arr == null) || (ignoreEditors() && (aType == AuthorType.editor))) return;

    arr.getObjs().forEach(jObj ->
    {
      String firstName = jObj.getStrSafe("first_name"),
             lastName  = jObj.getStrSafe("last_name");

      if ((firstName.length() > 0) || (lastName.length() > 0))
        list.add(new BibAuthor(aType, new PersonName(firstName, lastName)));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getLists(List<BibAuthor> authorList, List<BibAuthor> editorList, List<BibAuthor> translatorList)
  {
    authorList    .clear();
    editorList    .clear();
    translatorList.clear();

    getList(jsonObj.getArray("authors"    ), authorList    , AuthorType.author    );
    getList(jsonObj.getArray("editors"    ), editorList    , AuthorType.editor    );
    getList(jsonObj.getArray("translators"), translatorList, AuthorType.translator);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void add(BibAuthor bibAuthor)
  {
    String aTypeStr;

    switch (bibAuthor.getType())
    {
      case author     : aTypeStr = "authors"    ; break;
      case translator : aTypeStr = "translators"; break;

      case editor     :

        if (ignoreEditors()) return;

        aTypeStr = "editors"    ; break;

      default         : return;
    }

    JsonObj personObj = new JsonObj();

    personObj.put("first_name", removeAllParentheticals(bibAuthor.getGiven()));
    personObj.put("last_name", bibAuthor.getFamily());

    JsonArray jArr = jsonObj.getArray(aTypeStr);
    if (jArr == null)
      jsonObj.put(aTypeStr, jArr = new JsonArray());

    jArr.add(personObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
