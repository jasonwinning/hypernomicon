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

package org.hypernomicon.bib.mendeley;

import java.util.*;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import com.google.common.collect.Iterators;

import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

class MendeleyAuthors extends BibAuthors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final JsonObj jsonObj;
  private final EntryType entryType;

//---------------------------------------------------------------------------

  MendeleyAuthors(JsonObj jsonObj, EntryType entryType)
  {
    this.jsonObj = jsonObj;
    this.entryType = entryType;
  }

  MendeleyAuthors(Iterable<BibAuthor> otherAuthors, EntryType entryType)
  {
    jsonObj = new JsonObj();
    this.entryType = entryType;

    setAll(otherAuthors);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean ignoreEditors()
  {
    return ignoreEditors(entryType);
  }

  static boolean ignoreEditors(EntryType entryType)
  {
    return switch (entryType)
    {
      case etBookChapter, etEncyclopediaArticle

        -> true;  // Mendeley stores the editor of work X's parent as the editor of X. Hypernomicon doesn't do that.
                  // The best way to avoid most problems that can result from this is probably for Hypernomicon to just ignore
                  // editors for work types where the parent's editor will often appear in the child's bibliography entry.
      default

        -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private List<BibAuthor> getList(JsonArray arr, AuthorType authorType)
  {
    List<BibAuthor> list = new ArrayList<>();

    if ((arr == null) || (ignoreEditors() && (authorType == AuthorType.editor))) return list;

    arr.getObjs().forEach(jObj ->
    {
      String firstName = jObj.getStrSafe("first_name"),
             lastName  = jObj.getStrSafe("last_name");

      if (strNotNullOrEmpty(firstName) || strNotNullOrEmpty(lastName))
        list.add(new BibAuthor(authorType, new PersonName(firstName, lastName)));
    });

    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<BibAuthor> iterator()
  {
    return Iterators.unmodifiableIterator(Iterators.concat(getList(jsonObj.getArray("authors"    ), AuthorType.author    ).iterator(),
                                                           getList(jsonObj.getArray("editors"    ), AuthorType.editor    ).iterator(),
                                                           getList(jsonObj.getArray("translators"), AuthorType.translator).iterator()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setAll(Iterable<BibAuthor> otherAuthors)
  {
    int nextAuthorInsertNdx = 0,
        nextEditorInsertNdx = 0,
        nextTransInsertNdx  = 0;

    jsonObj.remove("authors");
    jsonObj.remove("translators");

    if (ignoreEditors() == false)
      jsonObj.remove("editors");

    for (BibAuthor otherAuthor : otherAuthors)
    {
      if (otherAuthor.getIsAuthor())
        nextAuthorInsertNdx = add(otherAuthor, nextAuthorInsertNdx);

      if (otherAuthor.getIsEditor())
        nextEditorInsertNdx = add(new BibAuthor(otherAuthor.getName(), otherAuthor.getPerson(), true, false), nextEditorInsertNdx);

      if (otherAuthor.getIsTrans())
        nextTransInsertNdx = add(new BibAuthor(otherAuthor.getName(), otherAuthor.getPerson(), false, true), nextTransInsertNdx);
    }

    // No need to remove anything here since we cleared the author types that are
    // being modified at the start.
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int add(BibAuthor bibAuthor, int nextInsertNdx)
  {
    String authorTypeStr;

    if (bibAuthor.getIsEditor())
    {
      if (bibAuthor.getIsTrans()) throw newAssertionError(84316);
      if (ignoreEditors()) return nextInsertNdx;

      authorTypeStr = "editors";
    }
    else if (bibAuthor.getIsTrans()) authorTypeStr = "translators";
    else                             authorTypeStr = "authors";

    JsonObj personObj = new JsonObj();

    personObj.put("first_name", removeAllParentheticals(bibAuthor.firstName()));
    personObj.put("last_name", bibAuthor.lastName());

    jsonObj.getOrAddArray(authorTypeStr).add(personObj);

    return nextInsertNdx + 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
