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

import java.util.ArrayList;
import java.util.Map.Entry;

import com.google.common.collect.ImmutableTable;
import com.google.common.collect.Iterators;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.bib.authors.BibAuthor.AuthorType.*;
import static org.hypernomicon.bib.data.EntryType.*;

public class ZoteroAuthors extends BibAuthors
{
  private static final ImmutableTable<EntryType, String, AuthorType> creatorTypes = buildCreatorTypes();

  private final JsonArray creatorsArr;
  private final EntryType entryType;

  ZoteroAuthors(JsonArray creatorsArr, EntryType entryType)
  {
    this.creatorsArr = creatorsArr;
    this.entryType = entryType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clear()
  {
    Iterators.removeIf(creatorsArr.getObjs(), creatorObj ->
    {
      AuthorType aType = getAuthorTypeForStr(creatorObj.getStrSafe("creatorType"));

      boolean keep = (aType == null) || ((aType == editor) && ignoreEditors());

      return keep == false;    // If the creatorType does not map onto a Hypernomicon-aware
    });                        // type (author, editor, or translator), then ignore it
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean ignoreEditors()
  {
    switch (entryType)
    {
      case etBookChapter : case etEncyclopediaArticle : case etConferencePaper : case etDictionaryEntry :

        return true; // Zotero stores the editor of work X's parent as the editor of X. Hypernomicon doesn't do that.
                     // The best way to avoid most problems that can result from this is probably for Hypernomicon to just ignore
                     // editors for work types where the parent's editor will often appear in the child's bibliography entry.
      default :

        return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getLists(ArrayList<BibAuthor> authorList, ArrayList<BibAuthor> editorList, ArrayList<BibAuthor> translatorList)
  {
    authorList.clear();
    editorList.clear();
    translatorList.clear();

    creatorsArr.getObjs().forEach(creatorObj ->
    {
      AuthorType aType = getAuthorTypeForStr(creatorObj.getStrSafe("creatorType"));
      if ((aType == null) || ((aType == editor) && ignoreEditors())) return;

      ArrayList<BibAuthor> list = null;

      switch (aType)
      {
        case author     : list = authorList;     break;
        case editor     : list = editorList;     break;
        case translator : list = translatorList; break;
      }

      String firstName = creatorObj.getStrSafe("firstName"),
             lastName  = creatorObj.getStrSafe("lastName");

      if ((firstName.length() > 0) || (lastName.length() > 0))
        list.add(new BibAuthor(aType, new PersonName(firstName, lastName)));
      else
        list.add(new BibAuthor(aType, new PersonName(creatorObj.getStrSafe("name"))));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void add(BibAuthor bibAuthor)
  {
    AuthorType aType = bibAuthor.getType();
    if ((aType == editor) && ignoreEditors()) return;

    String aTypeStr = getCreatorTypeStr(aType);
    if (safeStr(aTypeStr).length() == 0) return;

    JsonObj creatorObj = new JsonObj();
    creatorObj.put("creatorType", aTypeStr);

    String firstName = bibAuthor.getGiven();

    while (firstName.contains("("))
      firstName = removeFirstParenthetical(firstName);

    creatorObj.put("firstName", firstName);
    creatorObj.put("lastName", bibAuthor.getFamily());

    // Now the new author should be inserted before the authors that don't map to a Hypernomicon author type

    int insertNdx = -1;
    for (int ndx = 0; ndx < creatorsArr.size(); ndx++)
    {
      if (getAuthorTypeForStr(creatorsArr.getObj(ndx).getStrSafe("creatorType")) == null)
      {
        insertNdx = ndx;
        break;
      }
    }

    if (insertNdx == -1)
      creatorsArr.add(creatorObj);
    else
      creatorsArr.add(insertNdx, creatorObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private AuthorType getAuthorTypeForStr(String str)
  {
    return creatorTypes.get(entryType, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getCreatorTypeStr(AuthorType authorType)
  {
    return getCreatorTypeStr(entryType, authorType);
  }

  static String getCreatorTypeStr(EntryType entryType, AuthorType authorType)
  {
    if (authorType == null) return "";

    return findFirst(creatorTypes.row(entryType).entrySet(), ent -> ent.getValue().equals(authorType), "", Entry::getKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ImmutableTable<EntryType, String, AuthorType> buildCreatorTypes()
  {
    return new ImmutableTable.Builder<EntryType, String, AuthorType>()

      .put(etArtwork, "artist", author)

      .put(etAudioRecording, "performer", author)

      .put(etBill, "sponsor", author)
      .put(etBill, "cosponsor", author)

      .put(etBlogPost, "author", author)

      .put(etBook, "author", author)
      .put(etBook, "editor", editor)
      .put(etBook, "translator", translator)

      .put(etBookChapter, "author", author)
      .put(etBookChapter, "editor", editor)
      .put(etBookChapter, "translator", translator)

      .put(etCase, "author", author)

      .put(etConferencePaper, "author", author)
      .put(etConferencePaper, "editor", editor)
      .put(etConferencePaper, "translator", translator)

      .put(etDictionaryEntry, "author", author)
      .put(etDictionaryEntry, "editor", editor)
      .put(etDictionaryEntry, "translator", translator)

      .put(etDocument, "author", author)
      .put(etDocument, "editor", editor)
      .put(etDocument, "translator", translator)

      .put(etEmail, "author", author)

      .put(etEncyclopediaArticle, "author", author)
      .put(etEncyclopediaArticle, "editor", editor)
      .put(etEncyclopediaArticle, "translator", translator)

      .put(etFilm, "director", author)

      .put(etForumPost, "author", author)

      .put(etHearing, "contributor", author)

      .put(etInstantMessage, "author", author)

      .put(etInterview, "interviewee", author)
      .put(etInterview, "translator", translator)

      .put(etJournalArticle, "author", author)
      .put(etJournalArticle, "editor", editor)
      .put(etJournalArticle, "translator", translator)

      .put(etLetter, "author", author)

      .put(etMagazineArticle, "author", author)
      .put(etMagazineArticle, "translator", translator)

      .put(etManuscript, "author", author)
      .put(etManuscript, "translator", translator)

      .put(etMap, "cartographer", author)

      .put(etNewspaperArticle, "author", author)
      .put(etNewspaperArticle, "translator", translator)

      .put(etPatent, "inventor", author)

      .put(etPodcast, "podcaster", author)

      .put(etPresentation, "presenter", author)

      .put(etRadioBroadcast, "producer", author)

      .put(etReport, "author", author)
      .put(etReport, "translator", translator)

      .put(etSoftware, "programmer", author)

      .put(etStatute, "author", author)

      .put(etTVBroadcast, "director", author)

      .put(etThesis, "author", author)

      .put(etVideoRecording, "producer", author)

      .put(etWebPage, "author", author)
      .put(etWebPage, "translator", translator)

      .build();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
