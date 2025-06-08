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

package org.hypernomicon.bib.zotero;

import java.util.*;
import java.util.Map.Entry;

import com.google.common.collect.ImmutableTable;

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

//---------------------------------------------------------------------------

public class ZoteroAuthors extends BibAuthors
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final ImmutableTable<EntryType, String, AuthorType> creatorTypes = buildCreatorTypes();

  private final JsonArray creatorsArr;
  private final EntryType entryType;

//---------------------------------------------------------------------------

  ZoteroAuthors(JsonArray creatorsArr, EntryType entryType)
  {
    this.creatorsArr = creatorsArr;
    this.entryType = entryType;
  }

  ZoteroAuthors(Iterable<BibAuthor> otherAuthors, EntryType entryType)
  {
    creatorsArr = new JsonArray();
    this.entryType = entryType;

    setAll(otherAuthors);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean ignoreEditors() { return switch (entryType)
  {
    case etBookChapter, etEncyclopediaArticle, etConferencePaper, etDictionaryEntry

      -> true; // Zotero stores the editor of work X's parent as the editor of X. Hypernomicon doesn't do that.
               // The best way to avoid most problems that can result from this is probably for Hypernomicon to just ignore
               // editors for work types where the parent's editor will often appear in the child's bibliography entry.
    default

      -> false;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Iterator<BibAuthor> iterator()
  {
    return creatorsArr.objStream().map(creatorObj ->
    {
      AuthorType aType = getAuthorTypeForStr(creatorObj.getStrSafe("creatorType"));
      if ((aType == null) || ((aType == editor) && ignoreEditors())) return null;

      String firstName = creatorObj.getStrSafe("firstName"),
             lastName  = creatorObj.getStrSafe("lastName");

       return new BibAuthor(aType, strNotNullOrBlank(firstName) || strNotNullOrBlank(lastName) ?
         new PersonName(firstName, lastName)
       :
         new PersonName(creatorObj.getStrSafe("name")));

    }).filter(Objects::nonNull).iterator();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setAll(Iterable<BibAuthor> otherAuthors)
  {
    int nextInsertNdx = 0;

    for (BibAuthor otherAuthor : otherAuthors)
    {
      if (otherAuthor.getIsAuthor())
        nextInsertNdx = add(otherAuthor, nextInsertNdx);

      if (otherAuthor.getIsEditor())
        nextInsertNdx = add(new BibAuthor(otherAuthor.getName(), otherAuthor.getPerson(), true, false), nextInsertNdx);

      if (otherAuthor.getIsTrans())
        nextInsertNdx = add(new BibAuthor(otherAuthor.getName(), otherAuthor.getPerson(), false, true), nextInsertNdx);
    }

    // Now remove any authors that do map to a Hypernomicon author type, unless it
    // is an editor and ignoreEditors is true, after the last one that was added

    while (nextInsertNdx < creatorsArr.size())
    {
      if (dontOverwriteCreator(creatorsArr.getObj(nextInsertNdx)))
        nextInsertNdx++;
      else
        creatorsArr.remove(nextInsertNdx);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int add(BibAuthor bibAuthor, int nextInsertNdx)
  {
    AuthorType authorType;

    if (bibAuthor.getIsEditor())
    {
      if (bibAuthor.getIsTrans()) throw newAssertionError(84317);
      if (ignoreEditors()) return nextInsertNdx;

      authorType = editor;
    }
    else if (bibAuthor.getIsTrans()) authorType = translator;
    else                             authorType = author;

    String creatorTypeStr = getCreatorTypeStr(authorType);
    if (strNullOrBlank(creatorTypeStr)) return nextInsertNdx;

    JsonObj creatorObj = new JsonObj();
    creatorObj.put("creatorType", creatorTypeStr);

    creatorObj.put("firstName", removeAllParentheticals(bibAuthor.getGiven()));
    creatorObj.put("lastName", bibAuthor.getFamily());

    // Leave the authors that don't map to a Hypernomicon author type, and editors if
    // ignoreEditors is true, in the same positions as before

    while ((nextInsertNdx < creatorsArr.size()) && dontOverwriteCreator(creatorsArr.getObj(nextInsertNdx)))
      nextInsertNdx++;

    if (nextInsertNdx < creatorsArr.size())
      creatorsArr.set(nextInsertNdx, creatorObj);
    else
      creatorsArr.add(creatorObj);

    return nextInsertNdx + 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean dontOverwriteCreator(JsonObj creatorObj)
  {
    AuthorType authorType = getAuthorTypeForStr(creatorObj.getStrSafe("creatorType"));

    return (authorType == null) || ((authorType == editor) && ignoreEditors());
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

  private static String getCreatorTypeStr(EntryType entryType, AuthorType authorType)
  {
    if (authorType == null) return "";

    return findFirst(creatorTypes.row(entryType).entrySet(), ent -> ent.getValue() == authorType, "", Entry::getKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This a table that maps Zotero creator type to Hypernomicon author type for
   * each Zotero item type.
   * <p>
   * Only one Zotero creator type should map to each Hypernomicon author type for
   * any given item type, and only one Hypernomicon author type should be associated
   * with each Zotero creator type for any given item type.
   * <p>
   * Some Zotero creator types will not map to any Hypernomicon author type for
   * a given item type; that is okay, and means that Hypernomicon will ignore
   * those "authors".
   * @return The mapping table
   */
  private static ImmutableTable<EntryType, String, AuthorType> buildCreatorTypes()
  {
    return new ImmutableTable.Builder<EntryType, String, AuthorType>()

      .put(etArtwork, "artist", author)

      .put(etAudioRecording, "performer", author)

      .put(etBill, "sponsor", author)

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

      .put(etDataSet, "author", author)

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

      .put(etPreprint, "author", author)
      .put(etPreprint, "editor", editor)
      .put(etPreprint, "translator", translator)

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

      .put(etStandard, "author", author)

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
