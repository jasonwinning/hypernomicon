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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.BibData.YearType.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.HyperDB.Tag.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;

import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Person;
import org.hypernomicon.model.relations.ObjectGroup;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public class GoogleBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static GoogleBibData createFromJSON(JsonObj jsonObj, String queryIsbn)
  {
    try
    {
      jsonObj = jsonObj.getArray("items").getObj(0).getObj("volumeInfo");
    }
    catch (NullPointerException e)
    {
      return null;
    }

    return new GoogleBibData(jsonObj, queryIsbn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private GoogleBibData(JsonObj jsonObj, String queryIsbn)
  {
    super();

    String title    = jsonObj.getStrSafe("title"),
           subtitle = jsonObj.getStrSafe("subtitle");

    addStr(bfTitle, title);

    if (title.equalsIgnoreCase(subtitle) == false)
      addStr(bfTitle, subtitle);

    setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    setEntryType(parseGoogleBooksType(jsonObj.getStrSafe("printType"))); // supposedly this will either be "BOOK" or "MAGAZINE", nothing else

    String publishedDate = jsonObj.getStrSafe(ytPublishedDate.desc);
    if (publishedDate.length() > 0)
      setYear(publishedDate.substring(0, 4), ytPublishedDate);

    BibAuthors authors = getAuthors();
    JsonArray.toStrList(jsonObj.getArray("authors")).forEach(authStr -> authors.add(new BibAuthor(AuthorType.author, new PersonName(authStr))));

    nullSwitch(jsonObj.getArray("industryIdentifiers"), iiArr -> iiArr.getObjs().forEach(iiObj ->
    {
      if (iiObj.getStrSafe("type").toLowerCase().contains("isbn"))
        addISBN(iiObj.getStrSafe("identifier"));
    }));

    if (fieldNotEmpty(bfISBNs) == false)
      addISBN(queryIsbn);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseGoogleBooksType(String gbType)
  {
    switch (gbType)
    {
      case "BOOK"     : return etBook;
      case "MAGAZINE" : return etMagazine;

      default         : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getQueryUrl(String isbn) { return getQueryUrl(null, null, isbn); }

  public static String getQueryUrl(String title, List<ObjectGroup> authGroups, String isbn)
  {
    String url = "https://www.googleapis.com/books/v1/volumes?q=";

    if (isbn.length() > 0)
      return url + escapeURL("isbn:" + isbn, false);

    if (safeStr(title).length() == 0) return url;

    String auths = "", eds = "";
    if (authGroups != null)
    {
      for (ObjectGroup authGroup : authGroups)
      {
        boolean ed = authGroup.getValue(tagEditor).bool,
                tr = authGroup.getValue(tagTranslator).bool;

        HDT_Person person = authGroup.getPrimary();
        String name;

        if (person == null)
          name = convertToEnglishChars(new PersonName(authGroup.getPrimaryStr()).getLast());
        else
          name = person.getLastNameEngChar();

        if (ed)
          eds = eds + (eds.length() > 0 ? "+" : "") + "inauthor:" + name;
        else if (tr == false)
          auths = auths + (auths.length() > 0 ? "+" : "") + "inauthor:" + name;
      }
    }

    if (auths.length() == 0) auths = eds;

    title = convertToEnglishChars(title).trim();
    title = title.replace(":", "");
    title = title.replace("?", "");

    title = title.replace(' ', '+');

    if (title.length() > 0)
      url = url + escapeURL("\"" + title + "\"", false);

    if (auths.length() > 0)
    {
      if (title.length() > 0)
        url = url + "+";

      url = url + escapeURL(auths, false);
    }

    return url;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
