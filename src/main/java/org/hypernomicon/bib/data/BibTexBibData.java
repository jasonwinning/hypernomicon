/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.Util.*;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.List;
import java.util.Map.Entry;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.PersonName;
import org.jbibtex.BibTeXDatabase;
import org.jbibtex.BibTeXEntry;
import org.jbibtex.BibTeXParser;
import org.jbibtex.Key;
import org.jbibtex.LaTeXParser;
import org.jbibtex.LaTeXPrinter;
import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;
import org.jbibtex.Value;

public class BibTexBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addBibTexAuthor(String val, AuthorType authorType)
  {
    Arrays.stream(val.split("\n")).forEach(auth ->
    {
      if (auth.startsWith("and "))
        auth = auth.substring(4);

      auth = ultraTrim(auth);

      if (auth.length() > 0)
        authors.add(authorType, new PersonName(auth));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private BibTexBibData(BibTeXEntry entry) throws ParseException
  {
    super();

    LaTeXParser latexParser = new LaTeXParser();
    LaTeXPrinter latexPrinter = new LaTeXPrinter();

    setEntryType(parseBibTexType(entry.getType().getValue()));

    for (Entry<Key, Value> mapping : entry.getFields().entrySet())
    {
      String val = mapping.getValue().toUserString();

      if (val.indexOf('\\') > -1 || val.indexOf('{') > -1)
        val = latexPrinter.print(latexParser.parse(val));

      extractDOIandISBNs(val);

      switch (mapping.getKey().getValue())
      {
        case "address"   : setStr(bfPubLoc, val); break;
        case "author"    : addBibTexAuthor(val, AuthorType.author); break;
        case "booktitle" : setMultiStr(bfContainerTitle, Arrays.asList(val)); break;
        case "edition"   : setStr(bfEdition, val); break;
        case "editor"    : addBibTexAuthor(val, AuthorType.editor); break;
        case "journal"   : setMultiStr(bfContainerTitle, Arrays.asList(val)); break;
        case "language"  : setStr(bfLanguage, val); break;
        case "note"      : addStr(bfMisc, val); break;
        case "number"    : setStr(bfIssue, val); break;
        case "pages"     : setStr(bfPages, val); break;
        case "publisher" : setStr(bfPublisher, val); break;
        case "series"    :

          if (getMultiStr(bfContainerTitle).isEmpty())
            addStr(bfContainerTitle, val);
          break;

        case "title"     : addStr(bfTitle, val); break;
        case "type"      : setEntryType(parseBibTexType(val)); break;
        case "url"       : setStr(bfURL, val); break;
        case "volume"    : setStr(bfVolume, val); break;
        case "year"      : setYear(val, ytPublicationDate); break;

        case "doi" : case "isbn" : case "issn" : break; // captured already

        default          : addStr(bfMisc, mapping.getKey().getValue() + ": " + val); break;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData create(List<String> lines) throws TokenMgrException, ParseException
  {
    BibTeXParser parser = new BibTeXParser();

    BibTeXDatabase entries = parser.parse(new BufferedReader(new StringReader(String.join("\n", lines))));

    if ((entries == null) || entries.getEntries().isEmpty()) return null;

    BibTeXEntry entry = entries.getEntries().values().iterator().next();

    return new BibTexBibData(entry);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseBibTexType(String btType)
  {
    switch (btType.toLowerCase())
    {
      case "article"       : return etJournalArticle;
      case "book"          : return etBook;
      case "booklet"       : return etBooklet;
      case "conference"    : return etConferencePaper;
      case "inbook"        : return etBookChapter;
      case "incollection"  : return etBookChapter;
      case "inproceedings" : return etConferencePaper;
      case "manual"        : return etManual;
      case "mastersthesis" : return etMastersThesis;
      case "misc"          : return etOther;
      case "phdthesis"     : return etDoctoralThesis;
      case "proceedings"   : return etConferenceProceedings;
      case "techreport"    : return etTechnicalReport;
      case "unpublished"   : return etUnpublishedWork;

      default              : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
