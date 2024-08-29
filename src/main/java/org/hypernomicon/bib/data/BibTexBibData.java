/*
 * Copyright 2015-2024 Jason Winning
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
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.util.Util.*;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Map.Entry;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.BibliographicDate;
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

//---------------------------------------------------------------------------

public final class BibTexBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addBibTexAuthor(String val, AuthorType authorType)
  {
    Arrays.stream(val.split("\n")).forEachOrdered(auth ->
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
    LaTeXParser latexParser = new LaTeXParser();
    LaTeXPrinter latexPrinter = new LaTeXPrinter();

    setEntryType(parseBibTexType(entry.getType().getValue()));

    BibliographicDate monthDate = null;
    String yearStr = null;

    for (Entry<Key, Value> mapping : entry.getFields().entrySet())
    {
      String val = safeStr(mapping.getValue().toUserString());

      if (val.indexOf('\\') > -1 || val.indexOf('{') > -1)
        val = latexPrinter.print(latexParser.parse(val));

      extractDOIandISBNs(val);

      switch (mapping.getKey().getValue())
      {
        case "address"   : setStr(bfPubLoc, val); break;
        case "author"    : addBibTexAuthor(val, AuthorType.author); break;
        case "booktitle" : // fall through
        case "journal"   : setMultiStr(bfContainerTitle, safeListOf(val)); break;
        case "edition"   : setStr(bfEdition, val); break;
        case "editor"    : addBibTexAuthor(val, AuthorType.editor); break;
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
        case "month"     : monthDate = BibliographicDate.fromUserStr(val); break;
        case "year"      : yearStr = val; break;

        case "doi" : case "isbn" : case "issn" : break; // captured already

        default          : addStr(bfMisc, mapping.getKey().getValue() + ": " + val); break;
      }
    }

    yearStr = safeStr(yearStr);

    if (BibliographicDate.isEmpty(monthDate) && yearStr.isBlank()) return;

    boolean yearZeroIsOneBC = false;  // I don't know how BibTex stores BC years, this is just a guess.

    setDate(new BibliographicDate(monthDate == null ? 0 : monthDate.day, monthDate == null ? 0 : monthDate.month, yearStr, yearZeroIsOneBC), dtPublicationDate, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibTexBibData create(Iterable<String> lines) throws TokenMgrException, ParseException
  {
    BibTeXParser parser = new BibTeXParser();

    BibTeXDatabase entries = parser.parse(new BufferedReader(new StringReader(String.join("\n", lines))));

    if ((entries == null) || entries.getEntries().isEmpty()) return null;

    BibTeXEntry entry = entries.getEntries().values().iterator().next();

    return new BibTexBibData(entry);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseBibTexType(String btType) { return switch (btType.toLowerCase())
  {
    case "article"       -> etJournalArticle;
    case "book"          -> etBook;
    case "booklet"       -> etBooklet;
    case "conference",
         "inproceedings" -> etConferencePaper;
    case "inbook",
         "incollection"  -> etBookChapter;
    case "manual"        -> etManual;
    case "mastersthesis" -> etMastersThesis;
    case "phdthesis"     -> etDoctoralThesis;
    case "proceedings"   -> etConferenceProceedings;
    case "techreport"    -> etTechnicalReport;
    case "unpublished"   -> etUnpublishedWork;

    default              -> etOther;    // applies to "misc"
  }; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
