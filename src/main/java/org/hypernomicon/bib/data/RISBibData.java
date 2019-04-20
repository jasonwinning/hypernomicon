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
import static org.hypernomicon.util.Util.ultraTrim;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.PersonName;

public class RISBibData extends BibDataStandalone
{
  private class RISException extends Exception { private static final long serialVersionUID = -5457122244377661495L; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private RISBibData(List<String> lines) throws RISException
  {
    super();

    boolean gotType = false;

    for (String line : lines)
    {
      if (line.isEmpty() || line.equals("") || line.matches("^\\s*$"))
        continue;

      extractDOIandISBNs(line);

      Pattern p = Pattern.compile("^([A-Z][A-Z0-9])  -(.*)$");
      Matcher m = p.matcher(line);

      if (m.matches())
      {
        String tag = m.group(1),
               val = m.group(2).trim();

        if ( ! (gotType || tag.equals("TY")))
          throw new RISException();

        switch (tag)
        {
          case "DO": break; // DOI was captured already

          case "ER":

            if (gotType) return;
            throw new RISException();

          case "TY": setEntryType(parseRISType(val)); gotType = true; break;

          case "A1": case "A2": case "A3": case "A4": case "AU":

            getAuthors().add(AuthorType.author, new PersonName(val)); break;

          case "ED":

            getAuthors().add(AuthorType.editor, new PersonName(val)); break;

          case "CY": case "PP":

            setStr(bfPubLoc, val); break;

          case "DA": setYear(val, ytCopyright); break;
          case "PY": setYear(val, ytPublicationDate); break;
          case "Y1": setYear(val, ytCoverDisplayDate); break;
          case "Y2": setYear(val, ytCreated); break;

          case "OP": break;    // Original Publication
          case "RP": break;    // Reprint Edition

          case "ET": setStr(bfEdition, val); break;
          case "IS": setStr(bfIssue, val); break;

          case "JF": case "JO":

            addStr(bfContainerTitle, val); break;

          case "L1": case "L2": case "LK": case "UR":

            setStr(bfURL, val); break;

          case "LA": setStr(bfLanguage, val); break;
          case "PB": setStr(bfPublisher, val); break;


          case "SE": break;    // Section

          case "SP":

            String pages = getStr(bfPages);
            setStr(bfPages, pages.length() == 0 ? val : (val + "-" + pages));
            break;

          case "EP":

            pages = getStr(bfPages);
            setStr(bfPages, pages.length() == 0 ? val : (pages + "-" + val));
            break;

          case "TI": case "TT": case "T1": case "T2": case "T3":

            addStr(bfTitle, val);
            break;

          case "VL": case "VO":

            setStr(bfVolume, val); break;

          default :

            addStr(bfMisc, val); break;
        }
      }
      else if (ultraTrim(line).equals("ER"))
      {
        if (gotType) return;
        throw new RISException();
      }
    }

    throw new RISException(); // It has to end with "ER"; otherwise malformed
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData create(List<String> lines)
  {
    try
    {
      return new RISBibData(lines);
    }
    catch (RISException e)
    {
      return null;
    }

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseRISType(String risType)
  {
    switch (risType)
    {
      case "ABST"    : return etAbstract;
      case "ADVS"    : return etAudiovisualMaterial;
      case "AGGR"    : return etAggregatedDatabase;
      case "ANCIENT" : return etAncientText;
      case "ART"     : return etArtwork;
      case "BILL"    : return etBill;
      case "BLOG"    : return etBlogPost;
      case "BOOK"    : return etBook;
      case "CASE"    : return etCase;
      case "CHAP"    : return etBookChapter;
      case "CHART"   : return etChart;
      case "CLSWK"   : return etClassicalWork;
      case "COMP"    : return etSoftware;
      case "CONF"    : return etConferenceProceedings;
      case "CPAPER"  : return etConferencePaper;
      case "CTLG"    : return etCatalog;
      case "DATA"    : return etDataFile;
      case "DBASE"   : return etOnlineDatabase;
      case "DICT"    : return etDictionaryEntry;
      case "EBOOK"   : return etElectronicBook;
      case "ECHAP"   : return etElectronicBookSection;
      case "EDBOOK"  : return etEditedBook;
      case "EJOUR"   : return etElectronicArticle;
      case "ELEC"    : return etWebPage;
      case "ENCYC"   : return etEncyclopediaArticle;
      case "EQUA"    : return etEquation;
      case "FIGURE"  : return etFigure;
      case "GEN"     : return etUnentered;
      case "GOVDOC"  : return etGovernmentDocument;
      case "GRANT"   : return etGrant;
      case "HEAR"    : return etHearing;
      case "ICOMM"   : return etInternetCommunication;
      case "INPR"    : return etInPress;
      case "JFULL"   : return etJournal;
      case "JOUR"    : return etJournalArticle;
      case "LEGAL"   : return etRuling;
      case "MANSCPT" : return etManuscript;
      case "MAP"     : return etMap;
      case "MGZN"    : return etMagazineArticle;
      case "MPCT"    : return etFilm;
      case "MULTI"   : return etOnlineMultimedia;
      case "MUSIC"   : return etMusicScore;
      case "NEWS"    : return etNewspaperArticle;
      case "PAMP"    : return etPamphlet;
      case "PAT"     : return etPatent;
      case "PCOMM"   : return etPersonalCommunication;
      case "RPRT"    : return etReport;
      case "SER"     : return etSerialPublication;
      case "SLIDE"   : return etSlide;
      case "SOUND"   : return etAudioRecording;
      case "STAND"   : return etStandard;
      case "STAT"    : return etStatute;
      case "THES"    : return etThesis;
      case "UNPB"    : return etUnpublishedWork;
      case "VIDEO"   : return etVideoRecording;

      default        : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
