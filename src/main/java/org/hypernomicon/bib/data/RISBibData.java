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
import static org.hypernomicon.util.Util.*;

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

          case "SP": setStartPage(val); break;
          case "EP": setEndPage  (val); break;

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
    try { return new RISBibData(lines); }
    catch (RISException e) { return null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseRISType(String risType)
  {
    switch (risType)
    {
      case "ABST"    : return etAbstract;                  case "GOVDOC"  : return etGovernmentDocument;
      case "ADVS"    : return etAudiovisualMaterial;       case "GRANT"   : return etGrant;
      case "AGGR"    : return etAggregatedDatabase;        case "HEAR"    : return etHearing;
      case "ANCIENT" : return etAncientText;               case "ICOMM"   : return etInternetCommunication;
      case "ART"     : return etArtwork;                   case "INPR"    : return etInPress;
      case "BILL"    : return etBill;                      case "JFULL"   : return etJournal;
      case "BLOG"    : return etBlogPost;                  case "JOUR"    : return etJournalArticle;
      case "BOOK"    : return etBook;                      case "LEGAL"   : return etRuling;
      case "CASE"    : return etCase;                      case "MANSCPT" : return etManuscript;
      case "CHAP"    : return etBookChapter;               case "MAP"     : return etMap;
      case "CHART"   : return etChart;                     case "MGZN"    : return etMagazineArticle;
      case "CLSWK"   : return etClassicalWork;             case "MPCT"    : return etFilm;
      case "COMP"    : return etSoftware;                  case "MULTI"   : return etOnlineMultimedia;
      case "CONF"    : return etConferenceProceedings;     case "MUSIC"   : return etMusicScore;
      case "CPAPER"  : return etConferencePaper;           case "NEWS"    : return etNewspaperArticle;
      case "CTLG"    : return etCatalog;                   case "PAMP"    : return etPamphlet;
      case "DATA"    : return etDataFile;                  case "PAT"     : return etPatent;
      case "DBASE"   : return etOnlineDatabase;            case "PCOMM"   : return etPersonalCommunication;
      case "DICT"    : return etDictionaryEntry;           case "RPRT"    : return etReport;
      case "EBOOK"   : return etElectronicBook;            case "SER"     : return etSerialPublication;
      case "ECHAP"   : return etElectronicBookSection;     case "SLIDE"   : return etSlide;
      case "EDBOOK"  : return etEditedBook;                case "SOUND"   : return etAudioRecording;
      case "EJOUR"   : return etElectronicArticle;         case "STAND"   : return etStandard;
      case "ELEC"    : return etWebPage;                   case "STAT"    : return etStatute;
      case "ENCYC"   : return etEncyclopediaArticle;       case "THES"    : return etThesis;
      case "EQUA"    : return etEquation;                  case "UNPB"    : return etUnpublishedWork;
      case "FIGURE"  : return etFigure;                    case "VIDEO"   : return etVideoRecording;
      case "GEN"     : return etUnentered;                 default        : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
