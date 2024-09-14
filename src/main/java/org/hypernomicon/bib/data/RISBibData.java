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

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.PersonName;

//---------------------------------------------------------------------------

public final class RISBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class RISException extends Exception { @java.io.Serial private static final long serialVersionUID = -5457122244377661495L; }

//---------------------------------------------------------------------------

  private RISBibData(Iterable<String> lines) throws RISException
  {
    String jf = "", jo = "", t2 = "", singleTitle = "";
    boolean gotType = false;

    for (String line : lines)
    {
      if (line.isEmpty() || line.matches("^\\s*$"))
        continue;

      extractDOIandISBNs(line);

      Pattern p = Pattern.compile("^([A-Z][A-Z0-9])  -(.*)$");
      Matcher m = p.matcher(line);

      if (m.matches())
      {
        String tag = m.group(1),
               val = m.group(2).trim();

        if ((gotType == false) && ("TY".equals(tag) == false))
          throw new RISException();

        switch (tag)
        {
          case "DO": break; // DOI was captured already

          case "ER":

            setJournalTitle(jf, jo, t2, singleTitle);
            return;

          case "TY": setEntryType(parseRISType(val)); gotType = true; break;

          case "A1": case "A2": case "A3": case "A4": case "AU":

            getAuthors().add(AuthorType.author, new PersonName(val)); break;

          case "ED":

            getAuthors().add(AuthorType.editor, new PersonName(val)); break;

          case "CY": case "PP":

            setStr(bfPubLoc, val); break;

          case "DA": setDate(parseDate(val), dtCopyright       , true); break;
          case "PY": setDate(parseDate(val), dtPublicationDate , true); break;
          case "Y1": setDate(parseDate(val), dtCoverDisplayDate, true); break;
          case "Y2": setDate(parseDate(val), dtCreated         , true); break;

          case "OP": break;    // Original Publication
          case "RP": break;    // Reprint Edition
          case "ID": break;    // "Reference ID" (usually author last name and year with no space in between)

          case "ET": setStr(bfEdition, val); break;
          case "IS": setStr(bfIssue, val); break;

          case "JF": jf = val; break;
          case "JO": jo = val; break;

          case "L1": case "L2": case "LK": case "UR":

            setStr(bfURL, val); break;

          case "LA": setStr(bfLanguage, val); break;
          case "PB": setStr(bfPublisher, val); break;

          case "SE": break;    // Section

          case "SP": setStartPage(val); break;
          case "EP": setEndPage  (val); break;

          case "TI": case "TT": case "T1":

            singleTitle = val;
            addStr(bfTitle, val);
            break;

          case "T2":

            t2 = val;
            addStr(bfTitle, val);
            break;

          case "T3":

            addStr(bfTitle, val);
            break;

          case "VL": case "VO":

            setStr(bfVolume, val); break;

          case "SN": addISSN(val); break;

          default :

            addStr(bfMisc, val); break;
        }
      }
      else if ("ER".equals(ultraTrim(line)))
      {
        if (gotType)
        {
          setJournalTitle(jf, jo, t2, singleTitle);
          return;
        }

        throw new RISException();
      }
    }

    throw new RISException(); // It has to end with "ER"; otherwise malformed
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern risDatePattern = Pattern.compile("^(\\d+)(?:\\/(\\d{0,2})(?:\\/(\\d{0,2})(?:(?:\\/|\\s)([^/]*))?)?)?$");

  private static BibliographicDate parseDate(String str)
  {
    // First, pattern match on entire string
    Matcher m = risDatePattern.matcher(str);

    boolean matched = m.find();

    String group1 = "", group2 = "", group3 = "";

    if (matched)
    {
      group1 = safeStr(m.group(1));
      group2 = safeStr(m.group(2));
      group3 = safeStr(m.group(3));

      return new BibliographicDate(parseInt(group3, 0), parseInt(group2, 0), group1, false);
    }

    return BibliographicDate.fromUserStr(str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setJournalTitle(String jf, String jo, String t2, String singleTitle)
  {
    if (getEntryType() != etJournalArticle)
      return;

    String containerTitle;

    if (safeStr(jf).isBlank() == false)
      containerTitle = jf;
    else if (safeStr(jo).isBlank() == false)
      containerTitle = jo;
    else
    {
      containerTitle = safeStr(t2);
      setMultiStr(bfTitle, safeListOf(singleTitle));
    }

    setMultiStr(bfContainerTitle, safeListOf(containerTitle));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static RISBibData create(List<String> lines)
  {
    try { return new RISBibData(lines); }
    catch (RISException e) { return null; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EntryType parseRISType(String risType) { return switch (risType)
  {
    case "ABST"    -> etAbstract;                  case "GOVDOC"  -> etGovernmentDocument;
    case "ADVS"    -> etAudiovisualMaterial;       case "GRANT"   -> etGrant;
    case "AGGR"    -> etAggregatedDatabase;        case "HEAR"    -> etHearing;
    case "ANCIENT" -> etAncientText;               case "ICOMM"   -> etInternetCommunication;
    case "ART"     -> etArtwork;                   case "INPR"    -> etInPress;
    case "BILL"    -> etBill;                      case "JFULL"   -> etJournal;
    case "BLOG"    -> etBlogPost;                  case "JOUR"    -> etJournalArticle;
    case "BOOK"    -> etBook;                      case "LEGAL"   -> etRuling;
    case "CASE"    -> etCase;                      case "MANSCPT" -> etManuscript;
    case "CHAP"    -> etBookChapter;               case "MAP"     -> etMap;
    case "CHART"   -> etChart;                     case "MGZN"    -> etMagazineArticle;
    case "CLSWK"   -> etClassicalWork;             case "MPCT"    -> etFilm;
    case "COMP"    -> etSoftware;                  case "MULTI"   -> etOnlineMultimedia;
    case "CONF"    -> etConferenceProceedings;     case "MUSIC"   -> etMusicScore;
    case "CPAPER"  -> etConferencePaper;           case "NEWS"    -> etNewspaperArticle;
    case "CTLG"    -> etCatalog;                   case "PAMP"    -> etPamphlet;
    case "DATA"    -> etDataFile;                  case "PAT"     -> etPatent;
    case "DBASE"   -> etOnlineDatabase;            case "PCOMM"   -> etPersonalCommunication;
    case "DICT"    -> etDictionaryEntry;           case "RPRT"    -> etReport;
    case "EBOOK"   -> etElectronicBook;            case "SER"     -> etSerialPublication;
    case "ECHAP"   -> etElectronicBookSection;     case "SLIDE"   -> etSlide;
    case "EDBOOK"  -> etEditedBook;                case "SOUND"   -> etAudioRecording;
    case "EJOUR"   -> etElectronicArticle;         case "STAND"   -> etStandard;
    case "ELEC"    -> etWebPage;                   case "STAT"    -> etStatute;
    case "ENCYC"   -> etEncyclopediaArticle;       case "THES"    -> etThesis;
    case "EQUA"    -> etEquation;                  case "UNPB"    -> etUnpublishedWork;
    case "FIGURE"  -> etFigure;                    case "VIDEO"   -> etVideoRecording;
    case "GEN"     -> etUnentered;                 default        -> etOther;
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
