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

package org.hypernomicon.bib.data;

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.items.BibliographicDate.DateType.*;
import static org.hypernomicon.util.Util.*;

import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.PersonName;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.Multimap;

//---------------------------------------------------------------------------

public final class RISBibData extends BibDataStandalone
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class RISException extends Exception { @java.io.Serial private static final long serialVersionUID = -5457122244377661495L; }

//---------------------------------------------------------------------------

  private RISBibData(Iterable<String> lines) throws RISException
  {
    Multimap<String, String> tagsAndValues = ArrayListMultimap.create();

//---------------------------------------------------------------------------
// Parsing loop
//---------------------------------------------------------------------------

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

        if (tagsAndValues.isEmpty() && ("TY".equals(tag) == false))  // TY must always be the first tag
          throw new RISException();

        switch (tag)
        {
          case "TY" :

            if (tagsAndValues.isEmpty() == false)
              throw new RISException();

            setEntryType(parseRISType(val));
            break;

          case "ER" :

            if (tagsAndValues.containsKey("TY") == false)
              throw new RISException();  // ER must always be the last tag, and TY must always be first

            break;

          default :

            break;
        }

        tagsAndValues.put(tag, val);
      }
      else if ("ER".equals(ultraTrim(line)))
      {
        if (tagsAndValues.containsKey("TY") == false)
          throw new RISException();  // ER must always be the last tag, and TY must always be first

        tagsAndValues.put("ER", "");
      }

      if (tagsAndValues.containsKey("ER"))
        break;
    }

    if (tagsAndValues.containsKey("ER") == false)
      throw new RISException(); // It has to end with "ER"; otherwise malformed

//---------------------------------------------------------------------------
// Now actually set the values
//---------------------------------------------------------------------------

    String jf = "", jo = "", t2 = "", singleTitle = "";

    for (Entry<String, String> entry : tagsAndValues.entries())
    {
      String tag = entry.getKey(), val = entry.getValue();

      switch (tag)
      {
        case "DO" : setStr(bfDOI, val); break; // DOI might have been captured already but prefer the explicit DO value

        case "A1" : case "AU" :

          getAuthors().add(AuthorType.author, new PersonName(val)); break;

        case "A2" :

          if (getEntryType().isChild())
            getAuthors().add(AuthorType.editor, new PersonName(val));

          break;

        case "A3" :

          if ((tagsAndValues.containsKey("ED") == false) && (tagsAndValues.containsKey("A2") == false))
            getAuthors().add(AuthorType.editor, new PersonName(val));

          break;

        case "A4" :

          if (getEntryType().isParent())
            getAuthors().add(AuthorType.translator, new PersonName(val));

          break;

        case "ED" :

          getAuthors().add(AuthorType.editor, new PersonName(val)); break;

        case "CY" : case "PP" :

          setStr(bfPubLoc, val); break;

        case "DA" : setDate(parseDate(val), dtCopyright       , true); break;
        case "PY" : setDate(parseDate(val), dtPublicationDate , true); break;
        case "Y1" : setDate(parseDate(val), dtCoverDisplayDate, true); break;
        case "Y2" : setDate(parseDate(val), dtCreated         , true); break;

        case "OP" : break;    // Original Publication
        case "RP" : break;    // Reprint Edition
        case "ID" : break;    // "Reference ID" (usually author last name and year with no space in between)

        case "ET" : setStr(bfEdition, val); break;
        case "IS" : setStr(bfIssue, val); break;

        case "JF" : jf = val; break;
        case "JO" : jo = val; break;

        case "L1" : case "L2" : case "L3" : case "L4" : case "LK" :

          if (tagsAndValues.containsKey("UR") == false)
            setStr(bfURL, val);

          break;

        case "UR" :

          setStr(bfURL, val);
          break;

        case "LA" : setStr(bfLanguage, val); break;
        case "PB" : setStr(bfPublisher, val); break;

        case "SE" : break;    // Section

        case "SP" : setStartPage(val); break;
        case "EP" : setEndPage  (val); break;

        case "TI" : case "T1" :

          singleTitle = val;
          addStr(bfTitle, val);
          break;

        case "T2" :

          t2 = val;
          addStr(bfTitle, val);
          break;

        case "T3" :

          addStr(bfTitle, val);
          break;

        case "VL" : case "VO" :

          setStr(bfVolume, val); break;

        case "SN" : addISSN(val); break;

        case "ER" : case "TY" : break;

        default :

          addStr(bfMisc, val); break;
      }
    }

    if (getEntryType() != etJournalArticle)
    {
      setMultiStr(bfTitle, safeListOf(singleTitle));
      setMultiStr(bfContainerTitle, safeListOf(t2));
      return;
    }

    String containerTitle;

    if (safeStr(t2).isBlank() == false)
    {
      setMultiStr(bfTitle, safeListOf(singleTitle));
      containerTitle = t2;
    }
    else if (safeStr(jf).isBlank() == false)
      containerTitle = jf;
    else
      containerTitle = safeStr(jo);

    setMultiStr(bfContainerTitle, safeListOf(containerTitle));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Pattern risDatePattern = Pattern.compile("^(\\d+)(?:\\/(\\d{0,2})(?:\\/(\\d{0,2})(?:(?:\\/|\\s)([^/]*))?)?)?$");

  private static BibliographicDate parseDate(String str)
  {
    // First, pattern match on entire string
    Matcher m = risDatePattern.matcher(str);

    boolean matched = m.find();

    if (matched)
    {
      String group1 = safeStr(m.group(1)),
             group2 = safeStr(m.group(2)),
             group3 = safeStr(m.group(3));

      return new BibliographicDate(parseInt(group3, 0), parseInt(group2, 0), group1, false);
    }

    return BibliographicDate.fromUserStr(str);
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
    case "ABST"    -> etAbstract;                  case "GRNT", "GRANT" -> etGrant;
    case "ADVS"    -> etAudiovisualMaterial;       case "HEAR"    -> etHearing;
    case "AGGR"    -> etAggregatedDatabase;        case "ICOMM"   -> etInternetCommunication;
    case "ANCIENT" -> etAncientText;               case "INTV"    -> etInterview;
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
    case "DICT"    -> etDictionaryEntry;           case "POD"     -> etPodcast;
    case "EBOOK"   -> etElectronicBook;            case "PRESS"   -> etPressRelease;
    case "ECHAP"   -> etElectronicBookSection;     case "RPRT"    -> etReport;
    case "EDBOOK"  -> etEditedBook;                case "SER"     -> etSerialPublication;
    case "EJOUR"   -> etElectronicArticle;         case "SLIDE"   -> etSlide;
    case "ELEC"    -> etWebPage;                   case "SOUND"   -> etAudioRecording;
    case "ENCYC"   -> etEncyclopediaArticle;       case "STAND"   -> etStandard;
    case "EQUA"    -> etEquation;                  case "STAT"    -> etStatute;
    case "FIGURE"  -> etFigure;                    case "THES"    -> etThesis;
    case "GEN"     -> etUnentered;                 case "UNPB"    -> etUnpublishedWork;
    case "GOVDOC"  -> etGovernmentDocument;        case "VIDEO"   -> etVideoRecording;

    default        -> etOther;
  };}



//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
