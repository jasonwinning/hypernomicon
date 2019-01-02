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

package org.hypernomicon.bib;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.jbibtex.BibTeXDatabase;
import org.jbibtex.BibTeXEntry;
import org.jbibtex.BibTeXParser;
import org.jbibtex.Key;
import org.jbibtex.LaTeXObject;
import org.jbibtex.LaTeXParser;
import org.jbibtex.LaTeXPrinter;
import org.jbibtex.ParseException;
import org.jbibtex.TokenMgrException;
import org.jbibtex.Value;

import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.bib.BibData.YearType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.bib.BibData.BibFieldType.*;
import static org.hypernomicon.bib.BibUtils.*;
import static org.hypernomicon.bib.BibData.EntryType.*;
import static org.hypernomicon.util.Util.*;

public abstract class BibData
{
  
//---------------------------------------------------------------------------  
  
  public static enum AuthorType
  {
    author, editor, translator
  }
  
//---------------------------------------------------------------------------  
  
  public static enum EntryType
  {
    etJournal, etJournalVolume, etJournalArticle, etJournalIssue, etJournalSection, etElectronicArticle, etInPress,
    etBook, etMonograph, etBookVolume, etBookSection, etBookChapter, etBookPart, etBookSet, etBookTrack, etEditedBook, etBookSeries, etMultiVolumeWork, etBooklet,
    etElectronicBook, etElectronicBookSection,
    etSerialPublication, etMagazine, etMagazineArticle, etNewspaper, etNewspaperArticle, etLetterToTheEditor, etNewsletter, etNewsletterArticle, 
    etWebPage, etPostedContent, etTwitterPost, etFacebookPost, etForumPost, etInstantMessage, etBlogPost, etEmail, etFeedItem, etInternetCommunication,    
    etConference, etConferencePaper, etConferenceProceedings, etPoster, etSymposium, etSymposiumPaper, etSymposiumProceedings, etPresentation,
    etReferenceBook, etReferenceEntry, etDictionaryEntry, etEncyclopediaArticle, etCatalog, etCatalogItem,
    etAncientText, etClassicalWork,
    etCase, etHearing, etStatute, etBill, etRegulation, etRuling, etGrant, etGovernmentDocument, 
    etAudiovisualMaterial, etOnlineMultimedia, etMusicScore, etAudioRecording, etRadioBroadcast, etTVBroadcast, etFilm, etVideoRecording, etPodcast, 
    etPortfolio, etArtwork,
    etIssueBrief, etReportSeries, etReport, etTechnicalReport, etApparatus, etMeasurementInstrument, etStandard, etStandardSeries, etManual, etPatent,
    etThesis, etMastersThesis, etDoctoralThesis, etManuscript, etUnpublishedWork, etWorkingPaper, etUnpublishedRawData, etPersonalCommunication, etDocument, 
    etMap, etChart, etEquation, etFigure, etSlide, etDataSet, etDataFile, etOnlineDatabase, etAggregatedDatabase, etSoftware, etComponent,
    etAbstract, etCommentary, etInterview, etArchivalDocument, etArchivalCollection, etLetter, etPamphlet, etBrochure,
    
    etUnentered,  // This just means the field hasn't been populated yet
    etOther,      // This means it is a type that does not fit into any of the above categories
    etNone        // This means it should be treated as a non-entry
  }
  
//---------------------------------------------------------------------------  
  
  public static enum BibFieldEnum
  {
    bfDOI, bfPublisher, bfPubLoc, bfURL, bfVolume, bfIssue, bfLanguage, bfEdition, bfPages, bfYear, bfAuthors, bfEditors, bfTranslators,
    bfTitle, bfISBNs, bfISSNs, bfContainerTitle, bfMisc, bfEntryType, bfWorkType
  }
  
//---------------------------------------------------------------------------
  
  public static enum BibFieldType
  {
    bftString, bftMultiString, bftEntryType, bftWorkType, bftAuthor
  }
  
//---------------------------------------------------------------------------
  
  private static HashMap<String, YearType> descToYearType = new HashMap<>();
  
  public static enum YearType
  {
    ytUnknown(""), 
    ytCreated("created"),
    ytCopyright("copyright"), 
    ytIssued("issued"), 
    ytPublishedDate("publishedDate"), 
    ytPublicationDate("publicationDate"), 
    ytPublishedPrint("published-print"),
    ytPublicationDisplayDate("publicationDisplayDate"), 
    ytCoverDate("coverDate"), 
    ytCoverDisplayDate("coverDisplayDate");
    
    private String desc;
    
    private YearType(String desc) { this.desc = desc; descToYearType.put(desc, this); }
    
    public String getDesc() { return desc; }
    
    public static YearType getByDesc(String desc) { return descToYearType.getOrDefault(desc, ytUnknown); }
    
    public static YearType highestPriority()
    {
      int ordinal = Integer.MIN_VALUE;
      YearType highestYT = null;
      
      for (YearType yt : EnumSet.allOf(YearType.class))
      {
        if (yt.ordinal() > ordinal)
        {
          highestYT = yt;
          ordinal = yt.ordinal();
        }
      }
      
      return highestYT;
    }
  }
  
//---------------------------------------------------------------------------

  private static final HashMap<BibFieldEnum, String> bibFieldEnumToName;
  protected static final HashMap<BibFieldEnum, BibFieldType> bibFieldEnumToType;
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static
  {
    bibFieldEnumToName = new HashMap<>();
    bibFieldEnumToType = new HashMap<>();
    
    addBibField(bfEntryType,      "Entry Type",                bftEntryType);
    addBibField(bfWorkType,       "Work Type",                 bftWorkType);
    addBibField(bfAuthors,        "Authors",                   bftAuthor);
    addBibField(bfContainerTitle, "Container Title",           bftMultiString); // For articles this is the journal title; for chapters it is the book title
    addBibField(bfDOI,            "DOI",                       bftString);      // Document Object ID only, without "doi:" or url
    addBibField(bfEdition,        "Edition",                   bftString);      // Information about publication edition
    addBibField(bfEditors,        "Editors",                   bftAuthor);
    addBibField(bfISBNs,          "ISBNs",                     bftMultiString); // International standard book numbers
    addBibField(bfISSNs,          "ISSNs",                     bftMultiString); // International standard serial numbers
    addBibField(bfIssue,          "Issue",                     bftString);      // Issue number
    addBibField(bfLanguage,       "Language",                  bftString);      // Language
    addBibField(bfMisc,           "Miscellaneous Information", bftMultiString); // Used internally to hold uncategorized extracted information
    addBibField(bfPages,          "Page Numbers",              bftString);      // Page range
    addBibField(bfPubLoc,         "Publisher Location",        bftString);      // Where published
    addBibField(bfPublisher,      "Publisher",                 bftString);      // May or may not include city
    addBibField(bfTitle,          "Title",                     bftMultiString); // Title of this work
    addBibField(bfTranslators,    "Translators",               bftAuthor);
    addBibField(bfURL,            "URL",                       bftString);      // URL where this work can be found
    addBibField(bfVolume,         "Volume",                    bftString);      // Volume number
    addBibField(bfYear,           "Year",                      bftString);      // Publication year
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addBibField(BibFieldEnum bibFieldEnum, String fieldName, BibFieldType type)
  {
    bibFieldEnumToName.put(bibFieldEnum, fieldName);
    bibFieldEnumToType.put(bibFieldEnum, type);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getFieldName(BibFieldEnum bibFieldEnum)         { return bibFieldEnumToName.get(bibFieldEnum); }
  public static BibFieldType getFieldType(BibFieldEnum bibFieldEnum)   { return bibFieldEnumToType.get(bibFieldEnum); }
  public static boolean bibFieldIsMultiLine(BibFieldEnum bibFieldEnum) { return bibFieldEnumToType.get(bibFieldEnum) == bftMultiString; }
  
  public abstract EntryType getEntryType();                              
  public abstract void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list);
  public abstract void setEntryType(EntryType entryType);
  public abstract void setStr(BibFieldEnum bibFieldEnum, String newStr);
  public abstract List<String> getMultiStr(BibFieldEnum bibFieldEnum);
  public abstract String getStr(BibFieldEnum bibFieldEnum);
  public abstract BibAuthors getAuthors();
  public abstract HDT_Work getWork();
  public abstract boolean linkedToWork();
  public abstract HDT_WorkType getWorkType();
  public abstract void setWorkType(HDT_WorkType workType);
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean entryTypeNotEmpty()                                    
  { 
    EntryType entryType = getEntryType();
    return (entryType != null) && (entryType != etNone) && (entryType != etUnentered); 
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getMultiStrSpaceDelimited(List<String> list)
  {
    String all = "";
    
    for (String one : list)
      all = all + (all.length() > 0 ? " " : "") + ultraTrim(one);
    
    return ultraTrim(all);
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean fieldNotEmpty(BibFieldEnum bibFieldEnum)
  {
    if (bibFieldIsMultiLine(bibFieldEnum))
      return getMultiStr(bibFieldEnum).size() > 0;
    
    return getStr(bibFieldEnum).length() > 0;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  public static HDT_WorkType convertEntryTypeToWorkType(EntryType et)
  {
    switch (et)
    {
      case etBook : case etBooklet: case etBookVolume: case etJournalIssue : case etMagazine : case etManual : case etMonograph :
      case etMultiVolumeWork : case etReferenceBook : case etEditedBook : case etAncientText : case etClassicalWork : case etElectronicBook :
        
        return HDT_WorkType.get(WorkTypeEnum.wtBook);
        
      case etAbstract : case etArchivalDocument : case etBookChapter : case etCommentary : case etConferencePaper :
      case etEncyclopediaArticle : case etJournalArticle : case etLetter : case etLetterToTheEditor :
      case etMagazineArticle : case etNewsletterArticle : case etNewspaperArticle : case etReferenceEntry : case etInPress : case etUnpublishedWork :
      case etReport : case etTechnicalReport : case etDictionaryEntry : case etWorkingPaper : case etElectronicArticle : case etGovernmentDocument :

        return HDT_WorkType.get(WorkTypeEnum.wtPaper);
        
      default : break;
    }
    
    return null;    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EntryType convertWorkTypeToEntryType(WorkTypeEnum workTypeEnum)
  {
    switch (workTypeEnum)
    {
      case wtBook:         return EntryType.etBook;
      case wtChapter:      return EntryType.etBookChapter;
      case wtNone:         return EntryType.etUnentered;
      case wtPaper:        return EntryType.etJournalArticle;
      case wtRecording:    return EntryType.etAudiovisualMaterial;
      case wtUnenteredSet: return EntryType.etNone;
      case wtWebPage:      return EntryType.etWebPage;

      default:             return EntryType.etUnentered;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setTitle(String newTitle)
  {
    setMultiStr(bfTitle, Collections.singletonList(newTitle));
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void addStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    List<String> list = getMultiStr(bibFieldEnum);
    list.add(newStr);
    setMultiStr(bibFieldEnum, list);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public void extractDOIandISBNs(String value)
  {    
    setDOI(value);
    addISBN(value);
    addISSN(value);
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
  
  public void setDOI(String newStr)
  {
    if (safeStr(newStr).length() == 0) return;
    String doi = matchDOI(newStr);
    if (doi.length() > 0)
      setStr(bfDOI, doi);
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void addISBN(String newStr)
  {    
    matchISBN(newStr).forEach(isbn -> addStr(bfISBNs, isbn));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void addISSN(String newStr)
  {
    matchISSN(newStr).forEach(issn -> addStr(bfISSNs, issn));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Authors have to be checked separately
  
  public boolean fieldsAreEqual(BibFieldEnum bibFieldEnum, BibData otherBD)
  {
    if ((fieldNotEmpty(bibFieldEnum) || otherBD.fieldNotEmpty(bibFieldEnum)) == false) return true;
    
    switch (bibFieldEnum)
    {
      case bfDOI       : case bfURL       : case bfVolume    : case bfIssue     : case bfPages     : case bfEntryType :
      case bfPublisher : case bfPubLoc    : case bfEdition   : case bfLanguage  : case bfYear      : case bfWorkType :
        
        if (ultraTrim(getStr(bibFieldEnum)).equals(ultraTrim(otherBD.getStr(bibFieldEnum))) == false) return false;
        break;
        
      case bfContainerTitle: case bfTitle: case bfMisc: case bfISBNs: case bfISSNs:
        
        if (strListsEqual(getMultiStr(bibFieldEnum), otherBD.getMultiStr(bibFieldEnum), false) == false) return false;        
        break;
        
      case bfAuthors: case bfEditors: case bfTranslators:

        break;
    }
    
    return true;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static ArrayList<String> jsonStrList(JsonArray jArr)
  {
    ArrayList<String> list = new ArrayList<>();
    
    if (jArr != null)    
      jArr.getStrs().forEach(list::add);
    
    return list;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData createFromGoogleJSON(JsonObj jsonObj, String queryIsbn)
  {
    try
    {
      jsonObj = jsonObj.getArray("items").getObj(0).getObj("volumeInfo");
    }
    catch (NullPointerException e)
    {
      return null;
    }

    BibDataStandalone bd = new BibDataStandalone();
    
    String title = jsonObj.getStrSafe("title");
    String subtitle = jsonObj.getStrSafe("subtitle");
    
    bd.addStr(bfTitle, title);
    
    if (title.equalsIgnoreCase(subtitle) == false)
      bd.addStr(bfTitle, subtitle);
       
    bd.setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    bd.setEntryType(parseGoogleBooksType(jsonObj.getStrSafe("printType"))); // supposedly this will either be "BOOK" or "MAGAZINE", nothing else
    
    String publishedDate = jsonObj.getStrSafe(ytPublishedDate.getDesc());
    if (publishedDate.length() > 0)
    {
      bd.setYear(publishedDate.substring(0, 4), ytPublishedDate);
    }
    
    ArrayList<String> authors = jsonStrList(jsonObj.getArray("authors"));
    
    for (int ndx = 0; ndx < authors.size(); ndx++)
    {
      BibAuthor author = new BibAuthor(AuthorType.author, new PersonName(authors.get(ndx)));
      bd.getAuthors().add(author);
    }
    
    JsonArray iiArr = jsonObj.getArray("industryIdentifiers");
    
    if (iiArr != null)
    {
      iiArr.getObjs().forEach(iiObj ->
      {
        if (iiObj.getStrSafe("type").toLowerCase().contains("isbn"))
          bd.addISBN(iiObj.getStrSafe("identifier"));
      });
    }
    
    if (bd.fieldNotEmpty(bfISBNs) == false)
      bd.addISBN(queryIsbn);
    
    return bd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData createFromBibTex(List<String> lines) throws TokenMgrException, ParseException
  {
    BibTeXParser parser = new BibTeXParser();
    LaTeXParser latexParser = new LaTeXParser();
    LaTeXPrinter latexPrinter = new LaTeXPrinter();
      
    BibTeXDatabase entries = parser.parse(new BufferedReader(new StringReader(String.join("\n", lines))));
    
    if ((entries == null) || (entries.getEntries().size() == 0)) return null;
    
    BibTeXEntry entry = entries.getEntries().values().iterator().next();
    
    BibDataStandalone bd = new BibDataStandalone();
    
    bd.setEntryType(parseBibTexType(entry.getType().getValue()));
    
    for (Entry<Key, Value> mapping : entry.getFields().entrySet())
    {
      String val = mapping.getValue().toUserString();
      
      if (val.indexOf('\\') > -1 || val.indexOf('{') > -1)
      {        
        List<LaTeXObject> latexObjects = latexParser. parse(val);
        val = latexPrinter.print(latexObjects);
      }
      
      bd.extractDOIandISBNs(val);
      
      switch (mapping.getKey().getValue())
      {
        case "address" :   bd.setStr(bfPubLoc, val); break;
        case "author" :    bd.addBibTexAuthor(val, AuthorType.author); break;
        case "booktitle" : bd.setMultiStr(bfContainerTitle, Collections.singletonList(val)); break;
        case "edition" :   bd.setStr(bfEdition, val); break;
        case "editor" :    bd.addBibTexAuthor(val, AuthorType.editor); break;
        case "journal" :   bd.setMultiStr(bfContainerTitle, Collections.singletonList(val)); break;
        case "language" :  bd.setStr(bfLanguage, val); break;
        case "note" :      bd.addStr(bfMisc, val); break;
        case "number" :    bd.setStr(bfIssue, val); break;
        case "pages" :     bd.setStr(bfPages, val); break;
        case "publisher" : bd.setStr(bfPublisher, val); break;
        case "series" :    
          
          if (bd.getMultiStr(bfContainerTitle).size() == 0) 
            bd.addStr(bfContainerTitle, val); 
          break;
          
        case "title" :     bd.addStr(bfTitle, val); break;
        case "type" :      bd.setEntryType(parseBibTexType(val)); break;          
        case "url" :       bd.setStr(bfURL, val); break;
        case "volume" :    bd.setStr(bfVolume, val); break;
        case "year" :      bd.setYear(val, ytPublicationDate); break;
        
        case "doi" : case "isbn" : case "issn" : break; // captured already
        
        default :          bd.addStr(bfMisc, mapping.getKey().getValue() + ": " + val); break;
      }
    }
    
    return bd;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void addBibTexAuthor(String val, AuthorType authorType)
  {
    for (String auth : val.split("\n"))
    {
      if (auth.startsWith("and "))
        auth = auth.substring(4);
      
      auth = ultraTrim(auth);
      
      if (auth.length() > 0)        
        getAuthors().add(authorType, new PersonName(auth));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData createFromRIS(List<String> lines)
  {
    BibDataStandalone bd = new BibDataStandalone();
    boolean gotType = false;
    
    for (String line : lines)
    {
      if (line.isEmpty() || line.equals("") || line.matches("^\\s*$"))
        continue;
      
      bd.extractDOIandISBNs(line);
      
      Pattern p = Pattern.compile("^([A-Z][A-Z0-9])  -(.*)$");
      Matcher m = p.matcher(line);
      
      if (m.matches())
      {
        String tag = m.group(1),
               val = m.group(2).trim();
        
        if ( ! (gotType || tag.equals("TY")))
          return null;

        switch (tag)
        {
          case "DO": break; // DOI was captured already
          
          case "ER": return gotType ? bd : null;
          case "TY": bd.setEntryType(parseRISType(val)); gotType = true; break;
            
          case "A1": case "A2": case "A3": case "A4": case "AU": 
            
            bd.getAuthors().add(AuthorType.author, new PersonName(val)); break;
            
          case "ED":
            
            bd.getAuthors().add(AuthorType.editor, new PersonName(val)); break;
            
          case "CY": case "PP":
            
            bd.setStr(bfPubLoc, val); break;
            
          case "DA": bd.setYear(val, ytCopyright); break;
          case "PY": bd.setYear(val, ytPublicationDate); break;
          case "Y1": bd.setYear(val, ytCoverDisplayDate); break;
          case "Y2": bd.setYear(val, ytCreated); break;
                        
          case "OP": break;    // Original Publication      
          case "RP": break;    // Reprint Edition
            
          case "ET": bd.setStr(bfEdition, val); break;
          case "IS": bd.setStr(bfIssue, val); break;
          
          case "JF": case "JO":
            
            bd.addStr(bfContainerTitle, val); break;
            
          case "L1": case "L2": case "LK": case "UR":
            
            bd.setStr(bfURL, val); break;
            
          case "LA": bd.setStr(bfLanguage, val); break;
          case "PB": bd.setStr(bfPublisher, val); break;
          
            
          case "SE": break;    // Section
            
          case "SP":
            
            String pages = bd.getStr(bfPages);
            bd.setStr(bfPages, pages.length() == 0 ? val : (val + "-" + pages));
            break;
            
          case "EP":

            pages = bd.getStr(bfPages);
            bd.setStr(bfPages, pages.length() == 0 ? val : (pages + "-" + val));
            break;
            
          case "TI": case "TT": case "T1": case "T2": case "T3":

            bd.addStr(bfTitle, val);
            break;
            
          case "VL": case "VO":
            
            bd.setStr(bfVolume, val); break;
            
          default :
            
            bd.addStr(bfMisc, val); break;
        }
      }
      else if (ultraTrim(line).equals("ER"))
        return gotType ? bd : null;
    }
    
    return null; // It has to end with "ER"; otherwise malformed
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static BibData createFromCrossrefJSON(JsonObj jsonObj, String queryDoi)
  { 
    try
    {
      jsonObj = jsonObj.getObj("message");
      
      jsonObj = nullSwitch(jsonObj.getArray("items"), jsonObj, items -> items.getObj(0));
    }
    catch (NullPointerException | IndexOutOfBoundsException e)
    {
      return null;
    }
   
    BibDataStandalone bd = new BibDataStandalone();
    
    bd.setStr(bfDOI, jsonObj.getStrSafe("DOI"));
    bd.setEntryType(parseCrossrefType(jsonObj.getStrSafe("type")));
    bd.setStr(bfPages, jsonObj.getStrSafe("page"));
    bd.setStr(bfPublisher, jsonObj.getStrSafe("publisher"));
    
    if (jsonObj.containsKey(ytPublishedPrint.getDesc()))
    {
      bd.setStr(bfYear, jsonObj.getObj(ytPublishedPrint.getDesc()).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      bd.yearType = ytPublishedPrint;
    }
    else if (jsonObj.containsKey(ytIssued.getDesc()))
    {
      bd.setStr(bfYear, jsonObj.getObj(ytIssued.getDesc()).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      bd.yearType = ytIssued;
    }
    else if (jsonObj.containsKey(ytCreated.getDesc()))
    {
      bd.setStr(bfYear, jsonObj.getObj(ytCreated.getDesc()).getArray("date-parts").getArray(0).getLongAsStrSafe(0));
      bd.yearType = ytCreated;
    }
      
    bd.setStr(bfURL, jsonObj.getStrSafe("URL"));
    bd.setStr(bfVolume, jsonObj.getStrSafe("volume"));
    bd.setStr(bfIssue, jsonObj.getStrSafe("issue"));
    
    ArrayList<String> title = jsonStrList(jsonObj.getArray("title"));
    ArrayList<String> subtitle = jsonStrList(jsonObj.getArray("subtitle"));
    
    if (strListsEqual(title, subtitle, true) == false)
      title.addAll(subtitle);
    
    bd.setMultiStr(bfTitle, title);    
    
    bd.setMultiStr(bfContainerTitle, jsonStrList(jsonObj.getArray("container-title")));
    bd.setMultiStr(bfISBNs, jsonStrList(jsonObj.getArray("ISBN")));
    bd.setMultiStr(bfISSNs, jsonStrList(jsonObj.getArray("ISSN")));
    
    bd.getAuthors().setFromCrossRefJson(jsonObj.getArray("author"), AuthorType.author);
    bd.getAuthors().setFromCrossRefJson(jsonObj.getArray("editor"), AuthorType.editor);
    bd.getAuthors().setFromCrossRefJson(jsonObj.getArray("translator"), AuthorType.translator);
        
    if (bd.fieldNotEmpty(bfDOI) == false)
      bd.setDOI(queryDoi);
    
    return bd;
  }
 
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String extractYear(String text)
  {
    Pattern p = Pattern.compile("(\\A|\\D)([12]\\d\\d\\d)(\\z|\\D)");
    Matcher m = p.matcher(text);

    if (m.find())
      return m.group(2);

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  private void addReportStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    appendToReport(getFieldName(bibFieldEnum), getStr(bibFieldEnum), list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addReportMultiStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    String line = "";
    
    for (String str : getMultiStr(bibFieldEnum))
    {  
      if (line.length() > 0) line = line + "; ";
      line = line + str;
    }
  
    appendToReport(getFieldName(bibFieldEnum), line, list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void appendToReport(String fieldName, String fieldVal, List<String> list)
  {
    if (fieldVal.trim().length() == 0) return;
    
    list.add(fieldName + ": " + fieldVal);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public String createReport()
  {
    ArrayList<String> list = new ArrayList<>();
    
    addReportStr(bfTitle, list);
    addReportStr(bfEntryType, list);
    appendToReport(getFieldName(bfAuthors), getAuthors().getStr(AuthorType.author), list);
    appendToReport(getFieldName(bfEditors), getAuthors().getStr(AuthorType.editor), list);
    appendToReport(getFieldName(bfTranslators), getAuthors().getStr(AuthorType.translator), list);
    addReportStr(bfYear, list);
    addReportStr(bfDOI, list);
    addReportMultiStr(bfISBNs, list);
    addReportStr(bfURL, list);
    addReportStr(bfContainerTitle, list);
    addReportStr(bfVolume, list);
    addReportStr(bfIssue, list);
    addReportStr(bfPages, list);
    addReportStr(bfPublisher, list);
    addReportStr(bfPubLoc, list);    
    addReportStr(bfEdition, list);
    addReportStr(bfMisc, list);
    addReportMultiStr(bfISSNs, list);
    
    return strListToStr(list, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void copyAllFieldsFrom(BibData bd, boolean includeAuthors, boolean includeEntryType)
  {
    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      BibFieldType type = bibFieldEnumToType.get(bibFieldEnum);
      
      switch (type)
      {
        case bftString :
          
          setStr(bibFieldEnum, bd.getStr(bibFieldEnum));
          break;
          
        case bftMultiString :
          
          setMultiStr(bibFieldEnum, bd.getMultiStr(bibFieldEnum));
          break;
          
        case bftEntryType :
          
          if (includeEntryType) setEntryType(bd.getEntryType());
          break;
          
        case bftWorkType :
          
          setWorkType(bd.getWorkType());
          break;
          
        case bftAuthor :

          break;
      }
    }
    
    if (includeAuthors == false) return;
    
    bd.getAuthors().forEach(getAuthors()::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public EnumSet<BibFieldEnum> fieldsWithExternalData()
  {
    EnumSet<BibFieldEnum> set = EnumSet.noneOf(BibFieldEnum.class);
    
    for (BibFieldEnum bibFieldEnum : EnumSet.allOf(BibFieldEnum.class))
    {
      switch (bibFieldEnum)
      {
        case bfAuthors:   case bfEditors:  case bfTranslators: case bfTitle:
        case bfDOI:       case bfISBNs:    case bfMisc:        case bfYear:
        case bfEntryType: case bfWorkType: case bfURL:
          
          break;
        
        default:

          if (this.fieldNotEmpty(bibFieldEnum))
            set.add(bibFieldEnum);
          
          break;
      }
    }
    
    return set;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
