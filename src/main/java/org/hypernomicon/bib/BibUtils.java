/*
 * Copyright 2015-2018 Jason Winning
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

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.bib.BibData.EntryType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.lang3.StringUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDDocumentInformation;
import org.apache.pdfbox.pdmodel.common.PDMetadata;
import org.apache.pdfbox.text.PDFTextStripper;

import com.adobe.internal.xmp.XMPException;
import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.model.items.Author;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.filePath.FilePath;

public class BibUtils
{

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static class PdfMetadata
  {
    public PDDocumentInformation docInfo = null;
    public XMPNode xmpRoot = null;
    public BibDataStandalone bd = new BibDataStandalone();

  //---------------------------------------------------------------------------  
  //--------------------------------------------------------------------------- 

    public void setDocInfo(PDDocumentInformation docInfo)
    {
      this.docInfo = docInfo;
      if (docInfo == null) return;
      
      extractDOIandISBNs(docInfo.getAuthor(), bd);
      extractDOIandISBNs(docInfo.getCreator(), bd);
      extractDOIandISBNs(docInfo.getKeywords(), bd);
      extractDOIandISBNs(docInfo.getProducer(), bd);
      extractDOIandISBNs(docInfo.getSubject(), bd);
      extractDOIandISBNs(docInfo.getTitle(), bd);
      extractDOIandISBNs(docInfo.getTrapped(), bd);
      
      for (String key : docInfo.getMetadataKeys())
        if (key.toLowerCase().contains("journaldoi") == false)
          extractDOIandISBNs(docInfo.getCustomMetadataValue(key), bd);
    }

  //---------------------------------------------------------------------------  
  //--------------------------------------------------------------------------- 

    public void setXmpRoot(XMPNode xmpRoot)
    {
      this.xmpRoot = xmpRoot;
      
      if (xmpRoot != null)
        xmpRoot.extractDOIandISBNs(bd);
    }
    
    //---------------------------------------------------------------------------  
    //--------------------------------------------------------------------------- 

    public BibData extractBibData()
    {
      if (bd == null) bd = new BibDataStandalone();

      if (safeStr(docInfo.getAuthor()).length() > 0)
      {                
        bd.getAuthors().clear();
        BibAuthorsStandalone.class.cast(bd.getAuthors()).setOneLiner(docInfo.getAuthor());
      }
      
      if (safeStr(docInfo.getTitle()).length() > 0)
        bd.setTitle(docInfo.getTitle());

      if (safeStr(docInfo.getSubject()).length() > 0)
        bd.addStr(bfMisc, docInfo.getSubject());
      
      if (xmpRoot != null)
        xmpRoot.extractBibData(bd);
      
      return bd;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  @SuppressWarnings("resource")
  public static PdfMetadata getPdfMetadata(FilePath filePath, PdfMetadata md) throws IOException, XMPException
  {
    PDDocument pdfDoc = null;
    
    try
    {
      pdfDoc = PDDocument.load(filePath.toFile());
      
      md.setDocInfo(pdfDoc.getDocumentInformation());
            
      PDMetadata metadata = pdfDoc.getDocumentCatalog().getMetadata();                   
      
      if (metadata != null)
      {
        byte[] byteArray = metadata.toByteArray();
        md.setXmpRoot(XMPNode.createRoot(byteArray));
      }

      if (md.bd.getStr(bfDOI).length() > 0)
        return md;

      PDFTextStripper pdfStripper = new PDFTextStripper();
      pdfStripper.setStartPage(1);
      pdfStripper.setEndPage(7);
      
      String parsedText = pdfStripper.getText(pdfDoc);
      
      BibData bd = new BibDataStandalone();
      extractDOIandISBNs(parsedText, bd);
      
      if (bd.getStr(bfDOI).length() == 0)
      {
        parsedText = parsedText.replaceAll("\\h*", ""); // remove whitespace
        extractDOIandISBNs(parsedText, bd);
      }
      
      md.bd.setStr(bfDOI, bd.getStr(bfDOI));
      if (md.bd.getMultiStr(bfISBNs).isEmpty())
        md.bd.setMultiStr(bfISBNs, bd.getMultiStr(bfISBNs));
      
      return md;
    }
    catch (IOException | XMPException e)
    {
      throw e; 
    }
    finally
    {
      if (pdfDoc != null)
      {
        try { pdfDoc.close(); }
        catch (IOException e) { throw e; }
      }
    }    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// DOI legal characters according to Crossref: "a-z", "A-Z", "0-9" and "-._;()/"
// But I've seen at least one Crossref DOI that included a colon 
  
  public static String matchDOI(String str) 
  {
    Pattern p = Pattern.compile("(\\A|\\D)(10\\.\\d{4,}[0-9.]*/[a-zA-Z0-9\\-._;:()/\\\\]+)(\\z|\\D)");
    Matcher m = p.matcher(str);

    if (m.find())
      return m.group(2);

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISSN(String str)
  {
    ArrayList<String> list = null;
    
    str = str.replaceAll("\\p{Pd}", "-").toUpperCase(); // treat all dashes the same
    
    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])(\\d{4}-\\d{3}[\\dxX])(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);
    
    while (m.find())
    {
      String found = m.group(2).replace("-", "");
      
      int n;
      
      int sum = 0;
      
      for (int x = 0; x < 8; x++)
      {
        char c = found.charAt(x);

        if (c == 'X')
          n = 10;
        else
          n = parseInt(String.valueOf(c), -1);
        
        sum = sum + (n * (8 - x));
      }

      if ((sum > 0) && ((sum % 11) == 0))
      {
        if (list == null)
          list = new ArrayList<>();
        
        found = m.group(2);
        
        if (list.contains(found) == false)
          list.add(found);
      }
    }
    
    return list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static List<String> matchISBN(String str)
  {
    ArrayList<String> list = new ArrayList<>();
    
    matchISBNiteration(str, list);
    
    while (str.contains(" "))
      str = str.replace(" ", "");

    matchISBNiteration(str, list);
    
    return list.size() == 0 ? null : list;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void matchISBNiteration(String str, ArrayList<String> list) 
  {
    str = str.replaceAll("\\p{Pd}", "-"); // treat all dashes the same
    str = str.replace('l', '1').replace('I', '1').replace('o', '0').replace('O', '0');
       
    while (str.contains("--"))
      str = str.replace("--", "-");
    
    Pattern p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){12}\\d)(\\z|[^0-9\\-])");
    Matcher m = p.matcher(str);
    
    while (m.find())
    {
      String found = m.group(2).replace("-", "");
      
      int n, sum = 0;
      for (int x = 0; x < 12; x++)
      {
        int coeff = ((x % 2) * 2) + 1;
        n = parseInt(String.valueOf(found.charAt(x)), -1);
        sum = sum + (coeff * n);
      }
      
      n = parseInt(StringUtils.right(found, 1), -1);
      
      if ((sum > 0) && (((10 - (sum % 10)) % 10) == n))
      {
        if (list.contains(found) == false)
          list.add(found);
      }
    }
    
    p = Pattern.compile("(\\A|\\G|[^0-9\\-])((\\d-?){9}[0-9xX])(\\z|[^0-9xX\\-])");
    m = p.matcher(str);
    
    while (m.find())
    {
      String found = m.group(2).toUpperCase().replace("-", "");
      int n;
      
      int sum1 = 0, sum2 = 0;
      
      for (int x = 0; x < 10; x++)
      {
        char c = found.charAt(x);

        if (c == 'X')
          n = 10;
        else
          n = parseInt(String.valueOf(c), -1);
        
        sum1 = sum1 + (n * (10 - x));
        sum2 = sum2 + (n * (x + 1));
      }
      
      if ((sum1 > 0) && (sum2 > 0) && ((sum1 % 11) == 0) && ((sum2 % 11) == 0))
      {       
        if (list.contains(found) == false)
          list.add(found);
      }
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static void extractDOIandISBNs(String value, BibData bd)
  {    
    if (value == null) return;
    if (value.length() == 0) return;
    
    String match = matchDOI(value);
    
    if (match.length() != 0)
      bd.setStr(bfDOI, match);
    
    List<String> matches = matchISBN(value);
    
    if (matches != null)
      for (String isbn : matches)
        bd.addStr(bfISBNs, isbn);
    
    matches = matchISSN(value);
    
    if (matches != null)
      for (String issn : matches)
        bd.addStr(bfISSNs, issn);
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static String getCrossrefUrl(HDT_Work work, String doi)
  {
    String url = "https://api.crossref.org/works";
    
    if (doi.length() > 0)
      return url + "/" + doi;
    
    url = url + "?";
    
    String auths = "";
    for (Author auth : work.getAuthors())
    {
      if (auth.getIsEditor() == false)
        if (auth.getIsTrans() == false)
          auths = auths + "+" + auth.getLastName();
    }
    
    String title = work.name();
    title = title.replace(":", "");
    title = title.replace("?", "");
     
    title = title.replace(' ', '+');    
    
    if (work.getYear().length() > 0)
    {
      int year = parseInt(work.getYear(), -1);
      if (year > 1929)
        url = url + "query=" + work.getYear() + "&";
    }
    
    if (auths.length() > 0)
      url = url + "query.author=" + escapeURL(auths, false);
    
    if (title.length() > 0)
    {
      if (auths.length() > 0)
        url = url + "&";
      
      if ((auths.length() == 0) && (work.getYear().length() == 0))
        url = url + "query=" + escapeURL(title, false); // For some ridiculous reason this works better when there is only a title
      else
        url = url + "query.title=" + escapeURL(title, false);
    }
    
    return url;
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static String getGoogleUrl(HDT_Work work, String isbn)
  {
    String url = "https://www.googleapis.com/books/v1/volumes?q=";
    
    if (isbn.length() > 0)
      return url + escapeURL("isbn:" + isbn, false);
    
    String auths = "";
    for (Author auth : work.getAuthors())
    {
      if (auth.getIsEditor() == false)
        if (auth.getIsTrans() == false)
        {
          if (auths.length() > 0)
            auths = auths + "+";
          
          auths = auths + "inauthor:" + auth.getLastName(true);
        }
    }
    
    String title = work.getNameEngChar();
    title = title.replace(":", "");
    title = title.replace("?", "");
     
    title = title.replace(' ', '+');
    
    if (title.length() > 0)
    {      
      url = url + escapeURL("\"" + title + "\"", false);
    }

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

  public static EntryType parseGoogleBooksType(String gbType)
  {
    switch (gbType)
    {
      case "BOOK" : return etBook;
      case "MAGAZINE" : return etMagazine;
      
      default : return etOther;
    }
  }
  
//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 
 
  public static EntryType parsePrismAggregationType(String paType)
  {
    switch (paType)
    {
      case "book" : return etBook;
      case "catalog" : return etCatalogItem;
      case "feed" : return etFeedItem;
      case "journal" : return etJournalArticle;
      case "magazine" : return etMagazineArticle;
      case "manual" : return etManual;
      case "newsletter" : return etNewsletterArticle;
      case "other" : return etOther;
      case "pamphlet": return etPamphlet;
      
      default : return etOther;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static EntryType parseCrossrefType(String crType)
  {
    switch (crType)
    {
      case "book" : return etBook;
      case "book-chapter" : return etBookChapter;
      case "book-part" : return etBookPart;
      case "book-section" : return etBookSection;
      case "book-series" : return etBookSeries;
      case "book-set" : return etBookSet;
      case "book-track" : return etBookTrack;
      case "component" : return etComponent;
      case "dataset" : return etDataSet;
      case "dissertation" : return etThesis;
      case "edited-book" : return etEditedBook;
      case "journal" : return etJournal;
      case "journal-article" : return etJournalArticle;
      case "journal-issue" : return etJournalIssue;
      case "journal-volume" : return etJournalVolume;
      case "monograph" : return etMonograph;
      case "other" : return etOther;
      case "posted-content" : return etPostedContent;
      case "proceedings" : return etConferenceProceedings;
      case "proceedings-article" : return etConferencePaper;
      case "reference-book" : return etReferenceBook;
      case "reference-entry" : return etReferenceEntry;
      case "report" : return etReport;
      case "report-series" : return etReportSeries;
      case "standard" : return etStandard;
      case "standard-series": return etStandardSeries;
      
      default : return etOther;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static EntryType parseRISType(String risType)
  {
    switch (risType)
    {
      case "ABST" : return etAbstract;
      case "ADVS" : return etAudiovisualMaterial;
      case "AGGR" : return etAggregatedDatabase;
      case "ANCIENT" : return etAncientText;
      case "ART" : return etArtwork;
      case "BILL" : return etBill;
      case "BLOG" : return etBlogPost;
      case "BOOK" : return etBook;
      case "CASE" : return etCase;
      case "CHAP" : return etBookChapter;
      case "CHART" : return etChart;
      case "CLSWK" : return etClassicalWork;
      case "COMP" : return etSoftware;
      case "CONF" : return etConferenceProceedings;
      case "CPAPER" : return etConferencePaper;
      case "CTLG" : return etCatalog;
      case "DATA" : return etDataFile;
      case "DBASE" : return etOnlineDatabase;
      case "DICT" : return etDictionaryEntry;
      case "EBOOK" : return etElectronicBook;
      case "ECHAP" : return etElectronicBookSection;
      case "EDBOOK" : return etEditedBook;
      case "EJOUR" : return etElectronicArticle;
      case "ELEC" : return etWebPage;
      case "ENCYC" : return etEncyclopediaArticle;
      case "EQUA" : return etEquation;
      case "FIGURE" : return etFigure;
      case "GEN" : return etUnentered;
      case "GOVDOC" : return etGovernmentDocument;
      case "GRANT" : return etGrant;
      case "HEAR" : return etHearing;
      case "ICOMM" : return etInternetCommunication;
      case "INPR" : return etInPress;
      case "JFULL" : return etJournal;
      case "JOUR" : return etJournalArticle;
      case "LEGAL" : return etRuling;
      case "MANSCPT" : return etManuscript;
      case "MAP" : return etMap;
      case "MGZN" : return etMagazineArticle;
      case "MPCT" : return etFilm;
      case "MULTI" : return etOnlineMultimedia;
      case "MUSIC" : return etMusicScore;
      case "NEWS" : return etNewspaperArticle;
      case "PAMP" : return etPamphlet;
      case "PAT" : return etPatent;
      case "PCOMM" : return etPersonalCommunication;
      case "RPRT" : return etReport;
      case "SER" : return etSerialPublication;
      case "SLIDE" : return etSlide;
      case "SOUND" : return etAudioRecording;
      case "STAND" : return etStandard;
      case "STAT" : return etStatute;
      case "THES" : return etThesis;
      case "UNPB" : return etUnpublishedWork;
      case "VIDEO" : return etVideoRecording;
      
      default : return etOther;
    }
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

  public static EntryType parseBibTexType(String btType)
  {
    switch (btType.toLowerCase())
    {
      case "article" : return etJournalArticle;
      case "book" : return etBook;
      case "booklet" : return etBooklet;
      case "conference" : return etConferencePaper;
      case "inbook" : return etBookChapter;
      case "incollection" : return etBookChapter;
      case "inproceedings" : return etConferencePaper;
      case "manual" : return etManual;
      case "mastersthesis" : return etMastersThesis;
      case "misc" : return etOther;
      case "phdthesis" : return etDoctoralThesis;
      case "proceedings" : return etConferenceProceedings;
      case "techreport" : return etTechnicalReport;
      case "unpublished" : return etUnpublishedWork;
      
      default : return etOther;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final EnumHashBiMap<EntryType, String> entryTypeMap = initMap();
  
  private static EnumHashBiMap<EntryType, String> initMap()
  {
    EnumHashBiMap<EntryType, String> map = EnumHashBiMap.create(EntryType.class);
    
    map.put(etJournal, "Journal");
    map.put(etJournalVolume, "Journal Volume");
    map.put(etJournalArticle, "Journal Article");
    map.put(etJournalIssue, "Journal Issue");
    map.put(etJournalSection, "Journal Section");
    map.put(etElectronicArticle, "Electronic Article");
    map.put(etInPress, "In Press");
    map.put(etBook, "Book");
    map.put(etMonograph, "Monograph");
    map.put(etBookVolume, "Book Volume");
    map.put(etBookSection, "Book Section");
    map.put(etBookChapter, "Book Chapter");
    map.put(etBookPart, "Book Part");
    map.put(etBookSet, "Book Set");
    map.put(etBookTrack, "Book Track");
    map.put(etEditedBook, "Edited Book");
    map.put(etBookSeries, "Book Series");
    map.put(etMultiVolumeWork, "Multi-volume Work");
    map.put(etBooklet, "Booklet");
    map.put(etElectronicBook, "Electronic Book");
    map.put(etElectronicBookSection, "Electronic Book Section");
    map.put(etSerialPublication, "Serial Publication");
    map.put(etMagazine, "Magazine");
    map.put(etMagazineArticle, "Magazine Article");
    map.put(etNewspaper, "Newspaper");
    map.put(etNewspaperArticle, "Newspaper Article");
    map.put(etLetterToTheEditor, "Letter to the Editor");
    map.put(etNewsletter, "Newsletter");
    map.put(etNewsletterArticle, "Newsletter Article");
    map.put(etWebPage, "Web Page");
    map.put(etPostedContent, "Posted Content");
    map.put(etTwitterPost, "Twitter Post");
    map.put(etFacebookPost, "Facebook Post");
    map.put(etForumPost, "Forum Post");
    map.put(etInstantMessage, "Instant Message");
    map.put(etBlogPost, "Blog Post");
    map.put(etEmail, "Email");
    map.put(etFeedItem, "Feed Item");
    map.put(etInternetCommunication, "Internet Communication");
    map.put(etConference, "Conference");
    map.put(etConferencePaper, "Conference Paper");
    map.put(etConferenceProceedings, "Conference Proceedings");
    map.put(etPoster, "Poster");
    map.put(etSymposium, "Symposium");
    map.put(etSymposiumPaper, "Symposium Paper");
    map.put(etSymposiumProceedings, "Symposium Proceedings");
    map.put(etPresentation, "Presentation");
    map.put(etReferenceBook, "Reference Book");
    map.put(etReferenceEntry, "Reference Entry");
    map.put(etDictionaryEntry, "Dictionary Entry");
    map.put(etEncyclopediaArticle, "Encyclopedia Article");
    map.put(etCatalog, "Catalog");
    map.put(etCatalogItem, "Catalog Item");
    map.put(etAncientText, "Ancient Text");
    map.put(etClassicalWork, "Classical Work");
    map.put(etCase, "Case");
    map.put(etHearing, "Hearing");
    map.put(etStatute, "Statute");
    map.put(etBill, "Bill");
    map.put(etRegulation, "Regulation");
    map.put(etRuling, "Ruling");
    map.put(etGrant, "Grant");
    map.put(etGovernmentDocument, "Government Document");
    map.put(etAudiovisualMaterial, "Audiovisual Material");
    map.put(etOnlineMultimedia, "Online Multimedia");
    map.put(etMusicScore, "Music Score");
    map.put(etAudioRecording, "Audio Recording");
    map.put(etRadioBroadcast, "Radio Broadcast");
    map.put(etTVBroadcast, "TV Broadcast");
    map.put(etFilm, "Film");
    map.put(etVideoRecording, "Video Recording");
    map.put(etPodcast, "Podcast");
    map.put(etPortfolio, "Portfolio");
    map.put(etArtwork, "Artwork");
    map.put(etIssueBrief, "Issue Brief");
    map.put(etReportSeries, "Report Series");
    map.put(etReport, "Report");
    map.put(etTechnicalReport, "Technical Report");
    map.put(etApparatus, "Apparatus");
    map.put(etMeasurementInstrument, "Measurement Instrument");
    map.put(etStandard, "Standard");
    map.put(etStandardSeries, "Standard Series");
    map.put(etManual, "Manual");
    map.put(etPatent, "Patent");
    map.put(etThesis, "Thesis");
    map.put(etMastersThesis, "Masters Thesis");
    map.put(etDoctoralThesis, "Doctoral Thesis");
    map.put(etManuscript, "Manuscript");
    map.put(etUnpublishedWork, "Unpublished Work");
    map.put(etWorkingPaper, "Working Paper");
    map.put(etUnpublishedRawData, "Unpublished Raw Data");
    map.put(etPersonalCommunication, "Personal Communication");
    map.put(etDocument, "Document");
    map.put(etMap, "Map");
    map.put(etChart, "Chart");
    map.put(etEquation, "Equation");
    map.put(etFigure, "Figure");
    map.put(etSlide, "Slide");
    map.put(etDataSet, "Data Set");
    map.put(etDataFile, "Data File");
    map.put(etOnlineDatabase, "Online Database");
    map.put(etAggregatedDatabase, "Aggregated Database");
    map.put(etSoftware, "Software");
    map.put(etComponent, "Component");
    map.put(etAbstract, "Abstract");
    map.put(etCommentary, "Commentary");
    map.put(etInterview, "Interview");
    map.put(etArchivalDocument, "Archival Document");
    map.put(etArchivalCollection, "Archival Collection");
    map.put(etLetter, "Letter");
    map.put(etPamphlet, "Pamphlet");
    map.put(etBrochure, "Brochure");
    map.put(etOther, "Other");
    
    return map;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EntryType parseEntryType(String et)
  {
    return entryTypeMap.inverse().getOrDefault(et, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String getEntryTypeName(EntryType et)
  {
    if (et == null) return "";
    
    return entryTypeMap.getOrDefault(et, "");
  }

//---------------------------------------------------------------------------  
//--------------------------------------------------------------------------- 

}
