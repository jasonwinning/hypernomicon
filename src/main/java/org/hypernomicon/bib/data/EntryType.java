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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;

import java.util.*;

import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;

//---------------------------------------------------------------------------

public enum EntryType
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  etJournal              ("Journal"),                 etJournalVolume        ("Journal Volume"),
  etJournalArticle       ("Journal Article"),         etJournalIssue         ("Journal Issue"),
  etJournalSection       ("Journal Section"),         etElectronicArticle    ("Electronic Article"),
  etInPress              ("In Press"),                etBook                 ("Book"),
  etMonograph            ("Monograph"),               etBookVolume           ("Book Volume"),
  etBookSection          ("Book Section"),            etBookChapter          ("Book Chapter"),
  etBookPart             ("Book Part"),               etBookSet              ("Book Set"),
  etBookTrack            ("Book Track"),              etEditedBook           ("Edited Book"),
  etBookSeries           ("Book Series"),             etMultiVolumeWork      ("Multi-volume Work"),
  etBooklet              ("Booklet"),                 etElectronicBook       ("Electronic Book"),
  etElectronicBookSection("Electronic Book Section"), etSerialPublication    ("Serial Publication"),
  etMagazine             ("Magazine"),                etMagazineArticle      ("Magazine Article"),
  etNewspaper            ("Newspaper"),               etNewspaperArticle     ("Newspaper Article"),
  etLetterToTheEditor    ("Letter to the Editor"),    etNewsletter           ("Newsletter"),
  etNewsletterArticle    ("Newsletter Article"),      etWebPage              ("Web Page"),
  etOnlineCourse         ("Online Course"),           etLectureNotes         ("Lecture Notes"),
  etWorkshopMaterial     ("Workshop Material"),       etPostedContent        ("Posted Content"),
  etTwitterPost          ("Twitter Post"),            etFacebookPost         ("Facebook Post"),
  etForumPost            ("Forum Post"),              etInstantMessage       ("Instant Message"),
  etBlogPost             ("Blog Post"),               etBlogSeries           ("Blog Series"),
  etEmail                ("Email"),                   etFeedItem             ("Feed Item"),
  etInternetCommunication("Internet Communication"),  etConference           ("Conference"),
  etConferencePaper      ("Conference Paper"),        etConferenceProceedings("Conference Proceedings"),
  etProceedingsSeries    ("Proceedings Series"),      etPeerReview           ("Peer Review"),
  etPoster               ("Poster"),                  etSymposium            ("Symposium"),
  etSymposiumPaper       ("Symposium Paper"),         etSymposiumProceedings ("Symposium Proceedings"),
  etPresentation         ("Presentation"),            etReferenceBook        ("Reference Book"),
  etReferenceEntry       ("Reference Entry"),         etDictionaryEntry      ("Dictionary Entry"),
  etEncyclopediaArticle  ("Encyclopedia Article"),    etCatalog              ("Catalog"),
  etCatalogItem          ("Catalog Item"),            etAncientText          ("Ancient Text"),
  etClassicalWork        ("Classical Work"),          etCase                 ("Case"),
  etHearing              ("Hearing"),                 etStatute              ("Statute"),
  etBill                 ("Bill"),                    etRegulation           ("Regulation"),
  etRuling               ("Ruling"),                  etGrant                ("Grant"),
  etGovernmentDocument   ("Government Document"),     etAudiovisualMaterial  ("Audiovisual Material"),
  etOnlineMultimedia     ("Online Multimedia"),       etMusicScore           ("Music Score"),
  etAudioRecording       ("Audio Recording"),         etRadioBroadcast       ("Radio Broadcast"),
  etTVBroadcast          ("TV Broadcast"),            etFilm                 ("Film"),
  etVideoRecording       ("Video Recording"),         etPodcast              ("Podcast"),
  etPodcastEpisode       ("Podcast Episode"),         etPortfolio            ("Portfolio"),
  etArtwork              ("Artwork"),                 etIssueBrief           ("Issue Brief"),
  etReportSeries         ("Report Series"),           etReport               ("Report"),
  etTechnicalReport      ("Technical Report"),        etWhitePaper           ("White Paper"),
  etApparatus            ("Apparatus"),               etMeasurementInstrument("Measurement Instrument"),
  etStandard             ("Standard"),                etStandardSeries       ("Standard Series"),
  etManual               ("Manual"),                  etPatent               ("Patent"),
  etThesis               ("Thesis"),                  etMastersThesis        ("Masters Thesis"),
  etDoctoralThesis       ("Doctoral Thesis"),         etManuscript           ("Manuscript"),
  etUnpublishedWork      ("Unpublished Work"),        etPreprint             ("Preprint"),
  etWorkingPaper         ("Working Paper"),           etUnpublishedRawData   ("Unpublished Raw Data"),
  etSurvey               ("Survey"),                  etDocument             ("Document"),
  etArchivalDocument     ("Archival Document"),       etArchivalCollection   ("Archival Collection"),
  etMap                  ("Map"),                     etChart                ("Chart"),
  etEquation             ("Equation"),                etFigure               ("Figure"),
  etSlide                ("Slide"),                   etDataSet              ("Data Set"),
  etDataFile             ("Data File"),               etOnlineDatabase       ("Online Database"),
  etAggregatedDatabase   ("Aggregated Database"),     etSoftware             ("Software"),
  etComponent            ("Component"),               etAbstract             ("Abstract"),
  etCommentary           ("Commentary"),              etInterview            ("Interview"),
  etPersonalCommunication("Personal Communication"),  etLetter               ("Letter"),
  etPressRelease         ("Press Release"),           etPamphlet             ("Pamphlet"),
  etBrochure             ("Brochure"),

  etOther                ("Other"),         // This means it is a type that does not fit into any of the
                                            // above categories and shouldn't be displayed in the Bib. Manager
                                            // because there's no JSON template defining its fields
  etUnentered            (""),              // This just means the field hasn't been populated yet
  etNone                 ("");              // This means it should be treated as a non-entry

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, EntryType> nameToType;
  private final String userFriendlyName;

//---------------------------------------------------------------------------

  EntryType(String userFriendlyName) { this.userFriendlyName = userFriendlyName; }

//---------------------------------------------------------------------------

  static
  {
    nameToType = new HashMap<>();
    Arrays.stream(values()).filter(et -> et.userFriendlyName.length() > 0)
                           .forEachOrdered(et -> nameToType.put(et.userFriendlyName, et));
  }

//---------------------------------------------------------------------------

  public String getUserFriendlyName()      { return userFriendlyName; }
  public static EntryType parse(String et) { return nameToType.getOrDefault(et, null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isChild()
  {
    return switch (this)
    {
      case etBookChapter, etEncyclopediaArticle  , etConferencePaper, etDictionaryEntry, etBookPart,
           etBookSection, etElectronicBookSection, etReferenceEntry , etSymposiumPaper

        -> true;

      default

        -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isParent()
  {
    return switch (this)
    {
      case etElectronicBook, etConferenceProceedings, etBook, etEditedBook,  etBookVolume, etMonograph,
           etReferenceBook , etSymposiumProceedings

        -> true;

      default

        -> false;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_WorkType toWorkType(EntryType et)
  {
    return et == null ? null : switch (et)
    {
      case etBook, etBooklet, etBookVolume, etJournalIssue, etMagazine, etManual, etMonograph, etMultiVolumeWork,
           etReferenceBook, etEditedBook, etAncientText, etClassicalWork, etElectronicBook ->

        HDT_WorkType.get(WorkTypeEnum.wtBook);

      case etAbstract, etArchivalDocument, etCommentary, etConferencePaper, etEncyclopediaArticle, etJournalArticle,
           etLetter, etLetterToTheEditor, etMagazineArticle, etNewsletterArticle, etNewspaperArticle, etUnpublishedWork,
           etReport, etTechnicalReport, etWorkingPaper, etElectronicArticle, etGovernmentDocument ->

        HDT_WorkType.get(WorkTypeEnum.wtPaper);

      case etThesis, etDoctoralThesis, etMastersThesis ->

        HDT_WorkType.get(WorkTypeEnum.wtThesis);

      case etBookChapter, etBookPart, etBookSection, etElectronicBookSection ->

        db.workTypes.getByID(db.prefs.getInt(PrefKey.DEFAULT_CHAPTER_WORK_TYPE_ID, -1));

      default -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EntryType fromWorkType(WorkTypeEnum workTypeEnum)
  {
    return switch (workTypeEnum)
    {
      case wtBook         -> etBook;
      case wtChapter      -> etBookChapter;
      case wtPaper        -> etJournalArticle;
      case wtRecording    -> etAudiovisualMaterial;
      case wtThesis       -> etThesis;
      case wtUnenteredSet -> etNone;
      case wtWebPage      -> etWebPage;

      default             -> etUnentered;   // Applies to wtNone
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
