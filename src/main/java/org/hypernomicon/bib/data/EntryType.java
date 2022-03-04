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

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.model.records.SimpleRecordTypes.WorkTypeEnum;

public enum EntryType
{
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
  etPostedContent        ("Posted Content"),          etTwitterPost          ("Twitter Post"),
  etFacebookPost         ("Facebook Post"),           etForumPost            ("Forum Post"),
  etInstantMessage       ("Instant Message"),         etBlogPost             ("Blog Post"),
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
  etPortfolio            ("Portfolio"),               etArtwork              ("Artwork"),
  etIssueBrief           ("Issue Brief"),             etReportSeries         ("Report Series"),
  etReport               ("Report"),                  etTechnicalReport      ("Technical Report"),
  etApparatus            ("Apparatus"),               etMeasurementInstrument("Measurement Instrument"),
  etStandard             ("Standard"),                etStandardSeries       ("Standard Series"),
  etManual               ("Manual"),                  etPatent               ("Patent"),
  etThesis               ("Thesis"),                  etMastersThesis        ("Masters Thesis"),
  etDoctoralThesis       ("Doctoral Thesis"),         etManuscript           ("Manuscript"),
  etUnpublishedWork      ("Unpublished Work"),        etWorkingPaper         ("Working Paper"),
  etUnpublishedRawData   ("Unpublished Raw Data"),    etPersonalCommunication("Personal Communication"),
  etDocument             ("Document"),                etMap                  ("Map"),
  etChart                ("Chart"),                   etEquation             ("Equation"),
  etFigure               ("Figure"),                  etSlide                ("Slide"),
  etDataSet              ("Data Set"),                etDataFile             ("Data File"),
  etOnlineDatabase       ("Online Database"),         etAggregatedDatabase   ("Aggregated Database"),
  etSoftware             ("Software"),                etComponent            ("Component"),
  etAbstract             ("Abstract"),                etCommentary           ("Commentary"),
  etInterview            ("Interview"),               etArchivalDocument     ("Archival Document"),
  etArchivalCollection   ("Archival Collection"),     etLetter               ("Letter"),
  etPamphlet             ("Pamphlet"),                etBrochure             ("Brochure"),

  etOther                ("Other"),         // This means it is a type that does not fit into any of the above categories
  etUnentered            (""),              // This just means the field hasn't been populated yet
  etNone                 ("");              // This means it should be treated as a non-entry

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, EntryType> nameToType;
  private final String userFriendlyName;

  private EntryType(String userFriendlyName) { this.userFriendlyName = userFriendlyName; }

  static
  {
    nameToType = new HashMap<>();
    Arrays.stream(values()).filter(et -> et.userFriendlyName.length() > 0)
                           .forEachOrdered(et -> nameToType.put(et.userFriendlyName, et));
  }

  public String getUserFriendlyName()      { return userFriendlyName; }
  public static EntryType parse(String et) { return nameToType.getOrDefault(et, null); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static HDT_WorkType toWorkType(EntryType et)
  {
    switch (et)
    {
      case etBook : case etBooklet: case etBookVolume: case etJournalIssue : case etMagazine : case etManual : case etMonograph :
      case etMultiVolumeWork : case etReferenceBook : case etEditedBook : case etAncientText : case etClassicalWork : case etElectronicBook :

        return HDT_WorkType.get(WorkTypeEnum.wtBook);

      case etAbstract : case etArchivalDocument : case etCommentary : case etConferencePaper :
      case etEncyclopediaArticle : case etJournalArticle : case etLetter : case etLetterToTheEditor :
      case etMagazineArticle : case etNewsletterArticle : case etNewspaperArticle : case etInPress : case etUnpublishedWork :
      case etReport : case etTechnicalReport : case etWorkingPaper : case etElectronicArticle : case etGovernmentDocument :

        return HDT_WorkType.get(WorkTypeEnum.wtPaper);

      case etThesis : case etDoctoralThesis : case etMastersThesis :

        return HDT_WorkType.get(WorkTypeEnum.wtThesis);

      default : break;
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static EntryType fromWorkType(WorkTypeEnum workTypeEnum)
  {
    switch (workTypeEnum)
    {
      case wtBook         : return etBook;
      case wtChapter      : return etBookChapter;
      case wtNone         : return etUnentered;
      case wtPaper        : return etJournalArticle;
      case wtRecording    : return etAudiovisualMaterial;
      case wtThesis       : return etThesis;
      case wtUnenteredSet : return etNone;
      case wtWebPage      : return etWebPage;

      default             : return etUnentered;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
