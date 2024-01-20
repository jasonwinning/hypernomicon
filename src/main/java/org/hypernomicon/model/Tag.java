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

package org.hypernomicon.model;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.Util.*;

import java.util.EnumMap;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.records.RecordType;

//---------------------------------------------------------------------------

public enum Tag
{
  tagPerson          ("person"            , "Person"                   , hdtPerson         , true),
  tagPersonStatus    ("person_status"     , "Status"                   , hdtPersonStatus   , true),
  tagInstitution     ("institution"       , "Institution"              , hdtInstitution    , true),
  tagInstitutionType ("institution_type"  , "Institution Type"         , hdtInstitutionType, true),
  tagRegion          ("region"            , "State/Region"             , hdtRegion         , true),
  tagCountry         ("country"           , "Country"                  , hdtCountry        , true),
  tagRank            ("rank"              , "Rank"                     , hdtRank           , true),
  tagInvestigation   ("investigation"     , "Investigation"            , hdtInvestigation  , true),
  tagDebate          ("debate"            , "Problem/Debate"           , hdtDebate         , true),
  tagArgument        ("argument"          , "Argument"                 , hdtArgument       , true),
  tagTerm            ("term"              , "Term"                     , hdtTerm           , true),
  tagConcept         ("concept"           , "Concept"                  , hdtConcept        , true),
  tagWork            ("work"              , "Work"                     , hdtWork           , true),
  tagWorkType        ("work_type"         , "Type of Work"             , hdtWorkType       , true),
  tagWorkLabel       ("work_label"        , "Work Label"               , hdtWorkLabel      , true),
  tagField           ("field"             , "Field"                    , hdtField          , true),
  tagSubfield        ("subfield"          , "Subfield"                 , hdtSubfield       , true),
  tagPosition        ("position"          , "Position"                 , hdtPosition       , true),
  tagPositionVerdict ("position_verdict"  , "Conclusion about Position", hdtPositionVerdict, true),
  tagArgumentVerdict ("argument_verdict"  , "Conclusion about Argument", hdtArgumentVerdict, true),
  tagMiscFile        ("misc_file"         , "Misc. File"               , hdtMiscFile       , true),
  tagWorkFile        ("work_file"         , "Work File"                , hdtWorkFile       , true),
  tagFolder          ("folder"            , "Folder"                   , hdtFolder         , true),
  tagNote            ("note"              , "Note"                     , hdtNote           , true),
  tagGlossary        ("glossary"          , "Glossary"                 , hdtGlossary       , true),
  tagHub             ("hub"               , "Record Hub"               , hdtHub            , true),
  tagPersonGroup     ("person_group"      , "Person Group"             , hdtPersonGroup    , true),
  tagFileType        ("file_type"         , "File Type"                , hdtFileType       , true),
  tagSense           ("sense"             , "Concept Sense"            , hdtConceptSense   , true),
  tagAuthor          ("author"            , "Author"                   , hdtPerson),
  tagLargerDebate    ("larger_debate"     , "Larger Debate"            , hdtDebate),
  tagLargerPosition  ("larger_position"   , "Larger Position"          , hdtPosition),
  tagParentNote      ("parent_note"       , "Parent Note"              , hdtNote),
  tagParentGlossary  ("parent_glossary"   , "Parent Glossary"          , hdtGlossary),
  tagParentConcept   ("parent_concept"    , "Parent Concept"           , hdtConcept),
  tagLinkedRecord    ("linked_record"     , "Linked Record"            , hdtAuxiliary),
  tagKeyWork         ("key_work"          , "Key Works"                , hdtAuxiliary),
  tagDisplayRecord   ("display_item"      , "Displayed Records"        , hdtAuxiliary),
  tagLargerWork      ("larger_work"       , "Larger Work"              , hdtWork),
  tagParentLabel     ("parent_label"      , "Parent Label"             , hdtWorkLabel),
  tagParentGroup     ("parent_group"      , "Parent Group"             , hdtPersonGroup),
  tagCounterargument ("counterargument"   , "Counterargument"          , hdtArgument),
  tagParentFolder    ("parent_folder"     , "Parent Folder"            , hdtFolder),
  tagParentInst      ("parent_institution", "Parent Institution"       , hdtInstitution),
  tagPictureFolder   ("picture_folder"    , "Picture Folder"           , hdtFolder),
  tagID              ("id"                , "Record ID"),
  tagType            ("type"              , "Record Type"),
  tagSortKey         ("sort_key"          , "Sort Key"),
  tagSearchKey       ("search_key"        , "Search Key"),
  tagRecord          ("record"            , "Record"),
  tagListName        ("list_name"         , "List Name"),
  tagFirstName       ("first_name"        , "First Name"),
  tagLastName        ("last_name"         , "Last Name"),
  tagWebURL          ("link"              , "Web URL"),
  tagORCID           ("orcid"             , "ORCID"),
  tagPicture         ("picture"           , "Picture"),
  tagPictureCrop     ("picture_crop"      , "Picture Crop"),
  tagWhyFamous       ("why_famous"        , "Description"),
  tagName            ("name"              , "Name"),
  tagCity            ("city"              , "City"),
  tagAbbreviation    ("abbreviation"      , "Abbreviation"),
  tagDescription     ("description"       , "Description"),
  tagMainText        ("main_text"         , "Description"),
  tagTitle           ("title"             , "Title"),
  tagFileName        ("file_name"         , "Filename"),
  tagYear            ("year"              , "Year"),
  tagBibEntryKey     ("bib_entry_key"     , "Bibliography Entry Key"),
  tagMiscBib         ("misc_bib"          , "Misc. Bib. Info"),
  tagDOI             ("doi"               , "DOI"),
  tagISBN            ("isbn"              , "ISBN"),
  tagInFileName      ("in_filename"       , "Included in File Name"),
  tagEditor          ("editor"            , "Editor"),
  tagTranslator      ("translator"        , "Translator"),
  tagPages           ("pages"             , "Pages"),
  tagStartPageNum    ("start_page"        , "Starting Page Number"),
  tagEndPageNum      ("end_page"          , "Ending Page Number"),
  tagAnnotated       ("annotated"         , "Annotated"),
  tagComments        ("comments"          , "Description"),
  tagDefinition      ("definition"        , "Definition"),
  tagText            ("text"              , "Text"),
  tagActive          ("active"            , "Active"),
  tagPast            ("past"              , "Past"),
  tagCreationDate    ("creation_date"     , "Date Created"),
  tagModifiedDate    ("modified_date"     , "Date Modified"),
  tagViewDate        ("view_date"         , "Date Last Viewed"),
  tagNone            (""                  , "");

//---------------------------------------------------------------------------

  public final int num;
  public final String name, header;
  public final RecordType objType;

  private final boolean isMainTagForObjType;

  private final static Map<Integer   , Tag> numToTag;
  private final static Map<String    , Tag> nameToTag;
  private final static Map<RecordType, Tag> objTypeToTag;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  Tag(String name, String header)
  {
    this(name, header, hdtNone, false);
  }

  Tag(String name, String header, RecordType objType)
  {
    this(name, header, objType, false);
  }

  Tag(String name, String header, RecordType objType, boolean isMainTagForObjType)
  {
    this.name = name;
    num = Math.abs(stringHash(name));
    this.header = header;
    this.objType = objType;
    this.isMainTagForObjType = isMainTagForObjType;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static
  {
    numToTag     = new HashMap<>();
    nameToTag    = new HashMap<>();
    objTypeToTag = new EnumMap<>(RecordType.class);

    EnumSet.allOf(Tag.class).forEach(tag ->
    {
      if (tag == tagNone) return;

      if (numToTag.containsKey(tag.num))
        throw new AssertionError(new HDB_InternalError(99215, "Duplicate tag hash codes."));

      numToTag .put(tag.num , tag);
      nameToTag.put(tag.name, tag);

      if (tag.isMainTagForObjType)
        objTypeToTag.put(tag.objType, tag);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Tag getTag(int num)     { return numToTag    .getOrDefault(num    , tagNone); }
  static Tag getTag(String name)        { return nameToTag   .getOrDefault(name   , tagNone); }
  static Tag getTag(RecordType objType) { return objTypeToTag.getOrDefault(objType, tagNone); }

  public static String getTypeTagStr(RecordType objType) { return nullSwitch(getTag(objType), ""     , tag -> tag.name   ); }
  public static RecordType parseTypeTagStr(String text)  { return nullSwitch(getTag(text   ), hdtNone, tag -> tag.objType); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
