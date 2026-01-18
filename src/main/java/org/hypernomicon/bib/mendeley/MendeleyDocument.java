/*
 * Copyright 2015-2026 Jason Winning
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

package org.hypernomicon.bib.mendeley;

import java.time.Instant;
import java.util.*;
import java.util.stream.Stream;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.BibField.BibFieldType;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.items.*;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.authors.Author.AuthorType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class MendeleyDocument extends BibEntry<MendeleyDocument, MendeleyFolder> implements MendeleyEntity
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MendeleyDocument(MendeleyWrapper mWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    super(mWrapper, thisIsBackup);

    update(jObj, false, false);
  }

//---------------------------------------------------------------------------

  public MendeleyDocument(MendeleyWrapper mWrapper, EntryType newType)
  {
    super(mWrapper, false);

    jObj.put(entryTypeKey, mWrapper.getEntryTypeMap().getOrDefault(newType, ""));

    jObj.put("id", "_!_" + randomAlphanumericStr(12));
  }

//---------------------------------------------------------------------------

  /**
   * This constructor is for unit tests only and will throw an assertion error if run
   * outside of a unit test thread.
   * @param mWrapper Mendeley wrapper
   * @param jObj Pre-populated JSON object
   * @param newType Entry type
   */
  private MendeleyDocument(MendeleyWrapper mWrapper, JsonObj jObj, EntryType newType)
  {
    super(mWrapper, false);

    assertThatThisIsUnitTestThread();

    jObj.put(entryTypeKey, mWrapper.getEntryTypeMap().getOrDefault(newType, ""));

    jObj.put("id", "_!_" + randomAlphanumericStr(12));
    jObj.put(Document_Last_Modified_JSON_Key, dateTimeToIso8601(Instant.now()));  // These two lines will convert this into
    update(jObj, false, true);                                                    // a "synced" entry, not a "new" one
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final List<String> Noneditable_Document_JSON_Keys = List.of
  (
    "profile_id", "id", "created", Document_Last_Modified_JSON_Key, "group_id", "accessed", "citation_key", "folder_uuids",
    "authored", "read", "hidden", "confirmed", "file_attached", "private_publication", "starred"
  );

//---------------------------------------------------------------------------

  @Override public String toString()                   { return exportStandaloneJsonObj(false).toString(); }
  @Override public String getKey()                     { return jObj.getStrSafe("id"); }
  @Override public String getURLtoViewEntryInRefMgr()  { return ""; }
  @Override public BibAuthors getAuthors()             { return linkedToWork() ? new WorkBibAuthors(getWork()) : new MendeleyAuthors(jObj, getEntryType()); }
  @Override public EntryType getEntryType()            { return getLibrary().parseEntryType(getEntryTypeStrFromSpecifiedJson(jObj)); }
  @Override public Instant lastModifiedOnServer()      { return MendeleyWrapper.getSyncInstantFromJsonStr(jObj.getStrSafe(Document_Last_Modified_JSON_Key)); }

  @Override protected boolean isNewEntry()             { return jObj.containsKey(Document_Last_Modified_JSON_Key) == false; }
  @Override protected void updateJsonObj(JsonObj jObj) { this.jObj = jObj; }
  @Override protected JsonArray getCollJsonArray()     { return jObj.getArray("folder_uuids"); }
  @Override protected String getUserID()               { return jObj.getStrSafe("profile_id"); }

  @Override protected void setAllAuthors(Iterable<Author> otherAuthors) { ((MendeleyAuthors) getAuthors()).setAll(otherAuthors); }

  static String getEntryTypeStrFromSpecifiedJson(JsonObj specJObj) { return specJObj.getStrSafe(entryTypeKey); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This factory method is for unit tests only and will throw an assertion error if run
   * outside of a unit test.
   * @param mWrapper Mendeley wrapper
   * @param jObj Pre-populated JSON object
   * @param newType Entry type
   * @return The new document
   */
  static MendeleyDocument createForUnitTest(MendeleyWrapper mWrapper, JsonObj jObj, EntryType newType)
  {
    MendeleyDocument entry = new MendeleyDocument(mWrapper, jObj, newType);

    mWrapper.addEntryForUnitTest(entry);

    return entry;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDate(BibliographicDate newDate)
  {
    if (linkedToWork())
    {
      getWork().setBibDate(newDate);
      return;
    }

    if (newDate.hasYear())
      jObj.put("year", Long.valueOf(newDate.year.numericValueWhereMinusOneEqualsOneBC()));
    else
      jObj.remove("year");

    if (newDate.hasMonth())
      jObj.put("month", Long.valueOf(newDate.month));
    else
      jObj.remove("month");

    if (newDate.hasDay())
      jObj.put("day", Long.valueOf(newDate.day));
    else
      jObj.remove("day");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfDOI  : getWork().setDOI (newStr); return;
        case bfURL  : getWork().setURL (newStr); return;
        default     : break;
      }
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfDOI :

        updateIdentifiers(fieldKey, newStr);

        return;

      case bfURL :

        if (strNullOrBlank(newStr))
          jObj.remove("websites");
        else
        {
          JsonArray jArr = new JsonArray();
          jArr.add(newStr);
          jObj.put("websites", jArr);
        }

        return;

      case bfVolume : case bfIssue   : case bfPages    : case bfPublisher :
      case bfPubLoc : case bfEdition : case bfLanguage :

        break;

      default : internalErrorPopup(90225); return;
    }

    if (strNullOrBlank(newStr))
    {
      jObj.remove(fieldKey);
      return;
    }

    if (jObj.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

    jObj.put(fieldKey, newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateIdentifiers(String fieldKey, String identifiersStr)
  {
    JsonObj idObj = jObj.getObj("identifiers");

    if (idObj == null)
    {
      if (strNullOrBlank(identifiersStr))
        return;

      jObj.put("identifiers", idObj = new JsonObj());
    }

    if (strNullOrBlank(identifiersStr))
    {
      idObj.remove(fieldKey);

      if (idObj.keySet().isEmpty())
        jObj.remove("identifiers");
    }
    else
      idObj.put(fieldKey, identifiersStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String entryTypeKey = "type";

  private static String getFieldKey(BibFieldEnum bibFieldEnum) { return switch (bibFieldEnum)
  {
    case bfEntryType -> entryTypeKey;
    case bfURL       -> "websites"; // array
    case bfVolume    -> "volume";
    case bfIssue     -> "issue";
    case bfPages     -> "pages";
    case bfPublisher -> "publisher";
    case bfPubLoc    -> "city"; // There is also a "country" field
    case bfEdition   -> "edition";
    case bfLanguage  -> "language";

    case bfContainerTitle -> "source";

    case bfISBNs -> "isbn";
    case bfISSNs -> "issn";
    case bfDOI   -> "doi";

    case bfTitle -> "title";
    case bfMisc  -> "notes";

    // Acccording to the API, "Three types of annotations are available. Notes are scoped to documents and provide a
    // high-level comments using styled text. Only a single note annotation can be attached to a document and
    // subsequent attempts to create further note annotations will fail. Note annotations have a value of note for the member type."

    case bfAuthors     -> "authors";
    case bfEditors     -> "editors";
    case bfTranslators -> "translators";
    case bfWorkType    -> "";

    default -> throw new IllegalArgumentException("Unexpected value: " + bibFieldEnum); // bfDate does not have a field key because there are separate field keys for year, month, and day
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibliographicDate getDateFromJson()
  {
    int year  = parseInt(safeStr(jObj.getAsStr("year" )), 0),
        month = parseInt(safeStr(jObj.getAsStr("month")), 0),
        day   = parseInt(safeStr(jObj.getAsStr("day"  )), 0);

    return new BibliographicDate(day, month, BibliographicYear.fromNumberWhereMinusOneEqualsOneBC(year));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getStr(BibFieldEnum bibFieldEnum)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfDOI   : return getWork().getDOI();
        case bfURL   : return getWork().getURL();
        case bfTitle : return getWork().name();
        default      : break;
      }
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfEntryType : return getEntryType().getUserFriendlyName();

      case bfDOI :

        return jObj.condObj("identifiers").condStrOrBlank("doi");

      case bfURL :

        JsonArray jArr = jObj.getArraySafe("websites");

        return jArr.isEmpty() ? "" : jArr.getStr(0);

      case bfVolume : case bfIssue   : case bfPages    : case bfPublisher :
      case bfPubLoc : case bfEdition : case bfLanguage :

        return jObj.getStrSafe(fieldKey);

      case bfAuthors     : return getAuthors().getStr(author);
      case bfEditors     : return getAuthors().getStr(editor);
      case bfTranslators : return getAuthors().getStr(translator);

      case bfContainerTitle : case bfTitle :

        return BibField.buildTitle(getMultiStr(bibFieldEnum));

      case bfDate : return getDateRawStr();

      case bfMisc : return strListToStr(getMultiStr(bibFieldEnum), false, true);

      default:
        break;
    }

    if (bibFieldEnum.getType() == BibFieldType.bftMultiString)
      return String.join("; ", getMultiStr(bibFieldEnum));

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setMultiStr(BibFieldEnum bibFieldEnum, List<String> list)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfTitle : getWork().setName(BibField.buildTitle(list));   return;
        case bfISBNs : getWork().setISBNs(list);                       return;
        case bfMisc  : getWork().setMiscBib(strListToStr(list, true)); return;
        default      : break;
      }
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfContainerTitle : case bfTitle :
      {
        String titleStr = strListToStr(list, false);
        if (jObj.getStrSafe(fieldKey).equals(safeStr(titleStr))) return;

        jObj.put(fieldKey, titleStr);

        return;
      }

      case bfMisc :
      {
        String miscStr = String.join("<br>", list);

        if (jObj.getStrSafe(fieldKey).equals(safeStr(miscStr))) return;

        jObj.put(fieldKey, miscStr);

        return;
      }

      case bfISBNs : case bfISSNs :
      {
        String identifiersStr = collEmpty(list) ? "" : strListToSpaceDelimitedStr(removeDuplicatesInPlace(new ArrayList<>(list)));

        updateIdentifiers(fieldKey, identifiersStr);

        return;
      }

      default : break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getMultiStr(BibFieldEnum bibFieldEnum)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfTitle : return List.of(safeStr(getWork().name()));
        case bfISBNs : return getWork().getISBNs();
        case bfMisc  : return convertMultiLineStrToStrList(getWork().getMiscBib(), true);
        default      : break;
      }
    }

    switch (bibFieldEnum)
    {
      case bfTitle : case bfContainerTitle :

        return convertMultiLineStrToStrList(jObj.getStrSafe(getFieldKey(bibFieldEnum)), false);

      case bfMisc  :

        String note = jObj.getStrSafe(getFieldKey(bibFieldEnum));

        note = note.replaceAll("<br>"   , "\n")
                   .replaceAll("<br/>"   , "\n")
                   .replaceAll("<[^>]*>", ""  );

        note = trimLines(note);

        return convertMultiLineStrToStrList(note, true);

      case bfISBNs : case bfISSNs :

        JsonObj idObj = jObj.getObj("identifiers");
        if (idObj == null) return new ArrayList<>();

        String str = idObj.getStrSafe(getFieldKey(bibFieldEnum));

        return bibFieldEnum == bfISBNs ? matchISBN(str) : matchISSN(str);

      default : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isSynced()
  {
    if (isNewEntry()) return false;
    if (thisIsBackup) return true;
    if (authorsChanged()) return false;

    return Arrays.stream(BibFieldEnum.values()).allMatch(bibFieldEnum ->
      bibFieldEnum == bfDate ?
        sameDateForSyncPurposes(backupItem.getDate(), getDate())
      :
        fieldsAreEqual(bibFieldEnum, backupItem, true));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean sameDateForSyncPurposes(BibliographicDate date1, BibliographicDate date2)
  {
    return (date1.day == date2.day) && (date1.month == date2.month) && (date1.year.numericValueWhereMinusOneEqualsOneBC() == date2.year.numericValueWhereMinusOneEqualsOneBC());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean authorsChanged()
  {
    EntryType entryType = getEntryType();

    // Create new "standalone" MendeleyAuthors object using the same logic that is used by exportStandaloneJsonObj
    // for comparison purposes.

    // The iterator method for both objects filters out editors if ignoreEditors is true.

    List<Author> list1 = new MendeleyAuthors(getAuthors(), entryType).normalizedList(false),
                 list2 = backupItem.getAuthors()                     .normalizedList(false);

    if (BibAuthors.authorListsAreEqual(list1, list2, true, true) == false)
      return true;

    // Now check book editors

    if (MendeleyAuthors.ignoreEditors(entryType))
    {
      JsonArray jArr1 = jObj.getArraySafe("editors"),
                jArr2 = ((MendeleyDocument) backupItem).jObj.getArraySafe("editors");

      if (jArr1.size() != jArr2.size()) return true;

      for (int ndx = 0; ndx < jArr1.size(); ndx++)
      {
        JsonObj ed1 = jArr1.getObj(ndx),
                ed2 = jArr2.getObj(ndx);

        if (ed1.getStrSafe("first_name").equals(ed2.getStrSafe("first_name")) == false) return true;
        if (ed1.getStrSafe("last_name" ).equals(ed2.getStrSafe("last_name" )) == false) return true;
      }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Convert the entry to a standalone JSON object
   * @param serverPatch True if we are patching a server document, so we have to set null to clear out values
   * @return The standalone JSON object
   */
  public JsonObj exportStandaloneJsonObj(boolean serverPatch)
  {
    JsonObj jStandaloneObj = jObj.deepCopy();

    if (isNewEntry())
      jStandaloneObj.remove("id");

    if (linkedToWork())
    {
      MendeleyDocument standaloneItem = new MendeleyDocument(getLibrary(), jStandaloneObj, true);

      standaloneItem.setStr(bfDOI, getStr(bfDOI));
      standaloneItem.setDate(getDate());

      String url = getStr(bfURL);
      if (url.startsWith(EXT_1) == false)
        standaloneItem.setStr(bfURL, url);

      standaloneItem.setMultiStr(bfISBNs, getMultiStr(bfISBNs));
      standaloneItem.setMultiStr(bfISSNs, getMultiStr(bfISSNs));
      standaloneItem.setMultiStr(bfMisc, getMultiStr(bfMisc));
      standaloneItem.setTitle(getStr(bfTitle));

      standaloneItem.setAllAuthors(getAuthors());
    }

    if (serverPatch)
    {
      // To clear an existing value when doing a Patch on the server,
      // you have to set the value to null.

      JsonObj jBackupObj = ((MendeleyDocument) backupItem).jObj;

      jBackupObj.keySet().forEach(key ->
      {
        if ((Noneditable_Document_JSON_Keys.contains(key) == false) && (jStandaloneObj.containsKey(key) == false))
          jStandaloneObj.putNull(key);
      });

      Noneditable_Document_JSON_Keys.forEach(jStandaloneObj::remove);
    }

    return jStandaloneObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setParentAuthorsFrom(BibAuthors authors)
  {
    JsonArray jsonArr = jObj.getArray("editors");
    if (jsonArr == null)
      jObj.put("editors", jsonArr = new JsonArray());

    jsonArr.clear();

    for (Author editor : streamToIterable(authors.normalizedList(false).stream().filter(Author::getIsEditor)))
    {
      JsonObj personObj = new JsonObj();

      personObj.put("first_name", removeAllParentheticals(editor.firstName()));
      personObj.put("last_name", editor.lastName());

      jsonArr.add(personObj);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void syncBookAuthorsTo(RelatedBibEntry relative)
  {
    MendeleyDocument dest = (MendeleyDocument) relative.entry();

    switch (relative.relation())
    {
      case Child:
      {
        dest.setParentAuthorsFrom(getAuthors());
        break;
      }

      case Parent:
      {
        JsonObj newVersion = dest.exportStandaloneJsonObj(false);

        // If parent has authors but the child doesn't have any parent-authors, don't do anything.
        // The parent authors might be correct, and the parent-authors might not have been set on
        // the child for some reason, for example if it was imported from another format.

        if ((newVersion.condArray("authors").size() > 0) && (jObj.condArray("editors").size() == 0))
          return;

        // Clear parent's authors

        newVersion.remove("authors");

        // Add child's parent-authors to parent

        JsonArray jsonArr = jObj.getArray("editors");
        newVersion.put("editors", jsonArr == null ? new JsonArray() : jsonArr.deepCopy());

        dest.getWork().getAuthors().setAll(new MendeleyAuthors(newVersion, dest.getEntryType()));

        break;
      }

      case Sibling:
      {
        JsonArray jsonArr = jObj.getArray("editors");
        dest.jObj.put("editors", jsonArr == null ? new JsonArray() : jsonArr.deepCopy());

        break;
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String createReport(boolean html)
  {
    return createReport(this, html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String createReport(MendeleyDocument document, boolean html)
  {
    ReportGenerator report = ReportGenerator.create(html);

    JsonObj jObj  = document.exportStandaloneJsonObj(false);

    jObj.keySet().forEach(key ->
    {
      if (Noneditable_Document_JSON_Keys.contains(key))
        return;

      String fieldName = formatMendeleyFieldName(key);

      switch (jObj.getType(key))
      {
        case OBJECT :

          JsonObj idObj = jObj.getObj("identifiers");
          idObj.keySet().forEach(idType ->
          {
            String typeStr = switch (idType)
            {
              case "arxiv" -> "ArXiv";

              case "doi",
                   "isbn",
                   "issn",
                   "pmid",
                   "ssrn"  -> idType.toUpperCase();

              default      -> formatMendeleyFieldName(idType);
            };

            report.addField(typeStr, makeReportString(report, typeStr, idObj.getStrSafe(idType)));
          });
          break;

        case ARRAY :

          JsonArray jArr = jObj.getArray(key);

          if ("authors".equals(key) || "editors".equals(key) || "translators".equals(key))
          {
            fieldName = formatMendeleyFieldName(key.substring(0, key.length() - 1));
            report.addField(fieldName, makeCreatorsReportContent(report, jArr, fieldName));
          }
          else
            report.addField(fieldName, makeReportArray(report, fieldName, jArr));

          break;

        case STRING :

          report.addField(fieldName, "notes".equals(key) ?
            report.makeRows(fieldName, document.getMultiStr(bfMisc).stream())
          :
            makeReportString(report, fieldName, jObj.getStrSafe(key)));
          break;

        case INTEGER :

          report.addField(fieldName, makeReportString(report, fieldName, jObj.getAsStr(key)));
          break;

        default:
          break;
      }
    });

    return report.render(document.getReportFieldOrder());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String formatMendeleyFieldName(String str)
  {
    return titleCase(str.replace('_', ' '));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeReportString(ReportGenerator report, String fieldName, String str)
  {
    if (str.isBlank()) return "";

    if ("Type".equals(fieldName))
      str = formatMendeleyFieldName(str);

    return report.makeRow(fieldName, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeCreatorsReportContent(ReportGenerator report, JsonArray creatorsArr, String type)
  {
    StringBuilder content = new StringBuilder();
    boolean foundAny = false;

    for (JsonObj node : creatorsArr.getObjs())
    {
      PersonName personName;
      String firstName = node.getStrSafe("first_name").strip(),
             lastName  = node.getStrSafe("last_name" ).strip();

      if (strNotNullOrEmpty(firstName) || strNotNullOrEmpty(lastName))
        personName = new PersonName(firstName, lastName);
      else
        continue;

      if (personName.isEmpty() == false)
      {
        if (foundAny)
          content.append(report.lineSeparator());
        else
          foundAny = true;

        content.append(report.makeRow(type, personName.getLastFirst()));
      }
    }

    return content.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeReportArray(ReportGenerator report, String fieldName, JsonArray jArr)
  {
    Stream<String> stream;

    if ("websites".equalsIgnoreCase(fieldName))
    {
      fieldName = "URL";
      stream = jArr.strStream().map(report::getUrlContent);
    }
    else
      stream = jArr.strStream();

    return report.makeRows(fieldName, stream);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getReportFieldOrder() { return List.of
  (
    "Type",
    "Title",
    "Year",
    "Author",
    "Editor",
    "Translator",
    "Source",
    "Edition",
    "Volume",
    "Issue",
    "Pages",
    "City",
    "Publisher"
  ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
