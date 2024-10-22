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

package org.hypernomicon.bib.mendeley;

import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager.RelatedBibEntry;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.BibField.BibFieldType;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.BibliographicYear;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
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
//---------------------------------------------------------------------------

  private static final List<String> Noneditable_Document_JSON_Keys = List.of
  (
    "profile_id", "id", "created", Document_Last_Modified_JSON_Key, "group_id", "accessed", "citation_key", "folder_uuids",
    "authored", "read", "hidden", "confirmed", "file_attached", "private_publication", "starred"
  );

//---------------------------------------------------------------------------

  @Override public String toString()                   { return exportStandaloneJsonObj(false).toString(); }
  @Override public String getKey()                     { return jObj.getStrSafe("id"); }
  @Override protected boolean isNewEntry()             { return jObj.containsKey(Document_Last_Modified_JSON_Key) == false; }
  @Override protected void updateJsonObj(JsonObj jObj) { this.jObj = jObj; }
  @Override protected JsonArray getCollJsonArray()     { return jObj.getArray("folder_uuids"); }
  @Override public String getURLtoViewEntryInRefMgr()  { return ""; }
  @Override public BibAuthors getAuthors()             { return linkedToWork() ? new WorkBibAuthors(getWork()) : new MendeleyAuthors(jObj, getEntryType()); }
  @Override public EntryType getEntryType()            { return getLibrary().parseEntryType(getEntryTypeStrFromSpecifiedJson(jObj)); }

  static String getEntryTypeStrFromSpecifiedJson(JsonObj specJObj) { return specJObj.getStrSafe(entryTypeKey); }

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

        if (safeStr(newStr).isBlank())
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

    if (safeStr(newStr).isBlank())
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
      if (safeStr(identifiersStr).isBlank())
        return;

      jObj.put("identifiers", idObj = new JsonObj());
    }

    if (safeStr(identifiersStr).isBlank())
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

        JsonArray jArr = jObj.getArray("websites");

        return (jArr != null) && (jArr.size() > 0) ? jArr.getStr(0) : "";

      case bfVolume : case bfIssue   : case bfPages    : case bfPublisher :
      case bfPubLoc : case bfEdition : case bfLanguage :

        return jObj.getStrSafe(fieldKey);

      case bfAuthors     : return getAuthors().getStr(AuthorType.author);
      case bfEditors     : return getAuthors().getStr(AuthorType.editor);
      case bfTranslators : return getAuthors().getStr(AuthorType.translator);

      case bfContainerTitle : case bfTitle :

        return BibField.buildTitle(getMultiStr(bibFieldEnum));

      case bfDate : return getDateRawStr();

      case bfMisc : return strListToStr(getMultiStr(bibFieldEnum), false, true);

      default:
        break;
    }

    if (bibFieldEnum.getType() == BibFieldType.bftMultiString)
      return getMultiStr(bibFieldEnum).stream().collect(Collectors.joining("; "));

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
        String miscStr = list.stream().collect(Collectors.joining("<br>"));

        if (jObj.getStrSafe(fieldKey).equals(safeStr(miscStr))) return;

        jObj.put(fieldKey, miscStr);

        return;
      }

      case bfISBNs : case bfISSNs :
      {
        String identifiersStr = collEmpty(list) ? "" : strListToSpaceDelimitedStr(removeDupsInStrList(new ArrayList<>(list)));

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
    List<BibAuthor> authorList1     = new ArrayList<>(), authorList2     = new ArrayList<>(),
                    editorList1     = new ArrayList<>(), editorList2     = new ArrayList<>(),
                    translatorList1 = new ArrayList<>(), translatorList2 = new ArrayList<>();

    getAuthors().getLists(authorList1, editorList1, translatorList1);
    backupItem.getAuthors().getLists(authorList2, editorList2, translatorList2);

    if ((authorList1    .size() != authorList2    .size()) ||
        (editorList1    .size() != editorList2    .size()) ||
        (translatorList1.size() != translatorList2.size()))   return true;

    // Now check book editors

    if (((MendeleyAuthors) backupItem.getAuthors()).ignoreEditors())
    {
      JsonArray jArr1 = jObj.getArray("editors"),
                jArr2 = ((MendeleyDocument) backupItem).jObj.getArray("editors");

      if ((jArr1 == null) != (jArr2 == null)) return true;
      if (jArr1 != null)
      {
        if (jArr1.size() != jArr2.size()) return true;

        for (int ndx = 0; ndx < jArr1.size(); ndx++)
        {
          JsonObj ed1 = jArr1.getObj(ndx),
                  ed2 = jArr2.getObj(ndx);

          if (ed1.getStrSafe("first_name").equals(ed2.getStrSafe("first_name")) == false) return true;
          if (ed1.getStrSafe("last_name" ).equals(ed2.getStrSafe("last_name" )) == false) return true;
        }
      }
    }

    for (int ndx = 0; ndx < authorList1.size(); ndx++)
      if (authorList1.get(ndx).getName().equalsExceptParenthetical(authorList2.get(ndx).getName()) == false) return true;

    for (int ndx = 0; ndx < editorList1.size(); ndx++)
      if (editorList1.get(ndx).getName().equalsExceptParenthetical(editorList2.get(ndx).getName()) == false) return true;

    for (int ndx = 0; ndx < translatorList1.size(); ndx++)
      if (translatorList1.get(ndx).getName().equalsExceptParenthetical(translatorList2.get(ndx).getName()) == false) return true;

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Convert the entry to a standalone JSON object
   * @param serverPatch True if we are patching a server document, so we have to set null to clear out values
   * @return The standalone JSON object
   */
  JsonObj exportStandaloneJsonObj(boolean serverPatch)
  {
    JsonObj jStandaloneObj = jObj.clone();

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

      BibAuthors standaloneAuthors = standaloneItem.getAuthors();
      standaloneAuthors.clear();

      getAuthors().forEach(standaloneAuthors::add);
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
    List<BibAuthor> authorList     = new ArrayList<>(),
                    editorList     = new ArrayList<>(),
                    translatorList = new ArrayList<>();

    authors.getLists(authorList, editorList, translatorList);

    JsonArray jsonArr = jObj.getArray("editors");
    if (jsonArr == null)
      jObj.put("editors", jsonArr = new JsonArray());

    jsonArr.clear();

    for (BibAuthor editor : editorList)
    {
      JsonObj personObj = new JsonObj();

      personObj.put("first_name", removeAllParentheticals(editor.getGiven()));
      personObj.put("last_name", editor.getFamily());

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

        newVersion.put("editors", jObj.getArray("editors").clone());

        dest.getWork().getAuthors().setAll(new MendeleyAuthors(newVersion, dest.getEntryType()));

        break;
      }

      case Sibling:
      {
        JsonArray jsonArr = jObj.getArray("editors");
        dest.jObj.put("editors", jsonArr == null ? new JsonArray() : jsonArr.clone());

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
      String firstName = ultraTrim(node.getStrSafe("first_name")),
             lastName  = ultraTrim(node.getStrSafe("last_name" ));

      if ((firstName.length() > 0) || (lastName.length() > 0))
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

  @Override public Instant lastModifiedOnServer()
  {
    return MendeleyWrapper.getSyncInstantFromJsonStr(jObj.getStrSafe(Document_Last_Modified_JSON_Key));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected String getUserName()
  {
    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
