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

package org.hypernomicon.bib.zotero;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager.RelatedBibEntry;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.data.BibField.BibFieldType;
import org.hypernomicon.bib.reports.ReportGenerator;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonArray.JsonObjIterator;
import org.hypernomicon.util.json.JsonObj;

import com.google.common.collect.ImmutableList;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

public class ZoteroItem extends BibEntry<ZoteroItem, ZoteroCollection> implements ZoteroEntity
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonObj jData;

//---------------------------------------------------------------------------

  public ZoteroItem(ZoteroWrapper zWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    super(zWrapper, thisIsBackup);

    update(jObj, false, false);
  }

//---------------------------------------------------------------------------

  public ZoteroItem(ZoteroWrapper zWrapper, EntryType newType)
  {
    super(zWrapper, false);

    jData = ZoteroWrapper.getTemplate(newType).clone();
    jData.put(entryTypeKey, zWrapper.getEntryTypeMap().getOrDefault(newType, ""));
    jObj.put("data", jData);

    jObj.put("key", "_!_" + randomAlphanumericStr(12));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()               { return exportStandaloneJsonObj(false).toString(); }
  @Override public String getKey()                 { return jObj.getStrSafe("key"); }
  @Override public long getVersion()               { return jObj.getLong("version", 0); }
  @Override protected boolean isNewEntry()         { return jObj.containsKey("version") == false; }
  @Override protected JsonArray getCollJsonArray() { return jObj.getObj("data").getArray("collections"); }
  @Override public BibAuthors getAuthors()         { return linkedToWork() ? new WorkBibAuthors(getWork()) : new ZoteroAuthors(jData.getOrAddArray("creators"), getEntryType()); }
  @Override public EntryType getEntryType()        { return getLibrary().parseEntryType(getEntryTypeStrFromSpecifiedJson(jData)); }

  static String getEntryTypeStrFromSpecifiedJson(JsonObj specJObj) { return specJObj.getStrSafe(entryTypeKey); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void updateJsonObj(JsonObj jObj)
  {
    this.jObj = jObj;
    jData = jObj.getObj("data");

    // The remainder of this function is being done because I have seen at least one case
    // where the "data" node had the version and key specified, but the root node didn't,
    // presumably due to a Zotero server bug

    if ((jObj.containsKey("version") == false) && jData.containsKey("version"))
    {
      long version = jData.getLong("version", 0);
      if (version > 0)
        jObj.put("version", version);
    }

    if ((jObj.containsKey("key") == false) && jData.containsKey("key"))
      jObj.put("key", jData.getStr("key"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setDate(BibliographicDate newDate)
  {
    setDate(newDate, null);
  }

  /**
   * This updates the date, including the structured BibliographicDate representation and in the JSON object.
   * @param newRawDateStr New date represented as a String
   */
  public void setDateFromRawStr(String newRawDateStr)
  {
    setDate(BibliographicDate.fromUserStr(newRawDateStr), newRawDateStr);
  }

  /**
   * This updates the date, always including in the JSON object if newRawDateStr is non-null. It assumes newDate and rawDateStr represent the same date and doesn't verify that.
   * @param newDate New date
   * @param newRawDateStr New date represented as a String to save to the JSON object
   */
  private void setDate(BibliographicDate newDate, String newRawDateStr)
  {
    if (linkedToWork())
    {
      getWork().setBibDate(newDate);
      if (newRawDateStr == null) return;
    }

    String parsedDateStr = ZoteroDate.bibDateToParsedDateStr(newDate, true);

    int posIntYear = Math.abs(newDate.year.numericValueWhereMinusOneEqualsOneBC());

    if ((posIntYear > 9999) || (posIntYear < 1000) || (parsedDateStr.length() != 10))
      parsedDateStr = "";  // Zotero will only recognize a "multi-part" date if the year is 4 digits and month and day are present

    String origRawDateStr = jData.getStrSafe("date"),
           origParsedDateStr = jObj.condObj("meta").condStrOrBlank("parsedDate");

    if ((safeStr(newRawDateStr).isBlank() == false) && newRawDateStr.equals(origRawDateStr))
    {
      return;  // Preserve current date in the Zotero item JSON if raw versions are the same
    }

    if ((BibliographicDate.isEmpty(newDate) == false) &&
        Objects.equals(newDate, BibliographicDate.fromUserStr(origRawDateStr)) &&
        (safeStr(origParsedDateStr).isBlank() || Objects.equals(newDate, ZoteroDate.parsedDateStrToBibDate(origParsedDateStr, false))))
    {
      return;  // Preserve current date in the Zotero item JSON if it represents the same date
    }

    if (newRawDateStr == null)
      newRawDateStr = newDate.displayToUser(true);   // For years < 1000, we need to send it to Zotero as 4 digits with leading zeros;
                                                     // otherwise Zotero won't recognize it as a year
    jData.put("date", newRawDateStr);

    if (parsedDateStr.isBlank())
    {
      if (jObj.containsKey("meta"))
        jObj.getObj("meta").remove("parsedDate");
    }
    else
    {
      if (jObj.containsKey("meta") == false)
        jObj.put("meta", new JsonObj());

      jObj.getObj("meta").put("parsedDate", parsedDateStr);
    }
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

    switch (bibFieldEnum)
    {
      case bfDOI : case bfURL : case bfISBNs : case bfMisc : case bfTitle : break;
      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false) return;
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfDOI       : case bfURL    : case bfVolume  : case bfIssue    : case bfPages :
      case bfPublisher : case bfPubLoc : case bfEdition : case bfLanguage :

        break;

      default : internalErrorPopup(90225); return;
    }

    if (jData.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

    jData.put(fieldKey, newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String entryTypeKey = "itemType";

  private static final List<String> titleKeyList = List.of(

      "publicationTitle", "bookTitle"   , "encyclopediaTitle", "proceedingsTitle", "dictionaryTitle",
      "forumTitle",       "programTitle", "websiteTitle"     , "blogTitle"       , "seriesTitle");

  private String getFieldKey(BibFieldEnum bibFieldEnum) { return switch (bibFieldEnum)
  {
    case bfEntryType -> entryTypeKey;
    case bfDOI       -> "DOI";
    case bfURL       -> "url";
    case bfVolume    -> "volume";
    case bfIssue     -> "issue";
    case bfPages     -> "pages";
    case bfPublisher -> "publisher";
    case bfPubLoc    -> "place";
    case bfEdition   -> "edition";
    case bfLanguage  -> "language";
    case bfDate      -> "date";

    case bfContainerTitle ->
    {
      JsonObj template = ZoteroWrapper.getTemplate(getEntryType());
      yield template == null ? "" : safeStr(findFirst(titleKeyList, template::containsKey));
    }

    case bfISBNs -> "ISBN";
    case bfISSNs -> "ISSN";

    case bfTitle -> "title";
    case bfMisc  -> "extra";

    case bfAuthors, bfEditors, bfTranslators -> "creators";

    case bfWorkType -> "";
  };}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean thisTypeHasFieldKey(BibFieldEnum bibFieldEnum)
  {
    JsonObj template = ZoteroWrapper.getTemplate(getEntryType());
    if (template == null) return false;

    String fieldKey = getFieldKey(bibFieldEnum);
    if (safeStr(fieldKey).isEmpty()) return false;

    return template.containsKey(fieldKey);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getDateRawStr()
  {
    if (linkedToWork())
    {
      BibliographicDate date = getDate();

      // If the date information in the work matches what's in the JSON object, use the raw value from the JSON object. Otherwise, use the date from the work record.

      if ((jObj.containsKey("meta") &&
           ZoteroDate.bibDateToParsedDateStr(date).equals(jObj.getObj("meta").getStrSafe("parsedDate")) &&
           (date.year.numericValueWhereMinusOneEqualsOneBC() > 0) &&
           safeStr(date.year.rawValue).equals(String.valueOf(date.year.numericValueWhereMinusOneEqualsOneBC()))) == false)
        return super.getDateRawStr();
    }

    return jData.getStrSafe(getFieldKey(bfDate));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibliographicDate getDateFromJson()
  {
    // Don't use parsedDate from meta Json node for year because we may lose whether it is supposed to be a negative (BC) year

    int day = 0, month = 0;

    JsonObj jMeta = jObj.getObj("meta");
    if (jMeta != null)
    {
      String parsedDateStr = jMeta.getStrSafe("parsedDate");
      BibliographicDate parsedDate = ZoteroDate.parsedDateStrToBibDate(parsedDateStr, false);

      day = parsedDate.day;
      month = parsedDate.month;
    }

    BibliographicDate date = BibliographicDate.fromUserStr(jData.getStrSafe(getFieldKey(bfDate)));
    return new BibliographicDate(day > 0 ? day : date.day, month > 0 ? month : date.month, date.year);
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

      case bfDOI       : case bfURL       : case bfVolume    : case bfIssue     : case bfPages :
      case bfPublisher : case bfPubLoc    : case bfEdition   : case bfLanguage  :

        return jData.getStrSafe(fieldKey);

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

    switch (bibFieldEnum)
    {
      case bfDOI : case bfURL : case bfISBNs : case bfMisc : case bfTitle : break;
      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false) return;
    }

    String fieldKey = getFieldKey(bibFieldEnum), newStr = null;

    switch (bibFieldEnum)
    {
      case bfContainerTitle : case bfTitle : newStr = strListToStr(list, false); break;
      case bfMisc           :                newStr = strListToStr(list, true ); break;

      case bfISBNs          : case bfISSNs : newStr = collEmpty(list) ? "" : strListToSpaceDelimitedStr(removeDuplicatesInPlace(new ArrayList<>(list))); break;

      default               : return;
    }

    if (jData.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

    jData.put(fieldKey, newStr);
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
      case bfTitle : return convertMultiLineStrToStrList(jData.getStrSafe(getFieldKey(bibFieldEnum)), false);

      case bfContainerTitle :

        String                        containerTitle = jData.getStrSafe("publicationTitle");
        if (containerTitle.isEmpty()) containerTitle = jData.getStrSafe("bookTitle");
        if (containerTitle.isEmpty()) containerTitle = jData.getStrSafe("seriesTitle");

        return convertMultiLineStrToStrList(containerTitle, false);

      case bfMisc : return convertMultiLineStrToStrList(jData.getStrSafe(getFieldKey(bibFieldEnum)), true);

      case bfISBNs : return matchISBN(jData.getStrSafe(getFieldKey(bibFieldEnum)));

      case bfISSNs : return matchISSN(jData.getStrSafe(getFieldKey(bibFieldEnum)));

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

    for (BibFieldEnum bibFieldEnum : BibFieldEnum.values())
    {
      if (thisTypeHasFieldKey(bibFieldEnum))
      {
        if (bibFieldEnum == bfDate)
        {
          // For certain dates, including B.C. dates and years below 1000, Zotero cannot be relied upon
          // to correctly parse the date. Sending a "multi-part" date (where the first part is already
          // parsed into ISO 8601) doesn't work in those cases, either. So we also have to check whether
          // the raw, unparsed values are the same.

          String backupDateRawStrNoLeadingZeros = BibliographicDate.removeLeadingZerosFromDateStr(backupItem.getDateRawStr());

          if ((fieldsAreEqual(bibFieldEnum, backupItem, true        ) == false)  &&
              (getDateRawStr().equals(backupItem.getDateRawStr()    ) == false)  &&
              (getDateRawStr().equals(backupDateRawStrNoLeadingZeros) == false))     // For years < 1000, we need to send it to Zotero as 4 digits with leading zeros;
            return false;                                                            // otherwise Zotero won't recognize it as a year. So we need to strip those zeros
        }                                                                            // when checking for equality.
        else if (fieldsAreEqual(bibFieldEnum, backupItem, true) == false)
          return false;
      }
    }

    return true;
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

    EntryType entryType = getEntryType();

    // Backup item will not have authors of certain types if they are not allowed by Zotero for this entry type

    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.author    ).isEmpty()) authorList1    .clear();
    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.editor    ).isEmpty()) editorList1    .clear();
    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.translator).isEmpty()) translatorList1.clear();

    if ((authorList1    .size() != authorList2    .size()) ||
        (editorList1    .size() != editorList2    .size()) ||
        (translatorList1.size() != translatorList2.size()))   return true;

    // Now check book editors

    if (((ZoteroAuthors) backupItem.getAuthors()).ignoreEditors())
    {
      JsonArray creatorsArr1 = jData.getOrAddArray("creators"),
                creatorsArr2 = ((ZoteroItem) backupItem).jData.getOrAddArray("creators");

      JsonArray jArr1 = new JsonArray(), jArr2 = new JsonArray();

      creatorsArr1.getObjs().forEach(creator ->
      {
        String type = creator.getStrSafe("creatorType");
        if ("editor".equals(type) || "bookAuthor".equals(type))
          jArr1.add(creator);
      });

      creatorsArr2.getObjs().forEach(creator ->
      {
        String type = creator.getStrSafe("creatorType");
        if ("editor".equals(type) || "bookAuthor".equals(type))
          jArr2.add(creator);
      });

      if (jArr1.size() != jArr2.size()) return true;

      for (int ndx = 0; ndx < jArr1.size(); ndx++)
      {
        JsonObj ed1 = jArr1.getObj(ndx),
                ed2 = jArr2.getObj(ndx);

        if (ed1.getStrSafe("creatorType").equals(ed2.getStrSafe("creatorType")) == false) return true;

        if (ed1.getStrSafe("firstName").equals(ed2.getStrSafe("firstName")) == false) return true;
        if (ed1.getStrSafe("lastName" ).equals(ed2.getStrSafe("lastName" )) == false) return true;
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

  JsonObj exportStandaloneJsonObj(boolean forUploadToServer)
  {
    JsonObj jStandaloneObj = jObj.clone();

    boolean missingKeysOK = !forUploadToServer;

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum)
    {
      case bfDOI : case bfDate : case bfURL : case bfISBNs : case bfMisc : case bfTitle :
        if (missingKeysOK)
          break;

        // Else: Fall through

      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false)
        {
          String fieldKey = getFieldKey(bibFieldEnum);
          if (fieldKey.length() > 0)
            jStandaloneObj.getObj("data").remove(getFieldKey(bibFieldEnum)); // This should be done even if missingKeysOK == true
        }

        break;
    }});

    if (linkedToWork())
    {
      ZoteroItem standaloneItem = new ZoteroItem(getLibrary(), jStandaloneObj, true);

      if (missingKeysOK || thisTypeHasFieldKey(bfDOI  )) standaloneItem.setStr(bfDOI, getStr(bfDOI));
      if (missingKeysOK || thisTypeHasFieldKey(bfDate )) standaloneItem.setDate(getDate());

      if (missingKeysOK || thisTypeHasFieldKey(bfURL  ))
      {
        String url = getStr(bfURL);
        if (url.startsWith(EXT_1) == false)
          standaloneItem.setStr(bfURL, url);
      }

      if (missingKeysOK || thisTypeHasFieldKey(bfISBNs)) standaloneItem.setMultiStr(bfISBNs, getMultiStr(bfISBNs));
      if (missingKeysOK || thisTypeHasFieldKey(bfMisc )) standaloneItem.setMultiStr(bfMisc, getMultiStr(bfMisc));
      if (missingKeysOK || thisTypeHasFieldKey(bfTitle)) standaloneItem.setTitle(getStr(bfTitle));

      BibAuthors standaloneAuthors = standaloneItem.getAuthors();
      standaloneAuthors.clear();

      getAuthors().forEach(standaloneAuthors::add);
    }

    if (forUploadToServer)
    {
      JsonObj jMeta = jStandaloneObj.getObj("meta");
      if (jMeta != null)
      {
        String parsedDateStr = jMeta.getStrSafe("parsedDate");

        if (parsedDateStr.length() == 10)  // Zotero only recognizes a multi-part date if the first 10 characters are a 10-character ISO date
          jStandaloneObj.getObj("data").put("date", parsedDateStr + ' ' + jStandaloneObj.getObj("data").getStrSafe("date"));  // Send up to the server as a "multi-part" date so Zotero doesn't have to parse a raw string

        jMeta.remove("parsedDate");
      }
    }

    return isNewEntry() ? jStandaloneObj.getObj("data") : jStandaloneObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setParentAuthorsFrom(BibAuthors authors)
  {
    List<BibAuthor> authorList     = new ArrayList<>(),
                    editorList     = new ArrayList<>(),
                    translatorList = new ArrayList<>();

    authors.getLists(authorList, editorList, translatorList);

    JsonArray creatorsArr = jData.getOrAddArray("creators");

    JsonObjIterator it = creatorsArr.getObjs();
    while (it.hasNext())
    {
      JsonObj creator = it.next();
      String type = creator.getStrSafe("creatorType");
      if ("editor".equals(type) || "bookAuthor".equals(type))
        it.remove();
    }

    for (BibAuthor author : authorList)
    {
      JsonObj creatorObj = new JsonObj();

      creatorObj.put("firstName", removeAllParentheticals(author.getGiven()));
      creatorObj.put("lastName", author.getFamily());
      creatorObj.put("creatorType", "bookAuthor");

      creatorsArr.add(creatorObj);
    }

    for (BibAuthor editor : editorList)
    {
      JsonObj creatorObj = new JsonObj();

      creatorObj.put("firstName", removeAllParentheticals(editor.getGiven()));
      creatorObj.put("lastName", editor.getFamily());
      creatorObj.put("creatorType", "editor");

      creatorsArr.add(creatorObj);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void syncBookAuthorsTo(RelatedBibEntry relative)
  {
    ZoteroItem dest = (ZoteroItem) relative.entry();

    switch (relative.relation())
    {
      case Child :
      {
        dest.setParentAuthorsFrom(getAuthors());
        break;
      }

      case Parent:
      {
        JsonObj jDestObj  = dest.exportStandaloneJsonObj(false),
                jDestData = nullSwitch(jDestObj.getObj("data"), jDestObj);

        JsonArray oldCreatorsArr = jDestData.getOrAddArray("creators"), newCreatorsArr = new JsonArray();

        // If parent has authors but the child doesn't have any parent-authors, don't do anything.
        // The parent authors might be correct, and the parent-authors might not have been set on
        // the child for some reason, for example if it was imported from another format.

        List<String> selfAuthorTypes   = List.of("editor", "author"),
                     parentAuthorTypes = List.of("editor", "bookAuthor");

        if (oldCreatorsArr.objStream().anyMatch(creator -> selfAuthorTypes.contains(creator.getStrSafe("creatorType"))))
          if (jData.getOrAddArray("creators").objStream().filter(creator -> parentAuthorTypes.contains(creator.getStrSafe("creatorType"))).findAny().isEmpty())
            return;

        // Clear parent's authors

        JsonObjIterator it = oldCreatorsArr.getObjs();
        while (it.hasNext())
        {
          JsonObj creator = it.next();
          if (selfAuthorTypes.contains(creator.getStrSafe("creatorType")))
            it.remove();
        }

        // Add child's parent-authors to parent

        jData.getOrAddArray("creators").getObjs().forEach(oldCreator ->
        {
          if (parentAuthorTypes.contains(oldCreator.getStrSafe("creatorType")))
          {
            JsonObj newCreator = oldCreator.clone();
            if ("bookAuthor".equals(newCreator.getStrSafe("creatorType")))
              newCreator.put("creatorType", "book");

            newCreatorsArr.add(newCreator);
          }
        });

        oldCreatorsArr.getObjs().forEach(creator -> newCreatorsArr.add(creator.clone()));

        dest.getWork().getAuthors().setAll(new ZoteroAuthors(newCreatorsArr, dest.getEntryType()));

        break;
      }

      case Sibling:
      {
        JsonArray destCreatorsArr = dest.jData.getOrAddArray("creators");

        JsonObjIterator it = destCreatorsArr.getObjs();
        while (it.hasNext())
        {
          JsonObj creator = it.next();
          String type = creator.getStrSafe("creatorType");
          if ("editor".equals(type) || "bookAuthor".equals(type))
            it.remove();
        }

        JsonArray srcCreatorsArr = jData.getOrAddArray("creators");

        srcCreatorsArr.getObjs().forEach(creator ->
        {
          String type = creator.getStrSafe("creatorType");
          if ("editor".equals(type) || "bookAuthor".equals(type))
            destCreatorsArr.add(creator.clone());
        });

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

  private static String createReport(ZoteroItem item, boolean html)
  {
    ReportGenerator report = ReportGenerator.create(html);

    JsonObj jObj  = item.exportStandaloneJsonObj(false),
            jData = nullSwitch(jObj.getObj("data"), jObj);

    jData.keySet().forEach(key ->
    {
      String fieldName = key;

      switch (fieldName)
      {
        case "relations" : case "collections" : case "key" :
        case "dateAdded" : case "accessDate"  : case "dateModified" :

          return;

        case "archiveID" :

          fieldName = "Archive ID";
          break;

        case "url"  :

          fieldName = "URL";
          break;

        case "ISBN" : case "DOI" : case "ISSN" :

          break;

        default :

          fieldName = camelToTitle(fieldName);
          break;
      }

      switch (jData.getType(key))
      {
        case ARRAY:

          JsonArray jArr = jData.getArray(key);

          if ("creators".equals(key))
            report.addField("Creators", makeCreatorsReportContent(report, jArr));
          else if ("tags".equals(key))
            report.addField("Tags", makeTagsReportContent(report, jArr));
          else
            report.addField(fieldName, makeReportArray(report, fieldName, jArr));

          break;

        case STRING:

          String value = jData.getStrSafe(key);

          if ("date".equals(key))
            value = BibliographicDate.removeLeadingZerosFromDateStr(value);

          report.addField(fieldName, makeReportString(report, fieldName, value));
          break;

        default:
          break;
      }
    });

    return report.render(item.getReportFieldOrder());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeReportString(ReportGenerator report, String fieldName, String str)
  {
    if (str.isEmpty()) return "";

    if ("Item Type".equals(fieldName))
      str = camelToTitle(str);

    if ("URL".equals(fieldName))
      str = report.getUrlContent(str);

    return report.makeRow(fieldName, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeTagsReportContent(ReportGenerator report, JsonArray tagsArr)
  {
    return report.makeRows("Tags", tagsArr.objStream().map(node -> node.getStrSafe("tag")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeCreatorsReportContent(ReportGenerator report, JsonArray creatorsArr)
  {
    StringBuilder content = new StringBuilder();
    boolean foundAny = false;

    for (JsonObj node : creatorsArr.getObjs())
    {
      String type = node.getStrSafe("creatorType");

      if (type.isEmpty()) continue;

      type = camelToTitle(type);
      PersonName personName;
      String firstName = ultraTrim(node.getStrSafe("firstName")),
             lastName  = ultraTrim(node.getStrSafe("lastName" ));

      if ((firstName.length() > 0) || (lastName.length() > 0))
        personName = new PersonName(firstName, lastName);
      else
      {
        personName = new PersonName(node.getStrSafe("name"));
        if (personName.isEmpty()) continue;
      }

      if (foundAny)
        content.append(report.lineSeparator());
      else
        foundAny = true;

      content.append(report.makeRow(type, personName.getLastFirst()));
    }

    return content.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeReportArray(ReportGenerator report, String fieldName, JsonArray jArr)
  {
    return report.makeRows(fieldName, jArr.strStream());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public List<String> getReportFieldOrder() { return List.of
  (
    "Title",
    "Item Type",
    "Date",
    "Creators",
    "Publication Title",
    "Book Title",
    "Edition",
    "Volume",
    "Issue",
    "Pages",
    "Place",
    "Publisher"
  ); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getURLtoViewEntryInRefMgr()
  {
    return isNewEntry() ? "" : jObj.condObj("links").condObj("alternate").condStrOrBlank("href");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected String getUserName()
  {
    String url = isNewEntry() ? "" : jObj.condObj("library").condObj("links").condObj("alternate").condStrOrBlank("href");

    if (url.isBlank()) return "";

    List<String> list = ImmutableList.copyOf((Iterator<String>)new SplitString(url, '/'));

    for (int ndx = list.size() - 1; ndx > 0; ndx--)
    {
      String str = list.get(ndx);
      if (str.length() > 0)
        return str;
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
