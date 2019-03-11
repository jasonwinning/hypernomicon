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

package org.hypernomicon.bib.zotero;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;

import org.hypernomicon.bib.BibAuthor;
import org.hypernomicon.bib.BibAuthors;
import org.hypernomicon.bib.BibField;
import org.hypernomicon.bib.BibUtils;
import org.hypernomicon.bib.WorkBibAuthors;
import org.hypernomicon.bib.lib.BibEntry;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import com.google.common.collect.Lists;

import static org.hypernomicon.bib.BibData.EntryType.*;
import static org.hypernomicon.bib.BibData.BibFieldEnum.*;
import static org.hypernomicon.bib.BibUtils.matchISBN;
import static org.hypernomicon.bib.BibUtils.matchISSN;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public class ZoteroItem extends BibEntry implements ZoteroEntity
{
  private final ZoteroWrapper zWrapper;
  private JsonObj jObj, jData;
  private ZoteroItem backupItem = null;

  ZoteroItem(ZoteroWrapper zWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    super(thisIsBackup);

    update(jObj, false, false);
    this.zWrapper = zWrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  ZoteroItem(ZoteroWrapper zWrapper, EntryType newType)
  {
    super(false);

    this.jObj = new JsonObj();
    this.jData = zWrapper.getTemplate(newType).clone();
    jData.put(getFieldKey(bfEntryType), ZoteroWrapper.entryTypeMap.getOrDefault(newType, ""));
    jObj.put("data", jData);
    this.zWrapper = zWrapper;

    jObj.put("key", "_!_" + randomAlphanumericStr(12));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ZoteroEntityType getType() { return ZoteroEntityType.zoteroItem; }
  @Override public String getEntryKey()       { return getKey(); }
  @Override public String toString()          { return jObj.toString(); }
  @Override public String getKey()            { return jObj.getStr("key"); }
  @Override public long getVersion()          { return jObj.getLong("version", 0); }
  @Override protected boolean isNewEntry()    { return jObj.containsKey("version") == false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getEntryURL()
  {
    if (isNewEntry()) return "";

    return nullSwitch(jObj.getObj("links"), "", links -> nullSwitch(links.getObj("alternate"), "", alt -> alt.getStrSafe("href")));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public HDT_WorkType getWorkType()
  {
    if (linkedToWork()) return getWork().workType.get();

    return convertEntryTypeToWorkType(getEntryType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setWorkType(HDT_WorkType workType)
  {
    if (linkedToWork()) getWork().workType.set(workType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public BibAuthors getAuthors()
  {
    if (linkedToWork()) return new WorkBibAuthors(getWork());

    return new ZoteroAuthors(jData.getArray(getFieldKey(bfAuthors)), getEntryType());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge)
  {
    this.jObj = jObj;
    this.jData = jObj.getObj("data");

    jObj.remove("synced");

    if (thisIsBackup)
    {
      jObj.remove("backupItem");
      return;
    }

    JsonObj jBackupObj;

    if (jObj.containsKey("backupItem"))
    {
      jBackupObj = jObj.getObj("backupItem");
      jObj.remove("backupItem");
    }
    else
      jBackupObj = jObj.clone();

    this.backupItem = new ZoteroItem(zWrapper, jBackupObj, true);

    if ((updatingExistingDataFromServer == false) || (linkedToWork() == false)) return;

    setMultiStr(bfTitle, backupItem.getMultiStr(bfTitle));
    setMultiStr(bfISBNs, backupItem.getMultiStr(bfISBNs));
    setMultiStr(bfMisc, backupItem.getMultiStr(bfMisc));
    setStr(bfDOI, backupItem.getStr(bfDOI));
    setStr(bfYear, backupItem.getStr(bfYear));
    setStr(bfURL, backupItem.getStr(bfURL));

    if (preMerge) return; // authors always get updated during merge

    if (authorsChanged() == false) return;

    zWrapper.doMerge(this, jBackupObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected List<String> getCollKeys(boolean deletedOK)
  {
    JsonArray collArray = jObj.getObj("data").getArray("collections");

    if (collArray != null)
      if ((zWrapper.getTrash().contains(this) == false) || deletedOK)
        return Lists.newArrayList((Iterable<String>)collArray.getStrs());

    return new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static EntryType parseZoteroType(String zType)
  {
    return ZoteroWrapper.entryTypeMap.inverse().getOrDefault(zType, etOther);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public EntryType getEntryType()
  {
    return parseZoteroType(jData.getStrSafe(getFieldKey(bfEntryType)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setEntryType(EntryType entryType)
  {
    if (entryType == getEntryType()) return;

    // jData.put("itemType", ZoteroWrapper.entryTypeMap.getOrDefault(entryType, ""));

    throw new UnsupportedOperationException("change Zotero entry type");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfYear : getWork().setYear   (newStr); return;
        case bfDOI  : getWork().setDOI    (newStr); return;
        case bfURL  : getWork().setWebLink(newStr); return;
        default     : break;
      }
    }

    switch (bibFieldEnum)
    {
      case bfDOI : case bfYear : case bfURL : case bfISBNs : case bfMisc : case bfTitle : break;
      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false) return;
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfYear :

        newStr = safeStr(newStr);
        if (newStr.matches("[12]\\d\\d\\d") == false) break;

        String oldDate = jData.getStrSafe(fieldKey);
        String oldYear = extractYear(oldDate);
        if (oldYear.length() == 0) break;

        if (oldYear.equals(newStr)) return;

        jData.put(fieldKey, oldDate.replaceFirst("[12]\\d\\d\\d", newStr)); // Leave all parts of the date other than the year intact
        return;

      case bfDOI       : case bfURL    : case bfVolume  : case bfIssue    : case bfPages :
      case bfPublisher : case bfPubLoc : case bfEdition : case bfLanguage :

        break;

      default : messageDialog("Internal error #90225", mtError); return;
    }

    if (jData.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

    jData.put(fieldKey, newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getFieldKey(BibFieldEnum bibFieldEnum)
  {
    switch (bibFieldEnum)
    {
      case bfEntryType : return "itemType";
      case bfDOI       : return "DOI";
      case bfURL       : return "url";
      case bfVolume    : return "volume";
      case bfIssue     : return "issue";
      case bfPages     : return "pages";
      case bfPublisher : return "publisher";
      case bfPubLoc    : return "place";
      case bfEdition   : return "edition";
      case bfLanguage  : return "language";
      case bfYear      : return "date";

      case bfContainerTitle :

        JsonObj template = zWrapper.getTemplate(getEntryType());
        if (template == null) return "";

        if      (template.containsKey("publicationTitle")) return "publicationTitle";
        else if (template.containsKey("bookTitle"       )) return "bookTitle";
        else if (template.containsKey("seriesTitle"     )) return "seriesTitle";

        return "";

      case bfISBNs : return "ISBN";
      case bfISSNs : return "ISSN";

      case bfTitle : return "title";
      case bfMisc  : return "extra";

      case bfAuthors: case bfEditors: case bfTranslators:

        return "creators";

      case bfWorkType: return "";
    }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean thisTypeHasFieldKey(BibFieldEnum bibFieldEnum)
  {
    JsonObj template = zWrapper.getTemplate(getEntryType());
    if (template == null) return false;

    String fieldKey = getFieldKey(bibFieldEnum);
    if (safeStr(fieldKey).equals("")) return false;

    return template.containsKey(fieldKey);
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
        case bfYear  : return getWork().getYear();
        case bfURL   : return getWork().getWebLink();
        case bfTitle : return getWork().name();
        default      : break;
      }
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfEntryType : return BibUtils.getEntryTypeName(getEntryType());

      case bfDOI       : case bfURL       : case bfVolume    : case bfIssue     : case bfPages :
      case bfPublisher : case bfPubLoc    : case bfEdition   : case bfLanguage  :

        return jData.getStrSafe(fieldKey);

      case bfYear : return extractYear(jData.getStrSafe(fieldKey));

      case bfAuthors     : return getAuthors().getStr(AuthorType.author);
      case bfEditors     : return getAuthors().getStr(AuthorType.editor);
      case bfTranslators : return getAuthors().getStr(AuthorType.translator);

      case bfContainerTitle : case bfTitle :

        return BibField.buildTitle(getMultiStr(bibFieldEnum));

      case bfMisc : return strListToStr(getMultiStr(bibFieldEnum), false, true);

      default:
        break;
    }

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
      case bfDOI : case bfYear : case bfURL : case bfISBNs : case bfMisc : case bfTitle : break;
      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false) return;
    }

    String fieldKey = getFieldKey(bibFieldEnum), newStr = null;

    switch (bibFieldEnum)
    {
      case bfContainerTitle : case bfTitle : newStr = strListToStr(list, false      ); break;
      case bfMisc           :                newStr = strListToStr(list, true       ); break;
      case bfISBNs          : case bfISSNs : newStr = getMultiStrSpaceDelimited(list); break;
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
        case bfTitle : return Collections.singletonList(getWork().name());
        case bfISBNs : return getWork().getISBNs();
        case bfMisc  : return convertMultiLineStrToStrList(getWork().getMiscBib(), true);
        default      : break;
      }
    }

    switch (bibFieldEnum)
    {
      case bfTitle : return convertMultiLineStrToStrList(jData.getStrSafe(getFieldKey(bibFieldEnum)), false);

      case bfContainerTitle :

        String                            containerTitle = jData.getStrSafe("publicationTitle");
        if (containerTitle.length() == 0) containerTitle = jData.getStrSafe("bookTitle");
        if (containerTitle.length() == 0) containerTitle = jData.getStrSafe("seriesTitle");

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

    return Arrays.stream(BibFieldEnum.values()).allMatch(bibFieldEnum ->
      (thisTypeHasFieldKey(bibFieldEnum) == false) || fieldsAreEqual(bibFieldEnum, backupItem));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean authorsChanged()
  {
    ArrayList<BibAuthor> authorList1     = new ArrayList<>(), authorList2     = new ArrayList<>(),
                         editorList1     = new ArrayList<>(), editorList2     = new ArrayList<>(),
                         translatorList1 = new ArrayList<>(), translatorList2 = new ArrayList<>();

    getAuthors().getLists(authorList1, editorList1, translatorList1);
    backupItem.getAuthors().getLists(authorList2, editorList2, translatorList2);

    EntryType entryType = getEntryType();

    // Backup item will not have authors of certain types if they are not allowed by Zotero for this entry type

    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.author    ).length() == 0) authorList1    .clear();
    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.editor    ).length() == 0) editorList1    .clear();
    if (ZoteroAuthors.getCreatorTypeStr(entryType, AuthorType.translator).length() == 0) translatorList1.clear();

    if (authorList1    .size() != authorList2    .size()) return true;
    if (editorList1    .size() != editorList2    .size()) return true;
    if (translatorList1.size() != translatorList2.size()) return true;

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

  @Override public void saveToDisk(JsonArray jArr)
  {
    if (thisIsBackup) return;

    JsonObj jDiskObj = jObj.clone();

    if (backupItem != null)
      jDiskObj.put("backupItem", backupItem.jObj);

    jArr.add(jDiskObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  JsonObj exportJsonObjForUploadToServer(boolean missingKeysOK)
  {
    JsonObj jServerObj = jObj.clone();

    EnumSet.allOf(BibFieldEnum.class).forEach(bibFieldEnum -> { switch (bibFieldEnum)
    {
      case bfDOI : case bfYear : case bfURL : case bfISBNs : case bfMisc : case bfTitle :
        break;

      default :

        if (thisTypeHasFieldKey(bibFieldEnum) == false)
        {
          String fieldKey = getFieldKey(bibFieldEnum);
          if (fieldKey.length() > 0)
            jServerObj.getObj("data").remove(getFieldKey(bibFieldEnum)); // This should be done even if missingKeysOK == true
        }

        break;
    }});

    if (linkedToWork())
    {
      ZoteroItem serverItem = new ZoteroItem(zWrapper, jServerObj, true);

      if (missingKeysOK || thisTypeHasFieldKey(bfDOI))   serverItem.setStr(bfDOI, getStr(bfDOI));
      if (missingKeysOK || thisTypeHasFieldKey(bfYear))  serverItem.setStr(bfYear, getStr(bfYear));
      if (missingKeysOK || thisTypeHasFieldKey(bfURL))   serverItem.setStr(bfURL, getStr(bfURL));
      if (missingKeysOK || thisTypeHasFieldKey(bfISBNs)) serverItem.setMultiStr(bfISBNs, getMultiStr(bfISBNs));
      if (missingKeysOK || thisTypeHasFieldKey(bfMisc))  serverItem.setMultiStr(bfMisc, getMultiStr(bfMisc));
      if (missingKeysOK || thisTypeHasFieldKey(bfTitle)) serverItem.setTitle(getStr(bfTitle));

      BibAuthors serverAuthors = serverItem.getAuthors();
      serverAuthors.clear();

      getAuthors().forEach(serverAuthors::add);
    }

    return isNewEntry() ? jServerObj.getObj("data") : jServerObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
