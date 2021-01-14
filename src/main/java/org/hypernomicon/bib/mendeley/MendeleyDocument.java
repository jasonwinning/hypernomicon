/*
 * Copyright 2015-2021 Jason Winning
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
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang3.StringUtils;
import org.hypernomicon.bib.BibEntry;
import org.hypernomicon.bib.BibManager.RelatedBibEntry;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthor.AuthorType;
import org.hypernomicon.bib.data.BibField.BibFieldEnum;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.authors.WorkBibAuthors;
import org.hypernomicon.bib.data.BibField;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import com.google.common.collect.Lists;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

public class MendeleyDocument extends BibEntry implements MendeleyEntity
{
  private final MendeleyWrapper mWrapper;

  public MendeleyDocument(MendeleyWrapper mWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    super(thisIsBackup);

    update(jObj, false, false);
    this.mWrapper = mWrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MendeleyDocument(MendeleyWrapper mWrapper, EntryType newType)
  {
    super(false);

    jObj = new JsonObj();
    jObj.put(getFieldKey(bfEntryType), MendeleyWrapper.entryTypeMap.getOrDefault(newType, ""));

    this.mWrapper = mWrapper;

    jObj.put("id", "_!_" + randomAlphanumericStr(12));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String toString()        { return jObj.toString(); }
  @Override public String getKey()          { return jObj.getStr("id"); }
  @Override protected boolean isNewEntry()  { return jObj.containsKey("last_modified") == false; }
  @Override public String getEntryURL()     { return ""; }
  @Override public BibAuthors getAuthors()  { return linkedToWork() ? new WorkBibAuthors(getWork()) : new MendeleyAuthors(jObj, getEntryType()); }
  @Override public EntryType getEntryType() { return parseMendeleyType(jObj.getStrSafe(getFieldKey(bfEntryType))); }

  @Override public LibraryWrapper<?, ?> getLibrary() { return mWrapper; }
  static EntryType parseMendeleyType(String mType)   { return MendeleyWrapper.entryTypeMap.inverse().getOrDefault(mType, etOther); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Instant lastModified()
  {
    String str = jObj.getStrSafe("last_modified");

    return str.isBlank() ? Instant.now()      // If it does not yet exist in Mendeley, then for Mendeley's purposes it should be considered brand-new
                         : parseIso8601(str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge)
  {
    this.jObj = jObj;

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

    backupItem = new MendeleyDocument(mWrapper, jBackupObj, true);

    if ((updatingExistingDataFromServer == false) || (linkedToWork() == false)) return;

    setMultiStr(bfTitle, backupItem.getMultiStr(bfTitle));
    setMultiStr(bfISBNs, backupItem.getMultiStr(bfISBNs));
    setMultiStr(bfMisc, backupItem.getMultiStr(bfMisc));
    setStr(bfDOI, backupItem.getStr(bfDOI));
    setStr(bfYear, backupItem.getStr(bfYear));

    String url = getStr(bfURL);
    if (url.startsWith(EXT_1) == false)
      setStr(bfURL, backupItem.getStr(bfURL));

    if (preMerge) return; // authors always get updated during merge

    if (authorsChanged() == false) return;

    mWrapper.doMerge(this, jBackupObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected List<String> getCollKeys(boolean deletedOK)
  {
    JsonArray collArray = jObj.getArray("folder_uuids");

    return (collArray != null) && ((mWrapper.getTrash().contains(this) == false) || deletedOK) ?
      Lists.newArrayList((Iterable<String>)collArray.getStrs())
    :
      new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setEntryType(EntryType entryType)
  {
    if (entryType == getEntryType()) return;

    // jObj.put("type", MendeleyWrapper.entryTypeMap.getOrDefault(entryType, ""));

    throw new UnsupportedOperationException("change Mendeley entry type");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void setStr(BibFieldEnum bibFieldEnum, String newStr)
  {
    if (linkedToWork())
    {
      switch (bibFieldEnum)
      {
        case bfYear : getWork().setYear(newStr); return;
        case bfDOI  : getWork().setDOI (newStr); return;
        case bfURL  : getWork().setURL (newStr); return;
        default     : break;
      }
    }

    String fieldKey = getFieldKey(bibFieldEnum);

    switch (bibFieldEnum)
    {
      case bfYear :

        try
        {
          jObj.put(fieldKey, Long.parseLong(safeStr(newStr)));
        }
        catch (NumberFormatException nfe)
        {
          jObj.putNull(fieldKey);
        }

        return;

      case bfDOI :

        JsonObj idObj = jObj.getObj("identifiers");

        if (idObj == null)
        {
          idObj = new JsonObj();
          jObj.put("identifiers", idObj);
        }

        if (safeStr(newStr).isBlank())
          idObj.putNull("doi");
        else
          idObj.put("doi", newStr);

        return;

      case bfURL :

        if (safeStr(newStr).isBlank())
          jObj.putNull("websites");
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

      default : messageDialog("Internal error #90225", mtError); return;
    }

    if (safeStr(newStr).isBlank())
    {
      jObj.putNull(fieldKey);
      return;
    }

    if (jObj.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

    jObj.put(fieldKey, newStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String getFieldKey(BibFieldEnum bibFieldEnum)
  {
    switch (bibFieldEnum)
    {
      case bfEntryType : return "type";
      case bfURL       : return "websites"; // array
      case bfVolume    : return "volume";
      case bfIssue     : return "issue";
      case bfPages     : return "pages";
      case bfPublisher : return "publisher";
      case bfPubLoc    : return "city"; // There is also a "country" field
      case bfEdition   : return "edition";
      case bfLanguage  : return "language";
      case bfYear      : return "year";

      case bfContainerTitle : return "source";

      case bfISBNs : return "isbn";
      case bfISSNs : return "issn";
      case bfDOI   : return "doi";

      case bfTitle : return "title";
      case bfMisc  : return "notes";

      // Acccording to the API, "Three types of annotations are available. Notes are scoped to documents and provide a
      // high-level comments using styled text. Only a single note annotation can be attached to a document and
      // subsequent attempts to create further note annotations will fail. Note annotations have a value of note for the member type."

      case bfAuthors     : return "authors";
      case bfEditors     : return "editors";
      case bfTranslators : return "translators";

      case bfWorkType : return "";
    }

    return "";
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

        return nullSwitch(jObj.getObj("identifiers"), "", idObj -> idObj.getStrSafe("doi"));

      case bfURL :

        JsonArray jArr = jObj.getArray("websites");

        return (jArr != null) && (jArr.size() > 0) ? jArr.getStr(0) : "";

      case bfVolume : case bfIssue   : case bfPages    : case bfPublisher :
      case bfPubLoc : case bfEdition : case bfLanguage :

        return jObj.getStrSafe(fieldKey);

      case bfYear : return jObj.getAsStr("year");

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

    String fieldKey = getFieldKey(bibFieldEnum), newStr = null;

    switch (bibFieldEnum)
    {
      case bfContainerTitle : case bfTitle :

        newStr = strListToStr(list, false);
        if (jObj.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

        jObj.put(fieldKey, newStr);

        return;

      case bfMisc :

        newStr = list.stream().map(StringBuilder::new).reduce((all, one) -> all.append("<br>").append(one)).orElse(new StringBuilder()).toString();

        if (jObj.getStrSafe(fieldKey).equals(safeStr(newStr))) return;

        jObj.put(fieldKey, newStr);

        return;

      case bfISBNs : case bfISSNs :

        if (collEmpty(list))
          newStr = "";
        else
        {
          List<String> list2 = new ArrayList<>(list);
          removeDupsInStrList(list2);
          newStr = ultraTrim(convertToSingleLine(strListToStr(list2, false)));
        }

        JsonObj idObj = jObj.getObj("identifiers");

        if (idObj == null)
        {
          idObj = new JsonObj();
          jObj.put("identifiers", idObj);
        }

        if (newStr.isEmpty())
          idObj.remove(fieldKey);
        else
          idObj.put(fieldKey, newStr);

        return;

      default : return;
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
        case bfTitle : return Collections.singletonList(getWork().name());
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
    {
      return (bibFieldEnum == bfYear) && backupItem.getStr(bfYear).isBlank() && (StringUtils.isNumeric(getStr(bfYear)) == false) ?
        true
      :
        fieldsAreEqual(bibFieldEnum, backupItem, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean authorsChanged()
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

    if (MendeleyAuthors.class.cast(backupItem.getAuthors()).ignoreEditors())
    {
      JsonArray jArr1 = jObj.getArray("editors"),
                jArr2 = MendeleyDocument.class.cast(backupItem).jObj.getArray("editors");

      if ((jArr1 == null) != (jArr2 == null)) return true;
      if (jArr1 != null)
      {
        if (jArr1.size() != jArr2.size()) return true;

        for (int ndx = 0; ndx < jArr1.size(); ndx++)
        {
          JsonObj ed1 = jArr1.getObj(ndx),
                  ed2 = jArr2.getObj(ndx);

          if ((ed1 == null) != (ed2 == null)) return true;

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

  JsonObj exportJsonObjForUploadToServer()
  {
    JsonObj jServerObj = jObj.clone();

    if (isNewEntry())
      jServerObj.remove("id");

    if (linkedToWork())
    {
      MendeleyDocument serverItem = new MendeleyDocument(mWrapper, jServerObj, true);

      serverItem.setStr(bfDOI, getStr(bfDOI));
      serverItem.setStr(bfYear, getStr(bfYear));

      String url = getStr(bfURL);
      if (url.startsWith(EXT_1) == false)
        serverItem.setStr(bfURL, url);

      serverItem.setMultiStr(bfISBNs, getMultiStr(bfISBNs));
      serverItem.setMultiStr(bfISSNs, getMultiStr(bfISSNs));
      serverItem.setMultiStr(bfMisc, getMultiStr(bfMisc));
      serverItem.setTitle(getStr(bfTitle));

      BibAuthors serverAuthors = serverItem.getAuthors();
      serverAuthors.clear();

      getAuthors().forEach(serverAuthors::add);
    }

    return jServerObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void syncBookAuthorsTo(RelatedBibEntry relative)
  {
    MendeleyDocument dest = (MendeleyDocument) relative.entry;

    switch (relative.relation)
    {
      case Child:

        BibAuthors authors = getAuthors();

        List<BibAuthor> authorList = new ArrayList<>(),
                        editorList = new ArrayList<>(),
                        translatorList = new ArrayList<>();

        authors.getLists(authorList, editorList, translatorList);

        JsonArray jsonArr = dest.jObj.getArray("editors");
        if (jsonArr == null)
        {
          jsonArr = new JsonArray();
          dest.jObj.put("editors", jsonArr);
        }

        jsonArr.clear();

        for (BibAuthor editor : editorList)
        {
          JsonObj personObj = new JsonObj();

          String firstName = removeAllParentheticals(editor.getGiven());

          personObj.put("first_name", firstName);
          personObj.put("last_name", editor.getFamily());

          jsonArr.add(personObj);
        }

        break;

      case Parent:

        JsonObj newVersion = dest.exportJsonObjForUploadToServer();
        newVersion.remove("authors");
        newVersion.put("editors", jObj.getArray("editors").clone());

        dest.getWork().getAuthors().setAll(new MendeleyAuthors(newVersion, dest.getEntryType()));

        break;

      case Sibling:

        jsonArr = jObj.getArray("editors");
        dest.jObj.put("editors", jsonArr == null ? new JsonArray() : jsonArr.clone());

        break;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
