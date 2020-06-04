/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.HDT_RecordBase.makeSortKeyByType;
import static org.hypernomicon.model.HyperDB.*;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.hypernomicon.bib.BibManager.RelatedBibEntry;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.authors.BibAuthor;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.mendeley.MendeleyDocument;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;
import org.hypernomicon.bib.zotero.ZoteroItem;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

public abstract class BibEntry extends BibData implements BibEntity
{
  protected final boolean thisIsBackup;
  protected JsonObj jObj;
  protected BibEntry backupItem = null;

  protected abstract void syncBookAuthorsTo(RelatedBibEntry relative);
  protected abstract List<String> getCollKeys(boolean deletedOK);
  protected abstract boolean isNewEntry();
  public abstract String getEntryURL();
  public abstract LibraryWrapper<?, ?> getLibrary();

  public BibEntry(boolean thisIsBackup)       { this.thisIsBackup = thisIsBackup; }

  @Override public HDT_Work getWork()         { return thisIsBackup ? null  : db.getWorkByBibEntryKey(getKey()); }
  @Override public boolean linkedToWork()     { return thisIsBackup ? false : getWork() != null; }
  @Override public HDT_WorkType getWorkType() { return linkedToWork() ? getWork().workType.get() : EntryType.toWorkType(getEntryType()); }

  @Override public void setWorkType(HDT_WorkType workType) { if (linkedToWork()) getWork().workType.set(workType); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <Entry_T extends BibEntry, Collection_T extends BibCollection> Entry_T create(LibraryType lType, LibraryWrapper<Entry_T, Collection_T> wrapper, JsonObj jObj, boolean thisIsBackup)
  {
    switch (lType)
    {
      case ltMendeley : return (Entry_T) new MendeleyDocument((MendeleyWrapper) wrapper, jObj, thisIsBackup);
      case ltZotero   : return (Entry_T) new ZoteroItem      ((ZoteroWrapper  ) wrapper, jObj, thisIsBackup);
      default         : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <Entry_T extends BibEntry, Collection_T extends BibCollection> Entry_T create(LibraryType lType, LibraryWrapper<Entry_T, Collection_T> wrapper, EntryType entryType)
  {
    switch (lType)
    {
      case ltMendeley : return (Entry_T) new MendeleyDocument((MendeleyWrapper) wrapper, entryType);
      case ltZotero   : return (Entry_T) new ZoteroItem      ((ZoteroWrapper  ) wrapper, entryType);
      default         : return null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int numericID()
  {
    return getLibrary().numericID(getKey());
  }

  public String getCBText()
  {
    String authorStr = getAuthors().getStr(),
           yearStr = getStr(bfYear),
           titleStr = getStr(bfTitle),
           cbStr = "";

    if (authorStr.length() > 0)
      cbStr = authorStr + " ";

    if (yearStr.length() > 0)
      cbStr += "(" + yearStr + ") ";

    if (titleStr.length() > 0)
      cbStr += titleStr;

    return cbStr;
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

  public void unassignWork()
  {
    HDT_Work work = getWork();
    if (work == null) return;

    work.setBibEntryKey("");

    BibData bd = work.getBibData();

    setStr(bfDOI, bd.getStr(bfDOI));
    setStr(bfYear, bd.getStr(bfYear));
    setStr(bfURL, bd.getStr(bfURL));
    setMultiStr(bfISBNs, bd.getMultiStr(bfISBNs));
    setTitle(bd.getStr(bfTitle));

    BibAuthors authors = getAuthors();
    authors.clear();

    bd.getAuthors().forEach(authors::add);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public int hashCode()
  {
    final int prime = 31;
    int result = 1;
    result = prime * result + (getKey() == null ? 0 : getKey().hashCode());
    result = prime * result + (thisIsBackup ? 1231 : 1237);
    return result;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean equals(Object obj)
  {
    if (this == obj) return true;
    if (obj == null) return false;
    if (getClass() != obj.getClass()) return false;
    BibEntry other = (BibEntry) obj;

    if (getKey() == null)
    {
      if (other.getKey() != null) return false;
    }
    else if (!getKey().equals(other.getKey())) return false;

    return thisIsBackup == other.thisIsBackup;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static Comparator<BibEntry> comparator()
  {
    return (e1, e2) ->
    {
      List<BibAuthor> authorList = new ArrayList<>(),
                      editorList = new ArrayList<>(),
                      translatorList = new ArrayList<>(),
                      authors1, authors2;

      e1.getAuthors().getLists(authorList, editorList, translatorList);

      authors1 = authorList.isEmpty() ? editorList : authorList;

      authorList = new ArrayList<>();
      editorList = new ArrayList<>();

      e2.getAuthors().getLists(authorList, editorList, translatorList);

      authors2 = authorList.isEmpty() ? editorList : authorList;

      int cResult, numAuthors = Math.max(authors1.size(), authors2.size());

      for (int ndx = 0; ndx < numAuthors; ndx++)
      {
        if ((ndx >= authors1.size()) || (ndx >= authors2.size()))
          return authors1.size() - authors2.size();

        cResult = authors1.get(ndx).getName().compareTo(authors2.get(ndx).getName());
        if (cResult != 0) return cResult;
      }

      cResult = e1.getStr(bfYear).compareTo(e2.getStr(bfYear));
      return cResult != 0 ?
        cResult
      :
        makeSortKeyByType(e1.getStr(bfTitle), hdtWork).compareTo(makeSortKeyByType(e1.getStr(bfTitle), hdtWork));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}