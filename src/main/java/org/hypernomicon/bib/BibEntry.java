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

package org.hypernomicon.bib;

import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.data.BibField.BibFieldEnum.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.records.HDT_RecordBase.*;
import static org.hypernomicon.model.HyperDB.*;

import java.util.*;

import org.hypernomicon.bib.BibManager.RelatedBibEntry;
import org.hypernomicon.bib.authors.BibAuthors;
import org.hypernomicon.bib.data.BibData;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.mendeley.MendeleyDocument;
import org.hypernomicon.bib.mendeley.MendeleyWrapper;
import org.hypernomicon.bib.zotero.ZoteroItem;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.model.authors.Author;
import org.hypernomicon.model.items.BibliographicDate;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_WorkType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

public abstract class BibEntry<BibEntry_T extends BibEntry<BibEntry_T, BibCollection_T>, BibCollection_T extends BibCollection> extends BibData implements BibEntity
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final boolean thisIsBackup;
  protected JsonObj jObj;
  protected BibEntry<BibEntry_T, BibCollection_T> backupItem = null;
  private final LibraryWrapper<BibEntry_T, BibCollection_T> libWrapper;
  private final BibEntry_T thisEntry;

  /**
   * This function assumes that authors is set as if it came from calling getAuthors() on the parent BibData.
   * @param authors The authors of the parent work.
   */
  protected abstract void setParentAuthorsFrom(BibAuthors authors);

  protected abstract void syncBookAuthorsTo(RelatedBibEntry relative);
  protected abstract JsonArray getCollJsonArray();
  protected abstract boolean isNewEntry();
  protected abstract boolean authorsChanged();
  protected abstract void updateJsonObj(JsonObj jObj);
  public abstract String getURLtoViewEntryInRefMgr();
  public abstract List<String> getReportFieldOrder();
  public abstract BibliographicDate getDateFromJson();
  protected abstract String getUserID();

//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public BibEntry(LibraryWrapper<BibEntry_T, BibCollection_T> libWrapper, boolean thisIsBackup)
  {
    this.libWrapper = libWrapper;
    this.thisIsBackup = thisIsBackup;

    jObj = new JsonObj();
    thisEntry = (BibEntry_T)this;
  }

//---------------------------------------------------------------------------

  public int numericID()                      { return getLibrary().numericID(getKey()); }
  public boolean linkedToWork()               { return (thisIsBackup != true) && (getWork() != null); }

  @SuppressWarnings("unchecked")
  protected final <T extends LibraryWrapper<BibEntry_T, BibCollection_T>> T getLibrary() { return (T)libWrapper; }

  @Override public HDT_Work getWork()         { return thisIsBackup ? null  : db.getWorkByBibEntryKey(getKey()); }
  @Override public HDT_WorkType getWorkType() { return linkedToWork() ? getWork().workType.get() : EntryType.toWorkType(getEntryType()); }

  @Override protected void setWorkType(HDT_WorkType workType) { if (linkedToWork()) getWork().workType.set(workType); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <Entry_T extends BibEntry<Entry_T, Collection_T>, Collection_T extends BibCollection> Entry_T create(LibraryWrapper<Entry_T, Collection_T> libWrapper, JsonObj jObj, boolean thisIsBackup)
  {
    return (Entry_T) switch (libWrapper.type())
    {
      case ltMendeley -> new MendeleyDocument((MendeleyWrapper) libWrapper, jObj, thisIsBackup);
      case ltZotero   -> new ZoteroItem      ((ZoteroWrapper  ) libWrapper, jObj, thisIsBackup);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  public static <Entry_T extends BibEntry<Entry_T, Collection_T>, Collection_T extends BibCollection> Entry_T create(LibraryWrapper<Entry_T, Collection_T> libWrapper, EntryType entryType)
  {
    return (Entry_T) switch (libWrapper.type())
    {
      case ltMendeley -> new MendeleyDocument((MendeleyWrapper) libWrapper, entryType);
      case ltZotero   -> new ZoteroItem      ((ZoteroWrapper  ) libWrapper, entryType);
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This should be called if Hypernomicon says an entry changed but the server says it didn't.
   */
  public void mergeWithBackupCopy()
  {
    update(backupItem.jObj, true, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final void update(JsonObj jObj, boolean updatingExistingDataFromServer, boolean preMerge)
  {
    updateJsonObj(jObj);

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
      jBackupObj = jObj.deepCopy();

    backupItem = create(libWrapper, jBackupObj, true);

    if ((updatingExistingDataFromServer == false) || (linkedToWork() == false)) return;

    setMultiStr(bfTitle, backupItem.getMultiStr(bfTitle));
    setMultiStr(bfISBNs, backupItem.getMultiStr(bfISBNs));
    setMultiStr(bfMisc, backupItem.getMultiStr(bfMisc));
    setStr(bfDOI, backupItem.getStr(bfDOI));
    setDate(backupItem.getDate());

    String url = getStr(bfURL);
    if (url.startsWith(EXT_1) == false)
      setStr(bfURL, backupItem.getStr(bfURL));

    if (preMerge) return; // authors always get updated during merge

    if (authorsChanged() == false) return;

    libWrapper.doMerge(thisEntry, jBackupObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public final BibliographicDate getDate()
  {
    return linkedToWork() ? getWork().getBibDate() : getDateFromJson();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  List<String> getCollKeys(boolean deletedOK)
  {
    JsonArray collArray = getCollJsonArray();

    return (collArray != null) && ((getLibrary().getTrash().contains(this) == false) || deletedOK) ?
      JsonArray.toStrArrayList(collArray)
    :
      new ArrayList<>();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToJsonArray(JsonArray jArr)
  {
    if (thisIsBackup) return;

    JsonObj jObjToSave = jObj.deepCopy();

    if (backupItem != null)
      jObjToSave.put("backupItem", backupItem.jObj);

    jArr.add(jObjToSave);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected final void setEntryType(EntryType entryType)
  {
    if (entryType == getEntryType()) return;

    throw new UnsupportedOperationException("change bibliographic entry type");
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
    setDate(bd.getDate());
    setStr(bfURL, bd.getStr(bfURL));
    setMultiStr(bfISBNs, bd.getMultiStr(bfISBNs));
    setTitle(bd.getStr(bfTitle));

    setAllAuthors(bd.getAuthors());
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
    BibEntry<?, ?> other = (BibEntry<?, ?>) obj;

    return Objects.equals(getKey(), other.getKey()) && (thisIsBackup == other.thisIsBackup);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // This function is similar to HDT_Work.compareTo

  public static Comparator<BibEntry<?, ?>> comparator()
  {
    return (e1, e2) ->
    {
      List<Author> list1 = e1.getAuthors().normalizedList(false),
                   list2 = e2.getAuthors().normalizedList(false);

      List<Author> authors1 = list1.stream().filter(Author::getIsAuthor).toList();
      if (authors1.isEmpty()) authors1 = list1.stream().filter(Author::getIsEditor).toList();

      List<Author> authors2 = list2.stream().filter(Author::getIsAuthor).toList();
      if (authors2.isEmpty()) authors2 = list2.stream().filter(Author::getIsEditor).toList();

      int cResult, numAuthors = Math.max(authors1.size(), authors2.size());

      for (int ndx = 0; ndx < numAuthors; ndx++)
      {
        if ((ndx >= authors1.size()) || (ndx >= authors2.size()))
          return authors1.size() - authors2.size();

        cResult = authors1.get(ndx).getName().compareTo(authors2.get(ndx).getName());
        if (cResult != 0) return cResult;
      }

      cResult = e1.getDate().compareTo(e2.getDate());

      return cResult != 0 ?
        cResult
      :
        makeSortKeyByType(e1.getStr(bfTitle), hdtWork).compareTo(makeSortKeyByType(e2.getStr(bfTitle), hdtWork));
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
