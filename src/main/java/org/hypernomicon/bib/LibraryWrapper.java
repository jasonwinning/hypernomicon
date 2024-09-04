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

package org.hypernomicon.bib;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.apache.http.client.methods.HttpUriRequest;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.model.Exceptions.HDB_InternalError;
import org.hypernomicon.model.items.HDI_OfflineTernary.Ternary;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.bib.data.EntryType.etOther;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

public abstract class LibraryWrapper<BibEntry_T extends BibEntry<BibEntry_T, BibCollection_T>, BibCollection_T extends BibCollection>
{
  //---------------------------------------------------------------------------

  public enum LibraryType
  {
    ltZotero("zotero", "Zotero"),
    ltMendeley("mendeley", "Mendeley");

    LibraryType(String descriptor, String userFriendlyName) { this.descriptor = descriptor; this.userFriendlyName = userFriendlyName; }
    private final String descriptor, userFriendlyName;

    public String getDescriptor()       { return descriptor; }
    public String getUserFriendlyName() { return userFriendlyName; }

    public static LibraryType getByDescriptor(String descriptor)
    {
      return findFirst(EnumSet.allOf(LibraryType.class), type -> type.descriptor.equals(descriptor));
    }
  }

  //---------------------------------------------------------------------------

  public static abstract class SyncTask extends HyperTask
  {
    public SyncTask() { super("SyncReferenceLibrary"); }

    protected boolean changed = false;
    public boolean getChanged() { return changed; }
  }

  private final List<String> keyList = new ArrayList<>();

  protected final Map<String, BibEntry_T> keyToAllEntry = new HashMap<>(), keyToTrashEntry = new HashMap<>();
  protected final Map<String, BibCollection_T> keyToColl = new HashMap<>();

  protected final JsonHttpClient jsonClient = new JsonHttpClient();
  protected SyncTask syncTask = null;
  protected HttpUriRequest request = null;
  private BiConsumer<String, String> keyChangeHndlr;
  private String userName = "";

  protected boolean didMergeDuringSync = false;

  public abstract SyncTask createNewSyncTask();
  public abstract void loadFromDisk(FilePath filePath) throws IOException, ParseException, HDB_InternalError;
  public abstract LibraryType type();
  public abstract EnumHashBiMap<EntryType, String> getEntryTypeMap();
  protected abstract void safePrefs();
  protected abstract String entryFileNode();
  protected abstract String collectionFileNode();

  public final Set<BibEntry_T> getTrash()                { return new LinkedHashSet<>(keyToTrashEntry.values()); }
  public final Set<BibEntry_T> getAllEntries()           { return new LinkedHashSet<>(keyToAllEntry.values()); }
  public final Map<String, BibCollection> getKeyToColl() { return Collections.unmodifiableMap(keyToColl); }

  public String getUserName()                            { return userName; }

  public EntryType parseEntryType(String typeStr)        { return getEntryTypeMap().inverse().getOrDefault(typeStr, etOther); }

  public BibEntry_T getEntryByKey(String key)            { return keyToAllEntry.get(key); }
  public BibEntry_T getEntryByID(int id)                 { return keyToAllEntry.get(keyList.get(id - 1)); }

  protected boolean syncTaskIsCancelled()                { return (syncTask != null) && syncTask.isCancelled(); }

  public final void setKeyChangeHandler(BiConsumer<String, String> hndlr) { keyChangeHndlr = hndlr; }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  protected final void clear()
  {
    keyToAllEntry  .clear();
    keyToTrashEntry.clear();
    keyToColl      .clear();
    keyList        .clear();

    userName = "";
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public final void stop()
  {
    if ((syncTask != null) && syncTask.isRunning())
      syncTask.cancel();

    if (request != null)
      request.abort();
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  protected void updateKey(String oldKey, String newKey)
  {
    if (keyToAllEntry.containsKey(oldKey))
      keyToAllEntry.put(newKey, keyToAllEntry.remove(oldKey));

    if (keyToTrashEntry.containsKey(oldKey))
      keyToTrashEntry.put(newKey, keyToTrashEntry.remove(oldKey));

    if (keyToColl.containsKey(oldKey))
      keyToColl.put(newKey, keyToColl.remove(oldKey));

    nullSwitch(db.getWorkByBibEntryKey(oldKey), work -> work.setBibEntryKey(newKey));

    keyChangeHndlr.accept(oldKey, newKey);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private volatile boolean fxThreadReturnValue;

  public boolean doMerge(BibEntry_T entry, JsonObj jObj)
  {
    runInFXThread(() ->
    {
      MergeWorksDlgCtrlr mwd;

      try
      {
        mwd = new MergeWorksDlgCtrlr("Merge Remote Changes with Local Changes", entry, BibEntry.create(this, jObj, true), null, null, entry.getWork(), false, false, Ternary.False);
      }
      catch (IOException e)
      {
        errorPopup("Unable to initialize merge dialog window.");
        fxThreadReturnValue = false;
        return;
      }

      entry.update(jObj, true, true);

      if (mwd.showModal())
        mwd.mergeInto(entry);

      fxThreadReturnValue = true;

    }, true);

    didMergeDuringSync = true;

    return fxThreadReturnValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntry_T addEntry(EntryType newType)
  {
    BibEntry_T item = BibEntry.create(this, newType);

    keyToAllEntry.put(item.getKey(), item);

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<BibEntry_T> getCollectionEntries(String collKey)
  {
    return getNonTrashEntries().filter(item -> item.getCollKeys(false).contains(collKey));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<BibEntry_T> getNonTrashEntries()
  {
    return keyToAllEntry.values().stream().filter(item -> keyToTrashEntry.containsKey(item.getKey()) == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public Stream<BibEntry_T> getUnsorted()
  {
    Predicate<BibEntry_T> predicate = item ->
      keyToTrashEntry.containsKey(item.getKey()) ?
        false
      :
        item.getCollKeys(true).stream().noneMatch(keyToColl::containsKey);

    return keyToAllEntry.values().stream().filter(predicate);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected void loadFromJSON(JsonObj jObj)
  {
    jObj.getArray(entryFileNode()).getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(this, itemJsonObj, false);

      if (entry == null) return;

      keyToAllEntry.put(entry.getKey(), entry);
      if (userName.isBlank())
        userName = entry.getUserName();
    });

    nullSwitch(jObj.getArray("trash"), jArr -> jArr.getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(this, itemJsonObj, false);

      if (entry == null) return;

      keyToAllEntry.put(entry.getKey(), entry);
      keyToTrashEntry.put(entry.getKey(), entry);
    }));

    nullSwitch(jObj.getArray(collectionFileNode()), jArr -> jArr.getObjs().forEach(collJsonObj ->
    {
      BibCollection_T coll = BibCollection.create(type(), collJsonObj);

      if (coll != null)
        keyToColl.put(coll.getKey(), coll);
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveToDisk()
  {
    StringBuilder json = null;

    try
    {
      JsonObj jMainObj = new JsonObj();
      JsonArray jArr = new JsonArray();

      for (BibEntry_T entry : (Iterable<BibEntry_T>)getNonTrashEntries()::iterator)
        entry.saveToDisk(jArr);

      jMainObj.put(entryFileNode(), jArr);

      jArr = new JsonArray();

      for (BibEntry_T entry : keyToTrashEntry.values())
        entry.saveToDisk(jArr);

      jMainObj.put("trash", jArr);

      jArr = new JsonArray();

      for (BibCollection_T coll : keyToColl.values())
        coll.saveToDisk(jArr);

      jMainObj.put(collectionFileNode(), jArr);

      json = new StringBuilder(jMainObj.toString());
    }
    catch (Throwable e)
    {
      showStackTrace(e);
      return;
    }

    try
    {
      saveStringBuilderToFile(json, db.xmlPath(BIB_FILE_NAME));
      safePrefs();
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while saving bibliographic data to disk.");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public int numericID(String key)
  {
    int ndx = keyList.indexOf(key);

    if (ndx < 0)
    {
      ndx = keyList.size();
      keyList.add(key);
    }

    return ndx + 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
