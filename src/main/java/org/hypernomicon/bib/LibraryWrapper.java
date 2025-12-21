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

import java.io.IOException;
import java.nio.file.AccessDeniedException;
import java.util.*;
import java.util.function.*;
import java.util.stream.Stream;

import org.apache.http.client.methods.HttpUriRequest;

import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.dialogs.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.items.Ternary;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

//---------------------------------------------------------------------------

/**
 * Classes that extend this class form a "wrapper" around, and act as Hypernomicon's primary interface with, the
 * actual third-party reference manager's web services, local copy of JSON data, and overall library data model.
 * The corresponding BibEntry-derived class provides the interface to JSON data for individual entries in the
 * library.
 * @param <BibEntry_T> The type of bibliographic entry corresponding with this library
 * @param <BibCollection_T> The type of collection corresponding with this library
 */
public abstract class LibraryWrapper<BibEntry_T extends BibEntry<BibEntry_T, BibCollection_T>, BibCollection_T extends BibCollection>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum LibraryType
  {
    ltZotero("zotero", "Zotero"),
    ltMendeley("mendeley", "Mendeley");

    LibraryType(String descriptor, String userFriendlyName) { this.descriptor = descriptor; this.userFriendlyName = userFriendlyName; }

    public final String descriptor, userFriendlyName;

    public static LibraryType getByDescriptor(String descriptor)
    {
      return findFirst(EnumSet.allOf(LibraryType.class), type -> type.descriptor.equals(descriptor));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static abstract class SyncTask extends HyperTask
  {
    protected SyncTask(String message) { super("SyncReferenceLibrary", message, false); }

    protected boolean changed = false;
    public boolean getChanged() { return changed; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final List<String> keyList = new ArrayList<>();

  protected final Map<String, BibEntry_T> keyToAllEntry = new HashMap<>(), keyToTrashEntry = new HashMap<>();
  protected final Map<String, BibCollection_T> keyToColl = new HashMap<>();

  protected final JsonHttpClient jsonClient = new JsonHttpClient();
  protected SyncTask syncTask = null;
  protected HttpUriRequest request = null;
  private BiConsumer<String, String> keyChangeHndlr;

  protected boolean didMergeDuringSync = false;

//---------------------------------------------------------------------------

  protected abstract void savePrefs();
  protected abstract String entryFileNode();
  protected abstract String collectionFileNode();
  protected abstract BibAuthKeys getAuthKeys();

  public abstract void enableSyncOnThisComputer(BibAuthKeys authKeys, String userID, String userName, boolean reestablishing) throws HyperDataException;
  public abstract void getProfileInfoFromServer(Consumer<String> successHndlr, Consumer<Throwable> failHndlr);
  public abstract SyncTask createNewSyncTask(String message) throws HyperDataException;
  public abstract LibraryType type();
  public abstract EnumHashBiMap<EntryType, String> getEntryTypeMap();
  public abstract String getUserID();
  public abstract String getUserName();

  public final Set<BibEntry_T> getTrash()                { return new LinkedHashSet<>(keyToTrashEntry.values()); }
  public final Set<BibEntry_T> getAllEntries()           { return new LinkedHashSet<>(keyToAllEntry.values()); }
  public final Map<String, BibCollection> getKeyToColl() { return Collections.unmodifiableMap(keyToColl); }

  public String getUserFriendlyName()                    { return type().userFriendlyName; }

  public EntryType parseEntryType(String typeStr)        { return getEntryTypeMap().inverse().getOrDefault(typeStr, etOther); }

  public BibEntry_T getEntryByKey(String key)            { return keyToAllEntry.get(key); }
  public BibEntry_T getEntryByID(int id)                 { return keyToAllEntry.get(keyList.get(id - 1)); }

  final void setKeyChangeHandler(BiConsumer<String, String> hndlr) { keyChangeHndlr = hndlr; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected AccessDeniedException newAccessDeniedException()
  {
    return new AccessDeniedException("Valid " + getUserFriendlyName() + " access token not found. Go to Tools \u279c Settings \u279c Bibliography Manager \u279c Re-Establish Access to acquire a new access token.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static String syncErrorMessage(Throwable e)
  {
    return "An error occurred while syncing: " + getThrowableMessage(e);
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
        mwd = new MergeWorksDlgCtrlr("Merge Remote Changes with Local Changes", Stream.of(entry, BibEntry.create(this, jObj, true)),
                                     entry.getWork(), false, false, Ternary.False, nullSwitch(entry.getWork(), null, HDT_Work::filePath), true);
      }
      catch (IOException e)
      {
        errorPopup("Unable to initialize merge dialog window.");
        fxThreadReturnValue = false;
        return;
      }

      entry.update(jObj, true, true);

      mwd.showModal();       // Just do the merge because the cancel button gets
      mwd.mergeInto(entry);  // removed from the merge window in this situation

      fxThreadReturnValue = true;

    }, true);

    didMergeDuringSync = true;

    return fxThreadReturnValue;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public BibEntry_T addEntry(EntryType newType)
  {
    BibEntry_T entry = BibEntry.create(this, newType);

    keyToAllEntry.put(entry.getKey(), entry);

    return entry;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void addEntryForUnitTest(BibEntry_T entry)
  {
    assertThatThisIsUnitTestThread();

    keyToAllEntry.put(entry.getKey(), entry);
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

  Stream<BibEntry_T> getUnsorted()
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
    jObj.getOrAddArray(entryFileNode()).getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(this, itemJsonObj, false);

      if (entry == null) return;

      keyToAllEntry.put(entry.getKey(), entry);
    });

    jObj.getArraySafe("trash").getObjs().forEach(itemJsonObj ->
    {
      BibEntry_T entry = BibEntry.create(this, itemJsonObj, false);

      if (entry == null) return;

      keyToAllEntry.put(entry.getKey(), entry);
      keyToTrashEntry.put(entry.getKey(), entry);
    });

    jObj.getArraySafe(collectionFileNode()).getObjs().forEach(collJsonObj ->
    {
      BibCollection_T coll = BibCollection.create(type(), collJsonObj);

      if (coll != null)
        keyToColl.put(coll.getKey(), coll);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void saveAllToPersistentStorage()
  {
    if (db.isOffline()) return;

    StringBuilder json = null;

    try
    {
      JsonObj jMainObj = new JsonObj();

//---------------------------------------------------------------------------

      {
        JsonArray jArr = new JsonArray();

        getNonTrashEntries().forEach(entry -> entry.saveToJsonArray(jArr));

        jMainObj.put(entryFileNode(), jArr);
      }

//---------------------------------------------------------------------------

      {
        JsonArray jArr = new JsonArray();

        keyToTrashEntry.values().forEach(entry -> entry.saveToJsonArray(jArr));

        jMainObj.put("trash", jArr);
      }

//---------------------------------------------------------------------------

      {
        JsonArray jArr = new JsonArray();

        keyToColl.values().forEach(coll -> coll.saveToJsonArray(jArr));

        jMainObj.put(collectionFileNode(), jArr);
      }

//---------------------------------------------------------------------------

      json = new StringBuilder(jMainObj.toString());
    }
    catch (Throwable e)
    {
      logThrowable(e);
      errorPopup("An error occurred while saving bibliographic data: " + getThrowableMessage(e));
      return;
    }

    try
    {
      saveStringBuilderToFile(json, db.xmlPath(BIB_FILE_NAME), XML_FILES_CHARSET);
      savePrefs();
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while saving bibliographic data: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Deprecated
  public final void saveAuthKeysToDBSettings()
  {
    try
    {
      BibAuthKeys.saveToDBSettings(getAuthKeys());
    }
    catch (Exception e)
    {
      errorPopup("An error occurred while saving access token: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void removeSecretsFromKeyring()
  {
    BibAuthKeys.removeFromKeyring(getAuthKeys(), getUserID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Save secrets to keyring only if it is not empty and they are not saved to the keyring yet
   * @return False if it is non-empty and saving wasn't successful; true otherwise
   */
  public boolean saveSecretsToKeyringIfUnsaved()
  {
    return BibAuthKeys.saveToKeyringIfUnsaved(getAuthKeys(), getUserID());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Determines whether the authKeys can immediately be saved to the keyring but haven't been yet
   * @return True if userID is not null or blank, authKeys are not null or blank, but they have
   * not been saved to the keyring yet. False otherwise.
   */
  public boolean secretsStillNeedToBeSavedToKeyring()
  {
    return strNotNullOrBlank(getUserID()) && BibAuthKeys.stillNeedsToBeSavedToKeyring(getAuthKeys());
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
