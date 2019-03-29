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

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.workMerge.MergeWorksDialogController;
import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.BibEntryRow;
import org.hypernomicon.bib.lib.LibraryWrapper;
import org.hypernomicon.bib.zotero.ZoteroEntity.ZoteroEntityType;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.JsonHttpClient;

import static org.hypernomicon.bib.BibData.EntryType.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class ZoteroWrapper extends LibraryWrapper<ZoteroItem, ZoteroCollection>
{
  private final String apiKey, userID;
  private final JsonHttpClient jsonClient;
  private long offlineLibVersion = -1, onlineLibVersion = -1;
  private Instant backoffTime = null, retryTime = null;

  static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();

  private static EnumMap<EntryType, JsonObj> templates = null;

  @Override public LibraryType type() { return LibraryType.ltZotero; }

  private static enum ZoteroCmd
  {
    readItems,
    readTopItems,
    readTrash,
    readItem,
    readItemChildren,
    readItemTags,
    readTags,
    readCollections,
    readTopCollections,
    readCollection,
    readCollectionItems,
    readCollectionTopItems,
    readCollectionTags,

    readChangedItemVersions,
    readChangedCollVersions,
    readTrashVersions,
    readDeletions,

    writeItems
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ZoteroWrapper(String apiKey, String userID)
  {
    this.apiKey = apiKey;
    this.userID = userID;
    jsonClient = new JsonHttpClient();
  }

  JsonObj getTemplate(BibData.EntryType type) { return templates.get(type); }

  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doWriteCommand(ZoteroCmd command, String jsonPostData) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + "/";

    switch (command)
    {
      case writeItems:
        url += "items";
        break;

      default:
        return null;
    }

    JsonArray jsonArray = doHttpRequest(url, true, jsonPostData);

    switch (jsonClient.getStatusCode())
    {
      case HttpStatus.SC_OK :
      case HttpStatus.SC_NOT_MODIFIED :
      case HttpStatus.SC_PRECONDITION_FAILED :

        return jsonArray;
    }

    throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getReasonPhrase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doReadCommand(ZoteroCmd command, String itemKey, String collectionKey) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + "/";

    switch (command)
    {
      case readCollection:
        url += "collections/" + collectionKey;
        break;

      case readCollectionItems:
        url += "collections/" + collectionKey + "/items";
        break;

      case readCollectionTags:
        url += "collections/" + collectionKey + "/tags";
        break;

      case readCollectionTopItems:
        url += "collections/" + collectionKey + "/items/top";
        break;

      case readCollections:

        if (collectionKey.length() > 0)
          url += "collections?collectionKey=" + collectionKey;
        else
          url += "collections";
        break;

      case readItem:
        url += "items/" + itemKey;
        break;

      case readItemChildren:
        url += "items/" + itemKey + "/children";
        break;

      case readItemTags:
        url += "items/" + itemKey + "/tags";
        break;

      case readItems:

        if (itemKey.length() > 0)
          url += "items?itemKey=" + itemKey;
        else
          url += "items";
        break;

      case readDeletions:
        url += "deleted?since=" + offlineLibVersion;
        break;

      case readChangedItemVersions:
        url += "items?since=" + offlineLibVersion + "&format=versions";
        break;

      case readChangedCollVersions:
        url += "collections?since=" + offlineLibVersion + "&format=versions";
        break;

      case readTrashVersions:
        url += "items/trash?format=versions";
        break;

      case readTags:
        url += "tags";
        break;

      case readTopCollections:
        url += "collections/top";
        break;

      case readTopItems:
        url += "items/top";
        break;

      case readTrash:
        if (itemKey.length() > 0)
          url += "items/trash?itemKey=" + itemKey;
        else
          url += "items/trash";
        break;

      default:
        return null;
    }

    JsonArray jsonArray = doHttpRequest(url, false, null);

    switch (jsonClient.getStatusCode())
    {
      case HttpStatus.SC_OK :
      case HttpStatus.SC_NOT_MODIFIED :
      case HttpStatus.SC_PRECONDITION_FAILED :

        return jsonArray;
    }

    throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getReasonPhrase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doHttpRequest(String url, boolean isPost, String postJsonData) throws IOException, UnsupportedOperationException, ParseException, TerminateTaskException
  {
    StringBuilder apiVersion = new StringBuilder();
    MutableInt totalResults = new MutableInt(-1);
    JsonArray jsonArray = null;
    RequestBuilder rb;

    if (retryTime != null)
    {
      while ((retryTime.compareTo(Instant.now()) > 0) && (syncTask.isCancelled() == false))
        sleepForMillis(30);
    }

    if (backoffTime != null)
    {
      while ((backoffTime.compareTo(Instant.now()) > 0) && (syncTask.isCancelled() == false))
        sleepForMillis(30);
    }

    if (syncTask.isCancelled()) throw new TerminateTaskException();

    if (isPost)
      rb = RequestBuilder.post()
          .setHeader("Zotero-Write-Token", generateWriteToken())
          .setEntity(new StringEntity(postJsonData, UTF_8));
    else
      rb = RequestBuilder.get();

    request = rb
        .setUri(url)
        .setHeader(HttpHeaders.CONTENT_TYPE, "application/json")
        .setHeader("Zotero-API-Version", "3")
        .setHeader("Zotero-API-Key", apiKey)
        .setHeader("Zotero-Write-Token", generateWriteToken())
        .setHeader("If-Unmodified-Since-Version", String.valueOf(offlineLibVersion))
        .build();

    try
    {
      jsonArray = jsonClient.requestArrayInThisThread(request);
    }
    catch(SocketException e)
    {
      if (syncTask.isCancelled())
      {
        request = null;
        throw new TerminateTaskException();
      }

      request = null;
      throw e;
    }

    nullSwitch(jsonClient.getHeaders(), headers -> headers.forEach(header ->
    {
      int sec;

      switch (header.getName())
      {
        case "Zotero-API-Version"    : assignSB(apiVersion, header.getValue()); break;
        case "Total-Results"         : totalResults.setValue(parseInt(header.getValue(), -1)); break;
        case "Last-Modified-Version" : onlineLibVersion = parseInt(header.getValue(), -1); break;
        case "Backoff":

          sec = parseInt(header.getValue(), -1);
          if (sec > 0)
            backoffTime = Instant.now().plusMillis(sec * 1000);

          break;

        case "Retry-After":

          sec = parseInt(header.getValue(), -1);
          if (sec > 0)
            retryTime = Instant.now().plusMillis(sec * 1000);

          break;
      }
    }));

    request = null;
    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String generateWriteToken()
  {
    return randomHexStr(32);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getCreatorTypes()
  {
    JsonObj jObj = new JsonObj();

    try
    {
      for (String zType : new String[]{"artwork", "audioRecording", "bill", "blogPost", "book", "bookSection", "case", "computerProgram",
        "conferencePaper", "dictionaryEntry", "document", "email", "encyclopediaArticle", "film", "forumPost", "hearing", "instantMessage",
        "interview", "journalArticle", "letter", "magazineArticle", "manuscript", "map", "newspaperArticle", "patent", "podcast",
        "presentation", "radioBroadcast", "report", "statute", "tvBroadcast", "thesis", "videoRecording", "webpage"})
      {
        jObj.put(zType, doHttpRequest("https://api.zotero.org/itemTypeCreatorTypes?itemType=" + zType, false, null));
      }

      StringBuilder json = new StringBuilder(jObj.toString());

      FilePath filePath = db.getRootFilePath().resolve(ZOTERO_CREATOR_TYPES_FILE_NAME);

      saveStringBuilderToFile(json, filePath);
    }
    catch (UnsupportedOperationException | IOException | ParseException | TerminateTaskException e)
    {
      e.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getTemplates()
  {
    JsonArray jArr = new JsonArray();

    try
    {
      for (String zType : new String[]{"artwork", "audioRecording", "bill", "blogPost", "book", "bookSection", "case", "computerProgram",
        "conferencePaper", "dictionaryEntry", "document", "email", "encyclopediaArticle", "film", "forumPost", "hearing", "instantMessage",
        "interview", "journalArticle", "letter", "magazineArticle", "manuscript", "map", "newspaperArticle", "patent", "podcast",
        "presentation", "radioBroadcast", "report", "statute", "tvBroadcast", "thesis", "videoRecording", "webpage"})
      {
        jArr.add(doHttpRequest("https://api.zotero.org/items/new?itemType=" + zType, false, null).getObj(0));
      }

      StringBuilder json = new StringBuilder(jArr.toString());

      FilePath filePath = db.getRootFilePath().resolve(ZOTERO_TEMPLATE_FILE_NAME);

      saveStringBuilderToFile(json, filePath);
    }
    catch (UnsupportedOperationException | IOException | ParseException | TerminateTaskException e)
    {
      e.printStackTrace();
    }
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private boolean syncChangedEntriesToServer() throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    ArrayList<ZoteroItem> uploadQueue = new ArrayList<>(); // implemented as array because indices are returned by server
    JsonArray jArr = new JsonArray();

    getAllEntries().forEach(entry ->
    {
      if (entry.isSynced() == false)
        uploadQueue.add(entry);
    });

    if (uploadQueue.size() == 0) return false;

    int statusCode = HttpStatus.SC_OK;

    while ((uploadQueue.size() > 0) && (statusCode == HttpStatus.SC_OK) && (syncTask.isCancelled() == false))
    {
      jArr.clear();

      int uploadCount = (uploadQueue.size() > 50) ? 50 : uploadQueue.size();
      for (int ndx = 0; ndx < uploadCount; ndx++)
        jArr.add(uploadQueue.get(ndx).exportJsonObjForUploadToServer(false));

      jArr = doWriteCommand(ZoteroCmd.writeItems, jArr.toString());

      if (syncTask.isCancelled()) throw new TerminateTaskException();

      statusCode = jsonClient.getStatusCode();

      if (statusCode == HttpStatus.SC_OK)
      {
        JsonObj jSuccess = jArr.getObj(0).getObj("successful");
        JsonObj jUnchanged = jArr.getObj(0).getObj("unchanged");
        JsonObj jFailed = jArr.getObj(0).getObj("failed");

        if ((jUnchanged.keySet().isEmpty() == false) || (jFailed.keySet().isEmpty() == false))
          showWriteErrorMessages(jUnchanged, jFailed, uploadQueue);

        jSuccess.keySet().forEach(queueNdx ->
        {
          JsonObj jObj = jSuccess.getObj(queueNdx);
          jObj.put("synced", "true");
          ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1)); // here we take advantage of the fact that the upload "queue" is an array

          String oldKey = item.getEntryKey();
          boolean newEntry = item.isNewEntry();

          item.update(jObj, false, false);

          if (newEntry)
            updateKey(oldKey, item.getKey());

          keyToAllEntry.putIfAbsent(item.getEntryKey(), item);

          if (item.getVersion() > onlineLibVersion)
            onlineLibVersion = item.getVersion();
        });

        for (int ndx = 0; ndx < uploadCount; ndx++)
          uploadQueue.remove(0);
      }
    }

    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void showWriteErrorMessages(JsonObj jUnchanged, JsonObj jFailed, ArrayList<ZoteroItem> uploadQueue)
  {
    ArrayList<String> errMsgList = Lists.newArrayList("Attempt(s) to upload changes to server failed:");

    String unchanged = jUnchanged.keySet().stream().map(jUnchanged::getStr).reduce((s1, s2) -> s1 + ", " + s2).orElse("");

    if (unchanged.length() > 0)
      errMsgList.add("Unchanged: " + unchanged);

    jFailed.keySet().forEach(queueNdx ->
    {
      ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1));
      JsonObj jError = jFailed.getObj(queueNdx);
      errMsgList.add(item.getEntryKey() + " code: " + jError.getLong("code", -1) + " " + jError.getStr("message"));
    });

    fxThreadReturnValue = null;

    runInFXThread(() ->
    {
      messageDialogSameThread(strListToStr(errMsgList, false), mtError);

      fxThreadReturnValue = Boolean.TRUE;
    });

    while (fxThreadReturnValue == null) sleepForMillis(100);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private <ZEntity extends ZoteroEntity> boolean getRemoteUpdates(ZoteroCmd versionsCmd, ZoteroCmd readCmd, Map<String, ZEntity> keyToEntity) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    ArrayList<String> downloadQueue = new ArrayList<>();

    JsonArray jArr = doReadCommand(versionsCmd, "", "");

    if (syncTask.isCancelled()) throw new TerminateTaskException();

    if ((jsonClient.getStatusCode() == HttpStatus.SC_OK) || (jsonClient.getStatusCode() == HttpStatus.SC_NOT_MODIFIED))
      if (onlineLibVersion <= offlineLibVersion)
        return true;

    if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
    {
      JsonObj jObj = jArr.getObj(0);

      jObj.keySet().forEach(key ->
      {
        ZEntity entity = keyToEntity.get(key);

        if (entity == null)
          downloadQueue.add(key);
        else
        {
          long onlineVersion = jObj.getLong(key, -1);
          if (entity.getVersion() < onlineVersion)
            downloadQueue.add(key);
        }
      });

      if (versionsCmd == ZoteroCmd.readTrashVersions) // This if block is necessary to determine if an item in the trash was remotely restored
        keyToTrashEntry.entrySet().removeIf(entry -> jObj.containsKey(entry.getKey()) == false);
    }

    String keys = "";
    while ((downloadQueue.size() > 0) && (jsonClient.getStatusCode() == HttpStatus.SC_OK))
    {
      int downloadCount = (downloadQueue.size() > 50) ? 50 : downloadQueue.size();
      for (int ndx = 0; ndx < downloadCount; ndx++)
        keys = keys + (keys.length() == 0 ? downloadQueue.get(ndx) : "," + downloadQueue.get(ndx));

      if (readCmd == ZoteroCmd.readCollections)
        jArr = doReadCommand(ZoteroCmd.readCollections, "", keys);
      else
        jArr = doReadCommand(readCmd, keys, "");

      if (syncTask.isCancelled()) throw new TerminateTaskException();

      if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
      {
        jArr.getObjs().forEach(jObj ->
        {
          String key = jObj.getStrSafe("key");
          ZEntity entity = keyToEntity.get(key);
          jObj.put("synced", "true");

          if (entity == null)
          {
            entity = (ZEntity) ZoteroEntity.create(this, jObj);

            if (entity != null)
              keyToEntity.put(key, entity);
          }
          else
          {
            if (entity.isSynced())
            {
              long onlineVersion = jObj.getLong("version", -1);
              if (entity.getVersion() < onlineVersion)
                entity.update(jObj, true, false);
            }
            else
            {
              if (readCmd == ZoteroCmd.readItems)
                doMerge((ZoteroItem)entity, jObj);
              else
                entity.update(jObj, true, false);     // Conflict resolution is only implemented for items, not collections
            }
          }

          downloadQueue.remove(key);
        });
      }
    }

    return (jsonClient.getStatusCode() == HttpStatus.SC_OK) || (jsonClient.getStatusCode() == HttpStatus.SC_NOT_MODIFIED);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private Boolean fxThreadReturnValue = null;

  boolean doMerge(ZoteroItem item, JsonObj jObj)
  {
    fxThreadReturnValue = null;

    runInFXThread(() ->
    {
      MergeWorksDialogController mwd = null;

      try
      {
        mwd = MergeWorksDialogController.create("Merge Remote Changes with Local Changes", item, new ZoteroItem(this, jObj, true),
                                                null, null, item.getWork(), false, false);
      }
      catch (IOException e)
      {
        messageDialog("Unable to initialize merge dialog window.", mtError);
        fxThreadReturnValue = Boolean.FALSE;
        return;
      }

      item.update(jObj, true, true);

      if (mwd.showModal())
        mwd.mergeInto(item);

      fxThreadReturnValue = Boolean.TRUE;
    });

    while (fxThreadReturnValue == null) sleepForMillis(100);

    return fxThreadReturnValue.booleanValue();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public SyncTask createNewSyncTask()
  {
    syncTask = new SyncTask() { @Override public Boolean call() throws TerminateTaskException, HyperDataException
    {
    //---------------------------------------------------------------------------
    // The algorithm for Zotero syncing is described here:
    // https://www.zotero.org/support/dev/web_api/v3/syncing

      int statusCode;

      try
      {

      /*********************************************/
      /*        Try sending local updates          */
      /*********************************************/

      if (syncChangedEntriesToServer() == false)
        statusCode = HttpStatus.SC_PRECONDITION_FAILED;
      else
      {
        statusCode = jsonClient.getStatusCode();
        changed = true;
      }

      if (syncTask.isCancelled()) throw new TerminateTaskException();

      /*********************************************/
      /*         Retrieve remote updates           */
      /*********************************************/

      while (statusCode == HttpStatus.SC_PRECONDITION_FAILED)
      {
        if (!getRemoteUpdates(ZoteroCmd.readChangedCollVersions, ZoteroCmd.readCollections, keyToColl)) return false;

        if (onlineLibVersion <= offlineLibVersion)
          return true;
        else
          changed = true;

        if (!getRemoteUpdates(ZoteroCmd.readChangedItemVersions, ZoteroCmd.readItems, keyToAllEntry  )) return false;
        if (!getRemoteUpdates(ZoteroCmd.readTrashVersions,       ZoteroCmd.readTrash, keyToTrashEntry)) return false;

        /*********************************************/
        /*       Retrieve remote deletions           */
        /*********************************************/

        JsonArray jArr = doReadCommand(ZoteroCmd.readDeletions, "", "");

        if (syncTask.isCancelled()) throw new TerminateTaskException();
        if (jsonClient.getStatusCode() != HttpStatus.SC_OK) return false;

        jArr.getObj(0).getArray("items").getStrs().forEach(key ->
        {
          if (keyToAllEntry.containsKey(key) && (keyToTrashEntry.containsKey(key) == false))
          {
            // A remote deletion will simply override local changes. Undocument code to change this behavior.

//            ZoteroItem item = keyToAllEntry.get(key);
//
//            if (item.isSynced())
//            {
              HDT_Work work = db.getWorkByBibEntryKey(key);
              if (work != null)
                work.setBibEntryKey("");

              keyToAllEntry.remove(key);
//            }
//            else
//            {
//              // Perform conflict resolution!
//              noOp();
//            }
          }
          else if (keyToTrashEntry.containsKey(key))
          {
            // A remote deletion will simply override local changes. Undocument code to change this behavior.

//            ZoteroItem item = keyToTrashEntry.get(key);
//
//            if (item.isSynced())
//            {
              keyToAllEntry.remove(key);
              keyToTrashEntry.remove(key);
//            }
//            else
//            {
//              // Perform conflict resolution!
//              noOp();
//            }
          }
        });

//        it = jArr.getObj(0).getArray("collections").strIterator();
//
//        while (it.hasNext())
//        {
//          String key = it.next();
        jArr.getObj(0).getArray("collections").getStrs().forEach(key ->
        {
          if (keyToColl.containsKey(key))
          {
            // A remote deletion will simply override local changes. Undocument code to change this behavior.

//            ZoteroCollection coll = keyToColl.get(key);
//
//            if (coll.isSynced())
//            {
              keyToColl.remove(key);
//            }
//            else
//            {
//              // Perform conflict resolution!
//              noOp();
//            }
          }
        });

        if (syncTask.isCancelled()) throw new TerminateTaskException();

        if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
          offlineLibVersion = onlineLibVersion;
        else
          return false;

        /*********************************************/
        /*      Try sending local updates again      */
        /*********************************************/
        syncChangedEntriesToServer();

        if (syncTask.isCancelled()) throw new TerminateTaskException();

        statusCode = jsonClient.getStatusCode();
      }

      offlineLibVersion = onlineLibVersion;

      if (app.debugging())
        System.out.println("libraryVersion: " + offlineLibVersion);

      return true;

      }
      catch (HttpResponseException e)
      {
        String msg = "An error occurred while syncing: " + String.valueOf(e.getStatusCode()) + " " + e.getMessage();
        throw new HyperDataException(msg, e);
      }
      catch (UnknownHostException e)
      {
        String msg = "Unable to connect to host: " + e.getMessage();
        throw new HyperDataException(msg, e);
      }
      catch (UnsupportedOperationException | IOException | ParseException e)
      {
        String msg = "An error occurred while syncing: " + e.getMessage();
        throw new HyperDataException(msg, e);
      }
    }};

    return syncTask;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromJSON(JsonObj jObj)
  {
    jObj.getArray("items").getObjs().forEach(itemJsonObj ->
    {
      ZoteroEntity entity = ZoteroEntity.create(this, itemJsonObj);

      if (entity == null) return;

      if (entity.getType() == ZoteroEntityType.zoteroItem)
      {
        ZoteroItem item = (ZoteroItem) entity;

        keyToAllEntry.put(entity.getKey(), item);
      }
    });

    nullSwitch(jObj.getArray("trash"), jArr -> jArr.getObjs().forEach(itemJsonObj ->
    {
      ZoteroEntity entity = ZoteroEntity.create(this, itemJsonObj);

      if (entity == null) return;

      if (entity.getType() == ZoteroEntityType.zoteroItem)
      {
        ZoteroItem item = (ZoteroItem) entity;

        keyToAllEntry.put(entity.getKey(), item);
        keyToTrashEntry.put(entity.getKey(), item);
      }
    }));

    nullSwitch(jObj.getArray("collections"), jArr -> jArr.getObjs().forEach(collJsonObj ->
    {
      ZoteroEntity entity = ZoteroEntity.create(this, collJsonObj);

      if (entity == null) return;

      if (entity.getType() == ZoteroEntityType.zoteroCollection)
      {
        ZoteroCollection coll = (ZoteroCollection) entity;

        keyToColl.put(coll.getKey(), coll);
      }
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void loadFromDisk(FilePath filePath) throws FileNotFoundException, IOException, ParseException
  {
    JsonObj jMainObj = null;
    keyToAllEntry.clear();
    keyToTrashEntry.clear();
    keyToColl.clear();

    try (InputStream in = new FileInputStream(filePath.toFile()))
    {
      jMainObj = parseJsonObj(new InputStreamReader(in, UTF_8));
    }
    catch (FileNotFoundException e) { noOp(); }

    if (jMainObj != null)
    {
      offlineLibVersion = db.prefs.getLong(PREF_KEY_BIB_LIBRARY_VERSION, -1);

      loadFromJSON(jMainObj);
    }

    initTemplates();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Object lock = new Object();

  private static void initTemplates() throws IOException, ParseException
  {
    synchronized (lock)
    {
      if (templates != null) return;

      StringBuilder sb = new StringBuilder();

      readResourceTextFile("resources/" + ZOTERO_TEMPLATE_FILE_NAME, sb, false);
      JsonArray jArr = parseJson(sb.toString());

      if (jArr != null)
      {
        templates = new EnumMap<>(EntryType.class);

        jArr.getObjs().forEach(jObj -> templates.put(ZoteroItem.parseZoteroType(jObj.getStrSafe("itemType")), jObj));
      }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToDisk(FilePath filePath)
  {
    JsonObj jMainObj = new JsonObj();

    JsonArray jArr = new JsonArray();

    for (ZoteroItem entry : getNonTrashEntries())
      entry.saveToDisk(jArr);

    jMainObj.put("items", jArr);

    jArr = new JsonArray();

    for (ZoteroItem entry : keyToTrashEntry.values())
      entry.saveToDisk(jArr);

    jMainObj.put("trash", jArr);

    jArr = new JsonArray();

    for (ZoteroCollection coll : keyToColl.values())
      coll.saveToDisk(jArr);

    jMainObj.put("collections", jArr);

    StringBuilder json = new StringBuilder(jMainObj.toString());

    try
    {
      saveStringBuilderToFile(json, filePath);
      db.prefs.putLong(PREF_KEY_BIB_LIBRARY_VERSION, offlineLibVersion);
    }
    catch (IOException e)
    {
      messageDialog("An error occurred while saving bibliographic data to disk.", mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public ZoteroItem addEntry(EntryType newType)
  {
    ZoteroItem item = new ZoteroItem(this, newType);

    keyToAllEntry.put(item.getEntryKey(), item);

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<ZoteroItem> getCollectionEntries(String collKey)
  {
    return getNonTrashEntries().stream().filter(item -> item.getCollKeys(false).contains(collKey))
                                        .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<ZoteroItem> getNonTrashEntries()
  {
    return keyToAllEntry.values().stream().filter(item -> keyToTrashEntry.containsKey(item.getEntryKey()) == false)
                                          .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<ZoteroItem> getUnsorted()
  {
    Predicate<ZoteroItem> predicate = item ->
    {
      if (keyToTrashEntry.containsKey(item.getEntryKey()))
        return false;

      return item.getCollKeys(true).stream().noneMatch(keyToColl::containsKey);
    };

    return keyToAllEntry.values().stream().filter(predicate).collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EnumHashBiMap<EntryType, String> initTypeMap()
  {
    EnumHashBiMap<EntryType, String> map = EnumHashBiMap.create(EntryType.class);

    map.put(etArtwork, "artwork");
    map.put(etAudioRecording, "audioRecording");
    map.put(etBill, "bill");
    map.put(etBlogPost, "blogPost");
    map.put(etBook, "book");
    map.put(etBookChapter, "bookSection");
    map.put(etCase, "case");
    map.put(etConferencePaper, "conferencePaper");
    map.put(etDictionaryEntry, "dictionaryEntry");
    map.put(etDocument, "document");
    map.put(etEmail, "email");
    map.put(etEncyclopediaArticle, "encyclopediaArticle");
    map.put(etFilm, "film");
    map.put(etForumPost, "forumPost");
    map.put(etHearing, "hearing");
    map.put(etInstantMessage, "instantMessage");
    map.put(etInterview, "interview");
    map.put(etJournalArticle, "journalArticle");
    map.put(etLetter, "letter");
    map.put(etMagazineArticle, "magazineArticle");
    map.put(etManuscript, "manuscript");
    map.put(etMap, "map");
    map.put(etNewspaperArticle, "newspaperArticle");
    map.put(etPatent, "patent");
    map.put(etPodcast, "podcast");
    map.put(etPresentation, "presentation");
    map.put(etRadioBroadcast, "radioBroadcast");
    map.put(etReport, "report");
    map.put(etSoftware, "computerProgram");
    map.put(etStatute, "statute");
    map.put(etTVBroadcast, "tvBroadcast");
    map.put(etThesis, "thesis");
    map.put(etVideoRecording, "videoRecording");
    map.put(etWebPage, "webpage");

    return map;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getHtml(BibEntryRow row)
  {
    ZoteroItem item = (ZoteroItem) nullSwitch(row, null, BibEntryRow::getEntry);
    if (item == null) return "";

    JsonObj jObj  = item.exportJsonObjForUploadToServer(true),
            jData = nullSwitch(jObj.getObj("data"), jObj);

    StringBuilder html = getHtmlStart();

    jData.keySet().forEach(key ->
    {
      String fieldName = key;

      switch (fieldName)
      {
        case "relations" : case "collections" : case "key" :
        case "dateAdded" : case "accessDate"  : case "dateModified" : return;

        case "url"  : fieldName = "URL"; break;

        case "ISBN" : case "DOI" : case "ISSN" : break;

        default : fieldName = camelToTitle(fieldName); break;
      }

      switch (jData.getType(key))
      {
        case ARRAY:

          JsonArray jArr = jData.getArray(key);

          if (key.equals("creators"))
            addCreatorsHtml(jArr, html);
          else
            addArrayHtml(fieldName, jArr, html);

          break;

        case STRING:

          addStringHtml(fieldName, jData.getStrSafe(key), html);
          break;

        default:
          break;
      }
    });

    return finishHtml(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addStringHtml(String fieldName, String str, StringBuilder html)
  {
    if (str.length() == 0) return;

    if (fieldName.equals("Item Type"))
      str = camelToTitle(str);

    if (fieldName.equals("URL"))
      str = anchorTag(str, str);

    addRowToHtml(fieldName, str, html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorsHtml(JsonArray creatorsArr, StringBuilder html)
  {
    creatorsArr.getObjs().forEach(node ->
    {
      String type = node.getStrSafe("creatorType");

      if (type.length() == 0) return;

      type = camelToTitle(type);
      PersonName personName;
      String firstName = ultraTrim(node.getStrSafe("firstName")),
             lastName = ultraTrim(node.getStrSafe("lastName"));

      if ((firstName.length() > 0) || (lastName.length() > 0))
        personName = new PersonName(firstName, lastName);
      else
      {
        personName = new PersonName(node.getStrSafe("name"));
        if (personName.isEmpty()) return;
      }

      addRowToHtml(type, personName.getLastFirst(), html);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addArrayHtml(String fieldName, JsonArray jArr, StringBuilder html)
  {
    addRowsToHtml(fieldName, Lists.newArrayList((Iterable<String>)jArr.getStrs()), html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
