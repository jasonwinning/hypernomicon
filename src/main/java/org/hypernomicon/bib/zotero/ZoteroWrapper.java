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

import static org.hypernomicon.App.app;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.json.JsonObj.*;
import static org.hypernomicon.bib.zotero.ZoteroWrapper.ZoteroHeader.*;

import java.io.*;
import java.net.SocketException;
import java.nio.file.*;
import java.time.Instant;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.Header;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;

import org.json.simple.parser.ParseException;
import org.jspecify.annotations.NonNull;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.Lists;

import javafx.concurrent.Worker.State;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.HyperTask;
import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.AsyncHttpClient.HttpRequestType;
import org.hypernomicon.util.CryptoUtil;
import org.hypernomicon.util.HttpHeader;

//---------------------------------------------------------------------------

public final class ZoteroWrapper extends LibraryWrapper<ZoteroItem, ZoteroCollection>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final String userID;
  private String apiKey, userName = "";
  private long offlineLibVersion = -1, onlineLibVersion = -1;
  private Instant backoffTime = null, retryTime = null;

  private static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();

  private static EnumMap<EntryType, JsonObj> templates = null;

  private enum ZoteroCmd
  {
    readItems , readTrash    , readTrashVersions, readChangedItemVersions, readProfile,
    writeItems, readDeletions, readCollections  , readChangedCollVersions
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ZoteroWrapper(String apiKey, String userID, String userName)
  {
    this.userID = userID;

    try { enableSyncOnThisComputer(apiKey, "", "", userID, userName, false); }
    catch (HyperDataException e) { throw new AssertionError(e); }
  }

//---------------------------------------------------------------------------

  static JsonObj getTemplate(EntryType type)   { return templates.get(type); }

  public static ZoteroWrapper createForTesting() { return new ZoteroWrapper("", "", ""); }

  @Override public LibraryType type()          { return LibraryType.ltZotero; }
  @Override public String entryFileNode()      { return "items"; }
  @Override public String collectionFileNode() { return "collections"; }
  @Override public String getUserName()        { return userName; }
  @Override public String getUserID()          { return userID; }

  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @NonNull
  public static ZoteroWrapper create(String apiKey, String userID, String userName, FilePath filePath) throws HDB_InternalError, IOException, ParseException
  {
    ZoteroWrapper wrapper = new ZoteroWrapper(apiKey, userID, userName);

    wrapper.loadAllFromJsonFile(filePath);

    return wrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static JsonObj getTemplateInitIfNecessary(EntryType type) throws IOException, ParseException, HDB_InternalError
  {
    if (templates == null)
      initTemplates();

    return templates.get(type);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doWriteCommand(ZoteroCmd command, String jsonPostData) throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + '/';

    switch (command)
    {
      case writeItems:
        url += "items";
        break;

      default:
        return null;
    }

    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.post, jsonPostData);

    return switch (jsonClient.getStatusCode())
    {
      case HttpStatus.SC_OK,
           HttpStatus.SC_NOT_MODIFIED,
           HttpStatus.SC_PRECONDITION_FAILED

        -> jsonArray;

      default

        -> throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getReasonPhrase());
    };

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doReadCommand(ZoteroCmd command, String itemKey, String collectionKey) throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + '/';

    switch (command)
    {
      case readProfile:
        url += "keys/current";
        break;

      case readCollections:
        if (collectionKey.length() > 0)
          url += "collections?collectionKey=" + collectionKey;
        else
          url += "collections";
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

      case readTrash:
        if (itemKey.length() > 0)
          url += "items/trash?itemKey=" + itemKey;
        else
          url += "items/trash";
        break;

      default:
        return null;
    }

    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.get, null);

    return switch (jsonClient.getStatusCode())
    {
      case HttpStatus.SC_OK,
           HttpStatus.SC_NOT_MODIFIED,
           HttpStatus.SC_PRECONDITION_FAILED

        -> jsonArray;

      case HttpStatus.SC_UNAUTHORIZED,
           HttpStatus.SC_FORBIDDEN

        -> throw newAccessDeniedException();

      default

        -> throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getReasonPhrase());
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum ZoteroHeader
  {
    Zotero_API_Version("Zotero-API-Version"),
    Zotero_API_Key("Zotero-API-Key"),
    Zotero_Write_Token("Zotero-Write-Token"),
    If_Unmodified_Since_Version("If-Unmodified-Since-Version"),
    Total_Results("Total-Results"),
    Last_Modified_Version("Last-Modified-Version"),
    Backoff("Backoff"),
    Retry_After("Retry-After"),
    None("None");

    private final String name;
    private static final Map<String, ZoteroHeader> headerMap = new HashMap<>();

    ZoteroHeader(String name) { this.name = name; }

    @Override public String toString() { return name; }

    static { EnumSet.allOf(ZoteroHeader.class).forEach(header -> headerMap.put(header.name.toLowerCase(), header)); }

    private static ZoteroHeader get(Header header) { return headerMap.getOrDefault(header.getName().toLowerCase(), None); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void waitUntilItIs(Instant time) throws CancelledTaskException
  {
    if (time == null) return;

    while (time.isAfter(Instant.now()))
    {
      sleepForMillis(30);
      HyperTask.throwExceptionIfCancelled(syncTask);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doHttpRequest(String url, HttpRequestType requestType, String postJsonData) throws IOException, UnsupportedOperationException, ParseException, CancelledTaskException
  {
    waitUntilItIs(retryTime);
    waitUntilItIs(backoffTime);

    RequestBuilder rb = switch (requestType)
    {
      case post -> RequestBuilder.post()
                                 .setHeader(Zotero_Write_Token.toString(), generateWriteToken())
                                 .setEntity(new StringEntity(postJsonData, UTF_8));

      case get  -> RequestBuilder.get();

      default   -> throw new UnsupportedOperationException(requestType.name());
    };

    rb = rb
      .setUri(url)
      .setHeader(HttpHeader.Content_Type.toString(), "application/json")
      .setHeader(Zotero_API_Version.toString(), "3");

    if (safeStr(apiKey).isBlank() == false)
      rb = rb.setHeader(Zotero_API_Key.toString(), apiKey);

    request = rb
      .setHeader(Zotero_Write_Token.toString(), generateWriteToken())
      .setHeader(If_Unmodified_Since_Version.toString(), String.valueOf(offlineLibVersion))
      .build();

    JsonArray jsonArray;

    try
    {
      jsonArray = jsonClient.requestArrayInThisThread(request);
    }
    catch (SocketException e)
    {
      request = null;

      HyperTask.throwExceptionIfCancelled(syncTask);

      throw e;
    }

    StringBuilder apiVersion = new StringBuilder();
    MutableInt totalResults = new MutableInt(-1);

    nullSwitch(jsonClient.getHeaders(), headers -> headers.forEach(header ->
    {
      int sec;

      switch (ZoteroHeader.get(header))
      {
        case Zotero_API_Version    : assignSB(apiVersion, header.getValue()); break;
        case Total_Results         : totalResults.setValue(parseInt(header.getValue(), -1)); break;
        case Last_Modified_Version : onlineLibVersion = parseInt(header.getValue(), -1); break;
        case Backoff :

          sec = parseInt(header.getValue(), -1);
          if (sec > 0)
            backoffTime = Instant.now().plusMillis(sec * 1000L);

          break;

        case Retry_After :

          sec = parseInt(header.getValue(), -1);
          if (sec > 0)
            retryTime = Instant.now().plusMillis(sec * 1000L);

          break;

        default : break;
      }
    }));

    request = null;

    HyperTask.throwExceptionIfCancelled(syncTask);

    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Generates a random hexadecimal string of 32 characters
   * <br>Example: 981579dd7bbb22edbcd44a8b21458bd3
   * @return the generated string
   */
  private static String generateWriteToken()
  {
    return randomHexStr(32);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public JsonObj getCreatorTypes() throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    JsonObj jObj = new JsonObj();

    for (String zType : streamToIterable(entryTypeMap.values().stream().sorted()))
    {
      jObj.put(zType, doHttpRequest("https://api.zotero.org/itemTypeCreatorTypes?itemType=" + zType, HttpRequestType.get, null));
    }

    return jObj;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public JsonArray getTemplates() throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    JsonArray jArr = new JsonArray();

    for (String zType : streamToIterable(entryTypeMap.values().stream().sorted()))
    {
      jArr.add(doHttpRequest("https://api.zotero.org/items/new?itemType=" + zType, HttpRequestType.get, null).getObj(0));
    }

    return jArr;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void retrieveMetadataAndSaveToFile(boolean getCreatorsNotTemplates)
  {
    try
    {
      StringBuilder json = new StringBuilder(getCreatorsNotTemplates ?
        getCreatorTypes().toString()
      :
        getTemplates().toString());

      FilePath filePath = db.xmlPath(getCreatorsNotTemplates ?
        ZOTERO_CREATOR_TYPES_FILE_NAME
      :
        ZOTERO_TEMPLATE_FILE_NAME);

      saveStringBuilderToFile(json, filePath);
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      e.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public SyncTask createNewSyncTask() { return syncTask = new SyncTask()
  {
    @Override public void call() throws CancelledTaskException, HyperDataException
    {

    //---------------------------------------------------------------------------
    // The algorithm for Zotero syncing is described here:
    // https://www.zotero.org/support/dev/web_api/v3/syncing

      didMergeDuringSync = false;

      try
      {

    /*********************************************/
    /*        Try sending local updates          */
    /*********************************************/

        int statusCode;

        if (syncChangedEntriesToServer() == false)
          statusCode = HttpStatus.SC_PRECONDITION_FAILED;
        else
        {
          statusCode = jsonClient.getStatusCode();
          changed = true;
        }

    /*********************************************/
    /*         Retrieve remote updates           */
    /*********************************************/

        while (statusCode == HttpStatus.SC_PRECONDITION_FAILED)
        {
          if (!getRemoteUpdates(ZoteroCmd.readChangedCollVersions, ZoteroCmd.readCollections, keyToColl)) return;

          if (onlineLibVersion <= offlineLibVersion)
            return;

          changed = true;

          if (!getRemoteUpdates(ZoteroCmd.readChangedItemVersions, ZoteroCmd.readItems, keyToAllEntry  )) return;
          if (!getRemoteUpdates(ZoteroCmd.readTrashVersions,       ZoteroCmd.readTrash, keyToTrashEntry)) return;

      /*********************************************/
      /*       Retrieve remote deletions           */
      /*********************************************/

          JsonArray jArr = doReadCommand(ZoteroCmd.readDeletions, "", "");

          if (jsonClient.getStatusCode() != HttpStatus.SC_OK) return;

          jArr.getObj(0).getArraySafe("items").strStream().forEach(key ->
          {
            if (keyToAllEntry.containsKey(key))
            {
              // A remote deletion will simply override local changes. Undocument code to change this behavior.

//              ZoteroItem item = keyToAllEntry.get(key);
//
//              if (item.isSynced())
//              {
                HDT_Work work = db.getWorkByBibEntryKey(key);
                if (work != null)
                  work.setBibEntryKey("");

                keyToAllEntry.remove(key);
                keyToTrashEntry.remove(key);
//              }
//              else
//              {
//                // Perform conflict resolution!
//                noOp();
//              }
            }
          });

          jArr.getObj(0).getArraySafe("collections").strStream().forEach(key ->
          {
            if (keyToColl.containsKey(key))
            {
              // A remote deletion will simply override local changes. Undocument code to change this behavior.

//              ZoteroCollection coll = keyToColl.get(key);
//
//              if (coll.isSynced())
//              {
                keyToColl.remove(key);
//              }
//              else
//              {
//                // Perform conflict resolution!
//                noOp();
//              }
            }
          });

          if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
            offlineLibVersion = onlineLibVersion;
          else
            return;

      /*********************************************/
      /*      Try sending local updates again      */
      /*********************************************/

          syncChangedEntriesToServer();

          statusCode = jsonClient.getStatusCode();
        }

        offlineLibVersion = onlineLibVersion;

        if (app.debugging)
          System.out.println("libraryVersion: " + offlineLibVersion);
      }
      catch (UnsupportedOperationException | IOException | ParseException e)
      {
        throw new HyperDataException("An error occurred while syncing: " + getThrowableMessage(e), e);
      }
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    private boolean syncChangedEntriesToServer() throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
    {
      // uploadQueue is implemented as an array because indices are returned by server
      List<ZoteroItem> uploadQueue = getAllEntries().stream().filter(Predicate.not(ZoteroItem::isSynced)).collect(Collectors.toCollection(ArrayList::new));

      if (uploadQueue.isEmpty()) return false;

      int statusCode = HttpStatus.SC_OK;
      JsonArray jArr = new JsonArray();

      while ((uploadQueue.size() > 0) && (statusCode == HttpStatus.SC_OK) && ((syncTask == null) || (syncTask.isCancelled() == false)))
      {
        jArr.clear();

        int uploadCount = Math.min(uploadQueue.size(), 50);
        for (int ndx = 0; ndx < uploadCount; ndx++)
          jArr.add(uploadQueue.get(ndx).exportStandaloneJsonObj(true));

        jArr = doWriteCommand(ZoteroCmd.writeItems, jArr.toString());

        statusCode = jsonClient.getStatusCode();

        if (statusCode == HttpStatus.SC_OK)
        {
          JsonObj jSuccess   = jArr.getObj(0).getObj("successful"),
                  jUnchanged = jArr.getObj(0).getObj("unchanged"),
                  jFailed    = jArr.getObj(0).getObj("failed");

          if ((jUnchanged.keySet().isEmpty() == false) || (jFailed.keySet().isEmpty() == false))
            showWriteErrorMessages(jUnchanged, jFailed, uploadQueue);

          jSuccess.keySet().forEach(queueNdx ->
          {
            JsonObj jObj = jSuccess.getObj(queueNdx);
            ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1)); // here we take advantage of the fact that the upload "queue" is an array

            String oldKey = item.getKey();
            boolean newEntry = item.isNewEntry();

            item.update(jObj, false, false);

            if (newEntry)
              updateKey(oldKey, item.getKey());

            keyToAllEntry.putIfAbsent(item.getKey(), item);

            if (item.getVersion() > onlineLibVersion)
              onlineLibVersion = item.getVersion();
          });

          jUnchanged.keySet().forEach(queueNdx -> getEntryByKey(jUnchanged.getStr(queueNdx)).mergeWithBackupCopy());

          uploadQueue.subList(0, uploadCount).clear();
        }
      }

      return true;
    }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

    @SuppressWarnings("unchecked")
    private <ZEntity extends ZoteroEntity> boolean getRemoteUpdates(ZoteroCmd versionsCmd, ZoteroCmd readCmd, Map<String, ZEntity> keyToEntity)
        throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
    {
      JsonArray jArr = doReadCommand(versionsCmd, "", "");

      if ((jsonClient.getStatusCode() == HttpStatus.SC_OK) || (jsonClient.getStatusCode() == HttpStatus.SC_NOT_MODIFIED))
        if (onlineLibVersion <= offlineLibVersion)
          return true;

      List<String> downloadQueue = new ArrayList<>();

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

      while ((downloadQueue.size() > 0) && (jsonClient.getStatusCode() == HttpStatus.SC_OK))
      {
        String keys = String.join(",", downloadQueue.subList(0, Math.min(downloadQueue.size(), 50)));

        jArr = readCmd == ZoteroCmd.readCollections ?
          doReadCommand(ZoteroCmd.readCollections, "", keys)
        :
          doReadCommand(readCmd, keys, "");

        if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
        {
          jArr.getObjs().forEach(jObj ->
          {
            String key = jObj.getStrSafe("key");
            ZEntity entity = keyToEntity.get(key);

            if (entity == null)
            {
              entity = (ZEntity)(readCmd == ZoteroCmd.readCollections ?
                new ZoteroCollection(jObj)
              :
                ZoteroItem.create(ZoteroWrapper.this, jObj));

              if (entity != null)
                keyToEntity.put(key, entity);
            }
            else
            {
              boolean okToMerge = true;

              if (readCmd != ZoteroCmd.readCollections)
              {
                ZoteroItem zItem = (ZoteroItem)entity;
                String entryTypeStr = ZoteroItem.getEntryTypeStrFromSpecifiedJson(jObj.getObj("data"));

                if (parseEntryType(entryTypeStr) == etOther)
                {
                  okToMerge = false;

                  if (zItem.linkedToWork())
                  {
                    int workID = zItem.getWork().getID();
                    zItem.unassignWork();
                    warningPopup("Unassigning work record due to unrecognized entry type: \"" + entryTypeStr + "\"\n\nWork ID: " + workID);
                  }
                }
              }

              if (entity.isSynced())
              {
                long onlineVersion = jObj.getLong("version", -1);
                if (entity.getVersion() < onlineVersion)
                  entity.update(jObj, true, false);
              }
              else
              {
                if (okToMerge && (readCmd != ZoteroCmd.readCollections))
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

  }; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void showWriteErrorMessages(JsonObj jUnchanged, JsonObj jFailed, List<ZoteroItem> uploadQueue)
  {
    List<String> errMsgList = Lists.newArrayList("Attempt(s) to upload changes to server failed:");

    String unchanged = jUnchanged.keySet().stream().map(jUnchanged::getStr).collect(Collectors.joining(", "));

    if (unchanged.length() > 0)
      errMsgList.add("Unchanged: " + unchanged);

    jFailed.keySet().forEach(queueNdx ->
    {
      ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1));
      JsonObj jError = jFailed.getObj(queueNdx);
      errMsgList.add(item.getKey() + " code: " + jError.getLong("code", -1) + ' ' + jError.getStr("message"));
    });

    errorPopup(strListToStr(errMsgList, false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getProfileInfoFromServer(Consumer<String> successHndlr, Consumer<Throwable> failHndlr)
  {

//---------------------------------------------------------------------------

    HyperTask hyperTask = new HyperTask("GetZoteroProfileInfo")
    {
      @Override protected void call() throws HyperDataException, CancelledTaskException
      {
        try
        {
          JsonObj profile = doReadCommand(ZoteroCmd.readProfile, "", "").getObj(0);

          userName = profile.getStrSafe("username");
        }
        catch (UnsupportedOperationException | IOException | ParseException e)
        {
          throw new HyperDataException("An error occurred while retrieving profile information from server: " + getThrowableMessage(e), e);
        }
      }
    };

//---------------------------------------------------------------------------

    hyperTask.runningProperty().addListener((ob, wasRunning, isRunning) ->
    {
      if (wasRunning && Boolean.FALSE.equals(isRunning))
      {
        if ((hyperTask.getState() == State.FAILED) || (hyperTask.getState() == State.CANCELLED))
        {
          failHndlr.accept(hyperTask.catchException());
          return;
        }

        successHndlr.accept(userName);
      }
    });

    hyperTask.startWithNewThreadAsDaemon();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void enableSyncOnThisComputer(String apiKey, String accessToken, String refreshToken, String userID, String userName, boolean save) throws HyperDataException
  {
    if (this.userID.equals(userID) == false)
      throw new HyperDataException("User ID for local entries is " + this.userID + ", but user ID from server is " + userID);

    this.apiKey = apiKey;
    this.userName = userName;

    if (save)
      saveRefMgrSecretsToDBSettings();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void savePrefs()
  {
    db.prefs.putLong(PrefKey.BIB_LIBRARY_VERSION, offlineLibVersion);
    db.prefs.put(PrefKey.BIB_USER_ID, userID);
    db.prefs.put(PrefKey.BIB_USER_NAME, userName);
    db.prefs.put(PrefKey.BIB_LIBRARY_TYPE, type().descriptor);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveRefMgrSecretsToDBSettings()
  {
    assert(app.debugging);

    try
    {
      db.prefs.put(PrefKey.BIB_API_KEY, CryptoUtil.encrypt("", apiKey));
    }
    catch (Exception e)
    {
      errorPopup("An error occurred while saving access token: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadAllFromJsonFile(FilePath filePath) throws IOException, ParseException, HDB_InternalError
  {
    JsonObj jMainObj = null;

    try (InputStream in = Files.newInputStream(filePath.toPath()))
    {
      jMainObj = parseJsonObj(new InputStreamReader(in, UTF_8));
    }
    catch (FileNotFoundException | NoSuchFileException e)
    {
      noOp();  // This happens when first linking to Zotero
    }

    if (jMainObj != null)
    {
      offlineLibVersion = db.prefs.getLong(PrefKey.BIB_LIBRARY_VERSION, -1);

      loadFromJSON(jMainObj);
    }

    initTemplates();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static synchronized void initTemplates() throws IOException, ParseException, HDB_InternalError
  {
    if (templates != null) return;

    templates = new EnumMap<>(EntryType.class);

    StringBuilder sb = new StringBuilder();

    readResourceTextFile("resources/" + ZOTERO_TEMPLATE_FILE_NAME, sb, false);
    JsonArray jArr = parseJson(sb.toString());

    if (jArr == null)
      throw new HDB_InternalError(77653);

    for (JsonObj jObj : jArr.getObjs())
    {
      EntryType entryType = entryTypeMap.inverse().getOrDefault(jObj.getStrSafe("itemType"), etOther);

      if (entryType == etOther)
        throw new HDB_InternalError(77654);

      templates.put(entryType, jObj);
    }

    // Make sure the set of item types in the templates is the same as the ones in the type map

    if ((entryTypeMap.keySet().size() != templates.keySet().size()) ||
        (entryTypeMap.keySet().containsAll(templates.keySet()) == false))
    {
      throw new HDB_InternalError(77655);
    }
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
    map.put(etPreprint, "preprint");
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

}
