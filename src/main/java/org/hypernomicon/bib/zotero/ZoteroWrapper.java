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
import static org.hypernomicon.bib.data.EntryType.*;
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
import java.util.List;
import java.util.Map;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.Lists;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.bib.BibEntryRow;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.items.PersonName;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.AsyncHttpClient.HttpRequestType;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class ZoteroWrapper extends LibraryWrapper<ZoteroItem, ZoteroCollection>
{
  private final String apiKey, userID;
  private long offlineLibVersion = -1, onlineLibVersion = -1;
  private Instant backoffTime = null, retryTime = null;

  static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();

  private static EnumMap<EntryType, JsonObj> templates = null;

  private static enum ZoteroCmd
  {
    readItems , readTrash    , readTrashVersions, readChangedItemVersions,
    writeItems, readDeletions, readCollections  , readChangedCollVersions
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ZoteroWrapper(String apiKey, String userID)
  {
    this.apiKey = apiKey;
    this.userID = userID;
  }

  JsonObj getTemplate(EntryType type)          { return templates.get(type); }

  @Override public LibraryType type()          { return LibraryType.ltZotero; }
  @Override public void safePrefs()            { db.prefs.putLong(PREF_KEY_BIB_LIBRARY_VERSION, offlineLibVersion); }
  @Override public String entryFileNode()      { return "items"; }
  @Override public String collectionFileNode() { return "collections"; }

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

    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.post, jsonPostData);

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

  private JsonArray doHttpRequest(String url, HttpRequestType requestType, String postJsonData) throws IOException, UnsupportedOperationException, ParseException, TerminateTaskException
  {
    RequestBuilder rb;

    if (retryTime != null)
    {
      while ((retryTime.compareTo(Instant.now()) > 0) && (syncTask.isCancelled() == false))
      {
        sleepForMillis(30);
        if (syncTask.isCancelled()) throw new TerminateTaskException();
      }
    }

    if (backoffTime != null)
    {
      while ((backoffTime.compareTo(Instant.now()) > 0) && (syncTask.isCancelled() == false))
      {
        sleepForMillis(30);
        if (syncTask.isCancelled()) throw new TerminateTaskException();
      }
    }

    switch (requestType)
    {
      case post : rb = RequestBuilder.post()
                                     .setHeader("Zotero-Write-Token", generateWriteToken())
                                     .setEntity(new StringEntity(postJsonData, UTF_8));
                  break;

      case get  : rb = RequestBuilder.get();
                  break;

      default : throw new UnsupportedOperationException(requestType.name());
    }

    request = rb
      .setUri(url)
      .setHeader(HttpHeaders.CONTENT_TYPE, "application/json")
      .setHeader("Zotero-API-Version", "3")
      .setHeader("Zotero-API-Key", apiKey)
      .setHeader("Zotero-Write-Token", generateWriteToken())
      .setHeader("If-Unmodified-Since-Version", String.valueOf(offlineLibVersion))
      .build();

    JsonArray jsonArray = null;

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

    StringBuilder apiVersion = new StringBuilder();
    MutableInt totalResults = new MutableInt(-1);

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

    if (syncTask.isCancelled()) throw new TerminateTaskException();

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
        jObj.put(zType, doHttpRequest("https://api.zotero.org/itemTypeCreatorTypes?itemType=" + zType, HttpRequestType.get, null));
      }

      StringBuilder json = new StringBuilder(jObj.toString());

      FilePath filePath = db.xmlPath(ZOTERO_CREATOR_TYPES_FILE_NAME);

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
        jArr.add(doHttpRequest("https://api.zotero.org/items/new?itemType=" + zType, HttpRequestType.get, null).getObj(0));
      }

      StringBuilder json = new StringBuilder(jArr.toString());

      FilePath filePath = db.xmlPath(ZOTERO_TEMPLATE_FILE_NAME);

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
    List<ZoteroItem> uploadQueue = new ArrayList<>(); // implemented as array because indices are returned by server
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

        for (int ndx = 0; ndx < uploadCount; ndx++)
          uploadQueue.remove(0);
      }
    }

    return true;
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private void showWriteErrorMessages(JsonObj jUnchanged, JsonObj jFailed, List<ZoteroItem> uploadQueue)
  {
    List<String> errMsgList = Lists.newArrayList("Attempt(s) to upload changes to server failed:");

    String unchanged = jUnchanged.keySet().stream().map(jUnchanged::getStr).reduce((s1, s2) -> s1 + ", " + s2).orElse("");

    if (unchanged.length() > 0)
      errMsgList.add("Unchanged: " + unchanged);

    jFailed.keySet().forEach(queueNdx ->
    {
      ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1));
      JsonObj jError = jFailed.getObj(queueNdx);
      errMsgList.add(item.getKey() + " code: " + jError.getLong("code", -1) + " " + jError.getStr("message"));
    });

    messageDialog(strListToStr(errMsgList, false), mtError, true);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private <ZEntity extends ZoteroEntity> boolean getRemoteUpdates(ZoteroCmd versionsCmd, ZoteroCmd readCmd, Map<String, ZEntity> keyToEntity) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    List<String> downloadQueue = new ArrayList<>();

    JsonArray jArr = doReadCommand(versionsCmd, "", "");

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

    while ((downloadQueue.size() > 0) && (jsonClient.getStatusCode() == HttpStatus.SC_OK))
    {
      String keys = "";

      int downloadCount = (downloadQueue.size() > 50) ? 50 : downloadQueue.size();
      for (int ndx = 0; ndx < downloadCount; ndx++)
        keys = keys + (keys.isEmpty() ? downloadQueue.get(ndx) : "," + downloadQueue.get(ndx));

      if (readCmd == ZoteroCmd.readCollections)
        jArr = doReadCommand(ZoteroCmd.readCollections, "", keys);
      else
        jArr = doReadCommand(readCmd, keys, "");

      if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
      {
        jArr.getObjs().forEach(jObj ->
        {
          String key = jObj.getStrSafe("key");
          ZEntity entity = keyToEntity.get(key);

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

  @Override public SyncTask createNewSyncTask()
  {
    syncTask = new SyncTask() { @Override public Boolean call() throws TerminateTaskException, HyperDataException
    {
    //---------------------------------------------------------------------------
    // The algorithm for Zotero syncing is described here:
    // https://www.zotero.org/support/dev/web_api/v3/syncing

      int statusCode;
      didMergeDuringSync = false;

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

        if (jsonClient.getStatusCode() != HttpStatus.SC_OK) return false;

        jArr.getObj(0).getArray("items").getStrs().forEach(key ->
        {
          if (keyToAllEntry.containsKey(key))
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

        if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
          offlineLibVersion = onlineLibVersion;
        else
          return false;

        /*********************************************/
        /*      Try sending local updates again      */
        /*********************************************/
        syncChangedEntriesToServer();

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

  @Override public void loadFromDisk(FilePath filePath) throws FileNotFoundException, IOException, ParseException
  {
    JsonObj jMainObj = null;
    clear();

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
            addFieldHtml("Creators", makeCreatorsHtml(jArr));
          else if (key.equals("tags"))
            addFieldHtml("Tags", makeTagsHtml(jArr));
          else
            addFieldHtml(fieldName, makeArrayHtml(fieldName, jArr));

          break;

        case STRING:

          addFieldHtml(fieldName, makeStringHtml(fieldName, jData.getStrSafe(key)));
          break;

        default:
          break;
      }
    });

    return compileHtml();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeStringHtml(String fieldName, String str)
  {
    if (str.isEmpty()) return "";

    if (fieldName.equals("Item Type"))
      str = camelToTitle(str);

    if (fieldName.equals("URL"))
      str = anchorTag(str, str);

    return makeHtmlRow(fieldName, str);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeTagsHtml(JsonArray tagsArr)
  {
    List<String> list = new ArrayList<>();

    tagsArr.getObjs().forEach(node -> list.add(node.getStrSafe("tag")));

    return makeHtmlRows("Tags", list);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeCreatorsHtml(JsonArray creatorsArr)
  {
    StringBuilder html = new StringBuilder();

    creatorsArr.getObjs().forEach(node ->
    {
      String type = node.getStrSafe("creatorType");

      if (type.isEmpty()) return;

      type = camelToTitle(type);
      PersonName personName;
      String firstName = ultraTrim(node.getStrSafe("firstName")),
             lastName  = ultraTrim(node.getStrSafe("lastName" ));

      if ((firstName.length() > 0) || (lastName.length() > 0))
        personName = new PersonName(firstName, lastName);
      else
      {
        personName = new PersonName(node.getStrSafe("name"));
        if (personName.isEmpty()) return;
      }

      html.append(makeHtmlRow(type, personName.getLastFirst()));
    });

    return html.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String makeArrayHtml(String fieldName, JsonArray jArr)
  {
    return makeHtmlRows(fieldName, Lists.newArrayList((Iterable<String>)jArr.getStrs()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected List<String> getHtmlFieldOrder()
  {
    return List.of(

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
        "Publisher");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
