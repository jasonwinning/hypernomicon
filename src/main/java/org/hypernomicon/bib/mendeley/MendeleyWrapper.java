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

package org.hypernomicon.bib.mendeley;

import static org.hypernomicon.model.HyperDB.*;
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
import java.util.EnumMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;
import org.json.simple.parser.ParseException;

import com.google.common.collect.EnumHashBiMap;
import com.google.common.collect.ImmutableSet;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.workMerge.MergeWorksDlgCtrlr;
import org.hypernomicon.bib.BibEntryRow;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.util.JsonHttpClient;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class MendeleyWrapper extends LibraryWrapper<MendeleyDocument, MendeleyFolder>
{
  @SuppressWarnings("unused")
  private String accessToken, requestToken;
  private final JsonHttpClient jsonClient;
  private Instant lastSyncTime = Instant.MIN;

  static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();

  private static EnumMap<EntryType, JsonObj> templates = null;

  @Override public LibraryType type() { return LibraryType.ltMendeley; }

  private static enum MendeleyCmd
  {
    readFolders,
    readDocuments,
    readTrash
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MendeleyWrapper(String apiKey, String requestToken)
  {
    this.accessToken = apiKey;
    this.requestToken = requestToken;
    jsonClient = new JsonHttpClient();
  }

  JsonObj getTemplate(EntryType type) { return templates.get(type); }

  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doHttpRequest(String url, boolean isPost, String postJsonData, String mediaType) throws IOException, UnsupportedOperationException, ParseException, TerminateTaskException
  {
    JsonArray jsonArray = null;
    MutableInt totalResults = new MutableInt(-1);

    RequestBuilder rb;

    if (syncTask.isCancelled()) throw new TerminateTaskException();

    if (isPost)
      rb = RequestBuilder.post()
          .setEntity(new StringEntity(postJsonData, UTF_8));
    else
      rb = RequestBuilder.get();

    request = rb
      .setUri(url)

      .setHeader("Authorization", "Bearer " + accessToken)
      .setHeader("Accept", mediaType)

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
      switch (header.getName())
      {
        case "Mendeley-Count"         : totalResults.setValue(parseInt(header.getValue(), -1)); break;

        case "Link" :

          System.out.println(header.getValue()); break;
      }
    }));

    request = null;
    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private <MEntity extends MendeleyEntity> boolean getRemoteUpdates(MendeleyCmd readCmd, Map<String, MEntity> keyToEntity) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    JsonArray jArr = doReadCommand(readCmd);

    System.out.println(jArr.toString());

    if (syncTask.isCancelled()) throw new TerminateTaskException();

    return (jsonClient.getStatusCode() == HttpStatus.SC_OK) || (jsonClient.getStatusCode() == HttpStatus.SC_NOT_MODIFIED);
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  private JsonArray doReadCommand(MendeleyCmd command) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.mendeley.com/", mediaType = "";

    switch (command)
    {
      case readFolders :
        url += "folders";
        mediaType = "application/vnd.mendeley-folder.1+json";
        break;

      case readDocuments :
        url += "documents?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50";
        mediaType = "application/vnd.mendeley-document.1+json";
        break;

      case readTrash :
        url += "trash?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50";
        mediaType = "application/vnd.mendeley-document.1+json";
        break;
    }

    JsonArray jsonArray = doHttpRequest(url, false, null, mediaType);

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

  private Boolean fxThreadReturnValue = null;

  boolean doMerge(MendeleyDocument item, JsonObj jObj)
  {
    fxThreadReturnValue = null;

    runInFXThread(() ->
    {
      MergeWorksDlgCtrlr mwd = null;

      try
      {
        mwd = MergeWorksDlgCtrlr.create("Merge Remote Changes with Local Changes", item, new MendeleyDocument(this, jObj, true),
                                        null, null, item.getWork(), false, false, false);
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

      int statusCode = HttpStatus.SC_PRECONDITION_FAILED;

      try
      {
//
//      /*********************************************/
//      /*        Try sending local updates          */
//      /*********************************************/
//
//      if (syncChangedEntriesToServer() == false)
//        statusCode = HttpStatus.SC_PRECONDITION_FAILED;
//      else
//      {
//        statusCode = jsonClient.getStatusCode();
//        changed = true;
//      }

      if (syncTask.isCancelled()) throw new TerminateTaskException();

      /*********************************************/
      /*         Retrieve remote updates           */
      /*********************************************/

      while (statusCode == HttpStatus.SC_PRECONDITION_FAILED)
      {
        if (!getRemoteUpdates(MendeleyCmd.readFolders, keyToColl)) return false;

//        if (onlineLibVersion <= offlineLibVersion)
//          return true;
//        else
          changed = true;
//
//        if (!getRemoteUpdates(MendeleyCmd.readChangedItemVersions, MendeleyCmd.readItems, keyToAllEntry  )) return false;
//        if (!getRemoteUpdates(MendeleyCmd.readTrashVersions,       MendeleyCmd.readTrash, keyToTrashEntry)) return false;
//
//        /*********************************************/
//        /*       Retrieve remote deletions           */
//        /*********************************************/
//
//        JsonArray jArr = doReadCommand(MendeleyCmd.readDeletions, "", "");
//
//        if (syncTask.isCancelled()) throw new TerminateTaskException();
//        if (jsonClient.getStatusCode() != HttpStatus.SC_OK) return false;
//
//        jArr.getObj(0).getArray("items").getStrs().forEach(key ->
//        {
//          if (keyToAllEntry.containsKey(key) && (keyToTrashEntry.containsKey(key) == false))
//          {
//            // A remote deletion will simply override local changes. Undocument code to change this behavior.
//
////            MendeleyDocument item = keyToAllEntry.get(key);
////
////            if (item.isSynced())
////            {
//              HDT_Work work = db.getWorkByBibEntryKey(key);
//              if (work != null)
//                work.setBibEntryKey("");
//
//              keyToAllEntry.remove(key);
////            }
////            else
////            {
////              // Perform conflict resolution!
////              noOp();
////            }
//          }
//          else if (keyToTrashEntry.containsKey(key))
//          {
//            // A remote deletion will simply override local changes. Undocument code to change this behavior.
//
////            MendeleyDocument item = keyToTrashEntry.get(key);
////
////            if (item.isSynced())
////            {
//              keyToAllEntry.remove(key);
//              keyToTrashEntry.remove(key);
////            }
////            else
////            {
////              // Perform conflict resolution!
////              noOp();
////            }
//          }
//        });
//
////        it = jArr.getObj(0).getArray("collections").strIterator();
////
////        while (it.hasNext())
////        {
////          String key = it.next();
//        jArr.getObj(0).getArray("collections").getStrs().forEach(key ->
//        {
//          if (keyToColl.containsKey(key))
//          {
//            // A remote deletion will simply override local changes. Undocument code to change this behavior.
//
////            MendeleyFolder coll = keyToColl.get(key);
////
////            if (coll.isSynced())
////            {
//              keyToColl.remove(key);
////            }
////            else
////            {
////              // Perform conflict resolution!
////              noOp();
////            }
//          }
//        });
//
//        if (syncTask.isCancelled()) throw new TerminateTaskException();
//
//        if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
//          offlineLibVersion = onlineLibVersion;
//        else
//          return false;
//
//        /*********************************************/
//        /*      Try sending local updates again      */
//        /*********************************************/
//        syncChangedEntriesToServer();
//
//        if (syncTask.isCancelled()) throw new TerminateTaskException();
//
//        statusCode = jsonClient.getStatusCode();
      }

//      offlineLibVersion = onlineLibVersion;
//
//      if (app.debugging())
//        System.out.println("libraryVersion: " + offlineLibVersion);

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
      String lastSyncTimeStr = db.prefs.get(PREF_KEY_BIB_LAST_SYNC_TIME, "");
      lastSyncTime = lastSyncTimeStr.isBlank() ? Instant.MIN : parseIso8601(lastSyncTimeStr);

//      loadFromJSON(jMainObj);
    }

//    initTemplates();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void saveToDisk(FilePath filePath)
  {
    JsonObj jMainObj = new JsonObj();

    JsonArray jArr = new JsonArray();

    for (MendeleyDocument entry : getNonTrashEntries())
      entry.saveToDisk(jArr);

    jMainObj.put("items", jArr);

    jArr = new JsonArray();

    for (MendeleyDocument entry : keyToTrashEntry.values())
      entry.saveToDisk(jArr);

    jMainObj.put("trash", jArr);

    jArr = new JsonArray();

    for (MendeleyFolder coll : keyToColl.values())
      coll.saveToDisk(jArr);

    jMainObj.put("collections", jArr);

    StringBuilder json = new StringBuilder(jMainObj.toString());

    try
    {
      saveStringBuilderToFile(json, filePath);
      db.prefs.put(PREF_KEY_BIB_LAST_SYNC_TIME, dateTimeToIso8601(lastSyncTime));
    }
    catch (IOException e)
    {
      messageDialog("An error occurred while saving bibliographic data to disk.", mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public MendeleyDocument addEntry(EntryType newType)
  {
    MendeleyDocument item = new MendeleyDocument(this, newType);

    keyToAllEntry.put(item.getEntryKey(), item);

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<MendeleyDocument> getCollectionEntries(String collKey)
  {
    return getNonTrashEntries().stream().filter(item -> item.getCollKeys(false).contains(collKey))
                                        .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<MendeleyDocument> getNonTrashEntries()
  {
    return keyToAllEntry.values().stream().filter(item -> keyToTrashEntry.containsKey(item.getEntryKey()) == false)
                                          .collect(ImmutableSet.toImmutableSet());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<MendeleyDocument> getUnsorted()
  {
    Predicate<MendeleyDocument> predicate = item ->
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

    return map;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getHtml(BibEntryRow row)
  {
    MendeleyDocument item = (MendeleyDocument) nullSwitch(row, null, BibEntryRow::getEntry);
    if (item == null) return "";

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
