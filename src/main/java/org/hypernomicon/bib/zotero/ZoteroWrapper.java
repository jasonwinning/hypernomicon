/*
 * Copyright 2015-2018 Jason Winning
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
import static org.hypernomicon.App.app;
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
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.http.Header;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;
import org.json.simple.parser.ParseException;

import com.google.common.base.Charsets;
import com.google.common.collect.EnumHashBiMap;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.workMerge.MergeWorksDialogController;
import org.hypernomicon.bib.BibData;
import org.hypernomicon.bib.BibData.EntryType;
import org.hypernomicon.bib.BibEntryRow;
import org.hypernomicon.bib.lib.LibraryWrapper;
import org.hypernomicon.bib.zotero.ZoteroEntity.ZoteroEntityType;
import org.hypernomicon.model.PersonName;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.JsonHttpClient;

import static org.hypernomicon.bib.BibData.EntryType.*;
import static org.hypernomicon.bib.zotero.ZoteroWrapper.ZoteroCommand.*;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class ZoteroWrapper extends LibraryWrapper<ZoteroItem, ZoteroCollection>
{
  private String apiKey;
  private JsonHttpClient jsonClient;
  private long offlineLibVersion = -1, onlineLibVersion = -1;
  private Instant backoffTime = null, retryTime = null;
  
  static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();
  
  private String userID = "";
  private static EnumMap<BibData.EntryType, JsonObj> templates = null;  

  @Override public LibraryType type() { return LibraryType.ltZotero; }
  
  public static enum ZoteroCommand
  {
    zoteroReadItems,
    zoteroReadTopItems,
    zoteroReadTrash,
    zoteroReadItem,
    zoteroReadItemChildren,
    zoteroReadItemTags,
    zoteroReadTags,
    zoteroReadCollections,
    zoteroReadTopCollections,
    zoteroReadCollection,
    zoteroReadCollectionItems,
    zoteroReadCollectionTopItems,
    zoteroReadCollectionTags,
    
    zoteroReadChangedItemVersions,
    zoteroReadChangedCollVersions,
    zoteroReadTrashVersions,
    zoteroReadDeletions,
    
    zoteroWriteItems
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public ZoteroWrapper(String apiKey, String userID)
  {
    this.apiKey = apiKey;
    this.userID = userID;
    jsonClient = new JsonHttpClient();
  }
  
  public JsonObj getTemplate(BibData.EntryType type) { return templates.get(type); }
  
  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doWriteCommand(ZoteroCommand command, String jsonPostData) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + "/";
    
    switch (command)
    {
      case zoteroWriteItems:
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

  private JsonArray doReadCommand(ZoteroCommand command, String itemKey, String collectionKey) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.zotero.org/users/" + userID + "/";
    
    switch (command)
    {
      case zoteroReadCollection:
        url += "collections/" + collectionKey;
        break;
        
      case zoteroReadCollectionItems:
        url += "collections/" + collectionKey + "/items";
        break;
        
      case zoteroReadCollectionTags:
        url += "collections/" + collectionKey + "/tags";
        break;
        
      case zoteroReadCollectionTopItems:
        url += "collections/" + collectionKey + "/items/top";
        break;
        
      case zoteroReadCollections:

        if (collectionKey.length() > 0)
          url += "collections?collectionKey=" + collectionKey;
        else
          url += "collections";
        break;
        
      case zoteroReadItem:
        url += "items/" + itemKey;
        break;
        
      case zoteroReadItemChildren:
        url += "items/" + itemKey + "/children";
        break;
        
      case zoteroReadItemTags:
        url += "items/" + itemKey + "/tags";
        break;
        
      case zoteroReadItems:
        
        if (itemKey.length() > 0)
          url += "items?itemKey=" + itemKey;
        else
          url += "items";
        break;
        
      case zoteroReadDeletions:
        url += "deleted?since=" + offlineLibVersion;
        break;
        
      case zoteroReadChangedItemVersions:
        url += "items?since=" + offlineLibVersion + "&format=versions";
        break;

      case zoteroReadChangedCollVersions:
        url += "collections?since=" + offlineLibVersion + "&format=versions";
        break;

      case zoteroReadTrashVersions:
        url += "items/trash?format=versions";
        break;

      case zoteroReadTags:
        url += "tags";
        break;
        
      case zoteroReadTopCollections:
        url += "collections/top";
        break;
        
      case zoteroReadTopItems:
        url += "items/top";
        break;
        
      case zoteroReadTrash:
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

  @SuppressWarnings("unused")
  private JsonArray doHttpRequest(String url, boolean isPost, String postJsonData) throws IOException, UnsupportedOperationException, ParseException, TerminateTaskException
  {
    String apiVersion = "";
    int totalResults = -1;
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
          .setEntity(new StringEntity(postJsonData, Charsets.UTF_8));
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
    
    if (jsonClient.getHeaders() != null)
      for (Header header : jsonClient.getHeaders())
      {
        int sec;
        
        switch (header.getName())
        {
          case "Zotero-API-Version"    : apiVersion = header.getValue(); break;
          case "Total-Results"         : totalResults = parseInt(header.getValue(), -1); break;                    
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
      }
      
    request = null;
    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String generateWriteToken()
  {
    return generateRandomHexString(32);
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
      
      FilePath filePath = db.getRootFilePath().resolve(new FilePath(ZOTERO_CREATOR_TYPES_FILE_NAME));
      
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
      
      FilePath filePath = db.getRootFilePath().resolve(new FilePath(ZOTERO_TEMPLATE_FILE_NAME));
      
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
    
    for (ZoteroItem entry : getAllEntries())
      if (entry.isSynced() == false)
        uploadQueue.add(entry);
    
    if (uploadQueue.size() == 0) return false;
    
    int statusCode = HttpStatus.SC_OK;
        
    while ((uploadQueue.size() > 0) && (statusCode == HttpStatus.SC_OK) && (syncTask.isCancelled() == false))
    {
      jArr.clear();
            
      int uploadCount = (uploadQueue.size() > 50) ? 50 : uploadQueue.size();
      for (int ndx = 0; ndx < uploadCount; ndx++)
        jArr.add(uploadQueue.get(ndx).exportJsonObjForUploadToServer(false));
      
      jArr = doWriteCommand(ZoteroCommand.zoteroWriteItems, jArr.toString());
      
      if (syncTask.isCancelled()) throw new TerminateTaskException();
      
      statusCode = jsonClient.getStatusCode();
      
      if (statusCode == HttpStatus.SC_OK)
      {
        JsonObj jSuccess = jArr.getObj(0).getObj("successful");                        
        JsonObj jUnchanged = jArr.getObj(0).getObj("unchanged");        
        JsonObj jFailed = jArr.getObj(0).getObj("failed");
        
        if ((jUnchanged.keySet().isEmpty() == false) || (jFailed.keySet().isEmpty() == false))
          showWriteErrorMessages(jUnchanged, jFailed, uploadQueue);
        
        for (String queueNdx : jSuccess.keySet())
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
        }
        
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
    ArrayList<String> errMsgList = new ArrayList<>();
    
    errMsgList.add("Attempt(s) to upload changes to server failed:");
    
    String unchanged = "";
    for (String queueNdx : jUnchanged.keySet())
      unchanged = unchanged + (unchanged.length() > 0 ? ", " : "") + jUnchanged.getStr(queueNdx);
    
    if (unchanged.length() > 0)
      errMsgList.add("Unchanged: " + unchanged);
    
    for (String queueNdx : jFailed.keySet())
    {
      ZoteroItem item = uploadQueue.get(parseInt(queueNdx, -1));
      JsonObj jError = jFailed.getObj(queueNdx);
      errMsgList.add(item.getEntryKey() + " code: " + jError.getLong("code", -1) + " " + jError.getStr("message")); 
    }
    
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
  private <ZEntity extends ZoteroEntity> boolean getRemoteUpdates(ZoteroCommand versionsCmd, ZoteroCommand readCmd, Map<String, ZEntity> keyToEntity) throws TerminateTaskException, UnsupportedOperationException, IOException, ParseException
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

      for (String key : jObj.keySet())
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
      }
      
      if (versionsCmd == zoteroReadTrashVersions) // This if block is necessary to determine if an item in the trash was remotely restored
      {
        Iterator<Entry<String, ZoteroItem>> it = keyToTrashEntry.entrySet().iterator();
        
        while (it.hasNext())
        {
          Entry<String, ZoteroItem> mapping = it.next();
          String key = mapping.getKey();
          if (jObj.containsKey(key) == false)
            it.remove();
        }
      }
    }

    String keys = "";
    while ((downloadQueue.size() > 0) && (jsonClient.getStatusCode() == HttpStatus.SC_OK))
    {
      int downloadCount = (downloadQueue.size() > 50) ? 50 : downloadQueue.size();
      for (int ndx = 0; ndx < downloadCount; ndx++)
        keys = keys + (keys.length() == 0 ? downloadQueue.get(ndx) : "," + downloadQueue.get(ndx));
      
      if (readCmd == ZoteroCommand.zoteroReadCollections)
        jArr = doReadCommand(ZoteroCommand.zoteroReadCollections, "", keys);
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
              if (readCmd == ZoteroCommand.zoteroReadItems)
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
  
  public Boolean fxThreadReturnValue = null;
  
  boolean doMerge(ZoteroItem item, JsonObj jObj)
  {
    fxThreadReturnValue = null;
    
    runInFXThread(() ->
    {
      MergeWorksDialogController mwd = null;
      
      try
      {
        mwd = MergeWorksDialogController.create("Merge Remote Changes with Local Changes", item, new ZoteroItem(this, jObj, true), null, null, false, false);
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
        if (!getRemoteUpdates(zoteroReadChangedCollVersions, zoteroReadCollections, keyToColl)) return false;
        
        if (onlineLibVersion <= offlineLibVersion) 
          return true; 
        else
          changed = true;
        
        if (!getRemoteUpdates(zoteroReadChangedItemVersions, zoteroReadItems, keyToAllEntry  )) return false;
        if (!getRemoteUpdates(zoteroReadTrashVersions,       zoteroReadTrash, keyToTrashEntry)) return false;
        
        /*********************************************/
        /*       Retrieve remote deletions           */ 
        /*********************************************/
        
        JsonArray jArr = doReadCommand(ZoteroCommand.zoteroReadDeletions, "", "");
        
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
    
    JsonArray jArr = jObj.getArray("trash");
    if (jArr == null) return;
    
    jArr.getObjs().forEach(itemJsonObj ->
    {
      ZoteroEntity entity = ZoteroEntity.create(this, itemJsonObj);
      
      if (entity == null) return;
      
      if (entity.getType() == ZoteroEntityType.zoteroItem)
      {
        ZoteroItem item = (ZoteroItem) entity;
        
        keyToAllEntry.put(entity.getKey(), item);
        keyToTrashEntry.put(entity.getKey(), item);
      }
    });
    
    jObj.getArray("collections").getObjs().forEach(collJsonObj ->
    {
      ZoteroEntity entity = ZoteroEntity.create(this, collJsonObj);
      
      if (entity == null) return;
      
      if (entity.getType() == ZoteroEntityType.zoteroCollection)
      {
        ZoteroCollection coll = (ZoteroCollection) entity;
        
        keyToColl.put(coll.getKey(), coll);
      }
    });    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  @Override public void loadFromDisk(FilePath filePath) throws FileNotFoundException, IOException, ParseException
  {
    JsonObj jMainObj = null;
    keyToAllEntry = new HashMap<>();
    keyToTrashEntry = new HashMap<>();
    keyToColl = new HashMap<>();
        
    try (InputStream in = new FileInputStream(filePath.toFile()))
    {
      jMainObj = parseJsonObj(new InputStreamReader(in, StandardCharsets.UTF_8));  
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
    LinkedHashSet<ZoteroItem> view = new LinkedHashSet<>();
    
    for (ZoteroItem item : getNonTrashEntries())
    {
      List<String> collKeys = item.getCollKeys(false);
      if (collKeys.contains(collKey))
        view.add(item);
    }
    
    return Collections.unmodifiableSet(view);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<ZoteroItem> getNonTrashEntries() 
  { 
    LinkedHashSet<ZoteroItem> view = new LinkedHashSet<>();
    
    for (ZoteroItem item : keyToAllEntry.values())
    {
      if (keyToTrashEntry.containsKey(item.getEntryKey()) == false)
        view.add(item);
    }
    
    return Collections.unmodifiableSet(view);
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public Set<ZoteroItem> getUnsorted()
  {
    LinkedHashSet<ZoteroItem> view = new LinkedHashSet<>();
    view.addAll(keyToAllEntry.values());
    
    Iterator<ZoteroItem> it = view.iterator();
    
    while (it.hasNext())
    {
      ZoteroItem item = it.next();
      
      if (keyToTrashEntry.containsKey(item.getEntryKey()))
      {
        it.remove();
        continue;
      }
      
      for (String collKey : item.getCollKeys(true))
      {
        if (keyToColl.containsKey(collKey))
        {
          it.remove();
          break;
        }
      }
    }
    
    return Collections.unmodifiableSet(view);
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
    if (row == null) return "";
    ZoteroItem item = (ZoteroItem) row.getEntry();
    if (item == null) return "";
    
    JsonObj jData = item.exportJsonObjForUploadToServer(true);
    if (jData.containsKey("data"))
      jData = jData.getObj("data");
    
    StringBuilder html = new StringBuilder();
    
    html.append("<html><head><style>")
        .append("td.fieldName { vertical-align: text-top; text-align: right; padding-right:10px; }</style></head><body>") 
        .append("<table style=\"font-size:9pt; font-family: -apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,Oxygen-Sans,Ubuntu,Cantarell,sans-serif; } line-height:10pt;\">");

    for (String key : jData.keySet())
    {
      String fieldName = key;
      
      switch (fieldName)
      {
        case "relations" : case "collections" : case "key" : 
        case "dateAdded" : case "accessDate" : case "dateModified" : continue;
          
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
    }
      
    html.append("</table></body></html>");
    return html.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addStringHtml(String fieldName, String str, StringBuilder html)
  {
    if (str.length() == 0) return;
    
    if (fieldName.equals("Item Type"))
      str = camelToTitle(str);
    
    html.append("<tr><td class=\"fieldName\">" + fieldName + "</td><td>")
        .append(str)
        .append("</td></tr>");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addCreatorsHtml(JsonArray creatorsArr, StringBuilder html)
  {
    creatorsArr.getObjs().forEach(node ->
    {
      String firstName = "", lastName = "", name = "", type = "";
      
      if (node.containsKey("firstName"))
        firstName = node.getStrSafe("firstName");
      
      if (node.containsKey("lastName"))
        lastName = node.getStrSafe("lastName");
      
      if (node.containsKey("name"))
        name = node.getStrSafe("name");
      
      if (node.containsKey("creatorType"))
        type = node.getStrSafe("creatorType");
      
      if (type.length() > 0)
      {
        type = camelToTitle(type);
        PersonName personName;
        
        if ((firstName.length() > 0) || (lastName.length() > 0))
          personName = new PersonName(firstName, lastName);
        else
          personName = new PersonName(name);
          
        if (personName.isEmpty() == false)
        {
          html.append("<tr><td class=\"fieldName\">" + type + "</td><td>")
          .append(personName.getLastFirst())
          .append("</td></tr>");
        }
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addArrayHtml(String fieldName, JsonArray jArr, StringBuilder html)
  {       
    ArrayList<String> list = new ArrayList<>();
    
    for (int ndx = 0; ndx < jArr.size(); ndx++)
    {
      String str = jArr.getStrSafe(ndx);
      list.add(str);
    }

    addStringListHtml(fieldName, list, html);    
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void addStringListHtml(String fieldName, List<String> list, StringBuilder html)
  {
    Iterator<String> it = list.iterator();
    while (it.hasNext())
    {
      String str = it.next();
      if (str == null)
        it.remove();
      else if (ultraTrim(str).length() == 0)
        it.remove();
    }
    
    if (list.size() == 0) return;
    
    html.append("<tr><td class=\"fieldName\">" + fieldName + "</td><td>");
    
    for (int ndx = 0; ndx < list.size(); ndx++)
    {
      html.append(list.get(ndx));
      if (ndx < (list.size() - 1))
        html.append("<br>");
    }
    
    html.append("</td></tr>");
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
