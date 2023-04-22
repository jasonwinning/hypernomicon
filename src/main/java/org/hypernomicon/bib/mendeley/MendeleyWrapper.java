/*
 * Copyright 2015-2023 Jason Winning
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
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.json.JsonObj.*;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.time.Instant;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import static java.nio.charset.StandardCharsets.*;

import org.apache.commons.lang3.mutable.MutableInt;
import org.apache.http.Header;
import org.apache.http.HttpHeaders;
import org.apache.http.HttpStatus;
import org.apache.http.client.HttpResponseException;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.entity.StringEntity;
import org.json.simple.parser.ParseException;

import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import com.google.common.collect.EnumHashBiMap;

import javafx.concurrent.Worker.State;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.AsyncHttpClient.HttpRequestType;
import org.hypernomicon.util.CryptoUtil;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class MendeleyWrapper extends LibraryWrapper<MendeleyDocument, MendeleyFolder>
{
  private String accessToken, refreshToken;
  private Instant lastSyncTime = Instant.EPOCH;

  private static final EnumHashBiMap<EntryType, String> entryTypeMap = initTypeMap();

  private enum MendeleyCmd
  {
    readFolders,
    readDocuments,
    readDeletedDocuments,
    readTrash,
    readProfile
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public MendeleyWrapper(String apiKey, String refreshToken)
  {
    this.accessToken = apiKey;
    this.refreshToken = refreshToken;
  }

  @Override public LibraryType type()          { return LibraryType.ltMendeley; }
  @Override public void safePrefs()            { db.prefs.put(PREF_KEY_BIB_LAST_SYNC_TIME, dateTimeToIso8601(lastSyncTime)); }
  @Override public String entryFileNode()      { return "documents"; }
  @Override public String collectionFileNode() { return "folders"; }

  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum MendeleyHeader
  {
    Mendeley_Count("Mendeley-Count"),
    Link("Link"),
    None("None");

    final private String name;
    final private static Map<String, MendeleyHeader> map = new HashMap<>();

    MendeleyHeader(String name) { this.name = name; }

    @Override public String toString() { return name; }

    static { EnumSet.allOf(MendeleyHeader.class).forEach(header -> map.put(header.name.toLowerCase(), header)); }

    private static MendeleyHeader get(Header header) { return map.getOrDefault(header.getName().toLowerCase(), None); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doHttpRequest(String url, HttpRequestType requestType, String jsonData, String mediaType, StringBuilder nextUrl) throws IOException, UnsupportedOperationException, ParseException, CancelledTaskException
  {
    if (syncTaskIsCancelled()) throw new CancelledTaskException();

    RequestBuilder rb;

    switch (requestType)
    {
      case get :
        rb = RequestBuilder.get().setHeader(HttpHeaders.ACCEPT, mediaType);
        break;

      case patch :
        rb = RequestBuilder.patch().setEntity(new StringEntity(jsonData, UTF_8))
                                   .setHeader(HttpHeaders.CONTENT_TYPE, mediaType)
                                   .setHeader(HttpHeaders.IF_UNMODIFIED_SINCE, dateTimeToHttpDate(lastSyncTime));
        break;

      case post :
        rb = RequestBuilder.post().setEntity(new StringEntity(jsonData, UTF_8))
                                  .setHeader(HttpHeaders.CONTENT_TYPE, mediaType)
                                  .setHeader(HttpHeaders.ACCEPT, mediaType);
        break;

      default : return null;
    }

    request = rb.setUri(url)
                .setHeader(HttpHeaders.AUTHORIZATION, "Bearer " + accessToken)
                .build();

    JsonArray jsonArray;

    try
    {
      jsonArray = jsonClient.requestArrayInThisThread(request);
    }
    catch(SocketException e)
    {
      if (syncTaskIsCancelled())
      {
        request = null;
        throw new CancelledTaskException();
      }

      request = null;
      throw e;
    }

    MutableInt totalResults = new MutableInt(-1);

    nullSwitch(jsonClient.getHeaders(), headers -> headers.forEach(header ->
    {
      switch (MendeleyHeader.get(header))
      {
        case Mendeley_Count : totalResults.setValue(parseInt(header.getValue(), -1)); break;

        case Link :

          String val = header.getValue();
          if (val.endsWith("rel=\"next\""))
            assignSB(nextUrl, val.split("<")[1].split(">")[0]);

          break;

        default : break;
      }
    }));

    request = null;

    if (syncTaskIsCancelled()) throw new CancelledTaskException();

    if (jsonClient.getStatusCode() == HttpStatus.SC_UNAUTHORIZED)
    {
      if ((jsonArray != null) && (jsonArray.size() > 0))
      {
        JsonObj jsonObj = jsonArray.getObj(0);
        if ("oauth/TOKEN_EXPIRED".equals(jsonObj.getStrSafe("errorId")))
        {
          try (OAuth20Service service = MendeleyOAuthApi.service())
          {
            OAuth2AccessToken token = service.refreshAccessToken(refreshToken);

            accessToken = token.getAccessToken();
            refreshToken = token.getRefreshToken();

            try
            {
              db.prefs.put(PREF_KEY_BIB_ACCESS_TOKEN , CryptoUtil.encrypt("", accessToken ));
              db.prefs.put(PREF_KEY_BIB_REFRESH_TOKEN, CryptoUtil.encrypt("", refreshToken));
            }
            catch (Exception e)
            {
              messageDialog("An error occurred while saving access token: " + e.getMessage(), mtError);
            }
          }
          catch (InterruptedException | ExecutionException e)
          {
            return jsonArray;
          }

          return doHttpRequest(url, requestType, jsonData, mediaType, nextUrl);
        }
      }
    }

    if (syncTaskIsCancelled()) throw new CancelledTaskException();

    return jsonArray;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public SyncTask createNewSyncTask() { return syncTask = new SyncTask()
  {
    @Override public void call() throws CancelledTaskException, HyperDataException
    {
    //---------------------------------------------------------------------------

      Set<String> trashedIDs = new HashSet<>(), nonTrashedIDs = new HashSet<>();

      try
      {
        do
        {
          didMergeDuringSync = false;

// Get list of remotely changed documents ---------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

          Map<String, JsonObj> remoteChangeIDtoObj = new HashMap<>();

          nonTrashedIDs.clear();
          doReadCommand(MendeleyCmd.readDocuments).getObjs().forEach(jObj ->
          {
            String entryKey = jObj.getStrSafe("id");
            remoteChangeIDtoObj.put(entryKey, jObj);
            nonTrashedIDs.add(entryKey);
            changed = true;
          });

// Get list of remotely changed documents in trash ------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

          trashedIDs.clear();
          doReadCommand(MendeleyCmd.readTrash).getObjs().forEach(jObj ->
          {
            String entryKey = jObj.getStrSafe("id");
            remoteChangeIDtoObj.put(entryKey, jObj);
            trashedIDs.add(entryKey);
            changed = true;
          });

// Get list of remotely deleted documents ---------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

          JsonArray remoteDeletions = doReadCommand(MendeleyCmd.readDeletedDocuments);

// Locally delete remotely deleted documents ------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

          remoteDeletions.getObjs().forEach(jObj ->
          {
            changed = true;
            String key = jObj.getStrSafe("id");

            if (keyToAllEntry.containsKey(key))
            {
              // A remote deletion will simply override local changes. Undocument code to change this behavior.

//              MendeleyDocument document = keyToAllEntry.get(key);
//
//              if (document.isSynced())
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

          lastSyncTime = Instant.now();

// If document that was assigned to a work now has unrecognized entry type, unassign it -----------------------------
// ------------------------------------------------------------------------------------------------------------------

          Set<String> dontMergeThese = new HashSet<>();

          remoteChangeIDtoObj.forEach((entryKey, jObj) ->
          {
            MendeleyDocument document = keyToAllEntry.get(entryKey);

            if (document == null)
              return;

            String entryTypeStr = MendeleyDocument.getEntryTypeStrFromSpecifiedJson(jObj);

            if (parseEntryType(entryTypeStr) == etOther)
            {
              dontMergeThese.add(entryKey);

              if (document.linkedToWork())
              {
                int workID = document.getWork().getID();
                document.unassignWork();
                messageDialog("Unassigning work record due to unrecognized entry type: \"" + entryTypeStr + "\"\n\nWork ID: " + workID, mtWarning);
              }
            }
          });

// Build list of locally changed documents and documents to merge; merge local and remote changes -------------------
// ------------------------------------------------------------------------------------------------------------------

          List<MendeleyDocument> localChanges = new ArrayList<>();

          getAllEntries().forEach(entry ->
          {
            if (entry.isSynced() == false)
            {
              localChanges.add(entry);
              String entryKey = entry.getKey();

              if (remoteChangeIDtoObj.containsKey(entryKey) && (dontMergeThese.contains(entryKey) == false))
              {
                doMerge(entry, remoteChangeIDtoObj.get(entryKey));
                remoteChangeIDtoObj.remove(entryKey);
              }
            }
          });

          remoteChangeIDtoObj.forEach((entryKey, jObj) ->
          {
            MendeleyDocument document = keyToAllEntry.get(entryKey);

            if (document == null)
              keyToAllEntry.put(entryKey, new MendeleyDocument(MendeleyWrapper.this, jObj, false));
            else
              document.update(jObj, true, false);
          });

// Create new documents on server and update locally changed documents on server, checking for 412 status -----------
// ------------------------------------------------------------------------------------------------------------------

          Iterator<MendeleyDocument> it = localChanges.iterator();
          while (it.hasNext() && (jsonClient.getStatusCode() != HttpStatus.SC_PRECONDITION_FAILED))
          {
            MendeleyDocument document = it.next();

            if (document.isNewEntry())
            {
              String oldKey = document.getKey();

              JsonArray jsonArray = createDocumentOnServer(document.exportJsonObjForUploadToServer());

              document.update(jsonArray.getObj(0), false, false);

              updateKey(oldKey, document.getKey());
              changed = true;
            }
            else
            {
              JsonArray jsonArray = updateDocumentOnServer(document.exportJsonObjForUploadToServer());
              if (jsonClient.getStatusCode() == HttpStatus.SC_OK)
              {
                document.update(jsonArray.getObj(0), false, false);
                changed = true;
              }
            }
          }

// If 412 status received, start over -------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        } while ((jsonClient.getStatusCode() == HttpStatus.SC_PRECONDITION_FAILED) || didMergeDuringSync);

// Get list of folders from server ----------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        Map<String, JsonObj> remoteFolderIDtoObj = new HashMap<>();
        doReadCommand(MendeleyCmd.readFolders).getObjs().forEach(jObj -> remoteFolderIDtoObj.put(jObj.getStrSafe("id"), jObj));

// Delete local folders not on list ---------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        List.copyOf(keyToColl.keySet()).forEach(collKey ->
        {
          if (remoteFolderIDtoObj.containsKey(collKey) == false)
          {
            // A remote deletion will simply override local changes. Undocument code to change this behavior.

//          MendeleyFolder folder = keyToColl.get(collKey);
//
//          if (folder.isSynced())
//          {
            keyToColl.remove(collKey);
            changed = true;
//          }
//          else
//          {
//            // Perform conflict resolution!
//            noOp();
//          }
          }
        });

// Create new local folders and update existing local folders -------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        remoteFolderIDtoObj.forEach((collKey, jObj) ->
        {
          MendeleyFolder folder = keyToColl.get(collKey);

          if (folder == null)
          {
            keyToColl.put(collKey, new MendeleyFolder(jObj));
            changed = true;
          }
          else
          {
            if (folder.lastModified().isBefore(parseIso8601(jObj.getStr("modified"))))
              changed = true;

            folder.update(jObj, true, false);
          }
        });

// Locally update whether documents are in the trash (using nonTrashedIDs and trashedIDs lists) ---------------------
// ------------------------------------------------------------------------------------------------------------------

        nonTrashedIDs.forEach(entryKey ->
        {
          if (keyToTrashEntry.remove(entryKey) != null)
            changed = true;
        });

        trashedIDs.forEach(entryKey ->
        {
          if (keyToTrashEntry.containsKey(entryKey))
            return;

          keyToTrashEntry.put(entryKey, keyToAllEntry.get(entryKey));
          changed = true;
        });
      }
      catch (HttpResponseException e)
      {
        String msg = "An error occurred while syncing: " + e.getStatusCode() + ' ' + e.getMessage();
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
    }
  }; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doReadCommand(MendeleyCmd command) throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.mendeley.com/", mediaType = "";

    switch (command)
    {
      case readFolders :
        url += "folders";
        mediaType = "application/vnd.mendeley-folder.1+json";
        break;

      case readDocuments :
        url += "documents?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50&view=all";
        mediaType = "application/vnd.mendeley-document.1+json";
        break;

      case readDeletedDocuments :
        url += "documents?deleted_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50";
        mediaType = "application/vnd.mendeley-document.1+json";
        break;

      case readTrash :
        url += "trash?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50&view=all";
        mediaType = "application/vnd.mendeley-document.1+json";
        break;

      case readProfile :
        url += "profiles/me";
        mediaType = "application/vnd.mendeley-profiles.1+json";
        break;
    }

    StringBuilder nextUrl = new StringBuilder();
    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.get, null, mediaType, nextUrl);

    while (nextUrl.length() > 0)
    {
      url = nextUrl.toString();
      assignSB(nextUrl, "");
      doHttpRequest(url, HttpRequestType.get, null, mediaType, nextUrl).getObjs().forEach(jsonArray::add);
    }

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

  private JsonArray createDocumentOnServer(JsonObj jsonObj) throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    String url       = "https://api.mendeley.com/documents",
           mediaType = "application/vnd.mendeley-document.1+json";

    if (jsonObj.getStrSafe("title").isEmpty())
      jsonObj.put("title", " ");

    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.post, jsonObj.toString(), mediaType, null);

    if (jsonClient.getStatusCode() == HttpStatus.SC_CREATED)
      return jsonArray;

    throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getReasonPhrase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray updateDocumentOnServer(JsonObj jsonObj) throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    String url       = "https://api.mendeley.com/documents/" + jsonObj.getStrSafe("id"),
           mediaType = "application/vnd.mendeley-document.1+json";

    if (jsonObj.getStrSafe("title").isEmpty())
      jsonObj.put("title", " ");

    jsonObj.remove("created");
    jsonObj.remove("last_modified");
    jsonObj.remove("profile_id");
    jsonObj.remove("id");
    jsonObj.remove("group_id");
    jsonObj.remove("accessed");

    JsonArray jsonArray = doHttpRequest(url, HttpRequestType.patch, jsonObj.toString(), mediaType, null);

    switch (jsonClient.getStatusCode())
    {
      case HttpStatus.SC_OK :
      case HttpStatus.SC_NOT_MODIFIED :
      case HttpStatus.SC_PRECONDITION_FAILED :

        return jsonArray;
    }

    String errMsg = jsonClient.getReasonPhrase();

    if (jsonArray != null)
    {
      String jsonMsg = nullSwitch(jsonArray.getObj(0), "", jObj -> jObj.getStrSafe("message"));
      if (jsonMsg.isBlank() == false)
        errMsg = jsonMsg;
    }

    throw new HttpResponseException(jsonClient.getStatusCode(), errMsg);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void loadFromDisk(FilePath filePath) throws IOException, ParseException
  {
    JsonObj jMainObj = null;
    clear();

    try (InputStream in = Files.newInputStream(filePath.toPath()))
    {
      jMainObj = parseJsonObj(new InputStreamReader(in, UTF_8));
    }
    catch (FileNotFoundException | NoSuchFileException e) { noOp(); }

    if (jMainObj == null) return;

    String lastSyncTimeStr = db.prefs.get(PREF_KEY_BIB_LAST_SYNC_TIME, "");
    lastSyncTime = lastSyncTimeStr.isBlank() ? Instant.EPOCH : parseIso8601(lastSyncTimeStr);

    loadFromJSON(jMainObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static EnumHashBiMap<EntryType, String> initTypeMap()
  {
    EnumHashBiMap<EntryType, String> map = EnumHashBiMap.create(EntryType.class);

    map.put(etJournalArticle, "journal");
    map.put(etBook, "book");
    map.put(etOther, "generic");
    map.put(etBookChapter, "book_section");
    map.put(etConferenceProceedings, "conference_proceedings");
    map.put(etWorkingPaper, "working_paper");
    map.put(etReport, "report");
    map.put(etWebPage, "web_page");
    map.put(etThesis, "thesis");
    map.put(etMagazineArticle, "magazine_article");
    map.put(etStatute, "statute");
    map.put(etPatent, "patent");
    map.put(etNewspaperArticle, "newspaper_article");
    map.put(etSoftware, "computer_program");
    map.put(etHearing, "hearing");
    map.put(etTVBroadcast, "television_broadcast");
    map.put(etEncyclopediaArticle, "encyclopedia_article");
    map.put(etCase, "case");
    map.put(etFilm, "film");
    map.put(etBill, "bill");

    return map;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getUserNameFromServer(Consumer<String> successHndlr, Consumer<Throwable> failHndlr)
  {
    StringBuffer emailAddress = new StringBuffer();

//---------------------------------------------------------------------------

    HyperTask hyperTask = new HyperTask("GetMendeleyUserName")
    {
      @Override protected void call() throws HyperDataException, CancelledTaskException
      {
        try
        {
          JsonObj profile = doReadCommand(MendeleyCmd.readProfile).getObj(0);

          assignSB(emailAddress, profile.getStrSafe("email"));
        }
        catch (HttpResponseException e)
        {
          String msg = "An error occurred while retrieving username from server: " + e.getStatusCode() + ' ' + e.getMessage();
          throw new HyperDataException(msg, e);
        }
        catch (UnknownHostException e)
        {
          String msg = "Unable to connect to host: " + e.getMessage();
          throw new HyperDataException(msg, e);
        }
        catch (UnsupportedOperationException | IOException | ParseException e)
        {
          String msg = "An error occurred while retrieving username from server: " + e.getMessage();
          throw new HyperDataException(msg, e);
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
          failHndlr.accept(hyperTask.getException());
          return;
        }

        successHndlr.accept(emailAddress.toString());
      }
    });

    hyperTask.startWithNewThreadAsDaemon();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
