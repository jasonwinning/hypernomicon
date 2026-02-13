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

package org.hypernomicon.bib.mendeley;

import static org.hypernomicon.App.*;
import static org.hypernomicon.bib.data.EntryType.*;
import static org.hypernomicon.bib.mendeley.MendeleyEntity.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.http.HttpStatusCode.*;
import static org.hypernomicon.util.json.JsonObj.*;

import java.io.*;
import java.net.SocketException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.time.Instant;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.function.Consumer;

import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpRequest.BodyPublishers;

import org.json.simple.parser.ParseException;
import org.jspecify.annotations.NonNull;

import com.github.scribejava.core.model.OAuth2AccessToken;
import com.github.scribejava.core.oauth.OAuth20Service;
import com.google.common.collect.EnumHashBiMap;

import javafx.concurrent.Worker.State;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.bib.LibraryWrapper;
import org.hypernomicon.bib.data.EntryType;
import org.hypernomicon.bib.mendeley.auth.MendeleyAuthKeys;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.http.*;
import org.hypernomicon.util.http.AsyncHttpClient.HttpRequestType;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

//---------------------------------------------------------------------------

public final class MendeleyWrapper extends LibraryWrapper<MendeleyDocument, MendeleyFolder>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private String userID = "", userName = "";
  private MendeleyAuthKeys authKeys;
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

  private MendeleyWrapper(MendeleyAuthKeys authKeys, String userID, String userName) throws HyperDataException
  {
    if (strNotNullOrBlank(userID) && BibAuthKeys.isEmpty(authKeys))
      authKeys = MendeleyAuthKeys.loadFromKeyring(userID);

    enableSyncOnThisComputer(authKeys, userID, userName, false);
  }

//---------------------------------------------------------------------------

  @Override public LibraryType type()          { return LibraryType.ltMendeley; }
  @Override public String entryFileNode()      { return "documents"; }
  @Override public String collectionFileNode() { return "folders"; }
  @Override public String getUserName()        { return userName; }
  @Override public String getUserID()          { return userID; }

  @Override public EnumHashBiMap<EntryType, String> getEntryTypeMap() { return entryTypeMap; }

  @Override protected MendeleyAuthKeys getAuthKeys() { return authKeys; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @NonNull
  public static MendeleyWrapper create(MendeleyAuthKeys authKeys, String userID, String userName, FilePath filePath) throws IOException, ParseException, HyperDataException
  {
    MendeleyWrapper wrapper = new MendeleyWrapper(authKeys, userID, userName);

    wrapper.loadAllFromJsonFile(filePath);

    if (strNotNullOrBlank(userID))
      return wrapper;

    String docUserID = findFirst(wrapper.getAllEntries(), doc -> strNotNullOrBlank(doc.getUserID()), MendeleyDocument::getUserID);

    if (docUserID != null)
    {
      wrapper.userID = docUserID;

      // We may not have been able to save before because the userID was blank
      BibAuthKeys.saveToKeyringIfUnsaved(authKeys, wrapper.userID);

      return wrapper;
    }

    wrapper.getProfileInfoFromServer(_userName ->
    {
      // We may not have been able to save before because the userID was blank
      BibAuthKeys.saveToKeyringIfUnsaved(authKeys, wrapper.userID);

    }, ex -> noOp());

    return wrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MendeleyWrapper createForTesting(String userID) throws HyperDataException
  {
    assertThatThisIsUnitTestThread();
    return new MendeleyWrapper(null, userID, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum MendeleyHeader
  {
    Mendeley_Count("Mendeley-Count"),
    Link("Link");

    private final String name;

    MendeleyHeader(String name) { this.name = name; }

    @Override public String toString() { return name; }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static Instant getSyncInstantFromJsonStr(String jsonStr)
  {
    return strNullOrBlank(jsonStr) ? Instant.EPOCH : parseIso8601(jsonStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record PagedResponse(JsonArray data, String nextUrl) {}

  private PagedResponse doHttpRequest(String url, HttpRequestType requestType, String jsonData, String mediaType) throws IOException, UnsupportedOperationException, ParseException, CancelledTaskException
  {
    return doHttpRequest(url, requestType, jsonData, mediaType, null);
  }

  private PagedResponse doHttpRequest(String url, HttpRequestType requestType, String jsonData, String mediaType, Instant ifUnmodifiedSince) throws UnsupportedOperationException, ParseException, CancelledTaskException, IOException
  {
    HyperTask.throwExceptionIfCancelled(syncTask);

    HttpRequest.Builder rb = AsyncHttpClient.requestBuilder(url)
      .header(HttpHeader.Authorization.toString(), "Bearer " + MendeleyAuthKeys.getAccessToken(authKeys));

    HttpRequest httpRequest = switch (requestType)
    {
      case get   -> rb.header(HttpHeader.Accept.toString(), mediaType)
                      .GET()
                      .build();

      case patch -> rb.header(HttpHeader.Content_Type.toString(), mediaType)
                      .header(HttpHeader.If_Unmodified_Since.toString(), dateTimeToHttpDate(ifUnmodifiedSince))
                      .method("PATCH", BodyPublishers.ofString(jsonData, StandardCharsets.UTF_8))
                      .build();

      case post ->  rb.header(HttpHeader.Content_Type.toString(), mediaType)
                      .header(HttpHeader.Accept.toString(), mediaType)
                      .POST(BodyPublishers.ofString(jsonData, StandardCharsets.UTF_8))
                      .build();

      default -> null;
    };

    if (httpRequest == null) return null;

    JsonArray jsonArray;

    try
    {
      jsonArray = jsonClient.requestArrayInThisThread(httpRequest);
    }
    catch (SocketException e)
    {
      if ((syncTask != null) && syncTask.isCancelled())
        throw new CancelledTaskException();

      throw e;
    }

    String[] nextUrl = { null };
    HttpHeaders headers = jsonClient.getHeaders();

    if (headers != null)
    {
      headers.firstValue(MendeleyHeader.Link.toString()).ifPresent(headerValue ->
      {
        if (headerValue.endsWith("rel=\"next\""))
        {
          int startNdx = headerValue.indexOf('<'),
              endNdx   = headerValue.indexOf('>');

          if ((startNdx >= 0) && (endNdx > startNdx))
            nextUrl[0] = headerValue.substring(startNdx + 1, endNdx);
        }
      });
    }

    HyperTask.throwExceptionIfCancelled(syncTask);

    if ((jsonClient.getStatusCode() == SC_BAD_REQUEST)  ||
        (jsonClient.getStatusCode() == SC_UNAUTHORIZED) ||
        (jsonClient.getStatusCode() == SC_FORBIDDEN))
    {
      if ((jsonArray != null) && (jsonArray.size() > 0))
      {
        JsonObj jsonObj = jsonArray.getObj(0);
        if ("oauth/TOKEN_EXPIRED".equals(jsonObj.getStrSafe("errorId")))
        {
          try (OAuth20Service service = MendeleyOAuthApi.service())
          {
            OAuth2AccessToken token = service.refreshAccessToken(MendeleyAuthKeys.getRefreshToken(authKeys));

            try
            {
              enableSyncOnThisComputer(MendeleyAuthKeys.createFromOauthToken(token), "", "", false);
            }
            catch (Exception e)
            {
              errorPopup("An error occurred while saving access token: " + getThrowableMessage(e));
            }
          }
          catch (InterruptedException | ExecutionException e)
          {
            return new PagedResponse(jsonArray, nextUrl[0]);
          }

          return doHttpRequest(url, requestType, jsonData, mediaType, ifUnmodifiedSince);
        }

        if (jsonObj.getStrSafe("errorId").startsWith("oauth"))
          throw newAccessDeniedException();
      }
    }

    HyperTask.throwExceptionIfCancelled(syncTask);

    return new PagedResponse(jsonArray, nextUrl[0]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public SyncTask createNewSyncTask(String message) throws HyperDataException
  {
    if (strNullOrBlank(userID))
      getProfileInfoFromServer();

    if (strNotNullOrBlank(userID) && BibAuthKeys.isEmpty(authKeys))
    {
      authKeys = MendeleyAuthKeys.loadFromKeyring(userID);

      enableSyncOnThisComputer(authKeys, userID, userName, false);
    }
    else
      BibAuthKeys.saveToKeyringIfUnsaved(authKeys, userID);

    return createNewSyncTaskInt(message);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private SyncTask createNewSyncTaskInt(String message) { return syncTask = new SyncTask(message) { @Override public void call() throws CancelledTaskException, HyperDataException
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
        });

// Get list of remotely changed documents in trash ------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        trashedIDs.clear();
        doReadCommand(MendeleyCmd.readTrash).getObjs().forEach(jObj ->
        {
          String entryKey = jObj.getStrSafe("id");
          remoteChangeIDtoObj.put(entryKey, jObj);
          trashedIDs.add(entryKey);
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

//            MendeleyDocument document = keyToAllEntry.get(key);
//
//            if (document.isSynced())
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

        lastSyncTime = Instant.now();

// Ignore documents with old modified date --------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

        remoteChangeIDtoObj.entrySet().removeIf(entry ->
        {
          MendeleyDocument document = keyToAllEntry.get(entry.getKey());

          return (document != null) && (document.lastModifiedOnServer().isBefore(getSyncInstantFromJsonStr(entry.getValue().getStr(Document_Last_Modified_JSON_Key))) == false);
        });

        if (remoteChangeIDtoObj.isEmpty() == false)
          changed = true;

        if (app.debugging)
          System.out.println("Number of Mendeley documents modified on server: " + (remoteChangeIDtoObj.size() + remoteDeletions.size()));

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
              warningPopup("Unassigning work record due to unrecognized entry type: \"" + entryTypeStr + "\"\n\nWork ID: " + workID);
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

        if (app.debugging)
          System.out.println("Number of Mendeley documents modified locally: " + localChanges.size());

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
        while (it.hasNext() && (jsonClient.getStatusCode() != SC_PRECONDITION_FAILED))
        {
          MendeleyDocument document = it.next();

          if (document.isNewEntry())
          {
            String oldKey = document.getKey();

            JsonArray jsonArray = createDocumentOnServer(document);

            document.update(jsonArray.getObj(0), false, false);

            updateKey(oldKey, document.getKey());
            changed = true;
          }
          else
          {
            JsonArray jsonArray = updateDocumentOnServer(document);
            if (jsonClient.getStatusCode() == SC_OK)
            {
              document.update(jsonArray.getObj(0), false, false);
              changed = true;
            }
            else if (jsonClient.getStatusCode() == SC_NOT_MODIFIED)
            {
              document.mergeWithBackupCopy();
              changed = true;
            }
          }
        }

// If 412 status received, start over -------------------------------------------------------------------------------
// ------------------------------------------------------------------------------------------------------------------

      } while ((jsonClient.getStatusCode() == SC_PRECONDITION_FAILED) || didMergeDuringSync);

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

//        MendeleyFolder folder = keyToColl.get(collKey);
//
//        if (folder.isSynced())
//        {
          keyToColl.remove(collKey);
          changed = true;
//        }
//        else
//        {
//          // Perform conflict resolution!
//          noOp();
//        }
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
          if (folder.lastModifiedOnServer().isBefore(getSyncInstantFromJsonStr(jObj.getStr(Folder_Last_Modified_JSON_Key))))
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
    catch (UnsupportedOperationException | IOException | ParseException e)
    {
      throw new HyperDataException(syncErrorMessage(e), e);
    }
  } }; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray doReadCommand(MendeleyCmd command) throws CancelledTaskException, UnsupportedOperationException, IOException, ParseException
  {
    String url = "https://api.mendeley.com/";

    String mediaType = switch (command)
    {
      case readFolders ->
      {
        url += "folders";
        yield "application/vnd.mendeley-folder.1+json";
      }

      case readDocuments ->
      {
        url += "documents?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50&view=all";
        yield "application/vnd.mendeley-document.1+json";
      }

      case readDeletedDocuments ->
      {
        url += "documents?deleted_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50";
        yield "application/vnd.mendeley-document.1+json";
      }

      case readTrash ->
      {
        url += "trash?modified_since=" + dateTimeToIso8601(lastSyncTime) + "&limit=50&view=all";
        yield "application/vnd.mendeley-document.1+json";
      }

      case readProfile ->
      {
        url += "profiles/me";
        yield "application/vnd.mendeley-profiles.1+json";
      }
    };

    PagedResponse response = doHttpRequest(url, HttpRequestType.get, null, mediaType);
    JsonArray jsonArray = response.data();

    while (response.nextUrl() != null)
    {
      response = doHttpRequest(response.nextUrl(), HttpRequestType.get, null, mediaType);
      response.data().getObjs().forEach(jsonArray::add);
    }

    return switch (jsonClient.getStatusCode())
    {
      case SC_OK,
           SC_NOT_MODIFIED,
           SC_PRECONDITION_FAILED ->

        jsonArray;

      default ->

        throwResponseException(jsonArray, "");
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray createDocumentOnServer(MendeleyDocument document) throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    JsonArray jsonArray = doHttpRequest("https://api.mendeley.com/documents",                // URL
                                        HttpRequestType.post,                                // request type
                                        document.exportStandaloneJsonObj(false).toString(),  // JSON data
                                        "application/vnd.mendeley-document.1+json")          // media type
                                        .data();

    return switch (jsonClient.getStatusCode())
    {
      case SC_CREATED ->

        jsonArray;

      default ->

        throwResponseException(jsonArray, "");
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray updateDocumentOnServer(MendeleyDocument document) throws UnsupportedOperationException, IOException, ParseException, CancelledTaskException
  {
    JsonArray jsonArray = doHttpRequest("https://api.mendeley.com/documents/" + document.getKey(),  // URL
                                        HttpRequestType.patch,                                      // request type
                                        document.exportStandaloneJsonObj(true).toString(),          // JSON data
                                        "application/vnd.mendeley-document.1+json",                 // media type
                                        document.lastModifiedOnServer())                            // if modified since
                                        .data();

    return switch (jsonClient.getStatusCode())
    {
      case SC_OK,
           SC_NOT_MODIFIED,
           SC_PRECONDITION_FAILED ->

        jsonArray;

      default ->

        throwResponseException(jsonArray, document.getKey());
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private JsonArray throwResponseException(JsonArray jsonArray, String documentID) throws HttpResponseException
  {
    StringBuilder serverMsg = new StringBuilder();

    new CondJsonArray(jsonArray).condObj(0).condStr("message", jsonMsg -> assignSB(serverMsg, jsonMsg));

    if (strNotNullOrBlank(documentID))
    {
      if (strNotNullOrEmpty(serverMsg))
        serverMsg.append("\n\n");

      serverMsg.append("Entry ID: ").append(documentID);
    }

    throw new HttpResponseException(jsonClient.getStatusCode(), jsonClient.getLastUrl(), serverMsg.isEmpty() ? null : serverMsg.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadAllFromJsonFile(FilePath filePath) throws IOException, ParseException
  {
    JsonObj jMainObj = null;

    try (InputStream in = Files.newInputStream(filePath.toPath()))
    {
      jMainObj = parseJsonObj(new InputStreamReader(in, XML_FILES_CHARSET));
    }
    catch (FileNotFoundException | NoSuchFileException e)
    {
      noOp();  // This happens when first linking to Mendeley
    }

    if (jMainObj == null) return;

    lastSyncTime = getSyncInstantFromJsonStr(db.prefs.get(PrefKey.BIB_LAST_SYNC_TIME, ""));

    loadFromJSON(jMainObj);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void savePrefs()
  {
    db.prefs.put(PrefKey.BIB_LAST_SYNC_TIME, dateTimeToIso8601(lastSyncTime));
    db.prefs.put(PrefKey.BIB_USER_ID, userID);
    db.prefs.put(PrefKey.BIB_USER_NAME, userName);
    db.prefs.put(PrefKey.BIB_LIBRARY_TYPE, type().descriptor);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static MendeleyWrapper getProfileInfoFromServer(MendeleyAuthKeys authKeys) throws HyperDataException
  {
    MendeleyWrapper wrapper = new MendeleyWrapper(authKeys, "", "");

    wrapper.getProfileInfoFromServer();

    return wrapper;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void getProfileInfoFromServer() throws HyperDataException
  {
    try
    {
      JsonObj profile = doReadCommand(MendeleyCmd.readProfile).getObj(0);

      userName = profile.getStrSafe("email");

      if (strNullOrBlank(userID))
        userID = profile.getStrSafe("id");

      if (userName.isBlank() || userID.isBlank())
        throw new HyperDataException("Unable to retrieve profile information from server");
    }
    catch (UnsupportedOperationException | IOException | ParseException e)
    {
      throw new HyperDataException("An error occurred while retrieving profile information from server: " + getThrowableMessage(e), e);
    }
    catch (CancelledTaskException e)
    {
      throw newAssertionError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void getProfileInfoFromServer(Consumer<String> successHndlr, Consumer<Throwable> failHndlr)
  {

//---------------------------------------------------------------------------

    HyperTask hyperTask = new HyperTask("GetMendeleyProfileInfo", false)
    {
      @Override protected void call() throws HyperDataException
      {
        getProfileInfoFromServer();
      }

    }.setSilent(true);

//---------------------------------------------------------------------------

    hyperTask.addDoneHandler(state ->
    {
      if ((state == State.FAILED) || (state == State.CANCELLED))
        failHndlr.accept(hyperTask.getException());
      else
        successHndlr.accept(userName);
    });

    hyperTask.startWithNewThreadAsDaemon();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void enableSyncOnThisComputer(BibAuthKeys authKeys, String userID, String userName, boolean reestablishing) throws HyperDataException
  {
    // If the access token is being refreshed, userID and userName will be passed in blank, and reestablishing will be false.
    // If the library is being loaded from persistent storage (whether authKeys came from DB settings or keyring), userID and
    //      userName may be blank, and reestablishing will be false.
    // If re-establishing access permission, userID and userName will not be blank, and reestablishing will be true.

    if (strNotNullOrBlank(userID))
    {
      // If reestablishing is false at this point, the library is being loaded from persistent storage.
      // In that case, this.userID is always blank, and the authKeys might have come from DB settings or the keyring.

      BibAuthKeys.saveToKeyringIfUnsaved(authKeys, userID);  // Save to the keyring even if these are auth keys for the wrong user;
                                                             // the passed-in authKeys must match the passed-in user ID at least
      if (this.userID.isBlank())
      {
        if (reestablishing && getAllEntries().isEmpty())
        {
          // The user is trying to re-establish access without the userID previously being saved and with no entries in the library.
          // It is not a good solution to try to get the existing userID from the server because we don't know the reason why the
          // user is trying to re-establish access (existing tokens may have been deleted from the account, etc.).

          // In this situation, there is insufficient local information to confirm whether the account corresponding
          // to the new access token is the same as the account this database was previously linked to. If not, then
          // if we accept the new access token, Mendeley folder information could get synced to the wrong account.

          throw new HyperDataException("Unable to re-establish access. You may need to unlink and re-link your Mendeley account.");
        }

        // If reestablishing is false, and this.userID is blank, then we loaded authKeys from DB settings.
        // Might as well do the check in that case too.

        for (MendeleyDocument document : getAllEntries())
        {
          String docUserID = document.getUserID();

          if (strNotNullOrBlank(docUserID) && (docUserID.equals(userID) == false))
            throw new HyperDataException("User ID for local entries is " + docUserID + ", but user ID from server is " + userID);
        }
      }
      else if (this.userID.equals(userID) == false)
        throw new HyperDataException("User ID for local entries is " + this.userID + ", but user ID from server is " + userID);

      this.userID = userID;
      this.userName = userName;
    }

    this.authKeys = (MendeleyAuthKeys) authKeys;

    BibAuthKeys.saveToKeyringIfUnsaved(this.authKeys, this.userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Unfortunately this requires AuthKeys to be loaded because for some
  // unfathomable reason, Mendeley (unlike Zotero) requires an access
  // token just to get the list of valid document types.

  public String checkDocumentTypesFromServer()
  {
    try
    {
      JsonArray jsonArray = doHttpRequest("https://api.mendeley.com/document_types",
                                          HttpRequestType.get,
                                          null,
                                          "application/vnd.mendeley-document-type.1+json")
                                          .data();

      EnumSet<EntryType> unusedTypes = EnumSet.copyOf(entryTypeMap.keySet());

      for (JsonObj jObj : jsonArray.getObjs())
      {
        String typeName = jObj.getStr("name");

        if (entryTypeMap.containsValue(typeName))
          unusedTypes.remove(entryTypeMap.inverse().get(typeName));
        else
          return "Unrecognized Mendeley document type: " + typeName;
      }

      if (unusedTypes.isEmpty() == false)
        return "One or more locally recognized Mendeley document type(s) not listed by server: " + unusedTypes;
    }
    catch (UnsupportedOperationException | IOException | ParseException | CancelledTaskException e)
    {
      return "Exception: " + getThrowableMessage(e);
    }

    return "";
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

}
