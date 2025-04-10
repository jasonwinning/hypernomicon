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

package org.hypernomicon.model;

import static java.nio.charset.StandardCharsets.*;
import static java.util.Collections.*;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.Tag.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.util.EnumMap;
import java.util.List;
import java.util.Map.Entry;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.io.FileUtils;

import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.HyperTask;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.HyperFavorites;
import org.hypernomicon.view.mainText.MainTextCtrlr;

import com.google.common.collect.SetMultimap;

import javafx.concurrent.Worker.State;

//---------------------------------------------------------------------------

public final class HyperDB extends AbstractHyperDB
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static AbstractHyperDB db;

  private final FolderTreeWatcher folderTreeWatcher;

  private FilePath lockFilePath = null;

//---------------------------------------------------------------------------

  private HyperDB(FolderTreeWatcher folderTreeWatcher)
  {
    this.folderTreeWatcher = folderTreeWatcher;

    synchronized(HyperDB.class)
    {
      if (db != null)
        throw new UnsupportedOperationException();

      db = this;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void create(FolderTreeWatcher folderTreeWatcher)
  {
    noOp(new HyperDB(folderTreeWatcher));
  }

//---------------------------------------------------------------------------

  @Override protected FolderTreeWatcher getFolderTreeWatcher() { return folderTreeWatcher; }

  @Override protected void updateRunningInstancesFile(FilePath newRootFilePath) { InterProcClient.updateRunningInstancesFile(newRootFilePath); }

  public static String getTypeName(RecordType type) { return nullSwitch(getTag(type), type == hdtNone ? "All" : "Unknown", tag -> tag.header); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean loadFromXMLFiles(List<FilePath> xmlFileList, boolean creatingNew, EnumMap<RecordType, VersionNumber> recordTypeToDataVersion, SetMultimap<Integer, Integer> workIDtoInvIDs)
  {
    return new HyperTask("LoadDatabase", "Loading database from folder " + rootFilePath + "...") { @Override protected void call() throws HyperDataException, CancelledTaskException
    {
      totalCount = 0;

      try
      {
        for (FilePath filePath : xmlFileList)
        {
          if (filePath.exists() == false)
            throw new HyperDataException("Unable to load database. Reason: File does not exist: " + filePath);

          totalCount += filePath.size();
        }
      }
      catch (IOException e) { throw new HyperDataException(e); }

      for (FilePath filePath : xmlFileList) loadFromXMLFile(creatingNew, filePath, recordTypeToDataVersion, workIDtoInvIDs, this);

    }}.runWithProgressDialog() == State.SUCCEEDED;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromXMLFile(boolean creatingNew, FilePath filePath, EnumMap<RecordType, VersionNumber> recordTypeToDataVersion,
                               SetMultimap<Integer, Integer> workIDtoInvIDs, HyperTask task) throws HyperDataException, CancelledTaskException
  {
    String fileDescription = filePath.toString(),
           fileName        = filePath.getNameOnly().toString();

    try
    {
      long fileSize = filePath.size();

      try (InputStream is = Files.newInputStream(filePath.toPath()))
      {
        loadFromXMLStream(creatingNew, is, recordTypeToDataVersion, workIDtoInvIDs, fileDescription, fileName, fileSize, task);
      }
    }
    catch (IOException e)
    {
      throw new HyperDataException(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public FilePath getRequestMessageFilePath(boolean useAppPrefs)
  {
    return useAppPrefs ?
      new FilePath(app.prefs.get(PrefKey.SOURCE_PATH, "")).resolve(REQUEST_MSG_FILE_NAME)
    :
      getRootPath(REQUEST_MSG_FILE_NAME);
  }

  @Override public FilePath getResponseMessageFilePath(boolean useAppPrefs)
  {
    return useAppPrefs ?
      new FilePath(app.prefs.get(PrefKey.SOURCE_PATH, "")).resolve(RESPONSE_MSG_FILE_NAME)
    :
      getRootPath(RESPONSE_MSG_FILE_NAME);
  }

  @Override public FilePath getLockFilePath(boolean useAppPrefs)
  {
    return useAppPrefs ?
      new FilePath(app.prefs.get(PrefKey.SOURCE_PATH, "")).resolve(LOCK_FILE_NAME)
    :
      getRootPath(LOCK_FILE_NAME);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum HDB_MessageType
  {
    hmtNone,
    hmtEchoRequest,
    hmtEchoReply,
    hmtUnlockRequest,
    hmtUnlockComplete
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getLockOwner()
  {
    FilePath filePath = getLockFilePath(true);

    if (filePath.exists())
    {
      List<String> s;

      try { s = FileUtils.readLines(filePath.toFile(), UTF_8); }
      catch (IOException e) { return "[Unknown]"; }

      if (collEmpty(s) == false)
        return s.get(0);
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getOtherLockOwner()
  {
    String owner = getLockOwner();

    if ((owner != null) && (getComputerName().equals(owner) == false))
      return owner;

    getLockFilePath(true).deletePromptOnFail(true);

    getRequestMessageFilePath (true).deletePromptOnFail(true);
    getResponseMessageFilePath(true).deletePromptOnFail(true);

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void lock() throws IOException
  {
    lockFilePath = getLockFilePath(true);

    FileUtils.writeLines(lockFilePath.toFile(), singletonList(getComputerName()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void unlock()
  {
    if (FilePath.isEmpty(lockFilePath)) return;

    lockFilePath.deletePromptOnFail(true);
    lockFilePath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void loadMainTextTemplates()
  {
    mainTextTemplates.clear();

    FilePath folderPath = xmlPath().resolve(DESC_TEMPLATE_FOLDER_NAME);

    if (folderPath.exists() == false) return;

    for (RecordType recordType : streamToIterable(MainTextCtrlr.displayedTypesStream()))
    {
      FilePath filePath = mainTextTemplateFilePath(recordType);
      if (filePath.exists() == false) continue;

      List<String> s;

      try { s = FileUtils.readLines(filePath.toFile(), UTF_8); }
      catch (IOException e)
      {
        errorPopup("An error occurred while trying to read description template files: " + getThrowableMessage(e));
        return;
      }

      mainTextTemplates.put(recordType, strListToStr(s, true));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void updateMainTextTemplate(RecordType recordType, String html) throws IOException
  {
    FilePath folderPath = xmlPath().resolve(DESC_TEMPLATE_FOLDER_NAME);

    if (folderPath.exists() == false)
      folderPath.createDirectory();

    FileUtils.writeLines(mainTextTemplateFilePath(recordType).toFile(), convertMultiLineStrToStrList(html, false));
    mainTextTemplates.put(recordType, html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public String getMainTextTemplate(RecordType recordType)
  {
    return mainTextTemplates.get(recordType);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath mainTextTemplateFilePath(RecordType recordType)
  {
    String fileName = recordType == hdtDebate ? "Debate" : FilePath.removeInvalidFileNameChars(getTag(recordType).header);

    return xmlPath().resolve(DESC_TEMPLATE_FOLDER_NAME).resolve(fileName + ".html");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void fileNoLongerInUse(FilePath filePath)
  {
    if (isDeletionInProgress() == false)
    {
      if (confirmDialog("No more records will be assigned to the file: \"" + filePath + "\". Should the file be permanently deleted?", false))
      {
        filePath.deletePromptOnFail(false);
        unmapFilePath(filePath);
      }

      return;
    }

    if (deleteFileAnswer == mrNone)
    {
      switch (seriesConfirmDialog("No more records will be assigned to the file: \"" + filePath + "\". Should the file be permanently deleted?"))
      {
        case mrYes      : break;
        case mrNoToAll  : deleteFileAnswer = mrNoToAll;  break;
        case mrYesToAll : deleteFileAnswer = mrYesToAll; break;
        default         : return;
      }
    }

    if (deleteFileAnswer == mrNoToAll) return;

    if (folderTreeWatcher.isDisabled() == false)
    {
      folderTreeWatcher.stop();
      folderTreeWatcher.disable();
    }

    filePath.deletePromptOnFail(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void errorMessage  (String msg)           { errorPopup  (msg); }
  @Override protected void warningMessage(String msg)           { warningPopup(msg); }

  @Override protected void loadSettings(boolean creatingNew, HyperFavorites favorites) throws HyperDataException
  {
    try (InputStream is = Files.newInputStream(xmlPath(SETTINGS_FILE_NAME).toPath()))
    {
      loadSettingsFromStream(is, creatingNew, favorites);
    }
    catch (IOException e)
    {
      throw new HyperDataException("An error occurred while attempting to read database settings: " + getThrowableMessage(e), e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean bringAllRecordsOnline()
  {
    return new HyperTask("BringDatabaseOnline", "Starting database session...", totalRecordCount()) { @Override protected void call() throws HyperDataException, CancelledTaskException
    {
      bringAllDatasetsOnline(this);

    }}.runWithProgressDialog() == State.SUCCEEDED;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void saveSourcePathToSystemSettings(String newPathStr)
  {
    app.prefs.put(PrefKey.SOURCE_PATH, newPathStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public FilePath extPath()
  {
    String path = app.prefs.get(PrefKey.EXT_FILES_1, "");
    return path.isBlank() ? null : new FilePath(path);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean checkChecksums()
  {
    for (Entry<String, String> entry : xmlChecksums.entrySet())
    {
      String hex = "";

      try (InputStream is = Files.newInputStream(xmlPath(entry.getKey()).toPath()))
      {
        hex = DigestUtils.md5Hex(is);
      }
      catch (IOException e) { noOp(); }

      if (hex.equalsIgnoreCase(entry.getValue()) == false)
      {
        if (confirmDialog("Changes have been made to the XML files from outside of this instance of " + appTitle + ". Overwrite these changes?", false))
          break;

        return false;
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override MentionsIndex createMentionsIndex(List<Runnable> completeHandlers)
  {
    return new MentionsIndex(completeHandlers, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void checkWhetherFoldersExist()
  {
    getRootFolder().checkExists();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
