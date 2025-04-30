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

import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.EnumMap;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.hypernomicon.App;
import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.bib.auth.BibAuthKeys;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.RecordType;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.HyperFavorites;

import com.google.common.collect.SetMultimap;

//---------------------------------------------------------------------------

/**
 * TestHyperDB is a specialized implementation of AbstractHyperDB used primarily for unit testing
 * and internal Hypernomicon database verification. It initializes from a template database
 * (blank_db.zip) and operates entirely in-memory, ensuring no modifications to persistent storage.
 * After calling TestHyperDB.instance(), the database is opened and online.
 * This class is designed to be non-interactive, making it ideal for automated testing scenarios.
 * <p>
 * Usage Example:
 * <pre>
 * {@code
 * TestHyperDB db = TestHyperDB.instance();
 * // Perform database operations
 * db.}{@link #closeAndOpen()};
 * </pre>
 * </p>
 * @see #closeAndOpen()
 */
public final class TestHyperDB extends AbstractHyperDB
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private byte[] settingsFileBuffer;

//---------------------------------------------------------------------------

  private TestHyperDB()
  {
    synchronized(HyperDB.class)
    {
      if (HyperDB.db != null)
        throw new UnsupportedOperationException();

      HyperDB.db = this;
    }
  }

//---------------------------------------------------------------------------

  public static TestHyperDB instance()
  {
    if (HyperDB.db != null)
    {
      if ((HyperDB.db instanceof TestHyperDB) == false)
        throw new AssertionError("HyperDB already exists");

      return (TestHyperDB) HyperDB.db;
    }

    TestHyperDB db = new TestHyperDB();

    db.open();

    return db;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean checkChecksums() { return true; }
  @Override public void fileNoLongerInUse(FilePath filePath) { }
  @Override public FilePath extPath() { return null; }
  @Override protected void lock() { }
  @Override protected void unlock() { }
  @Override protected void updateRunningInstancesFile(FilePath newRootFilePath) { }
  @Override protected void saveSourcePathToSystemSettings(String newPathStr) { }
  @Override public FilePath getRequestMessageFilePath(boolean useAppPrefs) { return new FilePath(""); }
  @Override public FilePath getResponseMessageFilePath(boolean useAppPrefs) { return new FilePath(""); }
  @Override public FilePath getLockFilePath(boolean useAppPrefs) { return new FilePath(""); }
  @Override public String getLockOwner() { return null; }
  @Override public String getOtherLockOwner() { return null; }
  @Override protected void checkWhetherFoldersExist() { }
  @Override public void updateMainTextTemplate(RecordType recordType, String html) { }
  @Override public String getMainTextTemplate(RecordType recordType) { return null; }
  @Override protected void loadMainTextTemplates() { }
  @Override protected FolderTreeWatcher getFolderTreeWatcher() { return null; }

  @Override MentionsIndex createMentionsIndex(List<Runnable> completeHandlers) { return new MentionsIndex(completeHandlers, false); }

  @Override protected void warningMessage(String msg) { System.out.println("Warning: " + msg); }
  @Override protected void errorMessage  (String msg) { System.out.println("Error: "   + msg); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean saveAllToPersistentStorage(HyperFavorites favorites)
  {
    throw new UnsupportedOperationException();
  }

  @Override public void linkBibLibrary(LibraryType libType, BibAuthKeys authKeys, String userID, String userName)
  {
    throw new UnsupportedOperationException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean loadFromXMLFiles(List<FilePath> xmlFileList, boolean creatingNew, EnumMap<RecordType, VersionNumber> recordTypeToDataVersion, SetMultimap<Integer, Integer> workIDtoInvIDs)
  {
    FilePathSet xmlFilePathSet = new FilePathSet(xmlFileList);
    FilePath settingsFilePath = xmlPath(SETTINGS_FILE_NAME);

    try (ZipInputStream zis = new ZipInputStream(App.class.getResourceAsStream(BLANK_DB_RESOURCE_NAME)))
    {
      ZipEntry entry;

      while ((entry = zis.getNextEntry()) != null)
      {
        if (entry.isDirectory() == false)
        {
          FilePath filePath = rootFilePath.resolve(new FilePath(entry.getName()));

          if (xmlFilePathSet.contains(filePath))
            loadFromXMLStream(creatingNew, new ByteArrayInputStream(zis.readAllBytes()), recordTypeToDataVersion, workIDtoInvIDs, entry.getName(), filePath.getNameOnly().toString(), entry.getSize(), null);
          else if (settingsFilePath.equals(filePath))
            settingsFileBuffer = zis.readAllBytes();
        }
      }
    }
    catch (CancelledTaskException e)
    {
      throw new AssertionError(e);
    }
    catch (IOException | HyperDataException e)
    {
      errorMessage("Unable to load database. Reason: " + getThrowableMessage(e));
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void loadSettings(boolean creatingNew, HyperFavorites favorites) throws HyperDataException
  {
    if (settingsFileBuffer == null)
    {
      throw new HyperDataException("Unable to read database settings: " + SETTINGS_FILE_NAME + " not found.");
    }

    loadSettingsFromStream(new ByteArrayInputStream(settingsFileBuffer), creatingNew, favorites);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean bringAllRecordsOnline()
  {
    try
    {
      bringAllDatasetsOnline(null);
    }
    catch (CancelledTaskException e)
    {
      throw new AssertionError(e);
    }
    catch (HyperDataException e)
    {
      errorMessage(e);
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void open()
  {
    if (isLoaded())
      throw new AssertionError("Already open");

    try
    {
      loadAllFromPersistentStorage(true, null, DesktopUtil.tempDir(), HDB_DEFAULT_FILENAME);
    }
    catch (HDB_InternalError e)
    {
      throw new AssertionError(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Closes the current in-memory instance of the TestHyperDB and reopens it to ensure a clean state.
   * <p>
   * This method is useful for resetting the database state between different test scenarios.
   *
   * @throws AssertionError if the database is already closed or if an error occurs during closing.
   */
  public void closeAndOpen()
  {
    if (isLoaded() == false)
      throw new AssertionError("Already closed");

    try
    {
      close(null);
    }
    catch (HDB_UnrecoverableInternalError e)
    {
      throw new AssertionError(e);
    }

    open();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
