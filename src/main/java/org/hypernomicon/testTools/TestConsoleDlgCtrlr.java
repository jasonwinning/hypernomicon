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

package org.hypernomicon.testTools;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.LibraryWrapper.LibraryType.*;
import static org.hypernomicon.fts.FTSUtil.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Consumer;
import java.util.prefs.Preferences;
import java.util.stream.IntStream;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.apache.lucene.search.Query;
import org.apache.pdfbox.Loader;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.text.PDFTextStripper;

import org.hypernomicon.FolderTreeWatcher;
import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.bib.*;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.fileManager.FileManagerTestRunner;
import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.fts.PDFJSTextExtractor;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;

import org.jodconverter.core.office.OfficeUtils;
import org.jodconverter.local.LocalConverter;
import org.jodconverter.local.office.LocalOfficeManager;

import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.DirectoryChooser;
import javafx.stage.FileChooser;

//---------------------------------------------------------------------------

public class TestConsoleDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnFromExisting, btnClose, btnCloseDB, btnSaveRefMgrSecrets, btnRemoveRefMgrSecrets, btnUseMendeleyID, btnNukeTest,
                       btnZoteroItemTemplates, btnZoteroCreatorTypes, btnLinkGenBefore, btnLinkGenAfter, btnTermsTabTests, btnFolderBypassTest,
                       btnSetupFMTest, btnRunFMTest, btnPdfExtract, btnPdfExtract2, btnPdfExtract3;
  @FXML private CheckBox chkFolderBypass, chkPdfDebug, chkWatcherEvents;
  @FXML private Label lblPdfTime, lblFtsDiagConvertedPath, lblFtsDiagStatus;
  @FXML private RadioButton rbZotero, rbMendeley, rbPdfJS;
  @FXML private Tab tabLinkGen;
  @FXML private TableColumn<FtsDiagMatch, String> colFtsDiagNdx, colFtsDiagTikaOffset, colFtsDiagTikaNormOffset, colFtsDiagTikaSnippet,
                                                  colFtsDiagPdfPage, colFtsDiagPdfNormOffset, colFtsDiagPdfSnippet;
  @FXML private TableView<FtsDiagMatch> tvFtsDiagMatches;
  @FXML private TextArea taPdfResult, taFtsDiagTika, taFtsDiagPdfJS;
  @FXML private TextField tfParent, tfFolderName, tfRefMgrUserID, tfPdfPath, tfPdfPath2, tfPdfPath3, tfPdfPage,
                          tfFtsDiagPath, tfFtsDiagQuery;
  @FXML private ToggleGroup tgLink;

  private final Map<Toggle, LibraryType> toggleToLibraryType;

  private List<String> cachedPdfJSPages = null, cachedPDFBoxPages = null;
  private FilePath ftsDiagConvertedPath;

  // Per-slot caches for multi-instance pdf.js testing
  private final String[] cachedPdfJSPaths = new String[3];
  private final List<?>[] cachedPdfJSSlotPages = new List<?>[3];

  // Tracks number of concurrent extractions; overall stopwatch runs while > 0
  private int activeExtractions = 0;

//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  public TestConsoleDlgCtrlr()
  {
    super("testTools/TestConsoleDlg", appTitle + " Test Console", true, true);

    initTextField(app.prefs, tfParent    , PrefKey.TRANSIENT_TEST_PARENT_PATH, "", null);
    initTextField(app.prefs, tfFolderName, PrefKey.TRANSIENT_TEST_FOLDER_NAME, "", null);
    initTextField(app.prefs, tfPdfPath   , PrefKey.PDF_EXTRACTION_TEST_PATH  , "", null);
    initTextField(app.prefs, tfPdfPath2  , PrefKey.PDF_EXTRACTION_TEST_PATH_2, "", null);
    initTextField(app.prefs, tfPdfPath3  , PrefKey.PDF_EXTRACTION_TEST_PATH_3, "", null);

    enableAllIff(db.isOnline(), btnFromExisting, btnCloseDB, btnZoteroItemTemplates, btnZoteroCreatorTypes, btnNukeTest, btnTermsTabTests, btnFolderBypassTest, tabLinkGen);

    toggleToLibraryType = Map.of(rbZotero, ltZotero, rbMendeley, ltMendeley);

    setToolTip(btnClose, "Close this window");

    btnSaveRefMgrSecrets  .setDisable(db.isOffline() || (db.bibLibraryIsLinked() == false));
    btnRemoveRefMgrSecrets.setDisable(db.isOffline() || (db.bibLibraryIsLinked() == false));
    btnUseMendeleyID      .setDisable(db.isOffline() || (db.bibLibraryIsLinked() == false) || (db.getBibLibrary().type() != ltMendeley));

    btnSaveRefMgrSecrets  .setOnAction(event -> db.getBibLibrary().saveAuthKeysToDBSettings());
    btnRemoveRefMgrSecrets.setOnAction(event -> db.getBibLibrary().removeSecretsFromKeyring());

    btnUseMendeleyID.setOnAction(event -> useCurrentMendeleyUserIDforUnitTests());

    btnZoteroItemTemplates.setOnAction(event -> ZoteroWrapper.retrieveMetadataAndSaveToFile(false));
    btnZoteroCreatorTypes .setOnAction(event -> ZoteroWrapper.retrieveMetadataAndSaveToFile(true ));

    btnLinkGenBefore      .setOnAction(event -> db.rebuildMentions("Before.csv"));
    btnLinkGenAfter       .setOnAction(event -> db.rebuildMentions("After.csv" ));

    chkFolderBypass.setSelected(db.folderDeletionBypassEnabled);
    chkFolderBypass.selectedProperty().addListener((ob, oldVal, newVal) -> db.folderDeletionBypassEnabled = newVal);

    chkWatcherEvents.setSelected(FolderTreeWatcher.consoleLogging);
    chkWatcherEvents.selectedProperty().addListener((ob, oldVal, newVal) -> FolderTreeWatcher.consoleLogging = newVal);

    if (db.bibLibraryIsLinked())
      tfRefMgrUserID.setText(db.getBibLibrary().getUserID());
  }

//---------------------------------------------------------------------------

  @FXML private void btnFromScratchClick () { createTransientTestDB(true ); }
  @FXML private void btnFromExistingClick() { createTransientTestDB(false); }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCloseDB()
  {
    stage.hide();

    Platform.runLater(() ->
    {
      ui.close(true);

      if (ui.isShuttingDown() == false)
        new TestConsoleDlgCtrlr().showModal();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnWelcomeWindowClick()
  {
    stage.hide();

    Platform.runLater(ui::showWelcomeWindow);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initTextField(Preferences prefs, TextField tf, String prefKey, String defValue, Consumer<String> handler)
  {
    tf.setText(prefs.get(prefKey, defValue));

    tf.textProperty().addListener((ob, ov, nv) ->
    {
      if (nv == null) return;

      prefs.put(prefKey, nv);
      if (handler != null) handler.accept(nv);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnBrowseClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath folderPath = FilePath.of(tfParent.getText());

    if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
    {
      if (db.isOnline())
        folderPath = db.getRootPath().getParent();

      if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
        folderPath = FilePath.of(userWorkingDir());
    }

    dirChooser.setInitialDirectory(folderPath.toFile());

    dirChooser.setTitle("Select parent folder of transient test database folder");

    FilePath filePath = showDirDialog(dirChooser);

    if (FilePath.isEmpty(filePath))
      return;

    if (filePath.isUnderDbRoot())
    {
      falseWithErrorPopup("Path \"" + filePath + "\" is within the directory structure of the currently loaded database.", tfFolderName);
      return;
    }

    tfParent.setText(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLaunchClick()
  {
    String folderNameStr = tfFolderName.getText();

    FilePath transientDBFilePath = strNullOrBlank(folderNameStr) ? getParentFilePath() : getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath)) return;

    launchFile(transientDBFilePath.exists() ? transientDBFilePath : transientDBFilePath.getParent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLinkGenLaunchClick()
  {
    FilePath dirPath = testDir().resolve(LINK_GEN_FOLDER_NAME);

    if (dirPath.exists() == false)
    {
      infoPopup("No mentions-index log files have been written yet. Run a Before or After rebuild first.");
      return;
    }

    launchFile(dirPath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createTransientTestDB(boolean fromScratch)
  {
    boolean restartWatcher = folderTreeWatcher.stop();
    FilePath transientDBFilePath = getTransientDBFilePath(true, false, null);
    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();

    if (FilePath.isEmpty(transientDBFilePath)) return;

    stage.hide();

    Platform.runLater(() ->
    {
      if (transientDBFilePath.exists())
      {
        if (db.isOnline() && db.getRootPath().equals(transientDBFilePath))
        {
          if (fromScratch == false)
          {
            errorPopup("The database located in the transient test folder is currently loaded.");
            new TestConsoleDlgCtrlr().showModal();
            return;
          }

          if (confirmDialog("The currently loaded database will be deleted. Continue?", false) == false)
          {
            new TestConsoleDlgCtrlr().showModal();
            return;
          }

          ui.close(false);

          if (ui.isShuttingDown())
            return;
        }
        else if (confirmDialog("The contents of folder \"" + transientDBFilePath + "\" will be deleted. Continue?", false) == false)
        {
          new TestConsoleDlgCtrlr().showModal();
          return;
        }

        if (FileDeletion.ofDirContentsOnly(transientDBFilePath).interactive().execute() == DeletionResult.ABORTED)
        {
          new TestConsoleDlgCtrlr().showModal();
          return;
        }
      }
      else
      {
        try
        {
          transientDBFilePath.createDirectory();
        }
        catch (IOException e)
        {
          errorPopup("Unable to create transient test database folder. Reason: " + getThrowableMessage(e));
          new TestConsoleDlgCtrlr().showModal();
          return;
        }
      }

      if (fromScratch && db.isOnline() && (ui.close(true) == false))
      {
        if (ui.isShuttingDown() == false)
          new TestConsoleDlgCtrlr().showModal();

        return;
      }

      ui.createTransientTestDB(transientDBFilePath, toggleToLibraryType.get(tgLink.getSelectedToggle()));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLoadClick()
  {
    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath))
      return;

    FilePath hdbFilePath = getHdbFile(transientDBFilePath);

    if (FilePath.isEmpty(hdbFilePath))
    {
      errorPopup("HDB file not found.");
      return;
    }

    stage.hide();

    Platform.runLater(() -> ui.openDB(hdbFilePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnClearClick()
  {
    FilePath transientDBFilePath = getTransientDBFilePath(true, false, null);

    if (FilePath.isEmpty(transientDBFilePath)) return;

    if (transientDBFilePath.exists() == false)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" does not exist.", tfFolderName);
      return;
    }

    String[] fileNameArr = transientDBFilePath.toFile().list();
    if ((fileNameArr == null) || (fileNameArr.length == 0))
    {
      infoPopup("Folder is already empty.");
      return;
    }

    clearTransientDB();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean clearTransientDB()
  {
    MutableBoolean nonEmptyWithNoHdbFile = new MutableBoolean(false);

    FilePath transientDBFilePath = getTransientDBFilePath(true, true, nonEmptyWithNoHdbFile);

    if (FilePath.isEmpty(transientDBFilePath)) return false;

    if (transientDBFilePath.exists() == false)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" does not exist.", tfFolderName);
      return false;
    }

    String[] fileNameArr = transientDBFilePath.toFile().list();
    if ((fileNameArr == null) || (fileNameArr.length == 0))
      return true;  // Already empty - success

    String prompt = nonEmptyWithNoHdbFile.isTrue() ?
      "Path \"" + transientDBFilePath + "\" is a non-empty directory with no HDB file. Are you sure you want to delete all contents?"
    :
      "Delete all contents of folder \"" + transientDBFilePath + "\"?";

    if (confirmDialog(prompt, false) == false)
      return false;

    DeletionResult result = FileDeletion.ofDirContentsOnly(transientDBFilePath).interactive().execute();

    return result != DeletionResult.ABORTED;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath getParentFilePath()
  {
    String parentStr = tfParent.getText();

    if (strNullOrBlank(parentStr))
    {
      falseWithErrorPopup("Select a parent folder.", tfParent);
      return null;
    }

    FilePath parentFilePath = FilePath.of(parentStr);

    if (FilePath.isEmpty(parentFilePath))
    {
      falseWithErrorPopup("Select a parent folder.", tfParent);
      return null;
    }

    if (parentFilePath.exists() == false)
    {
      falseWithErrorPopup("The selected parent folder does not exist.", tfParent);
      return null;
    }

    if (parentFilePath.isDirectory() == false)
    {
      falseWithErrorPopup("The selected parent path is not a directory.", tfParent);
      return null;
    }

    return parentFilePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Validate transient test database directory path and return FilePath object
   * @param modifying If true, means it should perform checks for modifying the directory
   * @param deleting If true, means it should perform checks for deleting the directory
   * @param nonEmptyWithNoHdbFile Output parameter set to true if the directory is non-empty
   * with no HDB file. Can be set to null if you don't need that information.
   * @return FilePath object
   */
  private FilePath getTransientDBFilePath(boolean modifying, boolean deleting, MutableBoolean nonEmptyWithNoHdbFile)
  {
    FilePath parentFilePath = getParentFilePath();

    if (FilePath.isEmpty(parentFilePath)) return null;

    String folderNameStr = tfFolderName.getText();

    if (strNullOrBlank(folderNameStr))
    {
      falseWithErrorPopup("Enter a folder name.", tfFolderName);
      return null;
    }

    if (FilePath.isFilenameValid(folderNameStr) == false)
    {
      falseWithErrorPopup("Folder name is not valid.", tfFolderName);
      return null;
    }

    FilePath transientDBFilePath = parentFilePath.resolve(folderNameStr);

    if (transientDBFilePath.exists() == false)
      return transientDBFilePath;

    if (transientDBFilePath.isDirectory() == false)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" is not a directory.", tfFolderName);
      return null;
    }

    if (modifying == false)
      return transientDBFilePath;

    if (db.isOnline() && (deleting || (db.getRootPath().equals(transientDBFilePath) == false)) && transientDBFilePath.isUnderDbRoot())
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" is within the directory structure of the currently loaded database.", tfFolderName);
      return null;
    }

    String[] fileNameArr = transientDBFilePath.toFile().list();

    if ((fileNameArr == null) || (fileNameArr.length == 0))
      return transientDBFilePath;

    if (Arrays.stream(fileNameArr).map(transientDBFilePath::resolve)
                                  .filter(FilePath::isFile)
                                  .noneMatch(filePath -> "hdb".equalsIgnoreCase(filePath.getExtensionOnly())))
    {
      if (nonEmptyWithNoHdbFile != null)
        nonEmptyWithNoHdbFile.setTrue();

      if (deleting == false)
      {
        falseWithErrorPopup("Path \"" + transientDBFilePath + "\" is a non-empty directory with no HDB file.", tfFolderName);
        return null;
      }
    }

    return transientDBFilePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void useCurrentMendeleyUserIDforUnitTests()
  {
    if (db.isOffline() || (db.bibLibraryIsLinked() == false)) return;

    LibraryWrapper<? extends BibEntry<?, ?>, ? extends BibCollection> bibLibrary = db.getBibLibrary();

    if (bibLibrary.type() != ltMendeley) return;

    String userID = bibLibrary.getUserID();

    if (strNotNullOrBlank(userID))
      app.prefs.put(PrefKey.BIB_UNIT_TEST_USER_ID, userID);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void nukeTest()
  {
    if (db.isOffline()) return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (db.getRootPath().equals(transientDBFilePath) == false)
    {
      errorPopup("This can only be done when the transient DB is loaded.");
      return;
    }

    if (confirmDialog("This will delete most of the records in the entire database. Proceed?", false) == false)
      return;

    db.recordDeletionTestInProgress = true;

    Random random = new Random();

    EnumSet<RecordType> types = EnumSet.allOf(RecordType.class);
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub, hdtFolder));  // Folders deleted last
    List<RecordType> typeList = List.copyOf(types);

    int deleteCtr = 0;

    while (types.stream().anyMatch(recordType -> (nextRecordToDelete(recordType) > 0)))
    {
      RecordType randomType;
      int randomID;

      do
      {
        randomType = typeList.get(random.nextInt(typeList.size()));

        randomID = db.records(randomType).getRandomUsedID(random);
      }
      while (randomID < 1);

      HDT_Record record = db.records(randomType).getByID(randomID);

      boolean doDelete = (HDT_Record.isEmpty(record, false) == false) && (db.isProtectedRecord(record, true) == false);

      // Glossary should only be deleted if it has no concepts
      if (doDelete && (randomType == hdtGlossary))
      {
        HDT_Glossary glossary = (HDT_Glossary) record;

        if (glossary.concepts.isEmpty() == false)
          doDelete = false;
      }

      if (doDelete)
      {
        db.deleteRecord(record);
        deleteCtr++;

        if ((deleteCtr % 100) == 0)
          System.out.println("Records deleted: " + deleteCtr);
      }
    }

    System.out.println("Non-folder records deleted: " + deleteCtr);

    // Delete folders last so the bypass preconditions hold (no non-folder records pointing to folders).

    deleteNonProtectedFolders(deleteCtr, "Records");

    System.out.println("Record deletion complete.");

    db.recordDeletionTestInProgress = false;
    db.rebuildMentions();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int nextRecordToDelete(RecordType recordType)
  {
    return db.records(recordType).stream().filter(record -> (HDT_Record.isEmpty(record, false) == false))
                                          .filter(record -> (db.isProtectedRecord(record, true) == false))
                                          .map(HDT_Record::getID)
                                          .findFirst()
                                          .orElse(-1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Delete all non-protected folders, sorted deepest first so children are deleted before parents.
   * @param deleteCount number of records already deleted, used for progress logging
   * @param recordLabel label to use in progress messages (e.g. "Records" or "Folders")
   * @return the updated total delete count (deleteCount + number of folders deleted)
   */
  private static int deleteNonProtectedFolders(int deleteCount, String recordLabel)
  {
    List<HDT_Folder> foldersToDelete = db.folders.stream()
      .filter(folder -> HDT_Record.isEmpty(folder, false) == false)
      .filter(folder -> db.isProtectedRecord(folder, true) == false)
      .sorted(Comparator.comparingInt(TestConsoleDlgCtrlr::folderDepth).reversed())
      .toList();

    for (HDT_Folder folder : foldersToDelete)
    {
      if (folder.isExpired()) continue;

      db.deleteRecord(folder);
      deleteCount++;

      if ((deleteCount % 100) == 0)
        System.out.println(recordLabel + " deleted: " + deleteCount);
    }

    return deleteCount;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static int folderDepth(HDT_Folder folder)
  {
    int depth = 0;

    while ((folder = folder.parentFolder()) != null)
      depth++;

    return depth;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Test to verify that folder deletion bypass produces identical results to the non-bypass path.
   * <p>
   * This test should be run on a copy of a real database. It:
   * <ol>
   *   <li>Loops through all HDT_Folder records</li>
   *   <li>For non-protected folders, severs links to HDT_WorkFile, HDT_MiscFile, HDT_Note, and HDT_Person records</li>
   *   <li>Loops again and deletes all non-protected folders</li>
   *   <li>Saves the database to XML</li>
   * </ol>
   * To verify correctness, run this test twice: once with bypass enabled and once disabled,
   * then diff the resulting XML files.
   */
  @FXML private void folderBypassTest()
  {
    if (db.isOffline()) return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (db.getRootPath().equals(transientDBFilePath) == false)
    {
      errorPopup("This can only be done when the transient DB is loaded.");
      return;
    }

    if (confirmDialog("This will sever folder links and delete non-protected folders. Proceed?", false) == false)
      return;

    db.recordDeletionTestInProgress = true;
    db.runningConversion = true;

    System.out.println("=== Folder Bypass Test: Severing non-folder links ===");

    // First pass: sever links from non-folder records to non-protected folders

    int severedCount = 0;

    for (HDT_Folder folder : List.copyOf(db.folders))
    {
      if (db.isProtectedRecord(folder, true))
        continue;

      // Sever links from HDT_WorkFile records

      for (HDT_WorkFile workFile : List.copyOf(db.<HDT_Folder, HDT_WorkFile>getSubjectList(rtFolderOfWorkFile, folder)))
      {
        workFile.getPath().clear(false);
        severedCount++;
      }

      // Sever links from HDT_MiscFile records

      for (HDT_MiscFile miscFile : List.copyOf(db.<HDT_Folder, HDT_MiscFile>getSubjectList(rtFolderOfMiscFile, folder)))
      {
        miscFile.getPath().clear(false);
        severedCount++;
      }

      // Sever links from HDT_Note records

      for (HDT_Note note : List.copyOf(db.<HDT_Folder, HDT_Note>getSubjectList(rtFolderOfNote, folder)))
      {
        note.folder.setID(-1);
        severedCount++;
      }

      // Sever links from HDT_Person picture folder

      for (HDT_Person person : List.copyOf(db.<HDT_Folder, HDT_Person>getSubjectList(rtPictureFolderOfPerson, folder)))
      {
        person.getPath().clear(false);
        severedCount++;
      }
    }

    System.out.println("Severed " + severedCount + " links.");
    System.out.println("=== Folder Bypass Test: Deleting non-protected folders ===");

    // Second pass: delete non-protected folders (deepest first so children are deleted before parents)

    int deleteCount = deleteNonProtectedFolders(0, "Folders");

    System.out.println("Deleted " + deleteCount + " folders total.");

    db.recordDeletionTestInProgress = false;
    db.runningConversion = false;
    db.rebuildMentions();

    System.out.println("=== Folder Bypass Test: Complete. ===");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void fileDeletionTest()
  {
    if (db.isOffline()) return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (db.getRootPath().equals(transientDBFilePath) == false)
    {
      errorPopup("This can only be done when the transient DB is loaded.");
      return;
    }

    FileDeletionTestRunner.runTests(db.getRootPath());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void setupFileManagerTest()
  {
    if (db.isOffline()) return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (db.getRootPath().equals(transientDBFilePath) == false)
    {
      errorPopup("This can only be done when the transient DB is loaded.");
      return;
    }

    FileManagerTestRunner.setupTestFiles(db.getRootPath("_test_fm"));

    btnSetupFMTest.setDisable(true);
    btnRunFMTest.setDisable(false);

    infoPopup("Test files created. Wait for all files to sync before running tests.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void fileManagerTest()
  {
    if (db.isOffline()) return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (db.getRootPath().equals(transientDBFilePath) == false)
    {
      errorPopup("This can only be done when the transient DB is loaded.");
      return;
    }

    btnRunFMTest.setDisable(true);

    FileManagerTestRunner.runTests(db.getRootPath("_test_fm"), () -> btnSetupFMTest.setDisable(false));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void copyForNukeTest()
  {
    if (db.isOffline())
    {
      errorPopup("No database is currently loaded.");
      return;
    }

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath))
    {
      errorPopup("Transient DB folder path needs to be entered.");
      return;
    }

    if (db.getRootPath().equals(transientDBFilePath))
    {
      errorPopup("Transient DB is currently loaded.");
      return;
    }

    if (clearTransientDB() == false)
      return;

    try
    {
      db.getHdbPath().copyTo(transientDBFilePath.resolve(db.getHdbPath().getNameOnly()) , false);

      FileUtils.copyDirectory(db.xmlPath().toFile(), transientDBFilePath.resolve(DEFAULT_XML_PATH).toFile());

      Path srcRoot = db.getRootPath().toPath(),
           dstRoot = transientDBFilePath.toPath();

      try (Stream<Path> dirs = Files.walk(srcRoot))
      {
        dirs.filter(Files::isDirectory).forEach(srcDir ->
        {
          try { Files.createDirectories(dstRoot.resolve(srcRoot.relativize(srcDir))); }
          catch (IOException e) { throw new UncheckedIOException(e); }
        });
      }

      infoPopup("Database copied successfully.");
    }
    catch (IOException | UncheckedIOException e)
    {
      errorPopup("Error while copying: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static FilePath getHdbFile(FilePath dir)
  {
    File[] files = dir.toFile().listFiles((_dir, name) -> name.endsWith(".hdb"));

    if ((files == null) || (files.length == 0))
      return null;

    return FilePath.of(files[0]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnLogMessageClick()
  {
    System.out.println("Test button clicked on instance " + InterProcClient.getInstanceID() + " at " + timeToUserReadableStr(LocalDateTime.now()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnAdHocTestClick()
  {

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnTermsTabTestClick()
  {
    stage.hide();

    Platform.runLater(() -> ui.termHyperTab().runTests());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnPdfBrowseClick()  { browseForPdf(tfPdfPath);  }
  @FXML private void btnPdfBrowse2Click() { browseForPdf(tfPdfPath2); }
  @FXML private void btnPdfBrowse3Click() { browseForPdf(tfPdfPath3); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void browseForPdf(TextField tf)
  {
    FileChooser fileChooser = new FileChooser();

    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("PDF files (*.pdf)", "*.pdf"));
    fileChooser.getExtensionFilters().add(new FileChooser.ExtensionFilter("All files (*.*)", "*.*"));

    FilePath curPath = FilePath.of(tf.getText());

    if (FilePath.isEmpty(curPath) == false)
    {
      FilePath parentDir = curPath.getParent();

      if ((parentDir != null) && parentDir.exists())
        fileChooser.setInitialDirectory(parentDir.toFile());
    }

    fileChooser.setTitle("Select PDF file");

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath) == false)
      tf.setText(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnPdfExtractClick()
  {
    String pathStr = tfPdfPath.getText();

    if (stripSafe(pathStr).isEmpty())
    {
      falseWithErrorPopup("Please select a PDF file.", tfPdfPath);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfPdfPath);
      return;
    }

    int page = parseInt(tfPdfPage.getText().trim(), -1);

    if (page < 1)
    {
      falseWithErrorPopup("Page number must be a number greater than zero.", tfPdfPage);
      return;
    }

    taPdfResult.clear();
    btnPdfExtract.setDisable(true);

    extractionStarted();

    if (rbPdfJS.isSelected())
      extractViaPdfJS(filePath, page);
    else
      extractViaPDFBox(filePath, page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnPdfExtract2Click() { extractSlot(1, tfPdfPath2, btnPdfExtract2); }
  @FXML private void btnPdfExtract3Click() { extractSlot(2, tfPdfPath3, btnPdfExtract3); }

//---------------------------------------------------------------------------

  @FXML private void btnPdfShowClick () { showSlotPage(0); }
  @FXML private void btnPdfShow2Click() { showSlotPage(1); }
  @FXML private void btnPdfShow3Click() { showSlotPage(2); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void showSlotPage(int slot)
  {
    int page = parsePage();
    if (page < 1) return;

    List<String> pages;

    if (slot == 0)
    {
      // Slot 0 uses the existing cache (which may be pdf.js or PDFBox)
      pages = rbPdfJS.isSelected() ? cachedPdfJSPages : cachedPDFBoxPages;
    }
    else
    {
      TextField pathField = (slot == 1) ? tfPdfPath2 : tfPdfPath3;

      // Only reuse the slot's cached pages if its file path is unchanged since extraction;
      // otherwise treat as no cache so a stale extraction isn't shown for a different file.

      pages = Objects.equals(pathField.getText(), cachedPdfJSPaths[slot]) ? (List<String>) cachedPdfJSSlotPages[slot] : null;
    }

    if (pages == null)
    {
      taPdfResult.setText("(no cached extraction for file " + (slot + 1) + ')');
      return;
    }

    taPdfResult.clear();
    showCachedPage(pages, page, "(file " + (slot + 1) + ", cached) ");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractSlot(int slot, TextField tf, Button btn)
  {
    String pathStr = tf.getText();

    if (stripSafe(pathStr).isEmpty())
    {
      falseWithErrorPopup("Please select a PDF file.", tf);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tf);
      return;
    }

    taPdfResult.clear();
    btn.setDisable(true);

    extractionStarted();

    int slotNum = slot + 1;

    System.out.println("Slot " + slotNum + ": starting extraction of " + filePath.getNameOnly());

    runOutsideFXThread(() ->
    {
      PDFJSTextExtractor extractor = new PDFJSTextExtractor();
      long startTime = System.nanoTime();

      try
      {
        extractor.initialize();

        System.out.println("Slot " + slotNum + ": browser initialized, extracting...");

        PDFJSTextExtractor.ExtractionResult result = extractor.extractText(filePath);

        long elapsed = System.nanoTime() - startTime;
        double seconds = elapsed / 1_000_000_000.0;

        System.out.println("Slot " + slotNum + ": extraction complete in " + String.format("%.2f", seconds) + 's'
          + (result != null ? " (" + result.pageCount() + " pages)" : " (null result)"));

        Platform.runLater(() ->
        {
          if (result != null && result.pageOffsets() != null)
          {
            cachedPdfJSPaths[slot] = pathStr;
            cachedPdfJSSlotPages[slot] = splitIntoPages(result);
          }

          btn.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        System.out.println("Slot " + slotNum + ": extraction failed: " + getThrowableMessage(e));

        Platform.runLater(() ->
        {
          btn.setDisable(false);
          extractionFinished();
        });
      }
      finally
      {
        extractor.dispose();
        System.out.println("Slot " + slotNum + ": browser disposed");
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractViaPdfJS(FilePath filePath, int page)
  {
    boolean debug = chkPdfDebug.isSelected();

    runOutsideFXThread(() ->
    {
      PDFJSTextExtractor extractor = new PDFJSTextExtractor();

      try
      {
        extractor.initialize();

        PDFJSTextExtractor.ExtractionResult result = debug
          ? extractor.extractText(filePath, true, page)
          : extractor.extractText(filePath);

        Platform.runLater(() ->
        {
          if (result == null)
          {
            taPdfResult.setText("(extraction returned null)");
          }
          else if (debug)
          {
            taPdfResult.setText(result.text());
          }
          else if (result.pageOffsets() == null)
          {
            taPdfResult.setText("(no page offsets available)");
          }
          else
          {
            cachedPdfJSPages = splitIntoPages(result);
            showCachedPage(cachedPdfJSPages, page, "");
          }

          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() ->
        {
          taPdfResult.setText("Error: " + getThrowableMessage(e));
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      finally
      {
        extractor.dispose();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractViaPDFBox(FilePath filePath, int page)
  {
    runOutsideFXThread(() ->
    {
      try (PDDocument doc = Loader.loadPDF(filePath.toFile()))
      {
        int pageCount = doc.getNumberOfPages();
        List<String> pages = new ArrayList<>(pageCount);

        PDFTextStripper stripper = new PDFTextStripper();
        stripper.setSortByPosition(true);
        stripper.setLineSeparator("\n");

        for (int pageNum = 1; pageNum <= pageCount; pageNum++)
        {
          stripper.setStartPage(pageNum);
          stripper.setEndPage(pageNum);
          pages.add(stripper.getText(doc));
        }

        Platform.runLater(() ->
        {
          cachedPDFBoxPages = pages;

          showCachedPage(pages, page, "");
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() ->
        {
          taPdfResult.setText("Error: " + getThrowableMessage(e));
          btnPdfExtract.setDisable(false);
          extractionFinished();
        });
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractionStarted()
  {
    activeExtractions++;

    if (activeExtractions == 1)
    {
      stopWatch1.resetAndStart();
      lblPdfTime.setText("...");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void extractionFinished()
  {
    activeExtractions--;

    if (activeExtractions == 0)
    {
      stopWatch1.stop();
      lblPdfTime.setText("Overall: " + stopWatch1.elapsedStr());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int parsePage()
  {
    int page = parseInt(tfPdfPage.getText().trim(), -1);
    if (page < 1)
    {
      falseWithErrorPopup("Page number must be a number greater than zero.", tfPdfPage);
      return -1;
    }
    return page;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showCachedPage(List<String> pages, int page, String timePrefix)
  {
    if (page > pages.size())
    {
      taPdfResult.setText("Page " + page + " is out of range (document has " + pages.size() + " pages).");
    }
    else
    {
      lblPdfTime.setText(timePrefix + stopWatch1.elapsedStr());
      taPdfResult.setText(pages.get(page - 1));
    }

    btnPdfExtract.setDisable(false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static List<String> splitIntoPages(PDFJSTextExtractor.ExtractionResult result)
  {
    int[] offsets = result.pageOffsets();
    String text = result.text();

    // Defensive against inconsistent ExtractionResults where pageOffsets claim positions
    // past text.length() (e.g., extractor returned a tiny text snippet but reported
    // full-document page boundaries). safeSubstring clamps, surfacing the inconsistency
    // in the UI rather than crashing.

    return IntStream.range(0, result.pageCount())
      .mapToObj(ndx -> safeSubstring(text, offsets[ndx], offsets[ndx + 1]))
      .toList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private record FtsDiagMatch(int ndx, int tikaOffset, int tikaNormPos, String tikaSnippet,
                              int pdfPage, int pdfNormPos, String pdfSnippet) {}

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagBrowseClick()
  {
    FileChooser fileChooser = new FileChooser();
    fileChooser.setTitle("Select file for FTS diagnostics");

    if (db.isLoaded())
      fileChooser.setInitialDirectory(db.getRootPath().toFile());

    FilePath filePath = showOpenDialog(fileChooser);

    if (FilePath.isEmpty(filePath)) return;

    tfFtsDiagPath.setText(filePath.toString());
    lblFtsDiagConvertedPath.setText("");
    ftsDiagConvertedPath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagConvertClick()
  {
    String pathStr = tfFtsDiagPath.getText();

    if (strNullOrBlank(pathStr))
    {
      falseWithErrorPopup("Please select a file.", tfFtsDiagPath);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfFtsDiagPath);
      return;
    }

    lblFtsDiagStatus.setText("Converting...");

    // Convert to PDF using JodConverter (same as OfficePreviewer)

    Thread convertThread = new HyperThread("FtsDiagConvert", () ->
    {
      try
      {
        FilePath tempDir = FilePath.of(System.getProperty("java.io.tmpdir")).resolve("hnFtsDiag_" + System.currentTimeMillis());
        Files.createDirectories(tempDir.toPath());

        FilePath outputPath = tempDir.resolve("converted.pdf");

        String officePath = getOfficeHome();

        if (officePath.isBlank())
        {
          Platform.runLater(() -> lblFtsDiagStatus.setText("No office installation path configured in settings."));
          return;
        }

        List<Integer> ports = new ArrayList<>();
        findAvailablePorts(1, ports);

        LocalOfficeManager officeManager = LocalOfficeManager.builder().officeHome(officePath).portNumbers(ports.getFirst()).build();

        officeManager.start();

        try
        {
          LocalConverter.make(officeManager).convert(filePath.toFile()).to(outputPath.toFile()).execute();
        }
        finally
        {
          OfficeUtils.stopQuietly(officeManager);
        }

        Platform.runLater(() ->
        {
          ftsDiagConvertedPath = outputPath;
          lblFtsDiagConvertedPath.setText(outputPath.toString());
          lblFtsDiagStatus.setText("Conversion complete.");
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() -> lblFtsDiagStatus.setText("Conversion failed: " + getThrowableMessage(e)));
      }
    });

    convertThread.setDaemon(true);
    convertThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagShowConvertedClick()
  {
    if (ftsDiagConvertedPath != null)
      highlightFileInExplorer(ftsDiagConvertedPath);
    else
      infoPopup("No converted file available. Click Convert first.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagExportClick()
  {
    try
    {
      FilePath exportDir = testDir().resolve("fts-diag");
      exportDir.createDirectories();

      String tikaText = taFtsDiagTika.getText(),
             pdfText  = taFtsDiagPdfJS.getText();

      if (strNullOrBlank(tikaText) && strNullOrBlank(pdfText))
      {
        infoPopup("No extraction data to export. Run Extract & Search first.");
        return;
      }

      if (strNullOrBlank(tikaText) == false)
        Files.writeString(exportDir.resolve("tika-normalized.txt").toPath(), tikaText);

      if (strNullOrBlank(pdfText) == false)
        Files.writeString(exportDir.resolve("pdfjs-normalized.txt").toPath(), pdfText);

      // Export match table as TSV

      StringBuilder tsv = new StringBuilder();
      tsv.append("#\tTika Offset\tTika Norm Pos\tTika Snippet\tPDF Page\tPDF Norm Pos\tPDF Snippet\n");

      for (FtsDiagMatch m : tvFtsDiagMatches.getItems())
      {
        tsv.append(m.ndx        ()).append('\t')
           .append(m.tikaOffset ()).append('\t')
           .append(m.tikaNormPos()).append('\t')
           .append(m.tikaSnippet()).append('\t')
           .append(m.pdfPage    ()).append('\t')
           .append(m.pdfNormPos ()).append('\t')
           .append(m.pdfSnippet ()).append('\n');
      }

      Files.writeString(exportDir.resolve("matches.tsv").toPath(), tsv.toString());

      highlightFileInExplorer(exportDir.resolve("matches.tsv"));
    }
    catch (IOException e)
    {
      errorPopup("Export failed: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnFtsDiagExtractClick()
  {
    String pathStr  = tfFtsDiagPath .getText(),
           queryStr = tfFtsDiagQuery.getText();

    if (strNullOrBlank(pathStr))
    {
      falseWithErrorPopup("Please select a file.", tfFtsDiagPath);
      return;
    }

    if (strNullOrBlank(queryStr))
    {
      falseWithErrorPopup("Please enter a query.", tfFtsDiagQuery);
      return;
    }

    FilePath filePath = FilePath.of(pathStr);

    if (filePath.exists() == false)
    {
      falseWithErrorPopup("File not found: " + pathStr, tfFtsDiagPath);
      return;
    }

    FullTextIndexer indexer = db.getFullTextIndexer();
    if (indexer == null)
    {
      errorPopup("Full-text indexer is not running.");
      return;
    }

    lblFtsDiagStatus.setText("Extracting...");

    Thread extractThread = new HyperThread("FtsDiagExtract", () ->
    {
      try
      {
        // Get Tika extraction from the Lucene index

        String dbRelPath = null;

        if (db.isLoaded())
        {
          try { dbRelPath = db.getRootPath().relativize(filePath).toString().replace('\\', '/'); }
          catch (Exception e) { /* not under DB root */ }
        }

        String tikaText = (dbRelPath != null) ? indexer.getStoredContent(dbRelPath) : null;

        // Get pdf.js extraction

        FilePath pdfPath = ftsDiagConvertedPath != null ? ftsDiagConvertedPath : filePath;
        FullTextIndexer.ExtractionResult pdfExtraction = null;

        String mime = getMediaType(filePath).toString();

        if (mime.contains("pdf") || (ftsDiagConvertedPath != null))
          pdfExtraction = indexer.extractPdfText(pdfPath);

        // Normalize both texts

        ArrayList<Integer> tikaPosMap = new ArrayList<>(),
                           pdfPosMap  = new ArrayList<>();

        String normTika   = (tikaText != null) ? normalizeForMatching(tikaText, tikaPosMap) : "",
               pdfRawText = ((pdfExtraction != null) && (pdfExtraction.text() != null)) ? pdfExtraction.text() : null;

        int[] pdfPageOffsets = (pdfExtraction != null) ? pdfExtraction.pageOffsets().clone() : null;

        if ((pdfRawText != null) && db.isLoaded())
          pdfRawText = stripConvertedPdfHeaders(pdfRawText, db.getRootPath().toString().replace('/', '\\'), pdfPageOffsets);

        String normPdf = (pdfRawText != null) ? normalizeForMatching(pdfRawText, pdfPosMap) : "";

        int[] tikaRevMap = (tikaText != null) ? buildReversePositionMap(tikaPosMap, tikaText.length()) : new int[0];

        // Parse and run the query against both texts using temporary indexes

        Query query;

        try (var analyzer = FullTextIndexer.createAnalyzer())
        {
          query = FullTextIndexer.createQueryParser(analyzer).parse(queryStr);
        }
        catch (Exception e)
        {
          Platform.runLater(() -> lblFtsDiagStatus.setText("Query parse error: " + e.getMessage()));
          return;
        }

        // Search Tika text

        List<FullTextIndexer.SearchResult.PageMatch> tikaMatches =
          (tikaText != null) ? FullTextIndexer.searchExtractedText(tikaText, null, query) : List.of();

        // Search pdf.js text

        List<FullTextIndexer.SearchResult.PageMatch> pdfMatches =
          ((pdfExtraction != null) && (pdfExtraction.text() != null))
            ? FullTextIndexer.searchExtractedText(pdfExtraction.text(), pdfPageOffsets, query) : List.of();

        // Build diagnostic match rows by mapping Tika matches to pdf.js positions

        List<FtsDiagMatch> diagMatches = new ArrayList<>();

        for (int ndx = 0; ndx < tikaMatches.size(); ndx++)
        {
          FullTextIndexer.SearchResult.PageMatch tm = tikaMatches.get(ndx);

          int tikaAbsOffset = tm.startOffset();
          if ((tm.hitRanges() != null) && (tm.hitRanges().isEmpty() == false))
            tikaAbsOffset += tm.hitRanges().getFirst().start();

          int tikaNormPos = ((tikaAbsOffset >= 0) && (tikaAbsOffset < tikaRevMap.length)) ? tikaRevMap[tikaAbsOffset] : -1;

          String tikaSnip = (tm.snippet() != null) ? tm.snippet().replaceAll("\\s+", " ").strip() : "";
          tikaSnip = safeSubstring(tikaSnip, 0, 60);

          // Align via the same helper production passage-click navigation uses, so the
          // diagnostics reflect what it actually computes (progressive context windows,
          // mappable-position requirement) rather than a separate approximation.

          int pdfNormPos = ((tikaNormPos >= 0) && (normPdf.isEmpty() == false)) ? findPdfNormPos   (tikaNormPos, normTika, normPdf, pdfPosMap.size()) : -1,
              pdfPage    = ((pdfNormPos  >= 0) && (pdfPageOffsets    != null )) ? pageForPdfNormPos(pdfNormPos, pdfPosMap, pdfPageOffsets) : -1;

          String pdfSnip = (pdfNormPos >= 0) ? safeSubstring(normPdf, pdfNormPos - 20, pdfNormPos + 40) : "";

          diagMatches.add(new FtsDiagMatch(ndx, tikaAbsOffset, tikaNormPos, tikaSnip, pdfPage, pdfNormPos, pdfSnip));
        }

        int tikaMatchCount = tikaMatches.size(), pdfMatchCount = pdfMatches.size();

        Platform.runLater(() ->
        {
          taFtsDiagTika.setText(normTika);
          taFtsDiagPdfJS.setText(normPdf);

          colFtsDiagNdx           .setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().ndx())));
          colFtsDiagTikaOffset    .setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().tikaOffset())));
          colFtsDiagTikaNormOffset.setCellValueFactory(cd -> new SimpleStringProperty(String.valueOf(cd.getValue().tikaNormPos())));
          colFtsDiagTikaSnippet   .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().tikaSnippet()));
          colFtsDiagPdfPage       .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfPage() > 0 ? String.valueOf(cd.getValue().pdfPage()) : "?"));
          colFtsDiagPdfNormOffset .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfNormPos() >= 0 ? String.valueOf(cd.getValue().pdfNormPos()) : "?"));
          colFtsDiagPdfSnippet    .setCellValueFactory(cd -> new SimpleStringProperty(cd.getValue().pdfSnippet()));

          tvFtsDiagMatches.getItems().setAll(diagMatches);

          // Click a row to scroll both text areas to the match position

          tvFtsDiagMatches.getSelectionModel().selectedItemProperty().addListener((ob, ov, nv) ->
          {
            if (nv == null) return;

            if (nv.tikaNormPos() >= 0)
              taFtsDiagTika.positionCaret(nv.tikaNormPos());

            if (nv.pdfNormPos() >= 0)
              taFtsDiagPdfJS.positionCaret(nv.pdfNormPos());
          });

          lblFtsDiagStatus.setText("Tika: " + tikaMatchCount + " matches, pdf.js: " + pdfMatchCount + " matches, " +
            diagMatches.size() + " mapped");
        });
      }
      catch (Exception e)
      {
        Platform.runLater(() -> lblFtsDiagStatus.setText("Error: " + getThrowableMessage(e)));
        e.printStackTrace();
      }
    });

    extractThread.setDaemon(true);
    extractThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
