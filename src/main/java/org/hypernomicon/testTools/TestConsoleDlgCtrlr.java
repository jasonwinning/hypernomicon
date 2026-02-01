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
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.File;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.bib.*;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

public class TestConsoleDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Tab tabLinkGen;
  @FXML private TextField tfParent, tfLinkGenParent, tfFolderName, tfRefMgrUserID;
  @FXML private Button btnFromExisting, btnClose, btnCloseDB, btnSaveRefMgrSecrets, btnRemoveRefMgrSecrets, btnUseMendeleyID, btnNukeTest,
                       btnZoteroItemTemplates, btnZoteroCreatorTypes, btnLinkGenBefore, btnLinkGenAfter, btnTermsTabTests;
  @FXML private RadioButton rbZotero, rbMendeley;
  @FXML private ToggleGroup tgLink;

  private final Map<Toggle, LibraryType> toggleToLibraryType;

//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  public TestConsoleDlgCtrlr()
  {
    super("testTools/TestConsoleDlg", appTitle + " Test Console", true, true);

    initTextField(app.prefs, tfParent       , PrefKey.TRANSIENT_TEST_PARENT_PATH, "", null);
    initTextField(app.prefs, tfFolderName   , PrefKey.TRANSIENT_TEST_FOLDER_NAME, "", null);
    initTextField(app.prefs, tfLinkGenParent, PrefKey.LINK_GENERATION_LOG_FOLDER, "", null);

    enableAllIff(db.isOnline(), btnFromExisting, btnCloseDB, btnZoteroItemTemplates, btnZoteroCreatorTypes, btnNukeTest, btnTermsTabTests, tabLinkGen);

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

    FilePath folderPath = new FilePath(tfParent.getText());

    if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
    {
      if (db.isOnline())
        folderPath = db.getRootPath().getParent();

      if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
        folderPath = new FilePath(userWorkingDir());
    }

    dirChooser.setInitialDirectory(folderPath.toFile());

    dirChooser.setTitle("Select parent folder of transient test database folder");

    FilePath filePath = showDirDialog(dirChooser);

    if (FilePath.isEmpty(filePath))
      return;

    if (db.isOnline() && db.getRootPath().contains(filePath))
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
    FilePath filePath = new FilePath(tfLinkGenParent.getText());

    if (FilePath.isEmpty(filePath)) return;

    launchFile(filePath);
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

        try
        {
          FileUtils.cleanDirectory(transientDBFilePath.toFile());
        }
        catch (IOException e)
        {
          errorPopup("Unable to clear the transient test database folder. Reason: " + getThrowableMessage(e));
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

  @FXML private void btnDeleteClick()
  {
    deleteTransientDB(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean deleteTransientDB(boolean deleteRootFolder)
  {
    MutableBoolean nonEmptyWithNoHdbFile = new MutableBoolean(false);

    FilePath transientDBFilePath = getTransientDBFilePath(true, true, nonEmptyWithNoHdbFile);

    if (FilePath.isEmpty(transientDBFilePath)) return false;

    if (transientDBFilePath.exists() == false)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" does not exist.", tfFolderName);
      return false;
    }

    String prompt = nonEmptyWithNoHdbFile.isTrue() ?
      (deleteRootFolder ?
        "Path \"" + transientDBFilePath + "\" is a non-empty directory with no HDB file. Are you sure you want to delete it?"
      :
        "Path \"" + transientDBFilePath + "\" is a non-empty directory with no HDB file. Are you sure you want to delete all contents?")
    :
      (deleteRootFolder ?
        "Delete folder \"" + transientDBFilePath + "\" and everything in it?"
      :
        "Delete all contents of folder \"" + transientDBFilePath + "\"?");

    if (confirmDialog(prompt, false) == false)
      return false;

    try
    {
      if (deleteRootFolder)
        transientDBFilePath.deleteDirectory(true);
      else
        FileUtils.cleanDirectory(transientDBFilePath.toFile());
    }
    catch (IOException e)
    {
      errorPopup("One or more files were not deleted. Reason: " + getThrowableMessage(e));
      return false;
    }

    return true;
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

    FilePath parentFilePath = new FilePath(parentStr);

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

    String[] fileNameArr = transientDBFilePath.toFile().list();
    if (fileNameArr == null)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" is not a directory.", tfFolderName);
      return null;
    }

    if (modifying == false)
      return transientDBFilePath;

    if (db.isOnline() && (deleting || (db.getRootPath().equals(transientDBFilePath) == false)) && db.getRootPath().contains(transientDBFilePath))
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" is within the directory structure of the currently loaded database.", tfFolderName);
      return null;
    }

    if (fileNameArr.length == 0)
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

    return transientDBFilePath.anyOpenFilesInDir() ? null : transientDBFilePath;  // anyOpenFilesInDir shows a popup if it returns true
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
    types.removeAll(EnumSet.of(hdtNone, hdtAuxiliary, hdtHub));
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

        System.out.printf("%5d %s%n", randomID, randomType.name().substring(3));

        noOp(deleteCtr++);

        if ((deleteCtr % 100) == 0)
          System.out.println("----------------------------------------------------- Records deleted: " + deleteCtr);
      }
    }

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

  @FXML private void copyForNukeTest()
  {
    if (db.isOffline())
      return;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath))
      return;

    if (db.getRootPath().equals(transientDBFilePath))
    {
      errorPopup("Transient DB is currently loaded.");
      return;
    }

    if (deleteTransientDB(false) == false)
      return;

    try
    {
      db.getHdbPath().copyTo(transientDBFilePath.resolve(db.getHdbPath().getNameOnly()) , false);

      FileUtils.copyDirectory(db.xmlPath().toFile(), transientDBFilePath.resolve(DEFAULT_XML_PATH).toFile());

      String command = "xcopy \"" + db.getRootPath() + "\" \"" + transientDBFilePath + "\" /t /e /y";

      ProcessBuilder processBuilder = new ProcessBuilder("cmd.exe", "/c", command);
      processBuilder.inheritIO();  // Redirects output to console
      Process process = processBuilder.start();
      process.waitFor(); // Wait for process to complete
    }
    catch (IOException | InterruptedException e)
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

    return new FilePath(files[0]);
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

  @FXML private void btnLinkGenBrowseClick()
  {
    DirectoryChooser dirChooser = new DirectoryChooser();

    FilePath folderPath = new FilePath(tfLinkGenParent.getText());

    if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
    {
      if (db.isOnline())
        folderPath = db.getRootPath().getParent();

      if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
        folderPath = new FilePath(userWorkingDir());
    }

    dirChooser.setInitialDirectory(folderPath.toFile());

    dirChooser.setTitle("Select folder where log files will be saved");

    FilePath filePath = showDirDialog(dirChooser);

    if (FilePath.isEmpty(filePath))
      return;

    tfLinkGenParent.setText(filePath.toString());
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

}
