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
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.*;
import java.time.LocalDateTime;
import java.util.*;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

import org.apache.commons.lang3.mutable.MutableBoolean;

import org.hypernomicon.App;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.bib.*;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.bib.zotero.ZoteroWrapper;
import org.hypernomicon.dialogs.NewArgDlgCtrlr;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.fileManager.FileManagerTestRunner;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;
import org.hypernomicon.util.file.deletion.FileDeletion.DeletionResult;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

/**
 * The Test Console: the launch surface for the in-app test tooling, opened
 * from a main-window menu item visible only when the application is running
 * under a debugger. This controller owns the window and the tabs that share
 * the transient test database's location (DB Creation, UI Tests) plus a few
 * small ones; the larger tools are tabs loaded from their own FXML files, each
 * with its own {@link TestConsoleTab} controller, the way the Settings dialog
 * loads its pages.
 */
public class TestConsoleDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** A tab of the console loaded from its own FXML file. The console calls
   *  {@code init} once the controls are injected; a tab that needs the
   *  console (the transient-database helpers) keeps the reference. */
  interface TestConsoleTab { void init(TestConsoleDlgCtrlr console); }

//---------------------------------------------------------------------------

  @FXML private Button btnFromExisting, btnClose, btnCloseDB, btnSaveRefMgrSecrets, btnRemoveRefMgrSecrets, btnUseMendeleyID,
                       btnZoteroItemTemplates, btnZoteroCreatorTypes, btnLinkGenBefore, btnLinkGenAfter, btnTermsTabTests,
                       btnSetupFMTest, btnRunFMTest;
  @FXML private RadioButton rbZotero, rbMendeley;
  @FXML private Tab tabDeletion, tabWebButtons, tabLinkGen, tabFtsDiagnostics, tabPdfExtraction;
  @FXML private TextField tfParent, tfFolderName, tfRefMgrUserID;
  @FXML private ToggleGroup tgLink;

  private final Map<Toggle, LibraryType> toggleToLibraryType;

//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  public TestConsoleDlgCtrlr()
  {
    super("testTools/TestConsoleDlg", appTitle + " Test Console", true, true);

    initTextField(app.prefs, tfParent    , PrefKey.TRANSIENT_TEST_PARENT_PATH, "", null);
    initTextField(app.prefs, tfFolderName, PrefKey.TRANSIENT_TEST_FOLDER_NAME, "", null);

    enableAllIff(db.isOnline(), btnFromExisting, btnCloseDB, btnZoteroItemTemplates, btnZoteroCreatorTypes, btnTermsTabTests, tabLinkGen);

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

    initTab(tabDeletion      , "DeletionTests"    );
    initTab(tabWebButtons    , "WebButtonsTest"   );
    initTab(tabFtsDiagnostics, "FtsDiagnostics"   );
    initTab(tabPdfExtraction , "PdfExtractionTest");
  }

//---------------------------------------------------------------------------

  @FXML private void btnFromScratchClick () { createTransientTestDB(true ); }
  @FXML private void btnFromExistingClick() { createTransientTestDB(false); }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initTab(Tab tab, String fxmlName)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource("testTools/" + fxmlName + ".fxml"));
      AnchorPane ap = loader.load();
      tab.setContent(ap);
      TestConsoleTab ctrlr = loader.getController();
      ctrlr.init(this);
    }
    catch (IOException e)
    {
      logThrowable(e);
    }
  }

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

  /** Binds a text field to a preference: shows the stored value and stores
   *  every edit. Shared with the tab controllers. */
  static void initTextField(Preferences prefs, TextField tf, String prefKey, String defValue, Consumer<String> handler)
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

  /** Empties the transient test folder after confirmation.
   *  @return false if the folder could not be validated, the user declined, or the deletion was aborted */
  boolean clearTransientDB()
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
  FilePath getTransientDBFilePath(boolean modifying, boolean deleting, MutableBoolean nonEmptyWithNoHdbFile)
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

  /**
   * Whether the database in the transient test folder is the loaded one, which
   * the tests that modify the loaded database require. Explains why not when it
   * is not (a folder that fails validation has already been explained); silent
   * when no database is loaded, since those tests' buttons are disabled then.
   */
  boolean requireTransientDBLoaded()
  {
    if (db.isOffline()) return false;

    FilePath transientDBFilePath = getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath)) return false;

    if (db.getRootPath().equals(transientDBFilePath)) return true;

    errorPopup("This can only be done when the transient DB is loaded.");
    return false;
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

  @FXML private void setupFileManagerTest()
  {
    if (requireTransientDBLoaded() == false) return;

    FileManagerTestRunner.setupTestFiles(db.getRootPath("_test_fm"));

    btnSetupFMTest.setDisable(true);
    btnRunFMTest.setDisable(false);

    infoPopup("Test files created. Wait for all files to sync before running tests.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void fileManagerTest()
  {
    if (requireTransientDBLoaded() == false) return;

    btnRunFMTest.setDisable(true);

    FileManagerTestRunner.runTests(db.getRootPath("_test_fm"), () -> btnSetupFMTest.setDisable(false));
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

  @FXML private void btnNewArgDlgTestClick()
  {
    stage.hide();

    Platform.runLater(NewArgDlgCtrlr::runTests);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
