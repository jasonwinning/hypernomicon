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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.bib.LibraryWrapper.LibraryType.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.Map;
import java.util.function.Consumer;
import java.util.prefs.Preferences;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.mutable.MutableBoolean;
import org.hypernomicon.bib.LibraryWrapper.LibraryType;
import org.hypernomicon.util.filePath.FilePath;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextField;
import javafx.scene.control.Toggle;
import javafx.scene.control.ToggleGroup;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

public class TestConsoleDlgCtrlr extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TextField tfParent, tfFolderName;
  @FXML private Button btnFromExisting, btnClose, btnCloseDB;
  @FXML private RadioButton rbZotero, rbMendeley;
  @FXML private ToggleGroup tgLink;

  private final Map<Toggle, LibraryType> toggleToLibraryType;

//---------------------------------------------------------------------------

  public TestConsoleDlgCtrlr()
  {
    super("TestConsoleDlg", appTitle + " Test Console", true);

    initTextField(app.prefs, tfParent    , PREF_KEY_TRANSIENT_TEST_PARENT_PATH, "", null);
    initTextField(app.prefs, tfFolderName, PREF_KEY_TRANSIENT_TEST_FOLDER_NAME, "", null);

    enableAllIff(db.isLoaded(), btnFromExisting, btnCloseDB);

    toggleToLibraryType = Map.of(rbZotero, ltZotero, rbMendeley, ltMendeley);

    setToolTip(btnClose, "Close this window");
  }

//---------------------------------------------------------------------------

  @FXML private void btnFromScratchClick () { createTransientTestDB(true ); }
  @FXML private void btnFromExistingClick() { createTransientTestDB(false); }

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private void btnCloseDB()
  {
    dialogStage.hide();

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
    dialogStage.hide();

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
      if (db.isLoaded())
        folderPath = db.getRootPath().getParent();

      if (FilePath.isEmpty(folderPath) || (folderPath.exists() == false))
        folderPath = new FilePath(userWorkingDir());
    }

    dirChooser.setInitialDirectory(folderPath.toFile());

    dirChooser.setTitle("Select parent folder of transient test database folder");

    FilePath filePath = showDirDialog(dirChooser);

    if (FilePath.isEmpty(filePath))
      return;

    if (db.isLoaded() && db.getRootPath().isSubpath(filePath))
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

    FilePath transientDBFilePath = safeStr(folderNameStr).isBlank() ? getParentFilePath() : getTransientDBFilePath(false, false, null);

    if (FilePath.isEmpty(transientDBFilePath)) return;

    launchFile(transientDBFilePath.exists() ? transientDBFilePath : transientDBFilePath.getParent());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createTransientTestDB(boolean fromScratch)
  {
    boolean restartWatcher = folderTreeWatcher.stop();
    FilePath transientDBFilePath = getTransientDBFilePath(true, false, null);
    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();

    if (FilePath.isEmpty(transientDBFilePath)) return;

    dialogStage.hide();

    Platform.runLater(() ->
    {
      if (transientDBFilePath.exists())
      {
        if (db.isLoaded() && db.getRootPath().equals(transientDBFilePath))
        {
          if (fromScratch == false)
          {
            errorPopup("The database located in the transient test folder is currently loaded.");
            new TestConsoleDlgCtrlr().showModal();
            return;
          }

          if (confirmDialog("The currently loaded database will be deleted. Continue?") == false)
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

      if (fromScratch && db.isLoaded() && (ui.close(true) == false))
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

  @FXML private void btnDeleteClick()
  {
    MutableBoolean nonEmptyWithNoHdbFile = new MutableBoolean(false);

    FilePath transientDBFilePath = getTransientDBFilePath(true, true, nonEmptyWithNoHdbFile);

    if (FilePath.isEmpty(transientDBFilePath)) return;

    if (transientDBFilePath.exists() == false)
    {
      falseWithErrorPopup("Path \"" + transientDBFilePath + "\" does not exist.", tfFolderName);
      return;
    }

    String prompt = nonEmptyWithNoHdbFile.isTrue() ?
      "Path \"" + transientDBFilePath + "\" is a non-empty directory with no HDB file. Are you sure you want to delete it?"
    :
      "Delete folder \"" + transientDBFilePath + "\" and everything in it?";

    if (confirmDialog(prompt) == false)
      return;

    try
    {
      transientDBFilePath.deleteDirectory(true);
    }
    catch (IOException e)
    {
      errorPopup("One or more files were not deleted. Reason: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FilePath getParentFilePath()
  {
    String parentStr = tfParent.getText();

    if (safeStr(parentStr).isBlank())
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

    if (safeStr(folderNameStr).isBlank())
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

    if (db.isLoaded() && (deleting || (db.getRootPath().equals(transientDBFilePath) == false)) && db.getRootPath().isSubpath(transientDBFilePath))
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

}
