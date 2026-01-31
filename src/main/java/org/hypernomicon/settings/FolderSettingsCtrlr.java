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

package org.hypernomicon.settings;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.settings.SettingsDlgCtrlr.SettingsControl;
import org.hypernomicon.util.file.FilePath;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

public class FolderSettingsCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TextField tfPapers, tfBooks, tfUnentered, tfPictures, tfTopical, tfMiscFiles, tfResults;
  @FXML private Button btnPapers, btnBooks, btnUnentered, btnPictures, btnTopical, btnMiscFiles, btnResults;
  @FXML private RadioButton rbBooks;

//---------------------------------------------------------------------------

  @Override public void save(boolean noDB) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    if (noDB) return;

    initRow(FolderIDPrefKey.PICTURES  , tfPictures , btnPictures );
    initRow(FolderIDPrefKey.BOOKS     , tfBooks    , btnBooks    );
    initRow(FolderIDPrefKey.PAPERS    , tfPapers   , btnPapers   );
    initRow(FolderIDPrefKey.RESULTS   , tfResults  , btnResults  );
    initRow(FolderIDPrefKey.UNENTERED , tfUnentered, btnUnentered);
    initRow(FolderIDPrefKey.MISC_FILES, tfMiscFiles, btnMiscFiles);
    initRow(FolderIDPrefKey.TOPICAL   , tfTopical  , btnTopical  );

    rbBooks.setSelected(db.prefs.getBoolean(PrefKey.THESIS_FOLDER_IS_BOOKS, false));

    rbBooks.selectedProperty().addListener((obs, ov, nv) -> db.prefs.putBoolean(PrefKey.THESIS_FOLDER_IS_BOOKS, nv));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateTextField(TextField tf, String prefKey)
  {
    tf.setText(db.getRootPath().relativize(db.getSpecialFolder(prefKey).filePath()).toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initRow(String prefKey, TextField tf, Button btn)
  {
    tf.setEditable(false);

    updateTextField(tf, prefKey);

    btn.setOnAction(event ->
    {
      DirectoryChooser dirChooser = new DirectoryChooser();

      HDT_Folder curFolder = db.getSpecialFolder(prefKey);

      dirChooser.setInitialDirectory(curFolder.filePath().toFile());
      dirChooser.setTitle("Select Folder");

      FilePath filePath = showDirDialog(dirChooser);

      if (FilePath.isEmpty(filePath)) return;

      HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, true);

      if (folder == null)
      {
        errorPopup("You must choose a subfolder of the main database folder.");
        return;
      }

      if (folder == curFolder)
        return;

      if (prefKey.equals(FolderIDPrefKey.UNENTERED))
      {
        if (folder.isSpecial(false))
        {
          errorPopup("That folder is already reserved for a different purpose.");
          return;
        }
      }
      else if (prefKey.equals(FolderIDPrefKey.RESULTS))
      {
        if (folder.isSpecial(true))
        {
          errorPopup("That folder or a subfolder is already reserved for a different purpose.");
          return;
        }
      }
      else
      {
        if ((folder == db.getXmlFolder()) || (folder.getID() == HyperDB.ROOT_FOLDER_ID) || (folder == db.getResultsFolder()) ||
            ancestorIsResultsFolder(folder) || (folder == db.getUnenteredFolder()))
        {
          errorPopup("That folder is already reserved for a different purpose.");
          return;
        }
      }

      db.setSpecialFolder(prefKey, folder);
      updateTextField(tf, prefKey);
      ui.update();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean ancestorIsResultsFolder(HDT_Folder folder)
  {
    HDT_Folder parentFolder = folder.parentFolder();

    if ((parentFolder == null) || (parentFolder.getID() == HyperDB.ROOT_FOLDER_ID)) return false;

    if (parentFolder == db.getResultsFolder()) return true;

    return ancestorIsResultsFolder(parentFolder);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
