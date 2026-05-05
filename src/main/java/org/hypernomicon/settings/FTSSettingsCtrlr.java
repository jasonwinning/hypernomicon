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
import static org.hypernomicon.Const.PrefKey.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.settings.SettingsDlgCtrlr.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.fts.FullTextIndexer;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.view.tableCells.ButtonCell.ButtonAction;
import org.hypernomicon.view.wrappers.*;

import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.stage.DirectoryChooser;

//---------------------------------------------------------------------------

public class FTSSettingsCtrlr implements SettingsControl
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnIndexStats, btnRebuildIndex;
  @FXML private CheckBox chkFTSIndexingEnabled;
  @FXML private Spinner<Integer> spnThreadCount;
  @FXML private TableView<HyperTableRow> tvExcludedFolders;
  @FXML private TextField tfExcludedFileMasks;

  private HyperTable htFolders;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    initCheckBox(app.prefs, chkFTSIndexingEnabled, FTS_INDEXING_ENABLED, true, null);

    int maxThreads = Runtime.getRuntime().availableProcessors(),
        stored = app.prefs.getInt(FTS_THREAD_COUNT, -1),
        initial = stored < 0 ? Math.max(maxThreads - 2, 1) : stored;

    IntSpinnerWrapper.of(spnThreadCount, 1, maxThreads, initial, () -> app.prefs.putInt(FTS_THREAD_COUNT, spnThreadCount.getValue()));

    btnIndexStats.setOnAction(event ->
    {
      FullTextIndexer indexer = db.isLoaded() ? db.getFullTextIndexer() : null;

      if (indexer == null)
        infoPopup("No index is currently active.");
      else
        longMessagePopup("Index Statistics", indexer.getStatistics());
    });

    btnRebuildIndex.setOnAction(event ->
    {
      FullTextIndexer indexer = db.isLoaded() ? db.getFullTextIndexer() : null;

      if (indexer == null)
      {
        errorPopup("No index is currently active.");
        return;
      }

      if (confirmDialog("""
        This will wipe the full-text index and rebuild it from scratch. \
        The rebuild may take a long time depending on the size of your database.

        Proceed?""", false))
      {
        indexer.rebuildIndex();
        infoPopup("Index rebuild started.");
      }
    });

    if (noDB) return;

    htFolders = new HyperTable(tvExcludedFolders, 0, true, "");

    htFolders.addLabelCol(hdtFolder);

    htFolders.addCustomActionCol(-1, "...", (row, colNdx) ->
    {
      DirectoryChooser dirChooser = new DirectoryChooser();
      dirChooser.setTitle("Select Folder to Exclude");
      dirChooser.setInitialDirectory(db.getRootPath().toFile());

      FilePath filePath = showDirDialog(dirChooser);

      if (FilePath.isEmpty(filePath)) return;

      if (filePath.isUnderDbRoot() == false)
      {
        errorPopup("The selected folder is not inside the database root folder.");
        return;
      }

      HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, false);

      if (folder == null)
      {
        errorPopup("Unable to resolve the selected folder.");
        return;
      }

      String relativePath = db.getRootPath().relativize(folder.filePath()).toString();
      row.setCellValue(0, folder, relativePath);

    }).setButtonTooltip(ButtonAction.baCustom, "Browse for folder to exclude");

    htFolders.addCustomActionCol(0, (row, btn) ->
    {
      HDT_Folder folder = row.getRecord(0);

      if ((folder == null) || FilePath.isEmpty(folder.filePath()))
      {
        btn.setVisible(false);
        return;
      }

      btn.setVisible(true);

      ImageView iv = imgViewFromRelPath("resources/images/rocket-fly.png");
      iv.setFitHeight(16);
      iv.setPreserveRatio(true);
      btn.setGraphic(iv);
    },
    (row, colNdx) ->
    {
      HDT_Folder folder = row.getRecord(0);

      if ((folder != null) && (FilePath.isEmpty(folder.filePath()) == false))
        launchFile(folder.filePath());

    }).setButtonTooltip(ButtonAction.baCustom, "Open folder in system explorer");

    htFolders.addRemoveMenuItem();
    htFolders.addChangeOrderMenuItem();

    loadFromIndexer();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void save(boolean noDB)
  {
    if (noDB || (htFolders == null)) return;

    saveToIndexer();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFromIndexer()
  {
    htFolders.clear();

    FullTextIndexer indexer = db.getFullTextIndexer();

    if (indexer == null) return;

    tfExcludedFileMasks.setText(indexer.getExcludedFileMasks());

    for (FilePath folderPath : indexer.getExcludedPaths())
    {
      if (folderPath.exists() == false) continue;

      HDT_Folder folder = HyperPath.getFolderFromFilePath(folderPath, false);

      if (folder == null) continue;

      String relativePath = db.getRootPath().relativize(folder.filePath()).toString();
      HyperTableRow row = htFolders.newDataRow();
      row.setCellValue(0, folder, relativePath);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void saveToIndexer()
  {
    FullTextIndexer indexer = db.getFullTextIndexer();

    if (indexer == null) return;

    indexer.setExcludedFileMasks(tfExcludedFileMasks.getText());

    List<FilePath> paths = new ArrayList<>();

    htFolders.dataRows().forEach(row ->
    {
      HDT_Folder folder = row.getRecord(0);

      if ((folder != null) && (FilePath.isEmpty(folder.filePath()) == false))
        paths.add(folder.filePath());
    });

    indexer.setExcludedPaths(paths);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
