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
import org.hypernomicon.fts.PDFJSTextExtractor;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Folder;
import org.hypernomicon.previewWindow.BrowserEngine;
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

  @FXML private Button btnIndexStats, btnRetryFailed, btnRebuildIndex;
  @FXML private CheckBox chkFTSDisabledForThisDb, chkFTSIndexingEnabled, chkNoExtractionTimeout;
  @FXML private Spinner<Integer> spnThreadCount, spnExtractionTimeout;
  @FXML private TableView<HyperTableRow> tvExcludedFolders;
  @FXML private TextField tfExcludedFileMasks;

  private HyperTable htFolders;
  private IntSpinnerWrapper extractionTimeoutWrapper;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init(boolean noDB)
  {
    initCheckBox(app.prefs, chkFTSIndexingEnabled, FTS_INDEXING_ENABLED, true, null);

    int maxThreads = Runtime.getRuntime().availableProcessors(),
        stored = app.prefs.getInt(FTS_THREAD_COUNT, -1),
        initial = stored < 0 ? Math.max(maxThreads - 2, 1) : stored;

    IntSpinnerWrapper.of(spnThreadCount, 1, maxThreads, initial, () -> app.prefs.putInt(FTS_THREAD_COUNT, spnThreadCount.getValue()));

    int defaultMin = PDFJSTextExtractor.DEFAULT_EXTRACTION_TIMEOUT_MINUTES,
        storedMin  = app.prefs.getInt(FTS_EXTRACTION_TIMEOUT, defaultMin),
        initialMin = (storedMin <= 0) ? defaultMin : Math.max(storedMin, 5);

    extractionTimeoutWrapper = IntSpinnerWrapper
        .withNone(spnExtractionTimeout, 5, 240, initialMin, chkNoExtractionTimeout, 0, storedMin <= 0, this::persistExtractionTimeout)
        .stepBy(5);  // the timeout steps by 5 minutes

    setToolTip(spnExtractionTimeout, "A file that takes longer than this to index is skipped and recorded as failed (see Retry Failed). Raise the limit, or choose No limit, to give slow files more time.");
    setToolTip(chkNoExtractionTimeout, "Index every file no matter how long it takes (no time limit).");

    setToolTip(btnIndexStats, "Display information about the current full-text search index including files that couldn't be indexed");

    btnIndexStats.setOnAction(event ->
    {
      FullTextIndexer indexer = db.isLoaded() ? db.getFullTextIndexer() : null;

      if (indexer == null)
        infoPopup("No index is currently active.");
      else
        longMessagePopup("Index Statistics", indexer::getStatistics);
    });

    setToolTip(btnRetryFailed, "Re-attempt to index files that previously were not able to be indexed. Click [Index Statistics] to see the list of files.");

    btnRetryFailed.setOnAction(event ->
    {
      FullTextIndexer indexer = db.isLoaded() ? db.getFullTextIndexer() : null;

      if (indexer == null)
      {
        errorPopup("No index is currently active.");
        return;
      }

      if (indexer.isIndexingEnabled() == false)
      {
        infoPopup("Full-text indexing is not currently running, so failed entries cannot be retried.");
        return;
      }

      int count = indexer.retryFailed();

      if (count == 0)
      {
        infoPopup("There are no failed or abandoned entries to retry.");
        return;
      }

      // The retry runs on the background indexing thread and is deferred until the
      // initial build is complete, so tell the user which of the two is happening

      String entriesStr = count + " failed or abandoned entr" + (count == 1 ? "y" : "ies");

      infoPopup((indexer.getState() == FullTextIndexer.IndexerState.BUILDING ?
        "An index build is currently in progress; it re-attempts previously failed entries as it goes. " +
        "Once it completes, indexing will be retried for whichever of the " + entriesStr + " still failed."
      :
        "Retrying full-text indexing for " + entriesStr + '.') + "\n\n" +
        "Tip: if a file failed because it took too long to index, raising the maximum time (or choosing No limit) may help it succeed.");
    });

    setToolTip(btnRebuildIndex, "Wipe the current full-text index and rebuild it from scratch");

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

    // Per-database "disable FTS on this computer" switch. Disabling deletes the index (at next startup), so
    // checking it requires confirmation. With no database loaded there is no "this database" to act on.

    if (noDB)
      chkFTSDisabledForThisDb.setDisable(true);
    else
      initCheckBox(app.prefs.node(FTS_DISABLED_DBS), chkFTSDisabledForThisDb, db.getDBID(), false, disabled ->
      {
        if (disabled && (confirmDialog("""
            Disabling full-text search for this database will DELETE its full-text index on this computer the next time the application starts.
            If you re-enable it later, the entire index is rebuilt from scratch, which can take a long time for a large database.
            If you only want to stop updating the index (keeping it searchable), uncheck "Enable full-text search indexing" instead.

            Continue?""", false) == false))
          chkFTSDisabledForThisDb.setSelected(false);
      });

    // When FTS is being disabled for this database, the indexing sub-options below are moot.

    chkFTSIndexingEnabled .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    spnThreadCount        .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    btnIndexStats         .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    chkNoExtractionTimeout.disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    spnExtractionTimeout  .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty().or(chkNoExtractionTimeout.selectedProperty()));

    if (jxBrowserDisabled)
    {
      // Without the browser engine (missing license key or startup failure), indexing cannot
      // run at all: retrying or rebuilding would only record every PDF as a failed entry.
      // The explanatory tooltip goes on the still-enabled checkbox because JavaFX does not
      // show tooltips on disabled controls.

      disableAll(btnRetryFailed, btnRebuildIndex);

      setToolTip(chkFTSIndexingEnabled, BrowserEngine.licenseKeyIsMissing()
        ? "The JxBrowser license key is not present, so the browser engine is unavailable and indexing cannot run regardless of this setting."
        : "The browser engine failed to initialize, so indexing cannot run this session regardless of this setting.");
    }
    else
    {
      btnRetryFailed .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
      btnRebuildIndex.disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    }

    if (noDB)
    {
      // The excluded patterns/folders are per-database settings read from (and saved to) the
      // current database's index. With no database loaded there is nothing to edit, so disable
      // them rather than leaving them active-but-empty: save() discards any changes when noDB.

      disableAll(tfExcludedFileMasks, tvExcludedFolders);

      return;
    }

    tfExcludedFileMasks.disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());
    tvExcludedFolders  .disableProperty().bind(chkFTSDisabledForThisDb.selectedProperty());

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

  /** Persists the per-computer PDF extraction timeout: 0 when "No timeout" is checked (read as infinite
   *  by {@code PDFJSTextExtractor.extractionTimeoutSeconds()}), otherwise the spinner's minutes value. */
  private void persistExtractionTimeout()
  {
    app.prefs.putInt(FTS_EXTRACTION_TIMEOUT, extractionTimeoutWrapper.getValue());
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
