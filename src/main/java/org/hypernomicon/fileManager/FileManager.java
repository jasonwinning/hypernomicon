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

package org.hypernomicon.fileManager;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.mainText.MainTextUtil.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.util.*;
import java.util.Map.Entry;
import java.util.function.Predicate;

import org.hypernomicon.HyperTask;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.dialogs.*;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_RecordWithDescription;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutAction;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.FilePathSet;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.tableCells.ReadOnlyCell;
import org.hypernomicon.view.wrappers.*;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.input.*;
import javafx.scene.text.Text;
import javafx.scene.web.WebView;

//---------------------------------------------------------------------------

public final class FileManager extends NonmodalWindow
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private Button btnForward, btnBack, btnCut, btnCopy, btnPaste, btnDelete, btnRename, btnRefresh,
                       btnMainWindow, btnPreviewWindow, btnNewFolder;
  @FXML private Label lblDescPlaceholder;
  @FXML private SplitPane spMain, spFiles, spRecords;
  @FXML private TreeView<FileRow> treeView;
  @FXML private TableView<FileRow> fileTV;
  @FXML private TableView<HyperTableRow> recordTV;
  @FXML private WebView webView;

  private static final String dialogTitle = "File Manager";

  private static FileManager instance;

  private final MenuItemSchema<HDT_RecordWithPath, FileRow> pasteMenuItem;

  private List<AbstractEntityWithPath> dragPaths = null;
  private List<? extends AbstractEntityWithPath> markedRows = null;
  private FilePath srcPathToHilite = null;
  private boolean clipboardCopying, needRefresh = false, alreadyRefreshing = false, suppressNeedRefresh = false, programmaticSelectionChange = false;
  private HDT_Folder curFolder;

  public final FolderTreeWrapper folderTree;
  private final FileTable fileTable;
  private final HyperTable recordTable;
  private final FolderHistory history;

//---------------------------------------------------------------------------

  FileRow getFolderRow()                { return nullSwitch(curFolder, null, folder -> folderTree.getRowsForRecord(folder).getFirst()); }
  public static void clearHistory()     { instance.history.clear(); }

  public static void setNeedRefresh()   { if ((instance != null) && (instance.suppressNeedRefresh == false)) instance.needRefresh = true; }

  public static void close(boolean exitingApp) { close(instance, exitingApp); }

  private List<? extends AbstractEntityWithPath> getSrcPaths(boolean dragging) { return dragging ? dragPaths : markedRows; }

//---------------------------------------------------------------------------

  public static FileManager instance()
  {
    if (instance == null) instance = new FileManager();

    return instance;
  }

//---------------------------------------------------------------------------

  private FileManager()
  {
    super("fileManager/FileManager", dialogTitle, PrefKey.FM_WINDOW_X, PrefKey.FM_WINDOW_Y, PrefKey.FM_WINDOW_WIDTH, PrefKey.FM_WINDOW_HEIGHT);

    history = new FolderHistory(btnForward, btnBack);
    fileTable = new FileTable(fileTV, this);
    folderTree = new FolderTreeWrapper(treeView, fileTable, history);
    recordTable = new HyperTable(recordTV, 1, false, TablePrefKey.FM_RECORDS, this);

    initContainers();

    fileTable.addContextMenuItem("Launch", Predicate.not(FileRow::isDirectory), fileRow -> launchFile(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Show in system explorer", fileRow -> highlightFileInExplorer(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Copy path to clipboard", fileRow -> copyToClipboard(fileRow.getFilePath().toString()));
    fileTable.addContextMenuItem("Rename", this::rename);

    fileTable.addContextMenuItem("Assign to new Misc. File record", fileRow -> fileRow.getRecord() == null, fileRow ->
    {
      ui.importMiscFile(fileRow, null);
      refresh();
    });

    fileTable.addContextMenuItem("Assign to new or existing Work record", fileRow -> fileRow.getRecord() == null, fileRow ->
    {
      ui.importWorkFile(null, fileRow.getFilePath(), true);
      refresh();
    });

    fileTable.addContextMenuItem("Assign to note record", FileRow::isDirectory, dirRow ->
    {
      HDT_Folder folder = dirRow.getRecord();
      ui.treeSelector.reset(folder, false);
      ui.treeSelector.addTargetType(hdtNote);
      ui.goToTreeRecord(folder.closestAncestorNote());
    });

    fileTable.addContextMenuItem("New folder under this folder...", FileRow::isDirectory, dirRow -> newFolder(dirRow.getRecord()));
    fileTable.addContextMenuItem("Cut", fileRow -> cutCopy(fileRow, false));
    fileTable.addContextMenuItem("Copy", fileRow -> cutCopy(fileRow, true));
    pasteMenuItem = fileTable.addContextMenuItem("Paste into this folder", FileRow::isDirectory, dirRow -> paste(dirRow, clipboardCopying, false));
    fileTable.addContextMenuItem("Delete", this::delete);

    btnCut      .setOnAction(event -> cutCopy(null, false));
    btnCopy     .setOnAction(event -> cutCopy(null, true));
    btnPaste    .setOnAction(event -> paste(null, clipboardCopying, false));
    btnDelete   .setOnAction(event -> delete(null));
    btnNewFolder.setOnAction(event -> newFolder(curFolder));

    btnBack   .setOnAction(event -> userRequestsToGoBackward());
    btnForward.setOnAction(event -> userRequestsToGoForward ());
    btnRefresh.setOnAction(event -> pruneAndRefresh(true));
    btnRename .setOnAction(event -> rename(null));

    btnMainWindow   .setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnPreviewWindow.setOnAction(event -> PreviewWindow.show(pvsManager));
    btnPaste.setDisable(true);
    pasteMenuItem.disabled = true;

    onShown = FileManager::refresh;

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus() || (Boolean.TRUE.equals(newValue) == false))
        return;

      if (needRefresh) refresh();
    });

    stage.addEventFilter(MouseEvent.MOUSE_CLICKED, event ->
    {
      if      (event.getButton() == MouseButton.BACK   ) Platform.runLater(this::userRequestsToGoBackward);
      else if (event.getButton() == MouseButton.FORWARD) Platform.runLater(this::userRequestsToGoForward );
      else                                               return;

      event.consume();
    });

    webView.setOnDragOver   (Event::consume);
    webView.setOnDragDropped(Event::consume);

    webView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));

    registerShortcuts();

    app.shortcuts.addListener((obs, ov, nv) -> registerShortcuts());

    recordTable.addDefaultMenuItems();

    setToolTip(btnBack         , "Previous folder in history");
    setToolTip(btnForward      , "Next folder in history");
    setToolTip(btnCut          , "Cut");
    setToolTip(btnCopy         , "Copy");
    setToolTip(btnPaste        , "Paste");
    setToolTip(btnDelete       , "Delete");
    setToolTip(btnNewFolder    , "Create new folder");
    setToolTip(btnRename       , "Rename selected file or folder");
    setToolTip(btnRefresh      , "Refresh");
    setToolTip(btnMainWindow   , "Return to main application window");
    setToolTip(btnPreviewWindow, "View selected record/file in Preview Window");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initContainers()
  {
    folderTree.getTreeModel().addParentChildRelation(rtParentFolderOfFolder, true);

    recordTable.addCol(hdtNone, ctIncremental);
    recordTable.addLabelCol(hdtNone)
      .setTextOverrunStyle(OverrunStyle.LEADING_ELLIPSIS)
      .setCellToolTipHndlr(row -> makeTooltip(row.getText(1)));

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue == oldValue)) return;

      FilePath filePath = Objects.requireNonNull(newValue.getValue().getFilePath(),
        "Folder/record sync invariant violated: Selected folder TreeItem has no file path (HDT_Folder record expired)");

      HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath, true);

      curFolder = folder;

      if (programmaticSelectionChange == false)
        history.add(new FolderHistoryItem(newValue.getValue(), null, null));

      fileTable.update(folder, newValue);
      updateRecordTable(null, false);
      stage.setTitle(dialogTitle + " - " + folder.filePath());
    });

    fileTV.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
      {
        PreviewWindow.clearPreview(pvsManager);
        return;
      }

      if (newValue == oldValue) return;

      PreviewWindow.disablePreviewUpdating = true;
      updateRecordTable(newValue, false);
      PreviewWindow.disablePreviewUpdating = false;

      history.updateCurrent(new FolderHistoryItem(folderTree.getSelectionModel().getSelectedItem().getValue(), newValue, null));

      if (selectNonBlankRecordRow() == false)
        PreviewWindow.setPreview(pvsManager, newValue.getFilePath());
    });

    recordTable.setOnShowMore(() -> updateRecordTable(fileTV.getSelectionModel().getSelectedItem(), true));

    fileTV.setRowFactory(thisTV ->
    {
      TableRow<FileRow> row = new TableRow<>();

      row.setOnMouseClicked(mouseEvent ->
      {
        if (mouseEvent.getButton().equals(MouseButton.PRIMARY) && (mouseEvent.getClickCount() == 2))
        {
          nullSwitch(row.getItem(), fileRow ->
          {
            if (fileRow.isDirectory())
              folderTree.getSelectionModel().select(fileRow.getTreeItem());
            else
              launchFile(fileRow.getFilePath());
          });
        }
      });

      fileTable.setupDragHandlers(row);

      row.itemProperty().addListener((ob, oldValue, newValue) ->
        row.setContextMenu(fileTable.createContextMenu(newValue, fileTable.getContextMenuSchemata())));

      return row;
    });

    recordTV.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == oldValue) return;

      if (newValue != null)
      {
        HDT_Record record = HyperTableCell.getRecord(newValue.getCell(1));
        history.updateCurrent(new FolderHistoryItem(folderTree.selectedItem().getValue(), fileTV.getSelectionModel().getSelectedItem(), record));

        if (record != null)
        {
          if (record.getType() == hdtWorkFile)
          {
            HDT_WorkFile workFile = (HDT_WorkFile) record;
            if (workFile.works.isEmpty() == false)
              record = workFile.works.getFirst();
          }

          showRecordDesc(record);

          setPreviewFromRecordTable();

          return;
        }
      }

      setPreviewFromRecordTable();

      clearRecordDesc();
    });

    recordTV.setPlaceholder(new Text("The selected file or folder has no associated records."));

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      HDT_Record record = recordTable.selectedRecord();
      if (record == null) return;

      handleJSEvent(prepHtmlForDisplay(HDT_Record.getDescHtml(record)), webView.getEngine());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    webViewAddZoom(webView, ZoomPrefKey.FILEMGR);

    webView.getEngine().setUserStyleSheetLocation(cssStrToDataURI(EMPTY_FONT_CSS));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerShortcuts()
  {
    Scene scene = stage.getScene();

    // Clear old accelerators first

    scene.getAccelerators().clear();

  //---------------------------------------------------------------------------

    // Hard-coded shortcuts

    scene.getAccelerators().putAll(IS_OS_MAC ? Map.of
    (
      new KeyCodeCombination(KeyCode.LEFT , KeyCombination.SHORTCUT_DOWN), () -> Platform.runLater(this::userRequestsToGoBackward),
      new KeyCodeCombination(KeyCode.RIGHT, KeyCombination.SHORTCUT_DOWN), () -> Platform.runLater(this::userRequestsToGoForward )
    )
    : Map.of
    (
      new KeyCodeCombination(KeyCode.LEFT , KeyCombination.ALT_DOWN     ), () -> Platform.runLater(this::userRequestsToGoBackward),
      new KeyCodeCombination(KeyCode.RIGHT, KeyCombination.ALT_DOWN     ), () -> Platform.runLater(this::userRequestsToGoForward )
    ));

  //---------------------------------------------------------------------------

    // User-defined shortcuts

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToBibManager   , () -> { if (db.bibLibraryIsLinked()) BibManager.show(true); });
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToMainWindow   , () -> ui.windows.focusStage(ui.getStage()));
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToPreviewWindow, PreviewWindow::show);

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.PreviewRecord, () -> { if (btnPreviewWindow.isDisabled() == false) btnPreviewWindow.fire(); });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showRecordDesc(HDT_Record record)
  {
    lblDescPlaceholder.setVisible(false);
    MainTextWrapper.setReadOnlyHTML(HDT_Record.getDescHtml(record), webView.getEngine());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearRecordDesc()
  {
    webView.getEngine().loadContent("");
    lblDescPlaceholder.setVisible(true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearSrcRows(boolean dragging)
  {
    if (dragging)
      dragPaths = null;
    else
    {
      markedRows = null;
      btnPaste.setDisable(true);
      pasteMenuItem.disabled = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unchecked")
  private void setSrcPaths(List<? extends AbstractEntityWithPath> paths, boolean dragging)
  {
    if (dragging)
    {
      dragPaths = (List<AbstractEntityWithPath>) paths;
      return;
    }

    markedRows = paths;
    btnPaste.setDisable(false);
    pasteMenuItem.disabled = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cutCopy(FileRow srcRow, boolean copying)
  {
    moveCopy(getMarkedRows(srcRow), copying, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean moveCopy(List<? extends AbstractEntityWithPath> items, boolean copying, boolean dragging)
  {
    //---------------------------------------------------------------------------
    // Determine what is to be moved/copied
    //---------------------------------------------------------------------------

    if (items.isEmpty())
    {
      clearSrcRows(dragging);
      return false;
    }

  //---------------------------------------------------------------------------
  // See if any of it can be ruled out off the bat
  //---------------------------------------------------------------------------

    if (copying == false)
    {
      for (AbstractEntityWithPath dragItem : items)
        if (canCutRow(dragItem, false) == false)
        {
          clearSrcRows(dragging);
          return false;
        }
    }

    if (dragging == false)
      clipboardCopying = copying;

    setSrcPaths(items, dragging);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private enum PasteAnswer { check, overwriteNone, overwriteAll }

  private boolean doPasteChecks(FileRow destRow, Map<FilePath, FilePath> srcToDest, boolean copying, boolean dragging)
  {
    final FilePathSet srcSet = new FilePathSet(), destSet = new FilePathSet();
    srcPathToHilite = null;

    if (new HyperTask("BuildFileList", "Building list of files...", false) { @Override protected void call() throws HyperDataException
    {
      for (AbstractEntityWithPath pathItem : getSrcPaths(dragging))
      {
        if (FilePath.isEmpty(srcPathToHilite))
          srcPathToHilite = pathItem.getFilePath();

        if (pathItem.isDirectory())
          pathItem.getFilePath().addDirContentsToSet(srcSet);

        srcSet.add(pathItem.getFilePath());
      }

    }}.runWithProgressDialog() != State.SUCCEEDED) return false;

    if (new HyperTask("PasteChecks", "Performing checks...", srcSet.size()) { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      FilePath baseDir = getSrcPaths(dragging).getFirst().getFilePath().getParent();

      for (FilePath srcFilePath : srcSet)
      {
        incrementAndUpdateProgress();

        if (copying == false)
        {
          if (db.isProtectedFile(srcFilePath, true))
            throw new HyperDataException("Unable to move path \"" + srcFilePath + "\".");
        }

        FilePath destFilePath = destRow.getFilePath().resolve(baseDir.relativize(srcFilePath));

        if (srcFilePath.equals(destFilePath))
          throw new HyperDataException("Source and destination are the same.");

        if ((copying == false) && srcFilePath.isDirectory() && srcFilePath.contains(destFilePath))
          throw new HyperDataException("The destination folder is a subfolder of the source folder.");

        destSet.add(destFilePath);
        srcToDest.put(srcFilePath, destFilePath);
      }

      for (FilePath destFilePath : destSet)
      {
        if (srcSet.contains(destFilePath))
          throw new HyperDataException("Destination path \"" + destFilePath + "\" is also one of the source paths.");

        if (destFilePath.exists())
        {
          if (destFilePath.isDirectory())
            throw new HyperDataException("A folder already exists at destination path \"" + destFilePath + "\".");
          if (db.isProtectedFile(destFilePath, true))
            throw new HyperDataException("Cannot overwrite destination path: \"" + destFilePath + "\".");
        }
      }

    }}.runWithProgressDialog() != State.SUCCEEDED) return false;

    PasteAnswer paUnrelated = PasteAnswer.check,
                paRelated   = PasteAnswer.check;

    Iterator<Entry<FilePath, FilePath>> it = srcToDest.entrySet().iterator();

    while (it.hasNext())
    {
      Entry<FilePath, FilePath> entry = it.next();

      Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(entry.getValue());
      boolean isRelated = (set.isEmpty() == false) && set.stream().anyMatch(hyperPath -> hyperPath.getRecordType() != hdtNone);

      if ((isRelated == false) && entry.getValue().exists())
      {
        switch (paUnrelated)
        {
          case check:

            switch (seriesConfirmDialog("Okay to overwrite existing file \"" + entry.getValue() + "\"?"))
            {
              case mrNo:

                it.remove();
                break;

              case mrNoToAll:

                it.remove();
                paUnrelated = PasteAnswer.overwriteNone;
                break;

              case mrYesToAll:

                paUnrelated = PasteAnswer.overwriteAll;
                break;

              default:
                break;
            }

            break;

          case overwriteNone:

            it.remove();
            break;

          default:
            break;
        }
      }
      else if (isRelated) // destination file is associated with a database record
      {
        switch (paRelated)
        {
          case check:

            StringBuilder confirmMessage = new StringBuilder("The file \"" + entry.getValue() + "\" is assigned to the following record(s):\n\n");

            set.stream().filter(hyperPath -> hyperPath.getRecordType() != hdtNone).forEach(hyperPath ->
              confirmMessage.append(getTypeName(hyperPath.getRecord().getType())).append(" ID ")
                            .append(hyperPath.getRecord().getID()).append(": ")
                            .append(hyperPath.getRecord().defaultChoiceText()).append('\n'));

            confirmMessage.append("\nOkay to overwrite the file with \"").append(entry.getKey()).append("\"?");

            switch (seriesConfirmDialog(confirmMessage.toString()))
            {
              case mrNo:

                it.remove();
                break;

              case mrNoToAll:

                it.remove();
                paRelated = PasteAnswer.overwriteNone;
                break;

              case mrYesToAll:

                paRelated = PasteAnswer.overwriteAll;
                break;

              default:
                break;
            }

            break;

          case overwriteNone:

            it.remove();
            break;

          default:
            break;
        }
      }
    }

    folderTreeWatcher.stop();

    return (new HyperTask("ObtainLocks", "Obtaining locks...", srcToDest.size()) { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      try
      {
        for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
        {
          incrementAndUpdateProgress();

          if ((copying == false) && (entry.getKey().canObtainLock() == false))
            throw new HyperDataException("Unable to obtain lock on path: \"" + entry.getKey() + '"');

          if (entry.getValue().canObtainLock() == false)
            throw new HyperDataException("Unable to obtain lock on path: \"" + entry.getValue() + '"');
        }
      }
      catch (IOException e)
      {
        throw new HyperDataException("Unable to obtain lock: " + getThrowableMessage(e), e);
      }

    }}.runWithProgressDialog() == State.SUCCEEDED) && (srcToDest.isEmpty() == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void paste(FileRow destRow, boolean copying, boolean dragging)
  {
    if ((destRow == null) && ((destRow = getFolderRow()) == null)) return;

    if (ui.cantSaveRecord()) return;

    Map<FilePath, FilePath> srcToDest = new HashMap<>();

    if (doPasteChecks(destRow, srcToDest, copying, dragging) == false)
    {
      if (folderTreeWatcher.isRunning() == false)
        folderTreeWatcher.createNewWatcherAndStart();

      return;
    }

    folderTreeWatcher.stop();

    boolean success = new HyperTask("PasteOperation", copying ? "Copying..." : "Moving...",
                                    srcToDest.size() * (copying ? 2L : 4L)) { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      suppressNeedRefresh = true;

      try
      {

      // Create directories that need to be created
      // ------------------------------------------

        for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
        {
          incrementAndUpdateProgress();

          FilePath srcFilePath  = entry.getKey(),
                   destFilePath = entry.getValue();

          if (srcFilePath.isDirectory())
            destFilePath.createDirectories();
        }

      // if copying, copy files
      // -----------------------------------------------

        if (copying)
        {
          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            incrementAndUpdateProgress();

            FilePath srcFilePath = entry.getKey(),
                     destFilePath = entry.getValue();

            if (srcFilePath.isDirectory() == false)
              srcFilePath.copyTo(entry.getValue(), false);

            HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);
          }
        }

      // if moving, move files
      // -----------------------------------------------

        else
        {
          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            incrementAndUpdateProgress();

            FilePath srcFilePath = entry.getKey(),
                     destFilePath = entry.getValue();

            HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);

            Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(srcFilePath);

            if (set.isEmpty())
            {
              if (srcFilePath.isDirectory() == false)
                srcFilePath.moveTo(destFilePath, false);
            }
            else
            {
              for (HyperPath hyperPath : set)
                if (hyperPath.getRecordType() != hdtFolder)
                  hyperPath.moveToFolder(folder.getID(), false, false, "");
            }
          }

      // if moving, update note records
      // ------------------------------

          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            FilePath srcFilePath = entry.getKey(),
                     destFilePath = entry.getValue();

            incrementAndUpdateProgress();

            if (srcFilePath.isDirectory() == false) continue;

            HDT_Folder folder = HyperPath.getFolderFromFilePath(srcFilePath, false);

            if (folder != null)
              List.copyOf(folder.notes).forEach(note -> note.folder.set(HyperPath.getFolderFromFilePath(destFilePath, false)));
          }

      // If moving, remove source directories that are now empty
      // -------------------------------------------------------

          for (FilePath srcFilePath : srcToDest.keySet())
          {
            incrementAndUpdateProgress();

            if (srcFilePath.isDirectory() && (srcFilePath.dirContainsAnyFiles(true) == false) && (srcFilePath.contains(db.getRootPath()) == false))
            {
              if (db.getRootPath().contains(srcFilePath))
                HyperPath.getFolderFromFilePath(srcFilePath, false).delete(false);
              else
                srcFilePath.deleteDirectory(false);
            }
          }
        }
      }
      catch (IOException e)
      {
        suppressNeedRefresh = false;
        throw new HyperDataException("An error occurred while trying to " + (copying ? "copy" : "move") + " the item(s): " + getThrowableMessage(e), e);
      }

      suppressNeedRefresh = false;

    }}.setShowDialogImmediately(true).runWithProgressDialog() == State.SUCCEEDED;

    suppressNeedRefresh = false;

    FilePath pathToHilite = srcToDest.isEmpty() ? null : srcToDest.get(srcPathToHilite);

    if (success || dragging)
      clearSrcRows(dragging);

    Platform.runLater(() ->
    {
      pruneAndRefresh(true);

      folderTreeWatcher.createNewWatcherAndStart();

      ui.update();

      if (FilePath.isEmpty(pathToHilite) == false)
        goToFilePath(pathToHilite, true);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // srcRow is non-null when invoked from right-clicking a column or dragging
  // srcRow is null when clicking a toolbar button

  List<? extends AbstractEntityWithPath> getMarkedRows(FileRow srcRow)
  {
    List<FileRow> rowList = fileTV.getSelectionModel().getSelectedItems();

    if (collEmpty(rowList) == false)
      return rowList.stream().map(EntityWithRow::new).toList();

    if (srcRow != null)
      return List.of(new EntityWithRow(srcRow));

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();

    if (fileRow != null)
      return List.of(new EntityWithRow(fileRow));

    if (curFolder != null)
      return List.of(new EntityWithRow(getFolderRow()));

    return Collections.emptyList();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void delete(FileRow fileRow)
  {
    if (ui.cantSaveRecord()) return;

  //---------------------------------------------------------------------------
  // Determine what is to be deleted
  //---------------------------------------------------------------------------

    List<? extends AbstractEntityWithPath> rowInfoList = getMarkedRows(fileRow);

    if (rowInfoList.isEmpty()) return;

  //---------------------------------------------------------------------------
  // See if any of it can be ruled out off the bat
  //---------------------------------------------------------------------------

    if (rowInfoList.stream().allMatch(rowInfo -> canCutRow(rowInfo, true)) == false)
      return;

  //---------------------------------------------------------------------------
  // Show the appropriate confirmation dialog
  //---------------------------------------------------------------------------

    if (rowInfoList.size() == 1)
    {
      AbstractEntityWithPath item = rowInfoList.getFirst();
      HyperPath hyperPath = item.getHyperPath();

      if (item.isRelated())
      {
        RecordType recordType = hyperPath.getRecord().getType();

        String confirmMsg = "The " + (recordType == hdtFolder ? "folder" : "file") + " \"" + hyperPath.getNameStr() + "\" ";

        switch (recordType)
        {
          case hdtPerson   : confirmMsg += "is assigned as a picture file for a person record. Permanently delete it anyway?"; break;
          case hdtMiscFile : confirmMsg += "is assigned to a misc. file record. Okay to permanently delete the file as well as the associated record?"; break;
          case hdtWorkFile : confirmMsg += "is assigned to a work file record. Permanently delete it anyway?"; break;
          case hdtFolder   : confirmMsg += "is assigned to a note record. Permanently delete it anyway?"; break;
          default          : internalErrorPopup(21292); return;
        }

        if (confirmDialog(confirmMsg, false) == false) return;
      }
      else
      {
        if (item.isDirectory())
        {
          if (confirmDialog("Are you sure you want to permanently delete the folder \"" + hyperPath.getNameStr() + "\" and all the files/subfolders it contains?", false) == false)
            return;
        }
        else
        {
          if (confirmDialog("Are you sure you want to permanently delete the file \"" + hyperPath.getNameStr() + "\"?", false) == false)
            return;
        }
      }
    }
    else
    {
      if (rowInfoList.stream().anyMatch(AbstractEntityWithPath::isRelated))
      {
        if (confirmDialog("One or more of the selected items is associated with a database record. Okay to permanently delete the " + rowInfoList.size() + " items and associated record(s)?", false) == false)
          return;
      }
      else
      {
        if (confirmDialog("Are you sure you want to permanently delete these " + rowInfoList.size() + " items?", false) == false)
          return;
      }
    }

  //---------------------------------------------------------------------------
  // Delete the records/folders
  //---------------------------------------------------------------------------

    folderTreeWatcher.stop();

    suppressNeedRefresh = true;

    noOp(rowInfoList.stream().allMatch(FileManager::deleteRow)); // Deletes rows until deleteRow returns false

    folderTree.prune();

    folderTreeWatcher.createNewWatcherAndStart();
    Platform.runLater(() ->
    {
      refresh();
      ui.update();
    });

    suppressNeedRefresh = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean deleteRow(AbstractEntityWithPath item)
  {
    HyperPath hyperPath = item.getHyperPath();
    HDT_RecordWithPath fileRecord = hyperPath.getRecord();
    FilePath filePath = hyperPath.filePath();

    if (item.isRelated() == false)
    {
      if (item.isDirectory())
        return ((HDT_Folder) fileRecord).delete(true);

      try { Files.delete(filePath.toPath()); }
      catch (IOException e)
      {
        return falseWithErrorPopup("Unable to delete the file: " + getThrowableMessage(e));
      }

      db.unmapFilePath(filePath);
      return true;
    }

    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

    for (HyperPath setPath : set)
    {
      try
      {
        setPath.filePath().delete(true);
        db.unmapFilePath(setPath.filePath());
        if ((setPath.getRecordType() != hdtNone) && (setPath.getRecordType() != hdtPerson))
          db.deleteRecord(setPath.getRecord());
      }
      catch (IOException e)
      {
        return falseWithErrorPopup("An error occurred while trying to delete \"" + setPath.filePath() + "\": " + getThrowableMessage(e));
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean canCutRow(AbstractEntityWithPath item, boolean deleting)
  {
    String opPast = deleting ? "deleted" : "moved";

    FilePath filePath = item.getFilePath();
    boolean isDir = filePath.isDirectory();

    HyperPath hyperPath = item.getHyperPath();

    if (db.isProtectedFile(filePath, true))
      return falseWithInfoPopup((isDir ? "The folder \"" : "The file \"") + filePath + "\" cannot be " + opPast + '.');

    if (deleting == false) return true;

    if (isDir && HyperPath.isInUse(hyperPath))
      return falseWithInfoPopup("The folder \"" + filePath + "\" cannot be deleted, because it contains one or more files or folders that are in use by the database.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void newFolder(HDT_Folder parentFolder)
  {
    if (parentFolder == null) return;

    RenameDlgCtrlr dlg = new RenameDlgCtrlr("Create Folder in: " + parentFolder.filePath(), ntFolder, "");

    if (dlg.showModal() == false) return;

    FilePath newFilePath;

    suppressNeedRefresh = true;

    try
    {
      newFilePath = parentFolder.filePath().resolve(dlg.getNewName());
      newFilePath.createDirectory();
    }
    catch (FileAlreadyExistsException e)
    {
      errorPopup("Unable to create the folder: A file with that name already exists.");
      return;
    }
    catch (IOException e)
    {
      errorPopup("Unable to create the folder: " + getThrowableMessage(e));
      return;
    }
    finally
    {
      suppressNeedRefresh = false;
    }

    pruneAndRefresh(true);
    goToFilePath(newFilePath, true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rename(FileRow fileRow)
  {
    if (ui.cantSaveRecord()) return;

    if (fileRow == null) fileRow = fileTV.getSelectionModel().getSelectedItem();
    if (fileRow == null) fileRow = getFolderRow();
    if (fileRow == null) return;

    HDT_RecordWithPath fileRecord = fileRow.getRecord();
    boolean isDir = fileRow.isDirectory(), cantRename;
    String noun;

    if (isDir) { noun = "Folder"; cantRename = isUnstoredRecord(fileRecord.getID(), hdtFolder) || fileRow.getFilePath().equals(db.xmlPath()); }
    else       { noun = "File";   cantRename = db.isProtectedFile(fileRow.getFilePath(), false); }

    if (cantRename)
    {
      infoPopup("That " + noun.toLowerCase() + " cannot be renamed.");
      return;
    }

    RenameDlgCtrlr dlg = new RenameDlgCtrlr("Rename " + noun + ": " + fileRow.getFilePath(), isDir ? ntFolder : ntFile, fileRow.getFileName());

    if (dlg.showModal() == false) return;

    FilePath srcFilePath = fileRow.getFilePath(),
             parentFilePath = srcFilePath.getParent(),
             destFilePath = parentFilePath.resolve(dlg.getNewName());

    if (destFilePath.exists())
    {
      errorPopup("A " + (destFilePath.isDirectory() ? "folder" : "file") + " with that name already exists.");
      return;
    }

    if (isDir)
    {
      suppressNeedRefresh = true;

      if (fileRow.rename(dlg.getNewName())) Platform.runLater(() ->
      {
        refresh();
        ui.update();
      });

      suppressNeedRefresh = false;
      return;
    }

    boolean success = true;

    try
    {
      suppressNeedRefresh = true;

      if (fileRecord == null)
      {
        FilePath filePath = fileRow.getFilePath();
        filePath.renameTo(dlg.getNewName());
        db.unmapFilePath(filePath);
      }
      else
        success = HyperPath.renameFile(fileRecord.filePath(), dlg.getNewName());
    }
    catch (IOException | HDB_InternalError e)
    {
      errorPopup("Unable to rename the " + noun.toLowerCase() + ": " + getThrowableMessage(e));
      return;
    }
    finally
    {
      suppressNeedRefresh = false;
    }

    if (success) Platform.runLater(() ->
    {
      ui.update();
      refresh();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void pruneAndRefresh(boolean ensureWatcherIsStopped)
  {
    if (instance != null) instance.doPruneAndRefresh(ensureWatcherIsStopped);
  }

  private void doPruneAndRefresh(boolean ensureWatcherIsStopped)
  {
    boolean restartWatcher = ensureWatcherIsStopped && folderTreeWatcher.stop();

    folderTree.prune();

    refresh();

    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void refresh()
  {
    if (instance != null) instance.doRefresh();
  }

  private void doRefresh()
  {
    if (alreadyRefreshing || (stage.isShowing() == false)) return;
    needRefresh = false;
    alreadyRefreshing = true;

    if (folderTree.selectedRecord() == null)
      folderTree.selectRecord(db.getRootFolder(), -1, false);

    folderTree.refresh();

    if (HDT_Record.isEmpty(curFolder, false))
      curFolder = null;

    if (curFolder == null)
    {
      fileTable.clear();
      recordTable.clear();
      stage.setTitle(dialogTitle);
      alreadyRefreshing = false;
      return;
    }

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();
    FilePath filePath = fileRow == null ? null : fileRow.getFilePath();

    fileTable.update(curFolder, folderTree.selectedItem());

    if (FilePath.isEmpty(filePath))
      updateRecordTable(null, false);

    stage.setTitle(dialogTitle + " - " + curFolder.filePath());

    fileTV.sort();

    if (FilePath.isEmpty(filePath) == false)
      fileTable.selectByFileName(filePath);

    alreadyRefreshing = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void userRequestsToGoBackward()
  {
    if (btnBack.isDisabled()) return;

    invokeHistoryItem(history.back());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void userRequestsToGoForward()
  {
    if (btnForward.isDisabled()) return;

    invokeHistoryItem(history.forward());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void invokeHistoryItem(FolderHistoryItem item)
  {
    programmaticSelectionChange = true;
    folderTree.selectRecord(item.folder(), -1, false);
    programmaticSelectionChange = false;

    if (item.fileName() != null)
      fileTable.selectByFileName(item.fileName());

    if (HDT_Record.isEmpty(item.record(), false) == false)
    {
      HyperTableRow row = recordTable.selectRowByRecord(item.record());
      if (row != null)
      {
        recordTV.requestFocus();
        return;
      }
    }

    if (item.fileName() != null)
      fileTV.requestFocus();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Selects an appropriate record row based on the file row selected.
   * <br>
   * It will prefer a record with some content in its description.
   * @return True if the record has a corresponding file (for displaying in the Preview window); false otherwise
   */
  private boolean selectNonBlankRecordRow()
  {
    HyperTableRow rowToPick = null;

    for (HyperTableRow row : recordTable.dataRows())
    {
      HDT_Record record = row.getRecord();
      if (record == null) continue;

      if (record.hasDesc() && strNotNullOrBlank(((HDT_RecordWithDescription) record).getDesc().getPlain()))
      {
        recordTable.selectRow(row);

        if (record instanceof HDT_RecordWithPath recordWithPath)
        {
          FilePath filePath = recordWithPath.filePath();
          if ((filePath != null) && filePath.isFile())
          {
            setPreviewFromRecordTable(); // This gets called by recordTable.selectRow but disablePreviewUpdating may have been true when recordTable.selectRow was first called;
            return true;                 // the last time it was called, the row selection had not actually changed, so we need to call setPreviewFromRecordTable manually
          }
        }

        return false;
      }

      if ((rowToPick == null) && (record instanceof HDT_RecordWithPath recordWithPath) && recordWithPath.pathNotEmpty())
        rowToPick = row;
    }

    if (rowToPick == null)
      return false;

    recordTable.selectRow(rowToPick);

    FilePath filePath = ((HDT_RecordWithPath)rowToPick.getRecord()).filePath();
    if ((filePath != null) && filePath.isFile())
    {
      setPreviewFromRecordTable(); // This gets called by recordTable.selectRow but disablePreviewUpdating may have been true when recordTable.selectRow was first called;
      return true;                 // the last time it was called, the row selection had not actually changed, so we need to call setPreviewFromRecordTable manually
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreviewFromRecordTable()
  {
    FilePath filePath = null;
    HDT_Record record = recordTable.selectedRecord();

    if (record instanceof HDT_RecordWithPath recordWithPath)
      filePath = recordWithPath.filePath();

    FilePath fileTablePath = nullSwitch(fileTV.getSelectionModel().getSelectedItem(), null, FileRow::getFilePath);

    if ((record != null) && EnumSet.of(hdtWork, hdtMiscFile, hdtWorkFile).contains(record.getType()) == false)
    {
      if      (FilePath.isEmpty(fileTablePath) == false) record = HyperPath.getRecordFromFilePath(fileTablePath);
      else if (FilePath.isEmpty(filePath     ) == false) record = HyperPath.getRecordFromFilePath(filePath);
    }

    if (record != null)
    {
      switch (record.getType())
      {
        case hdtWorkFile :
        {
          HDT_WorkFile workFile = (HDT_WorkFile) record;
          if (workFile.works.isEmpty() == false)
          {
            PreviewWindow.setPreview(pvsManager, workFile, workFile.works.getFirst());
            return;
          }

          record = recordTable.selectedRecord();
          break;
        }

        case hdtWork :
        {
          HDT_Work work = (HDT_Work) record;
          HDT_WorkFile workFile = null;

          if (FilePath.isEmpty(fileTablePath) == false)
          {
            HDT_RecordWithPath recordWP = HyperPath.getRecordFromFilePath(fileTablePath);
            if ((recordWP != null) && (recordWP.getType() == hdtWorkFile))
              workFile = (HDT_WorkFile) recordWP;
          }

          if ((workFile == null) && (work.workFiles.isEmpty() == false))
            workFile = work.workFiles.getFirst();

          if (workFile != null)
          {
            PreviewWindow.setPreview(pvsManager, workFile, work);
            return;
          }

          break;
        }

        case hdtMiscFile :

          PreviewWindow.setPreview(pvsManager, (HDT_MiscFile) record);
          return;

        default : break;
      }
    }

    PreviewWindow.setPreview(pvsManager, FilePath.isEmpty(fileTablePath) ? filePath : fileTablePath, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Populates the record table with records related to the given file row, or related to the
   * current folder if no file row is given. Handles three cases:
   * <ol>
   *   <li><b>Folder</b> ({@code fileRow} is null or a directory): shows the folder's relatives
   *       and attempts to auto-select a record row for the preview window.</li>
   *   <li><b>File without a database record</b>: shows notes associated with the file's parent
   *       folder. The caller is responsible for previewing the file itself.</li>
   *   <li><b>File with a database record</b> (WorkFile, MiscFile, etc.): adds the file's own
   *       record as the first row, then shows its relatives. The caller is responsible for
   *       preview handling in this case as well.</li>
   * </ol>
   *
   * @param fileRow     the selected row in the file table, or null if no file is selected
   * @param showingMore if true, retrieves all relatives with no limit (triggered by "show more")
   */
  private void updateRecordTable(FileRow fileRow, boolean showingMore)
  {
    HDT_RecordWithPath recordForRelatives;
    LinkedHashSet<HDT_Record> relatives = new LinkedHashSet<>();
    boolean hasMore = false,
            gettingNotesForFile = (fileRow != null) && (fileRow.isDirectory() == false),
            hasFileRecord = false;

    recordTable.clear();

    if (fileRow == null)
      recordForRelatives = curFolder;
    else if (fileRow.isDirectory())
      recordForRelatives = fileRow.getRecord();
    else
    {
      recordForRelatives = fileRow.getRecord();

      if (recordForRelatives != null)
        hasFileRecord = true;
      else
        recordForRelatives = fileRow.getFolder();
    }

    if (recordForRelatives == null) // This would only happen if nothing is selected in the Folder Tree, which should never happen
    {
      clearRecordDesc();
      return;
    }

    if (hasFileRecord == false)
      clearRecordDesc();

    hasMore = db.getRelatives(recordForRelatives, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS, gettingNotesForFile);

    if (hasFileRecord)
    {
      HyperTableRow row = recordTable.newDataRow();
      row.setCellValue(0, recordForRelatives, getTypeName(recordForRelatives.getType()));
      row.setCellValue(1, recordForRelatives);
    }

    Iterator<HDT_Record> relIt = relatives.iterator();

    while (relIt.hasNext())
    {
      HDT_Record relative = relIt.next();

      if (hasMore && (relIt.hasNext() == false))
      {
        recordTable.newShowMoreRow();
        break;
      }

      RecordType relativeType = relative.getType();

      if (relativeType != hdtFolder)
      {
        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, relative, getTypeName(relativeType));

        String displayText = (hasFileRecord == false) && (relativeType == hdtNote) ?
          ((HDT_Note) relative).extendedText(false)
        :
          relative.defaultChoiceText();

        row.setCellValue(1, relative, displayText);
      }
    }

    if ((hasFileRecord == false) && (selectNonBlankRecordRow() == false))
      PreviewWindow.clearPreview(pvsManager);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void goToFilePath(FilePath filePath, boolean hiliteIfFolder)
  {
    if (FilePath.isEmpty(filePath)) return;

    boolean justEnteringFolder = false;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath.getDirOnly(), true);

    if (filePath.isDirectory())
    {
      if (hiliteIfFolder && (filePath.equals(db.getRootPath()) == false))
        folder = folder.parentFolder();
      else
        justEnteringFolder = true;
    }

    folderTree.selectRecord(folder, 0, false);

    if (justEnteringFolder) return;

    fileTable.selectByFileName(filePath.getNameOnly());
    fileTV.requestFocus();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void setDividerPositions()
  {
    setDividerPosition(spMain   , DividerPositionPrefKey.MGR_MAIN_HORIZ   , 0);
    setDividerPosition(spFiles  , DividerPositionPrefKey.MGR_FILES_VERT   , 0);
    setDividerPosition(spRecords, DividerPositionPrefKey.MGR_RECORDS_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void getDividerPositions()
  {
    getDividerPosition(spMain   , DividerPositionPrefKey.MGR_MAIN_HORIZ   , 0);
    getDividerPosition(spFiles  , DividerPositionPrefKey.MGR_FILES_VERT   , 0);
    getDividerPosition(spRecords, DividerPositionPrefKey.MGR_RECORDS_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean isFocused()
  {
    return (instance != null) && instance.stage.isShowing() && instance.stage.isFocused();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show(FilePath filePath)
  {
    if ((instance == null) || FilePath.isEmpty(filePath)) return;

    show();
    instance.goToFilePath(filePath, false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show()
  {
    if (ui.cantSaveRecord(false) == false)
     show(instance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
