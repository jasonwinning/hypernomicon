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

package org.hypernomicon.fileManager;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Predicate;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.RenameDlgCtrlr;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.unities.HDT_RecordWithDescription;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.cellValues.HyperTableCell;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.MenuItemSchema;
import org.hypernomicon.view.wrappers.ReadOnlyCell;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.event.Event;
import javafx.fxml.FXML;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.web.WebView;
import javafx.stage.Modality;
import javafx.stage.StageStyle;

public class FileManager extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class HistoryItem
  {
    private final HDT_Folder folder;
    private final FilePath fileName;
    private final HDT_Record record;

    private HistoryItem(FileRow folderRow, FileRow fileRow, HDT_Record record)
    {
      folder = folderRow.getRecord();
      fileName = fileRow == null ? null : fileRow.getFilePath().getNameOnly();
      this.record = record;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class FolderHistory
  {
    private final List<HistoryItem> history = new ArrayList<>();
    private final Button btnForward, btnBack;
    private int ndx = -1;
    private boolean doAdd = true;

    private FolderHistory(Button btnForward, Button btnBack)
    {
      this.btnForward = btnForward;
      this.btnBack = btnBack;

      clear();
    }

    private void clear()
    {
      history.clear();
      ndx = -1;
      updateButtons();
    }

    private void add(HistoryItem newItem)
    {
      if (doAdd == false) return;

      while (history.size() > (ndx + 1))
        history.remove(ndx + 1);

      history.add(newItem);
      ndx++;
      updateButtons();
    }

    private void updateButtons()
    {
      btnBack.setDisable(ndx == 0);
      btnForward.setDisable(ndx == (history.size() - 1));
    }

    private HistoryItem back()
    {
      ndx--;
      updateButtons();
      return history.get(ndx);
    }

    private HistoryItem forward()
    {
      ndx++;
      updateButtons();
      return history.get(ndx);
    }

    private void updateCurrent(HistoryItem newItem)
    {
      history.set(ndx, newItem);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML private TreeView<FileRow> treeView;
  @FXML private TableView<FileRow> fileTV;
  @FXML private TableView<HyperTableRow> recordTV;
  @FXML private WebView webView;
  @FXML private Button btnForward, btnBack, btnCut, btnCopy, btnPaste, btnDelete, btnRename, btnRefresh,
                       btnMainWindow, btnPreviewWindow, btnNewFolder;
  @FXML private SplitPane spMain, spFiles, spRecords;

  private static final String dialogTitle = "File Manager";

  private final MenuItemSchema<HDT_RecordWithPath, FileRow> pasteMenuItem;

  private List<AbstractEntityWithPath> dragPaths = null;
  private List<? extends AbstractEntityWithPath> markedRows = null;
  private FilePath srcPathToHilite = null;
  private boolean clipboardCopying, needRefresh = false, alreadyRefreshing = false, suppressNeedRefresh = false;
  private HDT_Folder curFolder;

  public final FolderTreeWrapper folderTree;
  private final FileTable fileTable;
  private final HyperTable recordTable;
  private final FolderHistory history;

  FileRow getFolderRow()                { return nullSwitch(curFolder, null, folder -> folderTree.getRowsForRecord(folder).get(0)); }
  public void clearHistory()            { history.clear(); }
  public void setNeedRefresh()          { if (suppressNeedRefresh == false) needRefresh = true; }
  @Override protected boolean isValid() { return true; }

  private List<? extends AbstractEntityWithPath> getSrcPaths(boolean dragging) { return dragging ? dragPaths : markedRows; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public FileManager()
  {
    super("fileManager/FileManager", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);

    fileTable = new FileTable(fileTV, this);
    folderTree = new FolderTreeWrapper(treeView, fileTable);
    recordTable = new HyperTable(recordTV, 1, false, PREF_KEY_HT_FM_RECORDS, this);

    initContainers();

    fileTable.addContextMenuItem("Launch", Predicate.not(FileRow::isDirectory), fileRow -> launchFile(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Show in system explorer", fileRow -> highlightFileInExplorer(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Copy path to clipboard", fileRow -> copyToClipboard(fileRow.getFilePath().toString()));
    fileTable.addContextMenuItem("Rename", this::rename);

    fileTable.addContextMenuItem("New misc. file record", fileRow -> fileRow.getRecord() == null, fileRow ->
    {
      ui.importMiscFile(fileRow, null);
      refresh();
    });

    fileTable.addContextMenuItem("New work record", fileRow -> fileRow.getRecord() == null, fileRow ->
    {
      ui.importWorkFile(null, fileRow.getFilePath(), false);
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

    btnCut.setOnAction(event -> cutCopy(null, false));
    btnCopy.setOnAction(event -> cutCopy(null, true));
    btnPaste.setOnAction(event -> paste(null, clipboardCopying, false));
    btnDelete.setOnAction(event -> delete(null));
    btnNewFolder.setOnAction(event -> newFolder(curFolder));

    history = new FolderHistory(btnForward, btnBack);

    btnBack.setOnAction(event -> btnBackClick());
    btnForward.setOnAction(event -> btnForwardClick());
    btnRefresh.setOnAction(event -> pruneAndRefresh());
    btnRename.setOnAction(event -> rename(null));

    btnMainWindow.setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnPreviewWindow.setOnAction(event -> ui.openPreviewWindow(pvsManager));
    btnPaste.setDisable(true);
    pasteMenuItem.disabled = true;

    onShown = () ->
    {
      if (shownAlready() == false)
        setDividerPositions();

      refresh();

      ui.windows.push(dialogStage);
    };

    dialogStage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus()) return;

      if (Boolean.TRUE.equals(newValue) == false) return;

      ui.windows.push(dialogStage);

      if (needRefresh) refresh();
    });

    dialogStage.setOnHidden(event -> ui.windows.focusStage(ui.getStage()));

    dialogStage.addEventFilter(MouseEvent.MOUSE_CLICKED, event ->
    {
      if      (event.getButton() == MouseButton.BACK   ) Platform.runLater(this::btnBackClick   );
      else if (event.getButton() == MouseButton.FORWARD) Platform.runLater(this::btnForwardClick);
      else                                               return;

      event.consume();
    });

    webView.setOnDragOver(Event::consume);
    webView.setOnDragDropped(Event::consume);

    Scene scene = dialogStage.getScene();

    scene.getAccelerators().putAll(SystemUtils.IS_OS_MAC ? Map.of
    (
      new KeyCodeCombination(KeyCode.LEFT , KeyCombination.SHORTCUT_DOWN), () -> Platform.runLater(this::btnBackClick),
      new KeyCodeCombination(KeyCode.RIGHT, KeyCombination.SHORTCUT_DOWN), () -> Platform.runLater(this::btnForwardClick)
    )
    : Map.of
    (
      new KeyCodeCombination(KeyCode.LEFT , KeyCombination.ALT_DOWN     ), () -> Platform.runLater(this::btnBackClick),
      new KeyCodeCombination(KeyCode.RIGHT, KeyCombination.ALT_DOWN     ), () -> Platform.runLater(this::btnForwardClick)
    ));

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
    recordTable.addLabelCol(hdtNone);

    treeView.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue == oldValue)) return;

      HDT_Folder folder = HyperPath.getFolderFromFilePath(newValue.getValue().getFilePath(), true);

      curFolder = folder;
      history.add(new HistoryItem(newValue.getValue(), null, null));

      fileTable.update(folder, newValue);
      setCurrentFileRow(null, false);
      getStage().setTitle(dialogTitle + " - " + folder.filePath());
    });

    fileTV.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null)
      {
        previewWindow.clearPreview(pvsManager);
        return;
      }

      if (newValue == oldValue) return;

      previewWindow.disablePreviewUpdating = true;
      setCurrentFileRow(newValue, false);
      previewWindow.disablePreviewUpdating = false;

      history.updateCurrent(new HistoryItem(folderTree.getSelectionModel().getSelectedItem().getValue(), newValue, null));

      if (selectNonBlankRecordRow() == false)
        previewWindow.setPreview(pvsManager, newValue.getFilePath());
    });

    recordTable.setOnShowMore(() -> setCurrentFileRow(fileTV.getSelectionModel().getSelectedItem(), true));

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
        history.updateCurrent(new HistoryItem(folderTree.selectedItem().getValue(), fileTV.getSelectionModel().getSelectedItem(), record));

        if (record != null)
        {
          if (record.getType() == hdtWorkFile)
          {
            HDT_WorkFile workFile = (HDT_WorkFile) record;
            if (workFile.works.isEmpty() == false)
              record = workFile.works.get(0);
          }

          MainTextWrapper.setReadOnlyHTML(HDT_Record.getDescHtml(record), webView.getEngine());

          setPreviewFromRecordTable();

          return;
        }
      }

      setPreviewFromRecordTable();

      webView.getEngine().loadContent("");
    });

    webView.getEngine().titleProperty().addListener((ob, oldValue, newValue) ->
    {
      HDT_Record record = recordTable.selectedRecord();
      if (record == null) return;

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(HDT_Record.getDescHtml(record)), webView.getEngine());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_FILEMGR_ZOOM);
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

    if (new HyperTask("BuildFileList", "Building list of files...") { @Override protected void call() throws HyperDataException, CancelledTaskException
    {
      updateProgress(-1, 1);

      for (AbstractEntityWithPath pathItem : getSrcPaths(dragging))
      {
        if (FilePath.isEmpty(srcPathToHilite))
          srcPathToHilite = pathItem.getFilePath();

        if (pathItem.isDirectory())
          pathItem.getFilePath().addDirContentsToSet(srcSet);

        srcSet.add(pathItem.getFilePath());
      }

    }}.runWithProgressDialog() != State.SUCCEEDED) return false;

    if (new HyperTask("PasteChecks", "Performing checks...", srcSet.size() * 2L) { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      FilePath baseDir = getSrcPaths(dragging).get(0).getFilePath().getParent();

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

        if ((copying == false) && srcFilePath.isDirectory() && srcFilePath.isSubpath(destFilePath))
          throw new HyperDataException("The destination folder is a subfolder of the source folder.");

        destSet.add(destFilePath);
        srcToDest.put(srcFilePath, destFilePath);
      }

      for (FilePath destFilePath : destSet)
      {
        incrementAndUpdateProgress();

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
                            .append(hyperPath.getRecord().getCBText()).append('\n'));

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
                if (nullSwitch(hyperPath.getRecord(), hdtNone, HDT_Record::getType) != hdtFolder)
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

            if (srcFilePath.isDirectory() == false) return;

            HDT_Folder folder = HyperPath.getFolderFromFilePath(srcFilePath, false);

            if (folder != null)
              List.copyOf(folder.notes).forEach(note -> note.folder.set(HyperPath.getFolderFromFilePath(destFilePath, false)));
          }

      // If moving, remove source directories that are now empty
      // -------------------------------------------------------

          for (FilePath srcFilePath : srcToDest.keySet())
          {
            incrementAndUpdateProgress();

            if (srcFilePath.isDirectory() && (srcFilePath.dirContainsAnyFiles(true) == false) && (srcFilePath.isSubpath(db.getRootPath()) == false))
            {
              if (db.getRootPath().isSubpath(srcFilePath))
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

    }}.runWithProgressDialog() == State.SUCCEEDED;

    suppressNeedRefresh = false;

    FilePath pathToHilite = srcToDest.isEmpty() ? null : srcToDest.get(srcPathToHilite);

    if (success || dragging)
      clearSrcRows(dragging);

    Platform.runLater(() ->
    {
      pruneAndRefresh();

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
      AbstractEntityWithPath item = rowInfoList.get(0);
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

        if (confirmDialog(confirmMsg) == false) return;
      }
      else
      {
        if (item.isDirectory())
        {
          if (confirmDialog("Are you sure you want to permanently delete the folder \"" + hyperPath.getNameStr() + "\" and all the files/subfolders it contains?") == false)
            return;
        }
        else
        {
          if (confirmDialog("Are you sure you want to permanently delete the file \"" + hyperPath.getNameStr() + "\"?") == false)
            return;
        }
      }
    }
    else
    {
      if (rowInfoList.stream().anyMatch(AbstractEntityWithPath::isRelated))
      {
        if (confirmDialog("One or more of the selected items is associated with a database record. Okay to permanently delete the " + rowInfoList.size() + " items and associated record(s)?") == false)
          return;
      }
      else
      {
        if (confirmDialog("Are you sure you want to permanently delete these " + rowInfoList.size() + " items?") == false)
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
    HDT_RecordWithPath fileRecord = hyperPath == null ? null : hyperPath.getRecord();

    if (db.isProtectedFile(filePath, true))
      return falseWithInfoPopup((isDir ? "The folder \"" : "The file \"") + filePath + "\" cannot be " + opPast + '.');

    if (deleting == false) return true;

    if (isDir && ((HDT_Folder) fileRecord).containsFilesThatAreInUse())
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

    pruneAndRefresh();
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

  public void pruneAndRefresh()
  {
    boolean restartWatcher = folderTreeWatcher.stop();

    folderTree.prune();

    refresh();

    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refresh()
  {
    if (alreadyRefreshing || (dialogStage.isShowing() == false)) return;
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
      getStage().setTitle(dialogTitle);
      alreadyRefreshing = false;
      return;
    }

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();
    FilePath filePath = fileRow == null ? null : fileRow.getFilePath();

    fileTable.update(curFolder, folderTree.selectedItem());

    if (FilePath.isEmpty(filePath))
      setCurrentFileRow(null, false);

    getStage().setTitle(dialogTitle + " - " + curFolder.filePath());

    fileTV.sort();

    if (FilePath.isEmpty(filePath) == false)
      fileTable.selectByFileName(filePath);

    alreadyRefreshing = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnBackClick()
  {
    if (btnBack.isDisabled()) return;

    history.doAdd = false;
    invokeHistoryItem(history.back());
    history.doAdd = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void invokeHistoryItem(HistoryItem item)
  {
    folderTree.selectRecord(item.folder, -1, false);

    if (item.fileName != null)
      fileTable.selectByFileName(item.fileName);

    if (item.record != null)
    {
      HyperTableRow row = recordTable.selectRowByRecord(item.record);
      if (row != null)
      {
        recordTV.requestFocus();
        return;
      }
    }

    if (item.fileName != null)
      fileTV.requestFocus();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void btnForwardClick()
  {
    if (btnForward.isDisabled()) return;

    history.doAdd = false;
    invokeHistoryItem(history.forward());
    history.doAdd = true;
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

      if (record.hasDesc() && (((HDT_RecordWithDescription) record).getDesc().getPlain().trim().length() > 0))
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
          HDT_WorkFile workFile = (HDT_WorkFile)record;
          if (workFile.works.size() > 0)
          {
            previewWindow.setPreview(pvsManager, workFile, workFile.works.get(0));
            return;
          }

          record = recordTable.selectedRecord();
          break;
        }

        case hdtWork :
        {
          HDT_WorkFile workFile = null;

          if (FilePath.isEmpty(fileTablePath) == false)
          {
            HDT_RecordWithPath recordWP = HyperPath.getRecordFromFilePath(fileTablePath);
            if ((recordWP != null) && (recordWP.getType() == hdtWorkFile))
              workFile = (HDT_WorkFile) recordWP;
          }

          if ((workFile == null) && (FilePath.isEmpty(filePath) == false))
            workFile = (HDT_WorkFile) HyperPath.getRecordFromFilePath(filePath);

          if (workFile != null)
          {
            previewWindow.setPreview(pvsManager, workFile, (HDT_Work) record);
            return;
          }

          break;
        }

        case hdtMiscFile :

          previewWindow.setPreview(pvsManager, (HDT_MiscFile) record);
          return;

        default : break;
      }
    }

    previewWindow.setPreview(pvsManager, FilePath.isEmpty(fileTablePath) ? filePath : fileTablePath, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setCurrentFileRow(FileRow fileRow, boolean showingMore)
  {
    HDT_Folder folderRecord = null;
    LinkedHashSet<HDT_Record> relatives = new LinkedHashSet<>();
    boolean hasMore;

    recordTable.clear();

    if (fileRow == null)
      folderRecord = curFolder;
    else if (fileRow.isDirectory())
      folderRecord = fileRow.getRecord();

    if (folderRecord != null)
    {
      webView.getEngine().loadContent("");

      hasMore = db.getRelatives(folderRecord, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS, false);
    }
    else
    {
      HDT_RecordWithPath fileRecord = fileRow.getRecord();
      if (fileRecord == null)
      {
        webView.getEngine().loadContent("");

        folderRecord = fileRow.getFolder();
        hasMore = db.getRelatives(folderRecord, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS, true);
      }
      else
      {
        hasMore = db.getRelatives(fileRecord, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS, true);

        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, fileRecord, getTypeName(fileRecord.getType()));
        row.setCellValue(1, fileRecord, fileRecord.listName());
      }
    }

    Iterator<HDT_Record> relIt = relatives.iterator();

    while (relIt.hasNext())
    {
      HDT_Record relative = relIt.next();

      if ((hasMore) && (relIt.hasNext() == false))
      {
        recordTable.newShowMoreRow();
        break;
      }

      if (relative.getType() != hdtFolder)
      {
        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, relative, getTypeName(relative.getType()));
        row.setCellValue(1, relative, relative.listName());
      }
    }

    if (folderRecord != null)
    {
      if (selectNonBlankRecordRow() == false)
        previewWindow.setPreview(pvsManager, folderRecord.filePath());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToFilePath(FilePath filePath, boolean hiliteIfFolder)
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

  private void setDividerPositions()
  {
    setDividerPosition(spMain, PREF_KEY_MGR_MAIN_HORIZ, 0);
    setDividerPosition(spFiles, PREF_KEY_MGR_FILES_VERT, 0);
    setDividerPosition(spRecords, PREF_KEY_MGR_RECORDS_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void getDividerPositions()
  {
    if (shownAlready() == false) return;

    getDividerPosition(spMain, PREF_KEY_MGR_MAIN_HORIZ, 0);
    getDividerPosition(spFiles, PREF_KEY_MGR_FILES_VERT, 0);
    getDividerPosition(spRecords, PREF_KEY_MGR_RECORDS_HORIZ, 0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
