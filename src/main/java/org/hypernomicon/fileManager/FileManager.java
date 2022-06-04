/*
 * Copyright 2015-2022 Jason Winning
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
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

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
import java.util.stream.Collectors;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.dialogs.RenameDlgCtrlr;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.mainText.MainTextUtil;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableCell.CellSortMethod;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.MenuItemSchema;
import org.hypernomicon.view.wrappers.ReadOnlyCell;

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TreeView;
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

  private List<MarkedRowInfo> markedRows = null, dragRows = null;
  private FilePath srcPathToHilite = null;
  private boolean clipboardCopying, needRefresh = false, alreadyRefreshing = false, suppressNeedRefresh = false;
  private MenuItemSchema<HDT_RecordWithPath, FileRow> pasteMenuItem;
  private HDT_Folder curFolder;

  public FolderTreeWrapper folderTree;
  private FileTable fileTable;
  private HyperTable recordTable;
  private FolderHistory history;
  private static HyperTask task;
  private static long totalTaskCount, curTaskCount;

  FileRow getFolderRow()                { return nullSwitch(curFolder, null, folder -> folderTree.getRowsForRecord(folder).get(0)); }
  public void clearHistory()            { history.clear(); }
  public void setNeedRefresh()          { if (suppressNeedRefresh == false) needRefresh = true; }
  @Override protected boolean isValid() { return true; }

  private List<MarkedRowInfo> getSrcRows(boolean dragging) { return dragging ? dragRows : markedRows; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FileManager build()
  {
    return ((FileManager) createUsingFullPath("fileManager/FileManager", dialogTitle, true, StageStyle.DECORATED, Modality.NONE)).init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private FileManager init()
  {
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
    setToolTip(btnPreviewWindow, "Show preview window");

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initContainers()
  {
    fileTable = new FileTable(fileTV, this);
    folderTree = new FolderTreeWrapper(treeView, fileTable);

    folderTree.getTreeModel().addParentChildRelation(rtParentFolderOfFolder, true);

    recordTable = new HyperTable(recordTV, 1, false, PREF_KEY_HT_FM_RECORDS, this);

    recordTable.addCol(hdtNone, ctIncremental);
    recordTable.addCol(hdtNone, ctNone);

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
        previewWindow.setPreview(pvsManager, newValue.getFilePath(), null);
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

          String mainText = "";
          if (record.hasDesc())
            mainText = ((HDT_RecordWithDescription) record).getDesc().getHtml();

          MainTextWrapper.setReadOnlyHTML(mainText, webView.getEngine(), new TextViewInfo(), null);

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

      String mainText = "";
      if (record.hasDesc())
        mainText = ((HDT_RecordWithDescription) record).getDesc().getHtml();

      MainTextUtil.handleJSEvent(MainTextUtil.prepHtmlForDisplay(mainText), webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    MainTextUtil.webViewAddZoom(webView, PREF_KEY_FILEMGR_ZOOM);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearSrcRows(boolean dragging)
  {
    if (dragging)
      dragRows = null;
    else
    {
      markedRows = null;
      btnPaste.setDisable(true);
      pasteMenuItem.disabled = true;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setSrcRows(List<MarkedRowInfo> rows, boolean dragging)
  {
    if (dragging)
    {
      dragRows = rows;
      return;
    }

    markedRows = rows;
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

  boolean moveCopy(List<MarkedRowInfo> rows, boolean copying, boolean dragging)
  {
    //---------------------------------------------------------------------------
    // Determine what is to be moved/copied
    //---------------------------------------------------------------------------

    if (rows.isEmpty())
    {
      clearSrcRows(dragging);
      return false;
    }

  //---------------------------------------------------------------------------
  // See if any of it can be ruled out off the bat
  //---------------------------------------------------------------------------

    if (copying == false)
    {
      for (MarkedRowInfo rowInfo : rows)
        if (canCutRow(rowInfo, false) == false)
        {
          clearSrcRows(dragging);
          return false;
        }
    }

    if (dragging == false)
      clipboardCopying = copying;

    setSrcRows(rows, dragging);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private enum PasteAnswer { check, overwriteNone, overwriteAll }

  private boolean doPasteChecks(FileRow destRow, Map<FilePath, FilePath> srcToDest, boolean copying, boolean dragging)
  {
    final FilePathSet srcSet = new FilePathSet(), destSet = new FilePathSet();
    srcPathToHilite = null;

    task = new HyperTask("BuildFileList") { @Override protected void call() throws HyperDataException
    {
      updateMessage("Building list of files...");
      updateProgress(-1, -1);

      for (MarkedRowInfo rowInfo : getSrcRows(dragging))
      {
        if (FilePath.isEmpty(srcPathToHilite))
          srcPathToHilite = rowInfo.row.getFilePath();

        if (rowInfo.row.isDirectory())
          rowInfo.row.getFilePath().addDirContentsToSet(srcSet);

        srcSet.add(rowInfo.row.getFilePath());
      }
    }};

    if (task.runWithProgressDialog() != State.SUCCEEDED) return false;

    task = new HyperTask("PasteChecks") { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      updateMessage("Performing checks...");
      updateProgress(0, 1);

      totalTaskCount = srcSet.size() * 2L;
      curTaskCount = 0;

      FilePath baseDir = getSrcRows(dragging).get(0).row.getFilePath().getParent();

      for (FilePath srcFilePath : srcSet)
      {
        updateProgress(curTaskCount++, totalTaskCount);

        if (isCancelled())
          throw new CancelledTaskException();

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
        updateProgress(curTaskCount++, totalTaskCount);

        if (isCancelled())
          throw new CancelledTaskException();

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
    }};

    if (task.runWithProgressDialog() != State.SUCCEEDED) return false;

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

    task = new HyperTask("ObtainLocks") { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      updateMessage("Obtaining locks...");
      updateProgress(0, 1);

      totalTaskCount = srcToDest.size();
      curTaskCount = 0;

      try
      {
        for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
        {
          updateProgress(curTaskCount++, totalTaskCount);

          if (isCancelled())
            throw new CancelledTaskException();

          if ((copying == false) && (entry.getKey().canObtainLock() == false))
            throw new HyperDataException("Unable to obtain lock on path: \"" + entry.getKey() + '"');

          if (entry.getValue().canObtainLock() == false)
            throw new HyperDataException("Unable to obtain lock on path: \"" + entry.getValue() + '"');
        }
      }
      catch (IOException e)
      {
        throw new HyperDataException("Unable to obtain lock: " + e.getMessage(), e);
      }
    }};

    return (task.runWithProgressDialog() == State.SUCCEEDED) && (srcToDest.isEmpty() == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void paste(FileRow destRow, boolean copying, boolean dragging)
  {
    if ((destRow == null) && ((destRow = getFolderRow()) == null)) return;

    Map<FilePath, FilePath> srcToDest = new HashMap<>();

    if (doPasteChecks(destRow, srcToDest, copying, dragging) == false)
    {
      if (folderTreeWatcher.isRunning() == false)
        folderTreeWatcher.createNewWatcherAndStart();
      return;
    }

    folderTreeWatcher.stop();

    task = new HyperTask("PasteOperation") { @Override protected void call() throws CancelledTaskException, HyperDataException
    {
      if (copying)
      {
        updateMessage("Copying...");
        totalTaskCount = srcToDest.size() * 2L;
      }
      else
      {
        updateMessage("Moving...");
        totalTaskCount = srcToDest.size() * 4L;
      }

      updateProgress(0, 1);
      curTaskCount = 0;

      suppressNeedRefresh = true;

      try
      {

      // Create directories that need to be created
      // ------------------------------------------

        for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
        {
          updateProgress(curTaskCount++, totalTaskCount);

          if (isCancelled())
            throw new CancelledTaskException();

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
            updateProgress(curTaskCount++, totalTaskCount);

            if (isCancelled())
              throw new CancelledTaskException();

            FilePath srcFilePath = entry.getKey();

            if (srcFilePath.isDirectory() == false)
              srcFilePath.copyTo(entry.getValue(), false);
          }
        }

      // if moving, move files
      // -----------------------------------------------

        else
        {
          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            updateProgress(curTaskCount++, totalTaskCount);

            FilePath srcFilePath = entry.getKey(),
                     destFilePath = entry.getValue();
            HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);

            Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(srcFilePath);

            if (set.isEmpty())
            {
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

          srcToDest.forEach((srcFilePath, destFilePath) ->
          {
            updateProgress(curTaskCount++, totalTaskCount);

            if (srcFilePath.isDirectory() == false) return;

            HDT_Folder folder = HyperPath.getFolderFromFilePath(srcFilePath, false);

            new ArrayList<>(folder.notes).forEach(note -> note.folder.set(HyperPath.getFolderFromFilePath(destFilePath, false)));
          });

      // If moving, remove source directories that are now empty
      // -------------------------------------------------------

          for (FilePath srcFilePath : srcToDest.keySet())
          {
            updateProgress(curTaskCount++, totalTaskCount);

            if (srcFilePath.isDirectory() && (srcFilePath.dirContainsAnyFiles(true) == false))
              HyperPath.getFolderFromFilePath(srcFilePath, false).delete(false);
          }
        }
      }
      catch (IOException e)
      {
        suppressNeedRefresh = false;
        throw new HyperDataException("An error occurred while trying to " + (copying ? "copy" : "move") + " the item(s): " + e.getMessage(), e);
      }

      suppressNeedRefresh = false;
    }};

    boolean success = task.runWithProgressDialog() == State.SUCCEEDED;

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
        goToFilePath(pathToHilite);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class MarkedRowInfo
  {
    MarkedRowInfo(FileRow row) { this.row = row; }

    final FileRow row;
    private boolean related = false;

    private boolean isRelated() { return related; }
  }

  // srcRow is non-null when invoked from right-clicking a column or dragging
  // srcRow is null when clicking a toolbar button

  List<MarkedRowInfo> getMarkedRows(FileRow srcRow)
  {
    List<FileRow> rowList = fileTV.getSelectionModel().getSelectedItems();

    if (collEmpty(rowList) == false)
      return rowList.stream().map(MarkedRowInfo::new).collect(Collectors.toList());

    if (srcRow != null)
      return List.of(new MarkedRowInfo(srcRow));

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();

    if (fileRow != null)
      return List.of(new MarkedRowInfo(fileRow));

    if (curFolder != null)
      return List.of(new MarkedRowInfo(getFolderRow()));

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

    List<MarkedRowInfo> rowInfoList = getMarkedRows(fileRow);

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
      MarkedRowInfo rowInfo = rowInfoList.get(0);
      HyperPath hyperPath = rowInfo.row.getHyperPath();

      if (rowInfo.related)
      {
        RecordType recordType = hyperPath.getRecord().getType();

        String confirmMsg = "The " + (recordType == hdtFolder ? "folder" : "file") + " \"" + hyperPath.getNameStr() + "\" ";

        switch (recordType)
        {
          case hdtPerson   : confirmMsg += "is assigned as a picture file for a person record. Delete it anyway?"; break;
          case hdtMiscFile : confirmMsg += "is assigned to a misc. file record. Okay to delete the file as well as the associated record?"; break;
          case hdtWorkFile : confirmMsg += "is assigned to a work file record. Delete it anyway?"; break;
          case hdtFolder   : confirmMsg += "is assigned to a note record. Delete it anyway?"; break;
          default          : messageDialog("Internal error #21292", mtError); return;
        }

        if (confirmDialog(confirmMsg) == false) return;
      }
      else
      {
        if (rowInfo.row.isDirectory())
        {
          if (confirmDialog("Are you sure you want to delete the folder \"" + hyperPath.getNameStr() + "\" and all the files/subfolders it contains?") == false)
            return;
        }
        else
        {
          if (confirmDialog("Are you sure you want to delete the file \"" + hyperPath.getNameStr() + "\"?") == false)
            return;
        }
      }
    }
    else
    {
      if (rowInfoList.stream().anyMatch(MarkedRowInfo::isRelated))
      {
        if (confirmDialog("One or more of the selected items is associated with a database record. Okay to delete the " + rowInfoList.size() + " items and associated record(s)?") == false)
          return;
      }
      else
      {
        if (confirmDialog("Are you sure you want to delete these " + rowInfoList.size() + " items?") == false)
          return;
      }
    }

  //---------------------------------------------------------------------------
  // Delete the records/folders
  //---------------------------------------------------------------------------

    folderTreeWatcher.stop();

    suppressNeedRefresh = true;

    rowInfoList.stream().allMatch(FileManager::deleteRow); // Deletes rows until deleteRow returns false

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

  private static boolean deleteRow(MarkedRowInfo rowInfo)
  {
    HyperPath hyperPath = rowInfo.row.getHyperPath();
    HDT_RecordWithPath fileRecord = hyperPath.getRecord();
    FilePath filePath = hyperPath.filePath();

    if (rowInfo.related == false)
    {
      if (rowInfo.row.isDirectory())
        return ((HDT_Folder) fileRecord).delete(true);

      try { Files.delete(filePath.toPath()); }
      catch (IOException e)
      {
        return falseWithErrorMessage("Unable to delete the file: " + e.getMessage());
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
        return falseWithErrorMessage("An error occurred while trying to delete \"" + setPath.filePath() + "\": " + e.getMessage());
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean canCutRow(MarkedRowInfo rowInfo, boolean deleting)
  {
    String opPast = deleting ? "deleted" : "moved";
    HyperPath hyperPath = rowInfo.row.getHyperPath();
    HDT_RecordWithPath fileRecord = hyperPath.getRecord();

    if (hyperPath.getRecordsString().length() > 0)
      rowInfo.related = true;

    FilePath filePath = hyperPath.filePath();
    boolean isDir = filePath.isDirectory();

    if (db.isProtectedFile(filePath, true))
      return falseWithErrorMessage((isDir ? "The folder \"" : "The file \"") + filePath + "\" cannot be " + opPast + '.');

    if (deleting == false) return true;

    if (isDir && ((HDT_Folder) fileRecord).containsFilesThatAreInUse())
      return falseWithErrorMessage("The folder \"" + filePath + "\" cannot be deleted, because it contains one or more files or folders that are in use by the database.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void newFolder(HDT_Folder parentFolder)
  {
    if (parentFolder == null) return;

    RenameDlgCtrlr dlg = RenameDlgCtrlr.build("Create Folder in: " + parentFolder.filePath(), ntFolder, "");

    if (dlg.showModal() == false) return;

    suppressNeedRefresh = true;

    try
    {
      parentFolder.filePath().resolve(dlg.getNewName()).createDirectory();
    }
    catch (FileAlreadyExistsException e)
    {
      messageDialog("Unable to create the folder: A file with that name already exists.", mtError);
      return;
    }
    catch (IOException e)
    {
      messageDialog("Unable to create the folder: " + e.getMessage(), mtError);
      return;
    }
    finally
    {
      suppressNeedRefresh = false;
    }

    ui.update();
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
      messageDialog("That " + noun.toLowerCase() + " cannot be renamed.", mtError);
      return;
    }

    RenameDlgCtrlr dlg = RenameDlgCtrlr.build("Rename " + noun + ": " + fileRow.getFilePath(), isDir ? ntFolder : ntFile, fileRow.getFileName());

    if (dlg.showModal() == false) return;

    FilePath srcFilePath = fileRow.getFilePath(),
             parentFilePath = srcFilePath.getParent(),
             destFilePath = parentFilePath.resolve(dlg.getNewName());

    if (destFilePath.exists())
    {
      messageDialog("A " + (destFilePath.isDirectory() ? "folder" : "file") + " with that name already exists.", mtError);
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
      messageDialog("Unable to rename the " + noun.toLowerCase() + ": " + e.getMessage(), mtError);
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

    if (restartWatcher) folderTreeWatcher.createNewWatcherAndStart();

    refresh();
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

    if (HDT_Record.isEmpty(curFolder))
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
        return true;
      }

      if ((rowToPick == null) && (record instanceof HDT_RecordWithPath) && ((HDT_RecordWithPath) record).pathNotEmpty())
        rowToPick = row;
    }

    if (rowToPick == null)
      return false;

    recordTable.selectRow(rowToPick);
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreviewFromRecordTable()
  {
    FilePath filePath = null;
    HDT_Record record = recordTable.selectedRecord();

    if (record instanceof HDT_RecordWithPath)
      filePath = ((HDT_RecordWithPath) record).filePath();

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

          HDT_WorkFile workFile = (HDT_WorkFile)record;
          if (workFile.works.size() > 0)
          {
            HDT_Work work = workFile.works.get(0);

            previewWindow.setPreview(pvsManager, workFile.filePath(), work.getStartPageNum(workFile), work.getEndPageNum(workFile), work);
            return;
          }

          record = recordTable.selectedRecord();
          break;

        case hdtWork :

          workFile = null;

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
            HDT_Work work = (HDT_Work) record;

            previewWindow.setPreview(pvsManager, workFile.filePath(), work.getStartPageNum(workFile), work.getEndPageNum(workFile), work);
            return;
          }

          break;

        case hdtMiscFile :

          HDT_MiscFile miscFile = (HDT_MiscFile) record;

          previewWindow.setPreview(pvsManager, miscFile.filePath(), miscFile);
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

      hasMore = db.getRelatives(folderRecord, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS);
    }
    else
    {
      HDT_RecordWithPath fileRecord = fileRow.getRecord();
      if (fileRecord == null)
      {
        webView.getEngine().loadContent("");
        return;
      }

      hasMore = db.getRelatives(fileRecord, relatives, showingMore ? -1 : ReadOnlyCell.INCREMENTAL_ROWS);

      HyperTableRow row = recordTable.newDataRow();
      row.setCellValue(0, fileRecord, getTypeName(fileRecord.getType()));
      row.setCellValue(1, fileRecord, fileRecord.listName());
    }

    Iterator<HDT_Record> relIt = relatives.iterator();

    while (relIt.hasNext())
    {
      HDT_Record relative = relIt.next();

      if ((hasMore) && (relIt.hasNext() == false))
      {
        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, "",  hdtAuxiliary, CellSortMethod.smLast);
        row.setCellValue(1, "",  hdtNone, CellSortMethod.smLast);
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
        previewWindow.setPreview(pvsManager, folderRecord.filePath(), null);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void goToFilePath(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    HDT_Folder folder = HyperPath.getFolderFromFilePath(filePath.getDirOnly(), false);
    folderTree.selectRecord(folder, 0, false);

    if (filePath.isDirectory()) return;

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
