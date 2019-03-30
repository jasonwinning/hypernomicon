/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.fileManager;

import static org.hypernomicon.App.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.apache.commons.io.FileUtils;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithDescription;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;

import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.model.relations.RelationSet.RelationType.*;
import static org.hypernomicon.view.wrappers.HyperTableColumn.HyperCtrlType.*;
import static org.hypernomicon.view.dialogs.RenameDlgCtrlr.NameType.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;
import org.hypernomicon.view.HyperView.TextViewInfo;
import org.hypernomicon.view.dialogs.HyperDlg;
import org.hypernomicon.view.dialogs.RenameDlgCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.tabs.FileTabCtrlr;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.wrappers.HyperTable;
import org.hypernomicon.view.wrappers.HyperTableCell;
import org.hypernomicon.view.wrappers.HyperTableCell.HyperCellSortMethod;
import org.hypernomicon.view.wrappers.HyperTableRow;
import org.hypernomicon.view.wrappers.MenuItemSchema;
import org.hypernomicon.view.wrappers.ReadOnlyCell;
import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.SplitPane;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.control.TreeView;
import javafx.scene.input.MouseButton;
import javafx.scene.web.WebView;
import javafx.stage.Modality;
import javafx.stage.StageStyle;

public class FileManager extends HyperDlg
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class HistoryItem
  {
    private final HDT_Folder folder;
    private final FilePath fileName;
    private final HDT_Base record;

    private HistoryItem(FileRow folderRow, FileRow fileRow, HDT_Base record)
    {
      folder = (HDT_Folder) folderRow.getRecord();
      fileName = fileRow == null ? null : fileRow.getFilePath().getNameOnly();
      this.record = record;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class FolderHistory
  {
    private final List<HistoryItem> history = new ArrayList<>();
    private final Button btnForward, btnBack;
    private int ndx = -1;
    private boolean doAdd = true;

    private FolderHistory(Button btnForward, Button btnBack)
    {
      this.btnForward = btnForward;
      this.btnBack = btnBack;
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
  @FXML private TextField tfSlated;
  @FXML private Button btnForward, btnBack, btnCut, btnCopy, btnPaste, btnDelete, btnRename, btnRefresh,
                       btnMainWindow, btnPreviewWindow, btnNewFolder;
  @FXML private SplitPane spMain, spFiles, spRecords;

  private static final String dialogTitle = "File Manager";

  private List<MarkedRowInfo> markedRows = null, dragRows = null;
  private FilePath srcPathToHilite = null;
  private boolean clipboardCopying, needRefresh = false, suppressNeedRefresh = false;
  private MenuItemSchema<HDT_RecordWithPath, FileRow> pasteMenuItem;
  private HDT_Folder curFolder;

  public FolderTreeWrapper folderTree;
  private FileTable fileTable;
  private HyperTable recordTable;
  private FolderHistory history;
  private static HyperTask task;
  private static long totalTaskCount, curTaskCount;

  FileRow getFolderRow() { return nullSwitch(curFolder, null, folder -> folderTree.getRowsForRecord(folder).get(0)); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public boolean isValid() { return true; }

  public void setNeedRefresh()
  {
    if (suppressNeedRefresh) return;
    needRefresh = true;
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

  private List<MarkedRowInfo> getSrcRows(boolean dragging)
  {
    return dragging ? dragRows : markedRows;
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

  private static enum PasteAnswer { check, overwriteNone, overwriteAll }

  private boolean doPasteChecks(FileRow destRow, Map<FilePath, FilePath> srcToDest, boolean copying, boolean dragging)
  {
    final FilePathSet srcSet = new FilePathSet();
    final FilePathSet destSet = new FilePathSet();
    srcPathToHilite = null;

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Building list of files...");
      updateProgress(-1, -1);

      for (MarkedRowInfo rowInfo : getSrcRows(dragging))
      {
        if (FilePath.isEmpty(srcPathToHilite))
          srcPathToHilite = rowInfo.row.getFilePath();

        if (rowInfo.row.isDirectory())
          if (!rowInfo.row.getFilePath().addDirContentsToSet(srcSet))
            throw new TerminateTaskException();

        srcSet.add(rowInfo.row.getFilePath());
      }

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task)) return false;

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Performing checks...");
      updateProgress(0, 1);

      totalTaskCount = srcSet.size() * 2;
      curTaskCount = 0;

      FilePath baseDir = getSrcRows(dragging).get(0).row.getFilePath().getParent();

      for (FilePath srcFilePath : srcSet)
      {
        updateProgress(curTaskCount, totalTaskCount);
        curTaskCount++;

        if (isCancelled())
          throw new TerminateTaskException();

        if (copying == false)
        {
          if (db.isProtectedFile(srcFilePath))
            throw new TerminateTaskException("Unable to move path \"" + srcFilePath + "\".");
        }

        FilePath destFilePath = destRow.getFilePath().resolve(baseDir.relativize(srcFilePath));

        if (srcFilePath.equals(destFilePath))
          throw new TerminateTaskException("Source and destination are the same.");

        if (copying == false)
          if (srcFilePath.isDirectory())
            if (srcFilePath.isSubpath(destFilePath))
              throw new TerminateTaskException("The destination folder is a subfolder of the source folder.");

        destSet.add(destFilePath);
        srcToDest.put(srcFilePath, destFilePath);
      }

      for (FilePath destFilePath : destSet)
      {
        updateProgress(curTaskCount, totalTaskCount);
        curTaskCount++;

        if (isCancelled())
          throw new TerminateTaskException();

        if (srcSet.contains(destFilePath))
          throw new TerminateTaskException("Destination path \"" + destFilePath + "\" is also one of the source paths.");

        if (destFilePath.exists())
        {
          if (destFilePath.isDirectory())
            throw new TerminateTaskException("A folder already exists at destination path \"" + destFilePath + "\".");
          else if (db.isProtectedFile(destFilePath))
            throw new TerminateTaskException("Cannot overwrite destination path: \"" + destFilePath + "\".");
        }
      }

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task)) return false;

    PasteAnswer paUnrelated = PasteAnswer.check,
                paRelated = PasteAnswer.check;

    Iterator<Entry<FilePath, FilePath>> it = srcToDest.entrySet().iterator();

    while (it.hasNext())
    {
      Entry<FilePath, FilePath> entry = it.next();

      Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(entry.getValue());
      boolean isRelated = (set.isEmpty() == false) && set.stream().anyMatch(hyperPath -> hyperPath.getRecordType() != hdtNone);

      if (!isRelated && entry.getValue().exists())
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

            set.forEach(hyperPath ->
            {
              if (hyperPath.getRecordType() != hdtNone)
                confirmMessage.append(db.getTypeName(hyperPath.getRecord().getType())).append(" ID ")
                              .append(hyperPath.getRecord().getID()).append(": ")
                              .append(hyperPath.getRecord().getCBText()).append("\n");
            });

            confirmMessage.append("\nOkay to overwrite the file with \"" + entry.getKey() + "\"?");

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

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      updateMessage("Obtaining locks...");
      updateProgress(0, 1);

      totalTaskCount = srcToDest.size();
      curTaskCount = 0;

      try
      {
        for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
        {
          updateProgress(curTaskCount, totalTaskCount);
          curTaskCount++;

          if (isCancelled())
            throw new TerminateTaskException();

          if (copying == false)
            if (entry.getKey().canObtainLock() == false)
              throw new TerminateTaskException("Unable to obtain lock on path: \"" + entry.getKey() + "\"");

          if (entry.getValue().canObtainLock() == false)
            throw new TerminateTaskException("Unable to obtain lock on path: \"" + entry.getValue() + "\"");
        }
      }
      catch (IOException e)
      {
        throw new TerminateTaskException("Unable to obtain lock: " + e.getMessage(), e);
      }

      return true;
    }};

    if (!HyperTask.performTaskWithProgressDialog(task))
      return false;

    return !srcToDest.isEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void paste(FileRow destRow, boolean copying, boolean dragging)
  {
    Map<FilePath, FilePath> srcToDest = new HashMap<>();

    if (destRow == null)
      destRow = getFolderRow();

    if (destRow == null) return;

    if (!doPasteChecks(destRow, srcToDest, copying, dragging))
    {
      if (folderTreeWatcher.isRunning() == false)
        folderTreeWatcher.createNewWatcherAndStart();
      return;
    }

    folderTreeWatcher.stop();

    task = new HyperTask() { @Override protected Boolean call() throws Exception
    {
      if (copying)
      {
        updateMessage("Copying...");
        totalTaskCount = srcToDest.size() * 2;
      }
      else
      {
        updateMessage("Moving...");
        totalTaskCount = srcToDest.size() * 4;
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
          updateProgress(curTaskCount, totalTaskCount);
          curTaskCount++;

          if (isCancelled())
            throw new TerminateTaskException();

          FilePath srcFilePath  = entry.getKey(),
                   destFilePath = entry.getValue();

          if (srcFilePath.isDirectory())
            if (destFilePath.exists() == false)
              FileUtils.forceMkdir(destFilePath.toFile());
        }

      // if copying, copy files
      // -----------------------------------------------

        if (copying)
        {
          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            updateProgress(curTaskCount, totalTaskCount);
            curTaskCount++;

            if (isCancelled())
              throw new TerminateTaskException();

            FilePath srcFilePath = entry.getKey();

            if (srcFilePath.isDirectory() == false)
            {
              srcFilePath = entry.getKey();
              FilePath destFilePath = entry.getValue();

              if (!srcFilePath.copyTo(destFilePath, false))
                throw new TerminateTaskException();
            }
          }
        }

      // if moving, move files
      // -----------------------------------------------

        else
        {
          for (Entry<FilePath, FilePath> entry : srcToDest.entrySet())
          {
            updateProgress(curTaskCount, totalTaskCount);
            curTaskCount++;

            FilePath srcFilePath = entry.getKey(),
                     destFilePath = entry.getValue();
            HDT_Folder folder = HyperPath.getFolderFromFilePath(destFilePath.getDirOnly(), true);

            Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(srcFilePath);

            if (set.isEmpty())
            {
              if (!srcFilePath.moveTo(destFilePath, false))
                throw new TerminateTaskException();
            }
            else
            {
              for (HyperPath hyperPath : set)
              {
                HDT_RecordType recordType = hdtNone;

                if (hyperPath.getRecord() != null)
                  recordType = hyperPath.getRecord().getType();

                if (recordType != hdtFolder)
                  if (!hyperPath.moveToFolder(folder.getID(), false, false, ""))
                    throw new TerminateTaskException();
              }
            }
          }

      // if moving, update note records
      // ------------------------------

          srcToDest.forEach((srcFilePath, destFilePath) ->
          {
            updateProgress(curTaskCount++, totalTaskCount);

            if ((srcFilePath.exists() == false) || (srcFilePath.isDirectory() == false)) return;

            HDT_Folder folder = HyperPath.getFolderFromFilePath(srcFilePath, false);

            new ArrayList<>(folder.notes).forEach(note -> note.folder.set(HyperPath.getFolderFromFilePath(destFilePath, false)));
          });

      // If moving, remove source directories that are now empty
      // -------------------------------------------------------

          for (FilePath srcFilePath : srcToDest.keySet())
          {
            updateProgress(curTaskCount, totalTaskCount);
            curTaskCount++;

            if (srcFilePath.exists())
              if (srcFilePath.isDirectory())
                if (srcFilePath.dirContainsAnyFiles(true) == false)
                  HyperPath.getFolderFromFilePath(srcFilePath, false).delete(false);
          }
        }
      }
      catch (IOException e)
      {
        suppressNeedRefresh = false;
        throw new TerminateTaskException("An error occurred while trying to " + (copying ? "copy" : "move") + " the item(s): " + e.getMessage(), e);
      }

      suppressNeedRefresh = false;
      return true;
    }};

    boolean success = HyperTask.performTaskWithProgressDialog(task);

    suppressNeedRefresh = false;

    FilePath pathToHilite = srcToDest.isEmpty() ? null : srcToDest.get(srcPathToHilite);

    if (success || dragging)
      clearSrcRows(dragging);

    Platform.runLater(() ->
    {
      btnRefreshClick();
      folderTreeWatcher.createNewWatcherAndStart();

      ui.update();

      if (FilePath.isEmpty(pathToHilite) == false)
        goToFilePath(pathToHilite);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FileManager create()
  {
    FileManager managerDlg = HyperDlg.createUsingFullPath("view/fileManager/FileManager.fxml", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);
    managerDlg.init();
    return managerDlg;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    initContainers();

    fileTable.addContextMenuItem("Launch", fileRow -> fileRow.isDirectory() == false, fileRow -> launchFile(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Show in system explorer", fileRow -> highlightFileInExplorer(fileRow.getFilePath()));
    fileTable.addContextMenuItem("Copy path to clipboard", fileRow -> copyToClipboard(fileRow.getFilePath().toString()));
    fileTable.addContextMenuItem("Rename", this::rename);

    fileTable.addContextMenuItem("New misc. file record", fileRow -> fileRow.getRecord() == null, fileRow ->
    {
      HDT_MiscFile miscFile = db.createNewBlankRecord(hdtMiscFile);

      miscFile.getPath().assign(fileRow.getFolder(), fileRow.getFilePath().getNameOnly());
      ui.goToRecord(miscFile, true);

      FileTabCtrlr fileCtrlr = HyperTab.getHyperTab(miscFileTab);
      if (fileCtrlr.btnManageClick() == false)
      {
        miscFile.getPath().clear(false);
        ui.deleteCurrentRecord(false);
      }

      refresh();
    });

    fileTable.addContextMenuItem("Cut", fileRow -> cutCopy(fileRow, false));
    fileTable.addContextMenuItem("Copy", fileRow -> cutCopy(fileRow, true));
    pasteMenuItem = fileTable.addContextMenuItem("Paste into this folder", FileRow::isDirectory, dirRow -> paste(dirRow, clipboardCopying, false));
    fileTable.addContextMenuItem("Delete", this::delete);

    btnCut.setOnAction(event -> cutCopy(null, false));
    btnCopy.setOnAction(event -> cutCopy(null, true));
    btnPaste.setOnAction(event -> paste(null, clipboardCopying, false));
    btnDelete.setOnAction(event -> delete(null));
    btnNewFolder.setOnAction(event -> newFolder());

    history = new FolderHistory(btnForward, btnBack);

    btnBack.setOnAction(event -> btnBackClick());
    btnForward.setOnAction(event -> btnForwardClick());
    btnRefresh.setOnAction(event -> btnRefreshClick());
    btnRename.setOnAction(event -> rename(null));

    btnMainWindow.setOnAction(event -> ui.windows.focusStage(app.getPrimaryStage()));
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

    dialogStage.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus()) return;

      if (newValue == null) return;
      if (newValue == false) return;

      ui.windows.push(dialogStage);

      if (needRefresh) refresh();
    });

    dialogStage.setOnHidden(event -> ui.windows.focusStage(app.getPrimaryStage()));

    recordTable.addDefaultMenuItems();

    btnBack         .setTooltip(new Tooltip("Previous folder in history"));
    btnForward      .setTooltip(new Tooltip("Next folder in history"));
    btnCut          .setTooltip(new Tooltip("Cut"));
    btnCopy         .setTooltip(new Tooltip("Copy"));
    btnPaste        .setTooltip(new Tooltip("Paste"));
    btnDelete       .setTooltip(new Tooltip("Delete"));
    btnNewFolder    .setTooltip(new Tooltip("Create new folder"));
    btnRename       .setTooltip(new Tooltip("Rename selected file or folder"));
    btnRefresh      .setTooltip(new Tooltip("Refresh"));
    btnMainWindow   .setTooltip(new Tooltip("Return to main application window"));
    btnPreviewWindow.setTooltip(new Tooltip("Show preview window"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static class MarkedRowInfo
  {
    MarkedRowInfo(FileRow row, boolean related)
    {
      this.row = row;
      this.related = related;
    }

    final FileRow row;
    private boolean related;

    public boolean isRelated() { return related; }
  }

  List<MarkedRowInfo> getMarkedRows(FileRow srcRow)
  {
    List<MarkedRowInfo> rowInfoList = new ArrayList<>();
    List<FileRow> rowList = fileTV.getSelectionModel().getSelectedItems();

    if ((rowList != null) && (rowList.size() > 1))
    {
      rowList.forEach(fileRow -> rowInfoList.add(new MarkedRowInfo(fileRow, false)));
      return rowInfoList;
    }

    if (rowInfoList.isEmpty() && (srcRow != null))
    {
      rowInfoList.add(new MarkedRowInfo(srcRow, false));
      return rowInfoList;
    }

    if ((rowList != null) && (rowList.size() > 0))
    {
      rowList.forEach(fileRow -> rowInfoList.add(new MarkedRowInfo(fileRow, false)));
      return rowInfoList;
    }

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();

    if (fileRow != null)
      rowInfoList.add(new MarkedRowInfo(fileRow, false));
    else if (curFolder != null)
      rowInfoList.add(new MarkedRowInfo(getFolderRow(), false));

    return rowInfoList;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void delete(FileRow fileRow)
  {
    if (ui.cantSaveRecord(true)) return;

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
        String confirmMsg = "The file \"" + hyperPath.getNameStr() + "\" ";

        switch (hyperPath.getRecord().getType())
        {
          case hdtPerson :   confirmMsg = confirmMsg + "is assigned as a picture file for a person record. Delete it anyway?"; break;
          case hdtMiscFile : confirmMsg = confirmMsg + "is assigned to a misc. file record. Okay to delete the file as well as the associated record?"; break;
          case hdtWorkFile : confirmMsg = confirmMsg + "is assigned to a work file record. Delete it anyway?"; break;
          case hdtFolder :   confirmMsg = confirmMsg + "is assigned to a note record. Delete it anyway?"; break;
          default :          messageDialog("Internal error #21292", mtError); return;
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

    rowInfoList.stream().allMatch(this::deleteRow); // Deletes rows until deleteRow returns false

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

  private boolean deleteRow(MarkedRowInfo rowInfo)
  {
    HyperPath hyperPath = rowInfo.row.getHyperPath();
    HDT_RecordWithPath fileRecord = hyperPath.getRecord();
    FilePath filePath = hyperPath.getFilePath();

    if (rowInfo.related == false)
    {
      if (rowInfo.row.isDirectory())
      {
        return HDT_Folder.class.cast(fileRecord).delete(true);
      }
      else
      {
        try { Files.delete(filePath.toPath()); }
        catch (IOException e)
        {
          return falseWithErrorMessage("Unable to delete the file: " + e.getMessage());
        }

        db.unmapFilePath(filePath);
        return true;
      }
    }

    Set<HyperPath> set = HyperPath.getHyperPathSetForFilePath(filePath);

    for (HyperPath setPath : set)
    {
      try
      {
        setPath.getFilePath().delete(true);
        db.unmapFilePath(setPath.getFilePath());
        if (setPath.getRecordType() != hdtNone)
          db.deleteRecord(setPath.getRecord().getType(), setPath.getRecord().getID());
      }
      catch (IOException e)
      {
        return falseWithErrorMessage("An error occurred while trying to delete \"" + setPath.getFilePath() + "\": " + e.getMessage());
      }
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean canCutRow(MarkedRowInfo rowInfo, boolean deleting)
  {
    String opPast = deleting ? "deleted" : "moved";
    HyperPath hyperPath = rowInfo.row.getHyperPath();
    HDT_RecordWithPath fileRecord = hyperPath.getRecord();

    if (hyperPath.getRecordsString().length() > 0)
      rowInfo.related = true;

    FilePath filePath = hyperPath.getFilePath();
    boolean isDir = filePath.isDirectory();

    if ((fileRecord != null) && (deleting == false))
      if (fileRecord.getType() == hdtPerson)
        return falseWithErrorMessage("The file \"" + filePath + "\" cannot be moved: It is in use as a picture for person record ID " + fileRecord.getID() + ".");

    if (db.isProtectedFile(filePath))
      return falseWithErrorMessage((isDir ? "The folder \"" : "The file \"") + filePath + "\" cannot be " + opPast + ".");

    if (deleting == false) return true;

    if (isDir)
      if (HDT_Folder.class.cast(fileRecord).containsFilesThatAreInUse())
        return falseWithErrorMessage("The folder \"" + filePath + "\" cannot be deleted, because it contains one or more files or folders that are in use by the database.");

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void newFolder()
  {
    if (curFolder == null) return;

    RenameDlgCtrlr dlg = RenameDlgCtrlr.create("Create folder in: " + curFolder.getPath().getFilePath(), ntFolder, "");

    if (dlg.showModal() == false) return;

    suppressNeedRefresh = true;

    try
    {
      Files.createDirectory(curFolder.getPath().getFilePath().resolve(dlg.getNewName()).toPath());
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
    if (ui.cantSaveRecord(true)) return;

    if (fileRow == null) fileRow = fileTV.getSelectionModel().getSelectedItem();
    if (fileRow == null) fileRow = getFolderRow();
    if (fileRow == null) return;

    HDT_RecordWithPath fileRecord = fileRow.getRecord();
    boolean isDir = fileRow.isDirectory(), cantRename;
    String noun;

    if (isDir) { noun = "folder"; cantRename = HyperDB.isUnstoredRecord(fileRecord.getID(), hdtFolder); }
    else       { noun = "file";   cantRename = db.isProtectedFile(fileRow.getFilePath()); }

    if (cantRename)
    {
      messageDialog("That " + noun + " cannot be renamed.", mtError);
      return;
    }

    RenameDlgCtrlr dlg = RenameDlgCtrlr.create("Rename " + noun + ": " + fileRow.getFilePath(), ntFolder, fileRow.getFileName());

    if (dlg.showModal() == false) return;

    FilePath srcFilePath = fileRow.getFilePath();
    FilePath parentFilePath = srcFilePath.getParent();
    FilePath destFilePath = parentFilePath.resolve(dlg.getNewName());

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
        success = HyperPath.renameFile(fileRecord.getPath().getFilePath(), dlg.getNewName());
    }
    catch (IOException e)
    {
      messageDialog("Unable to rename the " + noun + ": " + e.getMessage(), mtError);
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

  private void btnRefreshClick()
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
    if (dialogStage.isShowing() == false) return;
    needRefresh = false;

    if (folderTree.selectedRecord() == null)
      folderTree.selectRecord(db.folders.getByID(HyperDB.ROOT_FOLDER_ID), -1, false);

    treeView.refresh();

    if (HDT_Record.isEmpty(curFolder))
      curFolder = null;

    if (curFolder == null)
    {
      fileTable.clear();
      recordTable.clear();
      getStage().setTitle(dialogTitle);
      return;
    }

    FileRow fileRow = fileTV.getSelectionModel().getSelectedItem();
    FilePath filePath = fileRow == null ? null : fileRow.getFilePath();

    fileTable.update(curFolder, folderTree.selectedItem());

    if (FilePath.isEmpty(filePath))
      setCurrentFileRow(null, false);

    getStage().setTitle(dialogTitle + " - " + curFolder.getPath().getFilePath());

    fileTV.sort();

    if (FilePath.isEmpty(filePath) == false)
      fileTable.selectByFileName(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void btnBackClick()
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

    if (item.fileName == null) return;

    fileTable.selectByFileName(item.fileName);

    if (item.record != null)
    {
      HyperTableRow row = findFirst(recordTable.getDataRows(), r -> r.getRecord() == item.record);
      if (row != null)
      {
        recordTable.selectRow(row);
        recordTV.requestFocus();
        return;
      }
    }

    fileTV.requestFocus();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void btnForwardClick()
  {
    if (btnForward.isDisabled()) return;

    history.doAdd = false;
    invokeHistoryItem(history.forward());
    history.doAdd = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initContainers()
  {
    fileTable = new FileTable(fileTV, PREF_KEY_HT_MGR_FILES);
    folderTree = new FolderTreeWrapper(treeView, fileTable);

    folderTree.getTreeModel().addParentChildRelation(rtParentFolderOfFolder, true);

    recordTable = new HyperTable(recordTV, 1, false, PREF_KEY_HT_FM_RECORDS, this);

    recordTable.addCol(hdtNone, ctIncremental);
    recordTable.addCol(hdtNone, ctNone);

    treeView.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue == oldValue)) return;

      HDT_Folder folder;
      try
      {
        folder = HyperPath.getFolderFromFilePath(newValue.getValue().getFilePath(), true);
      }
      catch (Exception e)
      {
        messageDialog("A file error occurred: " + e.getMessage(), mtError);
        return;
      }

      curFolder = folder;
      history.add(new HistoryItem(newValue.getValue(), null, null));

      fileTable.update(folder, newValue);
      setCurrentFileRow(null, false);
      getStage().setTitle(dialogTitle + " - " + folder.getPath().getFilePath());
    });

    fileTV.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
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
        previewWindow.setPreview(pvsManager, newValue.getFilePath(), 1, -1, null);
    });

    recordTable.setOnShowMore(() -> setCurrentFileRow(fileTV.getSelectionModel().getSelectedItem(), true));

    fileTV.setRowFactory(thisTV ->
    {
      TableRow<FileRow> row = new TableRow<>();

      row.setOnMouseClicked(mouseEvent ->
      {
        if ((mouseEvent.getButton().equals(MouseButton.PRIMARY)) && (mouseEvent.getClickCount() == 2))
        {
          FileRow fileRow = row.getItem();

          if (fileRow != null)
          {
            if (fileRow.isDirectory())
              folderTree.getSelectionModel().select(fileRow.getTreeItem());
            else
              launchFile(fileRow.getFilePath());
          }
        }
      });

      fileTable.setupDragHandlers(row);

      row.itemProperty().addListener((observable, oldValue, newValue) ->
      {
        if (newValue == null)
          row.setContextMenu(null);
        else
          row.setContextMenu(fileTable.createContextMenu(newValue, fileTable.getContextMenuSchemata()));
      });

      return row;
    });

    recordTV.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == oldValue) return;

      if (newValue != null)
      {
        HDT_Base record = HyperTableCell.getRecord(newValue.getCell(1));
        history.updateCurrent(new HistoryItem(folderTree.selectedItem().getValue(), fileTV.getSelectionModel().getSelectedItem(), record));

        if (record != null)
        {
          if (record.getType() == hdtWorkFile)
          {
            HDT_WorkFile workFile = HDT_WorkFile.class.cast(record);
            if (workFile.works.isEmpty() == false)
              record = workFile.works.get(0);
          }

          String mainText = "";
          if (record.hasDesc())
            mainText = HDT_RecordWithDescription.class.cast(record).getDesc().getHtml();

          MainTextWrapper.setReadOnlyHTML(getHtmlEditorText(mainText), webView.getEngine(), new TextViewInfo(), null);

          setPreviewFromRecordTable();

          return;
        }
      }

      setPreviewFromRecordTable();

      webView.getEngine().loadContent("");
    });

    webView.getEngine().titleProperty().addListener((ChangeListener<String>) (observable, oldValue, newValue) ->
    {
      HDT_Base record = recordTable.selectedRecord();
      if (record == null) return;

      String mainText = "";
      if (record.hasDesc())
        mainText = HDT_RecordWithDescription.class.cast(record).getDesc().getHtml();

      MainTextWrapper.handleJSEvent(getHtmlEditorText(mainText), webView.getEngine(), new TextViewInfo());
    });

    webView.setOnContextMenuRequested(event -> setHTMLContextMenu());

    MainTextWrapper.webViewAddZoom(webView, PREF_KEY_FILEMGR_ZOOM);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean selectNonBlankRecordRow()
  {
    HyperTableRow rowToPick = null;

    for (HyperTableRow row : recordTable.getDataRows())
    {
      HDT_Base record = row.getRecord();
      if (record != null)
      {
        if (record.hasDesc())
        {
          String text = HDT_RecordWithDescription.class.cast(record).getDesc().getPlain().trim();
          if (text.length() > 0)
          {
            recordTable.selectRow(row);
            return true;
          }
        }

        if (rowToPick == null)
          if (record instanceof HDT_RecordWithPath)
            if (HDT_RecordWithPath.class.cast(record).getPath().isEmpty() == false)
              rowToPick = row;
      }
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
    HDT_Base record = recordTable.selectedRecord();

    if (record instanceof HDT_RecordWithPath)
      filePath = HDT_RecordWithPath.class.cast(record).getPath().getFilePath();

    FilePath fileTablePath = nullSwitch(fileTV.getSelectionModel().getSelectedItem(), null, FileRow::getFilePath);

    if (record != null)
    {
      if (record.getType() == hdtWork)
      {
        HDT_WorkFile workFile = null;

        if (FilePath.isEmpty(fileTablePath) == false)
        {
          HDT_RecordWithPath recordWP = HyperPath.getFileFromFilePath(fileTablePath);
          if ((recordWP != null) && (recordWP.getType() == hdtWorkFile))
            workFile = (HDT_WorkFile) recordWP;
        }

        if (workFile == null)
          if (FilePath.isEmpty(filePath) == false)
            workFile = HDT_WorkFile.class.cast(HyperPath.getFileFromFilePath(filePath));

        if (workFile != null)
        {
          HDT_Work work = (HDT_Work) record;

          previewWindow.setPreview(pvsManager, workFile.getPath().getFilePath(), work.getStartPageNum(workFile), work.getEndPageNum(workFile), work);
          return;
        }
      }
      else if (record.getType() == hdtMiscFile)
      {
        HDT_MiscFile miscFile = (HDT_MiscFile) record;

        previewWindow.setPreview(pvsManager, miscFile.getPath().getFilePath(), -1, -1, miscFile);
        return;
      }
    }

    if (FilePath.isEmpty(fileTablePath) == false)
      previewWindow.setPreview(pvsManager, fileTablePath, 1, -1, record);
    else
      previewWindow.setPreview(pvsManager, filePath, 1, -1, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setCurrentFileRow(FileRow fileRow, boolean showingMore)
  {
    HDT_Folder folderRecord = null;
    LinkedHashSet<HDT_Base> relatives = new LinkedHashSet<>();
    boolean hasMore;

    recordTable.clear();

    if (fileRow == null)
      folderRecord = curFolder;
    else if (fileRow.isDirectory())
      folderRecord = (HDT_Folder) fileRow.getRecord();

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
      row.setCellValue(0, fileRecord, db.getTypeName(fileRecord.getType()));
      row.setCellValue(1, fileRecord, fileRecord.listName());
    }

    Iterator<HDT_Base> relIt = relatives.iterator();

    while (relIt.hasNext())
    {
      HDT_Base relative = relIt.next();

      if ((hasMore) && (relIt.hasNext() == false))
      {
        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, -1, "",  hdtAuxiliary, HyperCellSortMethod.hsmLast);
        row.setCellValue(1, -1, "",  hdtNone, HyperCellSortMethod.hsmLast);
        break;
      }

      if (relative.getType() != hdtFolder)
      {
        HyperTableRow row = recordTable.newDataRow();
        row.setCellValue(0, relative, db.getTypeName(relative.getType()));
        row.setCellValue(1, relative, relative.listName());
      }
    }

    if (folderRecord != null)
    {
      if (selectNonBlankRecordRow() == false)
        previewWindow.setPreview(pvsManager, folderRecord.getPath().getFilePath(), 1, -1, null);
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
