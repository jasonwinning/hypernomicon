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

package org.hypernomicon;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.Exceptions.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.HDB_MessageType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static java.nio.file.StandardWatchEventKinds.*;
import static org.hypernomicon.FolderTreeWatcher.WatcherEvent.WatcherEventKind.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.FileVisitResult;
import java.nio.file.FileVisitor;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.WatchEvent;
import java.nio.file.WatchKey;
import java.nio.file.WatchService;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.HyperDB.HDB_MessageType;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.PathInfo;
import org.hypernomicon.model.PathInfo.FileKind;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.filePath.FilePathSet;

import javafx.application.Platform;
import javafx.stage.Modality;

//---------------------------------------------------------------------------

public class FolderTreeWatcher
{
  static class WatcherEvent
  {
    static enum WatcherEventKind { wekRename, wekDelete, wekCreate, wekModify }

    private final WatcherEventKind kind;
    private final PathInfo oldPathInfo, newPathInfo;

    private WatcherEvent(WatcherEventKind kind, PathInfo oldPathInfo, PathInfo newPathInfo)
    {
      this.kind = kind;
      this.oldPathInfo = oldPathInfo;
      this.newPathInfo = newPathInfo;
    }

    private boolean isDirectory()
    {
      return ((oldPathInfo != null) && oldPathInfo.isDirectory()) || ((newPathInfo != null) && newPathInfo.isDirectory());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private class WatcherThread extends Thread
  {
    private boolean done = false;
    private final WatchService watcher;
    private final HashMap<WatchKey, HDT_Folder> watchKeyToDir;
    private boolean sentResponse = false;
    private HDB_MessageType requestType;

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    WatcherThread(WatchService watcher, HashMap<WatchKey, HDT_Folder> watchKeyToDir)
    {
      super();

      this.watcher = watcher;
      this.watchKeyToDir = watchKeyToDir;

      start();
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private void clearKeyQueue()
    {
      while (watcher.poll() != null);
    }

    //---------------------------------------------------------------------------
    //---------------------------------------------------------------------------

    @Override public void run()
    {
      clearKeyQueue();

      try
      {
        registerTree(db.getRootPath());
      }
      catch (IOException e)
      {
        e.printStackTrace();
        messageDialog("Unable to start watch service: " + e.getMessage(), mtError);
        return;
      }

      if (app.debugging())
        System.out.println("Watcher start");

      clearKeyQueue();

      while (done == false)
      {
        if (stopRequested)
        {
          stopRequested = false;
          return;
        }

        List<WatcherEvent> eventList = null;
        WatchKey watchKey = null;

        if (handleInterComputerMessage() == false)
        {
          try { watchKey = watcher.poll(FOLDER_TREE_WATCHER_POLL_TIME_MS, TimeUnit.MILLISECONDS); }
          catch (InterruptedException e1) { return; }
        }

        if (watchKey != null)
          eventList = new ArrayList<>();

        while (watchKey != null)
        {
          List<WatcherEvent> shortList = new ArrayList<>();

          for (final WatchEvent<?> event : watchKey.pollEvents())
          {
            @SuppressWarnings("unchecked")
            WatchEvent<Path> watchEvent = (WatchEvent<Path>) event;

            HDT_Folder folder = watchKeyToDir.get(watchKey);

            if (folder == null)
            {
              folder = HyperPath.getFolderFromFilePath(new FilePath((Path)watchKey.watchable()), false);
              watchKeyToDir.put(watchKey, folder);
            }

            if (folder.getID() > 0)
            {
              FilePath filePath = folder.filePath().resolve(new FilePath(watchEvent.context())); // This is what actually changed
              PathInfo newPathInfo = new PathInfo(filePath);
              WatcherEvent watcherEvent = null;

              if      (watchEvent.kind() == ENTRY_CREATE) watcherEvent = new WatcherEvent(wekCreate, null,        newPathInfo);
              else if (watchEvent.kind() == ENTRY_DELETE) watcherEvent = new WatcherEvent(wekDelete, newPathInfo, null       );
              else if (watchEvent.kind() == ENTRY_MODIFY) watcherEvent = new WatcherEvent(wekModify, newPathInfo, newPathInfo);

              if (watcherEvent != null)
                shortList.add(watcherEvent);
            }
          }

          int deleteNdx = -1, createNdx = -1;
          for (int ndx = 0; ndx < shortList.size(); ndx++)
          {
            WatcherEvent watcherEvent = shortList.get(ndx);

            if      (watcherEvent.kind == wekDelete) deleteNdx = ndx;
            else if (watcherEvent.kind == wekCreate) createNdx = ndx;
          }

          if ((deleteNdx >= 0) && (createNdx >= 0))
          {
            PathInfo oldPathInfo = shortList.get(deleteNdx).oldPathInfo,
                     newPathInfo = shortList.get(createNdx).newPathInfo;

            if (oldPathInfo.getParentFolder() == newPathInfo.getParentFolder())
            {
              shortList.add(new WatcherEvent(wekRename, oldPathInfo, newPathInfo));
              WatcherEvent watcherEvent = shortList.get(createNdx);
              shortList.remove(deleteNdx);
              shortList.remove(watcherEvent);
            }
          }

          eventList.addAll(shortList);

          watchKey.reset();

          watchKey = watcher.poll();
        }

        if (eventList != null)
        {
          try
          {
            processEventList(eventList);
          }
          catch (IOException e)
          {
            messageDialog("Unable to process watcher event list: " + e.getMessage(), mtError);
          }
        }
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private void processEventList(List<WatcherEvent> eventList) throws IOException
    {
      if (app.debugging())
      {
        System.out.println("---------------------------");
        System.out.println("New event list");
        System.out.println("---------------------------");
      }

      boolean dontImport = false;

      for (WatcherEvent watcherEvent : eventList)
      {
        PathInfo oldPathInfo = watcherEvent.oldPathInfo,
                 newPathInfo = watcherEvent.newPathInfo;

        if (ui.windows.getOutermostModality() != Modality.NONE)
          dontImport = true;

        switch (watcherEvent.kind)
        {
          case wekCreate:

            if (watcherEvent.isDirectory())
              registerTree(newPathInfo.getFilePath());
            else if ((newPathInfo.getFileKind() == FileKind.fkFile) &&
                     appPrefs.getBoolean(PREF_KEY_AUTO_IMPORT, true) &&
                     (newPathInfo.getParentFolder().getID() == HyperDB.UNENTERED_FOLDER_ID) &&
                     newPathInfo.getFilePath().getExtensionOnly().equalsIgnoreCase("pdf"))
              if (newPathInfo.getFilePath().size() > 0)
              {
                if ((alreadyImporting == false) && (dontImport == false))
                  Platform.runLater(() -> ui.newWorkAndWorkFile(null, newPathInfo.getFilePath()));

                alreadyImporting = true;
              }
              else
                downloading.add(newPathInfo.getFilePath());

            Platform.runLater(fileManagerDlg::refresh);

            break;

          case wekDelete:

            HyperPath hyperPath = oldPathInfo.getHyperPath();

            if (hyperPath != null)
            {
              if (hyperPath.getRecordsString().length() > 0)
              {
                if (watcherEvent.isDirectory())
                  messageDialog("There has been a change to a folder that is in use by the database. This may or may not cause a data integrity problem. Changes to database folders should be made using the Hypernomicon File Manager instead.", mtWarning);
                else
                {
                  FilePath oldPath = oldPathInfo.getFilePath();

                  Platform.runLater(() ->
                  {
                    sleepForMillis(2000);

                    if (oldPath.exists() == false)
                      messageDialog("A file that is in use by the database, \"" + oldPath.getNameOnly() + "\", has been deleted or moved from outside the program. This may or may not cause a data integrity problem. Changes to database files should be made using the Hypernomicon File Manager instead.", mtWarning);
                  });
                }
              }
              else if (watcherEvent.isDirectory())
                HDT_Folder.deleteFolderRecordTree((HDT_Folder) hyperPath.getRecord());
            }

            Platform.runLater(fileManagerDlg::refresh);

            break;

          case wekModify:

            if ((newPathInfo.getFileKind() == FileKind.fkFile) &&
                appPrefs.getBoolean(PREF_KEY_AUTO_IMPORT, true) &&
                downloading.contains(newPathInfo.getFilePath()))
            {
              if ((alreadyImporting == false) && (dontImport == false))
                Platform.runLater(() -> ui.newWorkAndWorkFile(null, newPathInfo.getFilePath()));

              alreadyImporting = true;
              downloading.remove(newPathInfo.getFilePath());
            }

            break;

          case wekRename:

            hyperPath = oldPathInfo.getHyperPath();

            boolean untrackedFile = (newPathInfo.getFileKind() == FileKind.fkFile) &&
                                    ((hyperPath == null) || hyperPath.getRecordsString().isBlank());

            if (untrackedFile)
            {
              if (appPrefs.getBoolean(PREF_KEY_AUTO_IMPORT, true) &&
                  (newPathInfo.getParentFolder().getID() == HyperDB.UNENTERED_FOLDER_ID) &&
                  (oldPathInfo.getFilePath().getExtensionOnly().equalsIgnoreCase("pdf") == false) &&
                  newPathInfo.getFilePath().getExtensionOnly().equalsIgnoreCase("pdf"))
              {
                if ((alreadyImporting == false) && (dontImport == false))
                  Platform.runLater(() -> ui.newWorkAndWorkFile(null, newPathInfo.getFilePath()));

                alreadyImporting = true;
              }
            }
            else if (hyperPath != null)
            {
              if (hyperPath.getRecordsString().length() > 0)
              {
                if (watcherEvent.isDirectory())
                  messageDialog("There has been a change to a folder that is in use by the database. This may or may not cause a data integrity problem. Changes to database folders should be made using the Hypernomicon File Manager instead.", mtWarning);
                else
                {
                  FilePath newPath = newPathInfo.getFilePath();
                  final HyperPath hyperPath2 = hyperPath;

                  Platform.runLater(() ->
                  {
                    sleepForMillis(2000);

                    if ((newPath.exists() == false) || oldPathInfo.getFilePath().equals(newPath)) return;

                    if (confirmDialog("A file that is in use by the database has been renamed from outside the program." + System.lineSeparator() +
                                      "This may or may not cause a data integrity problem." + System.lineSeparator() +
                                      "Should the record be reassigned to \"" + newPath.getNameOnly() + "\"?"))
                    {
                      if (newPath.exists())
                      {
                        hyperPath2.assign(hyperPath2.parentFolder(), newPath.getNameOnly());

                        HDT_RecordWithPath record = hyperPath2.getRecord();

                        if (record != null)
                        {
                          if ((record.getType() == hdtWorkFile) && (ui.activeTabEnum() == workTabEnum))
                          {
                            HDT_WorkFile workFile = (HDT_WorkFile) record;

                            if (workFile.works.contains(ui.activeRecord()))
                            {
                              if      (ui.workHyperTab().wdc != null) ui.workHyperTab().wdc.btnCancel.fire();
                              else if (ui.workHyperTab().fdc != null) ui.workHyperTab().fdc.btnCancel.fire();

                              ui.workHyperTab().refreshFiles();
                            }
                          }
                          else if ((record.getType() == hdtMiscFile) && (ui.activeTabEnum() == fileTabEnum))
                          {
                            if (ui.fileHyperTab().fdc != null)
                              ui.fileHyperTab().fdc.btnCancel.fire();

                            ui.fileHyperTab().refreshFile();
                          }
                        }
                      }
                      else
                        messageDialog("The file \"" + newPath.getNameOnly() + "\" no longer exists. Record was not changed.", mtError);
                    }
                  });
                }
              }
            }

            if ((hyperPath != null) && watcherEvent.isDirectory())
            {
              hyperPath.assign(hyperPath.parentFolder(), newPathInfo.getFilePath().getNameOnly());
              registerTree(newPathInfo.getFilePath());
            }

            Platform.runLater(fileManagerDlg::refresh);

            break;

          default:
            break;

        }

        if (app.debugging())
        {
          switch (watcherEvent.kind)
          {
            case wekCreate : System.out.println("Created: \""       + watcherEvent.newPathInfo + "\""); break;
            case wekDelete : System.out.println("Deleted: \""       + watcherEvent.oldPathInfo + "\""); break;
            case wekModify : System.out.println("Modified: \""      + watcherEvent.newPathInfo + "\""); break;
            case wekRename : System.out.println("Renamed: \""       + watcherEvent.oldPathInfo +
                                                "\" to: \""         + watcherEvent.newPathInfo.getFilePath().getNameOnly() + "\""); break;

            default        : System.out.println("Unknown event: \"" + watcherEvent.newPathInfo + "\""); break;
          }
        }
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean handleInterComputerMessage()
    {
      String compName = getComputerName();
      InterComputerMsg receivedMsg;

      if (sentResponse)
      {
        if (db.getRequestMessageFilePath().exists())
          return false;

        sentResponse = false;

        try { Files.delete(db.getResponseMessageFilePath().toPath()); } catch (IOException e) { noOp(); }

        if (requestType == hmtUnlockRequest)
        {
          done = true;

          Platform.runLater(() -> ui.shutDown(true, true, false));
        }

        return true;
      }

      if (db.getRequestMessageFilePath().exists() == false)
        return false;

      receivedMsg = InterComputerMsg.checkForMessage(db.getRequestMessageFilePath());
      requestType = hmtNone;

      if ((receivedMsg != null) && receivedMsg.getDest().equals(compName))
        requestType = receivedMsg.getType();

      switch (requestType)
      {
        case hmtEchoRequest :

          new InterComputerMsg(compName, receivedMsg.getSource(), hmtEchoReply).writeToDisk();
          sentResponse = true;
          return true;

        case hmtUnlockRequest :

          new InterComputerMsg(compName, receivedMsg.getSource(), hmtUnlockComplete).writeToDisk();
          sentResponse = true;
          return true;

        default :
          return true;
      }
    }
  }

  private WatchService watcher;
  private WatcherThread watcherThread;
  private final FilePathSet downloading = new FilePathSet();
  private final HashMap<WatchKey, HDT_Folder> watchKeyToDir = new HashMap<>();
  public static final int FOLDER_TREE_WATCHER_POLL_TIME_MS = 100;
  public static boolean alreadyImporting = false;
  private boolean stopRequested = false,
                  stopped = true,
                  disabled = false;

  public void disable()       { stop(); disabled = true; }
  public void enable()        { disabled = false; }
  public boolean isDisabled() { return disabled; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean createNewWatcherAndStart()
  {
    stop();

    if (disabled || db.isLoaded() == false) return false;

    watchKeyToDir.clear();

    try
    {
      watcher = FileSystems.getDefault().newWatchService();
    }
    catch (IOException e)
    {
      e.printStackTrace();
      return falseWithErrorMessage("Unable to start watch service: " + e.getMessage());
    }

    start();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerTree(FilePath rootFilePath) throws IOException
  {
    Files.walkFileTree(rootFilePath.toPath(), new FileVisitor<>()
    {
      /**
       * Invoked for a directory before entries in the directory are visited.
       */
      @Override public FileVisitResult preVisitDirectory(Path path, BasicFileAttributes attrs) throws IOException
      {
        Objects.requireNonNull(path);
        Objects.requireNonNull(attrs);

        if (new FilePath(path).exists() == false) return FileVisitResult.SKIP_SUBTREE;

        HDT_Folder folder = HyperPath.getFolderFromFilePath(new FilePath(path), true);

        if (folder == null)
          throw new IOException(new HDB_InternalError(92733));

        watchKeyToDir.put(path.register(watcher, ENTRY_CREATE, ENTRY_DELETE, ENTRY_MODIFY), folder);

        return FileVisitResult.CONTINUE;
      }

      /**
       * Invoked for a file in a directory.
       */
      @Override public FileVisitResult visitFile(Path path, BasicFileAttributes attrs) throws IOException
      {
        Objects.requireNonNull(path);
        Objects.requireNonNull(attrs);
        return FileVisitResult.CONTINUE;
      }

      /**
       * Invoked for a file that could not be visited.
       */
      @Override public FileVisitResult visitFileFailed(Path path, IOException exc) throws IOException
      {
        Objects.requireNonNull(path);

        if (new FilePath(path).exists() == false) return FileVisitResult.CONTINUE; // If folder doesn't exist just keep going

        throw exc;
      }

      /**
       * Invoked for a directory after entries in the directory, and all of their
       * descendants, have been visited.
       */
      @Override public FileVisitResult postVisitDirectory(Path path, IOException exc) throws IOException
      {
        Objects.requireNonNull(path);

        if (exc != null) throw exc;

        return FileVisitResult.CONTINUE;
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void start()
  {
    if (watcherThread != null)
    {
      if (watcherThread.isAlive()) return;
      stop();
    }

    watcherThread = new WatcherThread(watcher, watchKeyToDir);
    stopped = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean isRunning()
  {
    if (stopped == true) return false;
    return nullSwitch(watcherThread, false, WatcherThread::isAlive);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean stop()
  {
    boolean wasRunning = isRunning();

    if ((watcherThread != null) && watcherThread.isAlive())
    {
      stopRequested = true;
      try { watcherThread.join(); } catch (InterruptedException e) { noOp(); }
    }

    if ((watcher != null) && (stopped == false))
    {
      try
      {
        watcher.close();
        stopped = true;

        if (app.debugging())
          System.out.println("Watcher closed");
      }
      catch (IOException e)
      {
        if (app.debugging())
          System.out.println("Watcher close exception");
      }
    }

    watcherThread = null;
    return wasRunning;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
