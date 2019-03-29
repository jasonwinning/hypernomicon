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

import org.hypernomicon.model.HyperDB.HDB_MessageType;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.PathInfo;
import org.hypernomicon.model.records.*;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.tabs.FileTabController;
import org.hypernomicon.view.tabs.WorkTabController;
import javafx.application.Platform;

//---------------------------------------------------------------------------

public class FolderTreeWatcher
{
  static class WatcherEvent
  {
    static enum WatcherEventKind
    {
      wekRename,
      wekDelete,
      wekCreate,
      wekModify
    }

    private final WatcherEventKind kind;
    private final PathInfo oldPathInfo, newPathInfo;

    private WatcherEventKind getKind() { return kind; }
    private PathInfo getOldPathInfo()  { return oldPathInfo; }
    private PathInfo getNewPathInfo()  { return newPathInfo; }

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
        registerTree(db.getRootFilePath());
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
              FilePath filePath = folder.getPath().getFilePath().resolve(new FilePath(watchEvent.context())); // This is what actually changed
              PathInfo newPathInfo = new PathInfo(filePath);
              WatcherEvent watcherEvent = null;

              if      (watchEvent.kind() == ENTRY_CREATE)  watcherEvent = new WatcherEvent(wekCreate, null,        newPathInfo);
              else if (watchEvent.kind() == ENTRY_DELETE)  watcherEvent = new WatcherEvent(wekDelete, newPathInfo, null       );
              else if (watchEvent.kind() == ENTRY_MODIFY)  watcherEvent = new WatcherEvent(wekModify, newPathInfo, newPathInfo);

              if (watcherEvent != null)
                shortList.add(watcherEvent);
            }
          }

          int deleteNdx = -1, createNdx = -1;
          for (int ndx = 0; ndx < shortList.size(); ndx++)
          {
            WatcherEvent watcherEvent = shortList.get(ndx);

            if      (watcherEvent.getKind() == wekDelete) deleteNdx = ndx;
            else if (watcherEvent.getKind() == wekCreate) createNdx = ndx;
          }

          if ((deleteNdx >= 0) && (createNdx >= 0))
          {
            PathInfo oldPathInfo = shortList.get(deleteNdx).getOldPathInfo(),
                     newPathInfo = shortList.get(createNdx).getNewPathInfo();

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

      for (WatcherEvent watcherEvent : eventList)
      {
        PathInfo oldPathInfo = watcherEvent.getOldPathInfo(),
                 newPathInfo = watcherEvent.getNewPathInfo();

        switch (watcherEvent.getKind())
        {
          case wekCreate:

            if (watcherEvent.isDirectory())
              registerTree(newPathInfo.getFilePath());

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
            break;

          case wekRename:

            hyperPath = oldPathInfo.getHyperPath();

            if (hyperPath != null)
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

                    if (newPath.exists())
                    {
                      if (confirmDialog("A file that is in use by the database has been renamed from outside the program." + System.lineSeparator() +
                                        "This may or may not cause a data integrity problem." + System.lineSeparator() +
                                        "Should the record be reassigned to \"" + newPath.getNameOnly() + "\"?"))
                      {
                        if (newPath.exists())
                        {
                          hyperPath2.assign(hyperPath2.getParentFolder(), newPath.getNameOnly());

                          HDT_RecordWithPath record = hyperPath2.getRecord();

                          if (record != null)
                          {
                            if (record.getType() == hdtWorkFile)
                            {
                              HDT_WorkFile workFile = (HDT_WorkFile) record;

                              if (ui.activeTabEnum() == workTab)
                              {
                                if (workFile.works.contains(ui.activeRecord()))
                                {
                                  WorkTabController tabWorks = (WorkTabController) ui.activeTab();
                                  if      (tabWorks.wdc != null)  tabWorks.wdc.btnCancel.fire();
                                  else if (tabWorks.fdc != null)  tabWorks.fdc.btnCancel.fire();

                                  tabWorks.refreshFiles();
                                }
                              }
                            }
                            else if (record.getType() == hdtMiscFile)
                            {
                              if (ui.activeTabEnum() == miscFileTab)
                              {
                                FileTabController tabFiles = (FileTabController) ui.activeTab();
                                if (tabFiles.fdc != null)
                                  tabFiles.fdc.btnCancel.fire();

                                tabFiles.refreshFile();
                              }
                            }
                          }
                        }
                        else
                          messageDialog("The file \"" + newPath.getNameOnly() + "\" no longer exists. Record was not changed.", mtError);
                      }
                    }
                  });
                }
              }

              if (watcherEvent.isDirectory())
              {
                hyperPath.assign(hyperPath.getParentFolder(), newPathInfo.getFilePath().getNameOnly());
                registerTree(newPathInfo.getFilePath());
              }
            }

            Platform.runLater(fileManagerDlg::refresh);

            break;

          default:
            break;

        }

        if (app.debugging())
        {
          switch (watcherEvent.getKind())
          {
            case wekCreate: System.out.println("Created: \"" + watcherEvent.getNewPathInfo() + "\"");  break;
            case wekDelete: System.out.println("Deleted: \"" + watcherEvent.getOldPathInfo() + "\"");  break;
            case wekModify: System.out.println("Modified: \"" + watcherEvent.getNewPathInfo() + "\""); break;
            case wekRename: System.out.println("Renamed: \"" + watcherEvent.getOldPathInfo() +
                                               "\" to: \"" + watcherEvent.getNewPathInfo().getFilePath().getNameOnly() + "\""); break;

            default:        System.out.println("Unknown event: \"" + watcherEvent.getNewPathInfo() + "\""); break;
          }
        }
      }
    }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

    private boolean handleInterComputerMessage()
    {
      String compName = getComputerName();
      InterComputerMsg sentMsg, receivedMsg;

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

          sentMsg = new InterComputerMsg(compName, receivedMsg.getSource(), hmtEchoReply);
          sentMsg.writeToDisk();
          sentResponse = true;
          break;

        case hmtUnlockRequest :

          sentMsg = new InterComputerMsg(compName, receivedMsg.getSource(), hmtUnlockComplete);
          sentMsg.writeToDisk();
          sentResponse = true;
          break;

        default :
          break;
      }

      return true;
    }
  }

  private WatchService watcher;
  private WatcherThread watcherThread;
  private HashMap<WatchKey, HDT_Folder> watchKeyToDir;
  public static final int FOLDER_TREE_WATCHER_POLL_TIME_MS = 100;
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

    watchKeyToDir = new HashMap<>();

    try
    {
      watcher = FileSystems.getDefault().newWatchService();
    }
    catch (IOException e)
    {
      e.printStackTrace();
      messageDialog("Unable to start watch service: " + e.getMessage(), mtError);
      return false;
    }

    start();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerTree(FilePath rootFilePath) throws IOException
  {
    Files.walkFileTree(rootFilePath.toPath(), new FileVisitor<Path>()
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
    if (watcherThread == null)
    {
      watcherThread = new WatcherThread(watcher, watchKeyToDir);

      stopped = false;
    }
    else if (watcherThread.isAlive() == false)
    {
      stop();

      watcherThread = new WatcherThread(watcher, watchKeyToDir);

      stopped = false;
    }
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
