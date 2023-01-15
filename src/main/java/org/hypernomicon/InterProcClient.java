/*
 * Copyright 2015-2023 Jason Winning
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

import static java.nio.charset.StandardCharsets.*;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import org.apache.commons.io.FileUtils;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.util.filePath.FilePath;

public final class InterProcClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InterProcClient() { throw new UnsupportedOperationException(); }

//---------------------------------------------------------------------------

  private static final String tempFileName = "hypernomiconInstances.tmp",
                              thisInstanceID = randomAlphanumericStr(8);
  static final String UPDATE_CMD = "update";

  private static int portNum = -1;
  private static FilePath dbPath = new FilePath("");
  private static Map<String, AppInstance> idToInstance = new HashMap<>();
  private static InterProcDaemon daemon = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void setPortNum(int portNum)  { InterProcClient.portNum = portNum; }
  public static String getInstanceID() { return thisInstanceID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean firstRun = true;

  private static void loadFromFile()
  {
    idToInstance.clear();

    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));
    if (filePath.exists() == false) return;

    List<String> lines = null;

    try { lines = FileUtils.readLines(filePath.toFile(), UTF_8); }
    catch (IOException e) { noOp(); }

    if (collEmpty(lines) == false) lines.forEach(line ->
    {
      AppInstance instance = AppInstance.fromString(line);

      if (instance != null)
        idToInstance.put(instance.getID(), instance);
    });

    if (firstRun && idToInstance.isEmpty())
      PDFJSWrapper.clearContextFolder();

    firstRun = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void writeToFile()
  {
    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));

    startDaemonIfNotStartedYet();

    try
    {
      FileUtils.writeLines(filePath.toFile(), idToInstance.values());
    }
    catch (IOException e)
    {
      messageDialog("Unable to write to temporary file: " + e.getMessage(), mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void startDaemonIfNotStartedYet()
  {
    if (daemon != null)
      return;

    daemon = new InterProcDaemon();
    daemon.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void updateInstances()
  {
    Map<String, AppInstance> oldMap = idToInstance;
    idToInstance = new HashMap<>();

    oldMap.forEach((instanceID, instance) ->
    {
      if (instanceID.equals(thisInstanceID)) return;

      try (Socket clientSocket = new Socket("localhost", instance.getPortNum());
           PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
           BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
      {
        out.println(UPDATE_CMD);
        String line = null;
        for (; line == null; line = in.readLine());

        AppInstance newInstance = AppInstance.fromString(line);

        if (newInstance != null)
          idToInstance.put(newInstance.getID(), newInstance);
      }
      catch (IOException e) { noOp(); }
    });

    idToInstance.put(thisInstanceID, getInstance());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void refresh(FilePath newDbPath)
  {
    dbPath = FilePath.isEmpty(newDbPath) ? new FilePath("") : newDbPath;
    refresh();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void removeThisInstance()
  {
    loadFromFile();
    idToInstance.remove(thisInstanceID);
    writeToFile();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void refresh()
  {
    loadFromFile();
    updateInstances();
    writeToFile();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean checkFolder(FilePath newDbPath)
  {
    refresh();

    newDbPath = newDbPath.getDirOnly();

    for (AppInstance instance : idToInstance.values())
    {
      if (instance.getID().equals(thisInstanceID) == false)
        if (FilePath.isEmpty(instance.getDBPath()) == false)
          if (instance.getDBPath().isSubpath(newDbPath) || newDbPath.isSubpath(instance.getDBPath()))
            return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static AppInstance getInstance()
  {
    startDaemonIfNotStartedYet();

    while (portNum < 0)
      sleepForMillis(50);

    return new AppInstance(thisInstanceID, portNum, dbPath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
