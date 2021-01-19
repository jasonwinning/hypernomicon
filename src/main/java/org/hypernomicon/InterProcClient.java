/*
 * Copyright 2015-2021 Jason Winning
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

import static java.nio.charset.StandardCharsets.UTF_8;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.util.DesktopUtil.*;

import org.apache.commons.io.FileUtils;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.util.SplitString;
import org.hypernomicon.util.filePath.FilePath;

public class InterProcClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static class AppInstance
  {
    private String instanceID;
    private int portNum;
    private FilePath dbPath;

    private AppInstance(String instanceID, int portNum, FilePath dbPath)
    {
      this.instanceID = instanceID;
      this.portNum = portNum;
      this.dbPath = FilePath.isEmpty(dbPath) ? new FilePath("") : dbPath;
    }

  //---------------------------------------------------------------------------

    @Override public String toString()
    {
      return instanceID + ";" + String.valueOf(portNum) + ";" + dbPath;
    }

  //---------------------------------------------------------------------------

    private static AppInstance fromString(String line)
    {
      SplitString splitStr = new SplitString(line, ';');

      String instanceID = splitStr.next();
      int portNum = parseInt(splitStr.next(), -1);
      FilePath dbPath = new FilePath(splitStr.next());

      if (safeStr(instanceID).isBlank() || (portNum < 1)) return null;

      return new AppInstance(instanceID, portNum, dbPath);
    }
  }

//---------------------------------------------------------------------------

  private static final String tempFileName = "hypernomiconInstances.tmp",
                              thisInstanceID = randomAlphanumericStr(8);
  private static final int LOWEST_PORT = 59346;
  static final String UPDATE_CMD = "update";

  private static int portNum = -1;
  private static FilePath dbPath = new FilePath("");
  private static Map<String, AppInstance> idToInstance = new HashMap<>();
  private static InterProcDaemon daemon = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static int getPortNum()              { return portNum; }
  static AppInstance getInstance()     { return new AppInstance(thisInstanceID, portNum, dbPath); }
  public static String getInstanceID() { return thisInstanceID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean firstRun = true;

  private static void loadFromFile()
  {
    idToInstance.clear();

    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));
    if (filePath.exists() == false) return;

    List<String> s = null;

    try { s = FileUtils.readLines(filePath.toFile(), UTF_8); }
    catch (IOException e) { noOp(); }

    if (collEmpty(s) == false)
    {
      s.forEach(line ->
      {
        AppInstance instance = AppInstance.fromString(line);

        if (instance != null)
          idToInstance.put(instance.instanceID, instance);
      });
    }

    if (firstRun && idToInstance.isEmpty())
      PDFJSWrapper.clearContextFolder();

    firstRun = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void writeToFile()
  {
    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));

    if (daemon == null)
    {
      daemon = new InterProcDaemon();
      daemon.start();
    }

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

  private static void updateInstances()
  {
    Map<String, AppInstance> oldMap = idToInstance;
    idToInstance = new HashMap<>();

    oldMap.forEach((instanceID, instance) ->
    {
      if (instanceID.equals(thisInstanceID)) return;

      try (Socket clientSocket = new Socket("localhost", instance.portNum);
           PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
           BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
      {
        out.println(UPDATE_CMD);
        String line = null;
        for (; line == null; line = in.readLine());

        AppInstance newInstance = AppInstance.fromString(line);

        if (newInstance != null)
          idToInstance.put(newInstance.instanceID, newInstance);
      }
      catch (IOException e) { noOp(); }
    });

    if (portNum < 1)
    {
      portNum = LOWEST_PORT;

      Set<Integer> ports = idToInstance.values().stream().map(instance -> instance.portNum)
                                                         .collect(Collectors.toSet());

      while (ports.contains(portNum))
        portNum++;
    }

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
      if (instance.instanceID.equals(thisInstanceID) == false)
        if (FilePath.isEmpty(instance.dbPath) == false)
          if (instance.dbPath.isSubpath(newDbPath) || newDbPath.isSubpath(instance.dbPath))
            return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
