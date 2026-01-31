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

package org.hypernomicon;

import java.io.*;
import java.net.Socket;
import java.util.*;

import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.apache.commons.io.FileUtils;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

public final class InterProcClient
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private InterProcClient() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------

  private static final String tempFileName = "hypernomiconInstances.tmp",
                              thisInstanceID = randomAlphanumericStr(8);

  static final String UPDATE_CMD = "update";

  private static int portNum = -1;
  private static FilePath dbPath = new FilePath("");
  private static InterProcDaemon daemon = null;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void setPortNum(int portNum)  { InterProcClient.portNum = portNum; }
  public static String getInstanceID() { return thisInstanceID; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean firstRun = true;

  private static Map<String, AppInstance> loadFromFile()
  {
    Map<String, AppInstance> idToInstance = new HashMap<>();

    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));
    if (filePath.exists() == false) return idToInstance;

    List<String> lines = null;

    try { lines = filePath.readToStrList(); }
    catch (IOException e) { logThrowable(e); }

    if (collEmpty(lines) == false) lines.forEach(line ->
    {
      AppInstance instance = AppInstance.fromString(line);

      if (instance != null)
        idToInstance.put(instance.getID(), instance);
    });

    if (firstRun && idToInstance.isEmpty())
      PDFJSWrapper.clearContextFolder();

    firstRun = false;

    return idToInstance;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void writeToFile(Map<String, AppInstance> idToInstance)
  {
    FilePath filePath = tempDir().resolve(new FilePath(tempFileName));

    startDaemonIfNotStartedYet();

    try
    {
      FileUtils.writeLines(filePath.toFile(), idToInstance.values());
    }
    catch (IOException e)
    {
      errorPopup("Unable to write to temporary file: " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void startDaemonIfNotStartedYet()
  {
    if (daemon != null)
      return;

    (daemon = new InterProcDaemon()).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * For each running instance in the instance map (read from temp file), ask that instance
   * for updated information. Then, update the instance map with the updated information.
   * @param idToInstance The instance map
   */
  private static void updateInstances(Map<String, AppInstance> idToInstance)
  {
    Map<String, AppInstance> oldMap = new HashMap<>(idToInstance);
    idToInstance.clear();

    oldMap.forEach((instanceID, instance) ->
    {
      if (instanceID.equals(thisInstanceID)) return;

      try (Socket clientSocket = new Socket("localhost", instance.getPortNum());
           PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
           BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
      {
        out.println(UPDATE_CMD);
        String line = in.readLine();

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

  public static Map<String, AppInstance> updateRunningInstancesFile(FilePath newDbPath)
  {
    dbPath = FilePath.isEmpty(newDbPath) ? new FilePath("") : newDbPath;
    return updateRunningInstancesFile();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void removeThisInstanceFromInstancesTempFile()
  {
    Map<String, AppInstance> idToInstance = loadFromFile();
    idToInstance.remove(thisInstanceID);
    writeToFile(idToInstance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Map<String, AppInstance> updateRunningInstancesFile()
  {
    Map<String, AppInstance> idToInstance = loadFromFile();
    updateInstances(idToInstance);
    writeToFile(idToInstance);

    return idToInstance;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Check to make sure there is not already a running instance of Hypernomicon
   * using the same database folder
   * @param newDbPath The folder to check
   * @return True if there is another conflicting instance; false otherwise
   */
  public static boolean folderInUseByAnotherInstance(FilePath newDbPath)
  {
    newDbPath = newDbPath.getDirOnly();

    for (AppInstance instance : updateRunningInstancesFile().values())
      if (instance.getID().equals(thisInstanceID) == false)
        if (FilePath.isEmpty(instance.getDBPath()) == false)
          if (instance.getDBPath().isSubpath(newDbPath) || newDbPath.isSubpath(instance.getDBPath()))
            return true;

    return false;
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
