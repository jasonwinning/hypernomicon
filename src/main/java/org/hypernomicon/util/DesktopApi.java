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

package org.hypernomicon.util;

import static org.hypernomicon.util.Util.MessageDialogType.mtError;

import java.awt.Desktop;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hypernomicon.util.Util.*;

import org.apache.commons.lang3.SystemUtils;

import org.hypernomicon.util.filePath.FilePath;

import com.google.common.collect.Lists;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class DesktopApi
{
  @SuppressWarnings("unused")
  static boolean browse(String url)
  {
    if ((SystemUtils.IS_OS_WINDOWS) || (SystemUtils.IS_OS_MAC))
      return browseDesktop(url);

    try
    {
      new URI(url);
    }
    catch (URISyntaxException e)
    {
      return falseWithErrorMessage("An error occurred while trying to browse to: " + url + ". " + e.getMessage());
    }

    return openSystemSpecific(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean open(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return true;

    if ((SystemUtils.IS_OS_WINDOWS) || (SystemUtils.IS_OS_MAC))
      return openDesktop(filePath);

    return openSystemSpecific(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean edit(FilePath filePath)
  {
    if ((SystemUtils.IS_OS_WINDOWS) || (SystemUtils.IS_OS_MAC))
      return editDesktop(filePath);

    return openSystemSpecific(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean openSystemSpecific(String pathStr)
  {
    if (SystemUtils.IS_OS_LINUX)
    {
      if (runCommand("kde-open", "%s", pathStr)) return true;
      if (runCommand("gnome-open", "%s", pathStr)) return true;
      if (runCommand("xdg-open", "%s", pathStr)) return true;
      //if (runCommand("exo-open", "%s", path)) return true;
      //if (runCommand("gvfs-open", "%s", path)) return true;

      messageDialog("Unable to open the file: " + pathStr + ".", mtError);
    }

    if (SystemUtils.IS_OS_MAC)
    {
      if (runCommand("open", "%s", pathStr)) return true;
    }

    if (SystemUtils.IS_OS_WINDOWS)
    {
      if (runCommand("explorer", "%s", pathStr)) return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean browseDesktop(String url)
  {
    try
    {
      if (!Desktop.isDesktopSupported())
        return falseWithErrorMessage("An error occurred while trying to browse to: " + url + ".");

      if (!Desktop.getDesktop().isSupported(Desktop.Action.BROWSE))
        return falseWithErrorMessage("An error occurred while trying to browse to: " + url + ".");

      Desktop.getDesktop().browse(new URI(url));
      return true;
    }
    catch (Exception e)
    {
      return falseWithErrorMessage("An error occurred while trying to browse to: " + url + ". " + e.getMessage());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean openDesktop(FilePath filePath)
  {
    try
    {
      if (!Desktop.isDesktopSupported())
        return falseWithErrorMessage("An error occurred while trying to open the file: " + filePath);

      if (!Desktop.getDesktop().isSupported(Desktop.Action.OPEN))
        return falseWithErrorMessage("An error occurred while trying to open the file: " + filePath);

      Desktop.getDesktop().open(filePath.toFile());
      return true;
    }
    catch (Exception e)
    {
      return falseWithErrorMessage("An error occurred while trying to open the file: " + filePath + ". " + e.getMessage());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean editDesktop(FilePath filePath)
  {
    try
    {
      if (!Desktop.isDesktopSupported())
        return falseWithErrorMessage("An error occurred while trying to edit the file: " + filePath + ".");

      if (!Desktop.getDesktop().isSupported(Desktop.Action.EDIT))
        return falseWithErrorMessage("An error occurred while trying to edit the file: " + filePath + ".");

      Desktop.getDesktop().edit(filePath.toFile());
      return true;
    }
    catch (Exception e)
    {
      return falseWithErrorMessage("An error occurred while trying to edit the file: " + filePath + ". " + e.getMessage());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static boolean runCommand(String command, String args, String pathStr)
  {
    String[] parts = prepareCommand(command, args, pathStr);

    try
    {
      Process p = Runtime.getRuntime().exec(parts);
      if (p == null) return false;

      try { p.waitFor(); } catch(Exception e) { noOp(); }

      try
      {
        return p.exitValue() == 0;
      }
      catch (IllegalThreadStateException itse)
      {
        // Process is running.
        return false;
      }
    }
    catch (IOException e)
    {
      // Error running command.
      return false;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String[] prepareCommand(String command, String args, String pathStr)
  {
    List<String> parts = Lists.newArrayList(command);

    if (args != null)
    {
      for (String s : args.split(" "))
      {
        s = String.format(s, pathStr); // put in the filename thing

        parts.add(s.trim());
      }
    }

    return parts.toArray(new String[parts.size()]);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launchExplicit(String execPathStr, String ... params)
  {
    ArrayList<String> command = new ArrayList<>();

    try
    {
      if (SystemUtils.IS_OS_MAC)
      {
        Collections.addAll(command, "open", "-a", execPathStr);
        Collections.addAll(command, params);

        Runtime.getRuntime().exec(command.toArray(new String[0])).waitFor();
      }
      else
      {
        command.add(execPathStr);
        Collections.addAll(command, params);

        ProcessBuilder pb = new ProcessBuilder(command.toArray(new String[0]));
        pb.start();
      }
    }
    catch (IOException | InterruptedException e)
    {
      messageDialog("An error occurred while trying to start application: " + e.getMessage(), mtError);
    }
  }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}


