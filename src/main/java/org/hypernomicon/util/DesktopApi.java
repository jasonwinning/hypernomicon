/*
 * Copyright 2015-2020 Jason Winning
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

import static org.hypernomicon.util.Util.MessageDialogType.*;

import java.awt.Desktop;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.util.List;

import static org.hypernomicon.util.Util.*;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;

import org.hypernomicon.util.filePath.FilePath;

import com.google.common.collect.Lists;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class DesktopApi
{
  static boolean browse(String url)
  {
    if (SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC)
      return browseDesktop(url);

    try
    {
      @SuppressWarnings("unused") URI uri = new URI(url);
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

    if (SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC)
      return openDesktop(filePath);

    return openSystemSpecific(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean edit(FilePath filePath)
  {
    if (SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC)
      return editDesktop(filePath);

    return openSystemSpecific(filePath.toString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean openSystemSpecific(String pathStr)
  {
    StringBuilder sb = new StringBuilder();

    if (SystemUtils.IS_OS_LINUX)
    {
      if (exec(false, false, sb, "kde-open"  , pathStr)) return true;
      if (exec(false, false, sb, "gnome-open", pathStr)) return true;
      if (exec(false, false, sb, "xdg-open"  , pathStr)) return true;
//      if (exec(false, false, sb, "exo-open"  , pathStr)) return true;
//      if (exec(false, false, sb, "gvfs-open" , pathStr)) return true;
    }

    return falseWithErrorMessage("Unable to open the file: " + pathStr + (sb.length() > 0 ? "\n" + sb.toString() : "") + ".");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean browseDesktop(String url)
  {
    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)))
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
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.OPEN)))
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
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.EDIT)))
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

  public static boolean exec(boolean showErrMsg, boolean wait, StringBuilder errorSB, String... parts)
  {
    return exec(showErrMsg, wait, errorSB, Lists.newArrayList(parts));
  }

  public static boolean exec(boolean showErrMsg, boolean wait, StringBuilder errorSB, List<String> command)
  {
    if (SystemUtils.IS_OS_MAC)
    {
      command.set(0, "-a");
      command.set(0, "open");
    }

    ProcessBuilder pb = new ProcessBuilder(command);
    Process proc;
    int exitValue = 0;

    try
    {
      proc = pb.start();

      if (wait)
      {
        exitValue = proc.waitFor();
        
        try (InputStream is = proc.getErrorStream())
        {
          assignSB(errorSB, IOUtils.toString(is, StandardCharsets.UTF_8));
        }
      }
    }
    catch (IOException | InterruptedException e)
    {
      if (showErrMsg)
        messageDialog("An error occurred while trying to start application: " + e.getMessage(), mtError);

      return false;
    }

    if ((exitValue != 0) && showErrMsg)
      messageDialog("An error occurred while trying to start application: " + errorSB.toString(), mtError);

    return exitValue == 0;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
