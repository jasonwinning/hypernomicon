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

package org.hypernomicon.util;

import java.awt.Desktop;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Scanner;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.WebButton.WebButtonField.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.settings.LaunchCommandsDlgCtrlr;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.tabs.WorkTabCtrlr;

import com.google.common.collect.Lists;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public final class DesktopUtil
{

//---------------------------------------------------------------------------

  private DesktopUtil() { throw new UnsupportedOperationException(); }

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

    return falseWithErrorMessage("Unable to open the file: " + pathStr + (sb.length() > 0 ? "\n" + sb : "") + '.');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean browseDesktop(String url)
  {
    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)))
        return falseWithErrorMessage("An error occurred while trying to browse to: " + url + '.');

      Desktop.getDesktop().browse(makeURI(url));
      return true;
    }
    catch (IOException | URISyntaxException e)
    {
      return falseWithErrorMessage("An error occurred while trying to browse to: " + url + ". " + e.getMessage());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static URI makeURI(String url) throws URISyntaxException
  {
    if (url.contains(":") == false)
      return new URI(url);

    int pos = url.indexOf(':');
    String scheme = url.substring(0, pos),
           ssp = safeSubstring(url, pos + 1, url.length());

    if (ssp.contains("#") == false)
    {
      try
      {
        return new URI(url);
      }
      catch (URISyntaxException e)
      {
        return new URI(scheme, ssp, "");
      }
    }

    pos = ssp.indexOf('#');
    String fragment = safeSubstring(ssp, pos + 1, ssp.length());
    ssp = ssp.substring(0, pos);

    return new URI(scheme, ssp, fragment);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static boolean openFile(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return true;

    if ((SystemUtils.IS_OS_WINDOWS == false) && (SystemUtils.IS_OS_MAC == false))
      return openSystemSpecific(filePath.toString());

    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.OPEN)))
        return falseWithErrorMessage("An error occurred while trying to open the file: " + filePath);

      Desktop.getDesktop().open(filePath.toFile());
      return true;
    }
    catch (IOException e)
    {
      return falseWithErrorMessage("An error occurred while trying to open the file: " + filePath + ". " + e.getMessage());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean editFile(FilePath filePath)
  {
    if ((SystemUtils.IS_OS_WINDOWS == false) && (SystemUtils.IS_OS_MAC == false))
      return openSystemSpecific(filePath.toString());

    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.EDIT)))
        return falseWithErrorMessage("An error occurred while trying to edit the file: " + filePath + '.');

      Desktop.getDesktop().edit(filePath.toFile());
      return true;
    }
    catch (IOException e)
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
    int exitValue = 0;

    try
    {
      Process proc = pb.start();

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
      return falseWithErrMsgCond(showErrMsg, "An error occurred while trying to start application: " + e.getMessage());
    }

    return (exitValue == 0) || falseWithErrMsgCond(showErrMsg, "An error occurred while trying to start application: " + errorSB);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchORCID(String orcid, String first, String last)
  {
    if (safeStr(orcid).length() > 0)
    {
      openWebLink("http://orcid.org/" + escapeURL(orcid, false));
      return;
    }

    first = removeAllParentheticals(safeStr(first));
    last = removeAllParentheticals(safeStr(last));

    if ((first + last).isBlank())
      return;

    WebButton btn = new WebButton("ORCID", "ORCID");

    btn.addPattern("https://orcid.org/orcid-search/search?firstName=" + QueryName + "&lastName=" + LastName, QueryName, LastName);
    btn.addPattern("https://orcid.org/orcid-search/search?firstName=" + FirstName, FirstName);
    btn.addPattern("https://orcid.org/orcid-search/search?lastName=" + LastName, LastName);

    btn.first(QueryName, first);
    btn.next(FirstName, first);
    btn.next(LastName, last);
    btn.go();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchDOI(String str)
  {
    String doi = matchDOI(str);
    if (doi.length() > 0)
      openWebLink("http://dx.doi.org/" + escapeURL(doi, false));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static void openWebLink(String url)
  {
    url = url.trim();

    if (url.isEmpty()) return;

    if (url.startsWith(EXT_1) && (extPath() == null))
    {
      messageDialog(WorkTabCtrlr.NO_EXT_PATH_MESSAGE, mtWarning);
      return;
    }

    if (url.contains(":") == false)
      url = "http://" + url;

    if (SystemUtils.IS_OS_WINDOWS || SystemUtils.IS_OS_MAC)
    {
      browseDesktop(url);
      return;
    }

    try
    {
      noOp(makeURI(url));
    }
    catch (URISyntaxException e)
    {
      falseWithErrorMessage("An error occurred while trying to browse to: " + url + ". " + e.getMessage());
      return;
    }

    openSystemSpecific(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launchFile(FilePath filePath)
  {
    launchWorkFile(filePath, 1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void launchWorkFile(FilePath filePath, int pageNum)
  {
    if (FilePath.isEmpty(filePath)) return;

    String readerPath = app.prefs.get(PREF_KEY_PDF_READER, "");

    if (("pdf".equalsIgnoreCase(filePath.getExtensionOnly()) == false) || readerPath.isEmpty())
    {
      openFile(filePath);
      return;
    }

    if (pageNum < 1) pageNum = 1;

    LaunchCommandsDlgCtrlr.launch(readerPath, filePath, PREF_KEY_PDF_READER_COMMANDS, PREF_KEY_PDF_READER_COMMAND_TYPE, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void highlightFileInExplorer(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    try
    {
      if (SystemUtils.IS_OS_WINDOWS)
        Runtime.getRuntime().exec("explorer.exe /select,\"" + filePath + '"').waitFor();

      else if (SystemUtils.IS_OS_MAC)
        Runtime.getRuntime().exec(new String[] {"open", "-R", filePath.toString()}).waitFor();

      else if (SystemUtils.IS_OS_LINUX)
      {
        if (exec(false, false, new StringBuilder(), "nautilus", filePath.toString()) == false)
          launchFile(filePath.getDirOnly());  // this won't highlight the file in the folder
      }

      // xdg-mime query default inode/directory

      else
        launchFile(filePath.getDirOnly());  // this won't highlight the file in the folder
    }
    catch (IOException | InterruptedException e)
    {
      messageDialog("An error occurred while trying to show the file: " + filePath + ". " + e.getMessage(), mtError);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FilePath homeDir()
  {
    return new FilePath(System.getProperty("user.home"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static FilePath tempDir()
  {
    return new FilePath(System.getProperty("java.io.tmpdir"));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String userWorkingDir()
  {
    return System.getProperty("user.dir");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String computerName = null;

  private static String formatName(String name)
  {
    return ultraTrim(safeStr(name).replaceAll("\\p{C}", ""));
  }

  public static String getComputerName()
  {
    if (computerName != null) return computerName;

    String uuid = "", hostName = getHostName();

    try
    {
      String output;

      if (SystemUtils.IS_OS_WINDOWS)
      {
        output = execReadToString("wmic csproduct get UUID");

        SplitString ss = new SplitString(output, '\n');
        ss.next();
        uuid = ss.next();
      }
      else if (SystemUtils.IS_OS_MAC)
      {
        output = execReadToString("system_profiler SPHardwareDataType");

        String[] arr = output.split("UUID: ", 2);
        uuid = arr.length < 2 ? "" : new SplitString(arr[1], '\n').next();
      }
      else if (SystemUtils.IS_OS_LINUX)
      {
        uuid = execReadToString("cat /etc/machine-id");
      }
    }
    catch (IOException | InterruptedException e) { noOp(); }

    uuid = ultraTrim(safeStr(uuid));
    return computerName = uuid.isBlank() ? hostName : (hostName.replace("::::", "") + "::::" + uuid.toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getHostName()
  {
    String hostName = formatName(SystemUtils.getHostName());
    if (hostName.length() > 0) return hostName;

    hostName = formatName(System.getenv("HOSTNAME"));
    if (hostName.length() > 0) return hostName;

    hostName = formatName(System.getenv("COMPUTERNAME"));
    if (hostName.length() > 0) return hostName;

    try
    {
      hostName = formatName(execReadToString("hostname"));
      if (hostName.length() > 0) return hostName;
    }
    catch (IOException | InterruptedException e) { noOp(); }

    if (SystemUtils.IS_OS_WINDOWS == false) try
    {
      for (String line : FileUtils.readLines(new File("/etc/hostname"), (Charset)null))
      {
        hostName = formatName(line);
        if (hostName.length() > 0) return hostName;
      }
    }
    catch (IOException e) { noOp(); }

    try { hostName = formatName(InetAddress.getLocalHost().getHostName()); }
    catch (UnknownHostException e) { return ""; }

    if (hostName.isBlank())
      messageDialog("Unable to determine computer name", mtError);

    return hostName;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String execReadToString(String execCommand) throws IOException, InterruptedException
  {
    Process process = Runtime.getRuntime().exec(execCommand);
    String output = "";

    try (Scanner s = new Scanner(process.getInputStream()).useDelimiter("\\A"))
    {
      if (s.hasNext())
      {
        output = s.next();

        while (s.hasNext())
          s.next();
      }
    }

    process.waitFor();

    return safeStr(output);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
