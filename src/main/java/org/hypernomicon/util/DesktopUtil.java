/*
 * Copyright 2015-2025 Jason Winning
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
import java.net.MalformedURLException;
import java.net.ServerSocket;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.net.UnknownHostException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Scanner;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.WebButton.WebButtonField.*;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.http.client.utils.URIBuilder;
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

  private static void openSystemSpecific(String pathStr)
  {
    StringBuilder sb = new StringBuilder();

    if (SystemUtils.IS_OS_LINUX)
    {
      if (exec(false, false, sb, "kde-open"  , pathStr)) return;
      if (exec(false, false, sb, "gnome-open", pathStr)) return;
      if (exec(false, false, sb, "xdg-open"  , pathStr)) return;
//      if (exec(false, false, sb, "exo-open"  , pathStr)) return;
//      if (exec(false, false, sb, "gvfs-open" , pathStr)) return;
    }

    errorPopup("Unable to open the file: " + pathStr + (sb.length() > 0 ? "\n" + sb : "") + '.');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void browseDesktop(String url)
  {
    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)))
      {
        errorPopup("An error occurred while trying to browse to: " + url + '.');
        return;
      }

      Desktop.getDesktop().browse(makeURI(url));
    }
    catch (IOException | URISyntaxException e)
    {
      errorPopup("An error occurred while trying to browse to: " + url + ". " + getThrowableMessage(e));
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
        try
        {
          URL urlObj = new URL(url);
          return new URI(urlObj.getProtocol(), urlObj.getUserInfo(), urlObj.getHost(), urlObj.getPort(), urlObj.getPath(), urlObj.getQuery(), urlObj.getRef());
        }
        catch (URISyntaxException | MalformedURLException e1)
        {
          return new URI(scheme, ssp, "");
        }
      }
    }

    try
    {
      URIBuilder ub = new URIBuilder(url);  // Try this first because it doesn't do any escaping.
      return ub.build();
    }
    catch (URISyntaxException e)  // If that failed, then use the URI constructor, which does escaping.
    {
      pos = ssp.indexOf('#');
      String fragment = safeSubstring(ssp, pos + 1, ssp.length());
      ssp = ssp.substring(0, pos);

      return new URI(scheme, ssp, fragment);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void openFile(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    if ((SystemUtils.IS_OS_WINDOWS == false) && (SystemUtils.IS_OS_MAC == false))
    {
      openSystemSpecific(filePath.toString());
      return;
    }

    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.OPEN)))
      {
        errorPopup("An error occurred while trying to open the file: " + filePath);
        return;
      }

      Desktop.getDesktop().open(filePath.toFile());
    }
    catch (IOException | SecurityException | IllegalArgumentException e)
    {
      errorPopup("An error occurred while trying to open the file: " + filePath + ". " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void editFile(FilePath filePath)
  {
    if ((SystemUtils.IS_OS_WINDOWS == false) && (SystemUtils.IS_OS_MAC == false))
    {
      openSystemSpecific(filePath.toString());
      return;
    }

    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.EDIT)))
      {
        errorPopup("An error occurred while trying to edit the file: " + filePath + '.');
        return;
      }

      Desktop.getDesktop().edit(filePath.toFile());
    }
    catch (IOException | SecurityException | IllegalArgumentException e)
    {
      errorPopup("An error occurred while trying to edit the file: " + filePath + ". " + getThrowableMessage(e));
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
      return falseWithErrPopupCond(showErrMsg, "An error occurred while trying to start application: " + getThrowableMessage(e));
    }

    return (exitValue == 0) || falseWithErrPopupCond(showErrMsg, "An error occurred while trying to start application: " + errorSB);
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

    if (url.startsWith(EXT_1) && (db.extPath() == null))
    {
      warningPopup(WorkTabCtrlr.NO_EXT_PATH_MESSAGE);
      return;
    }

    if (url.contains(":"))
    {
      if (SystemUtils.IS_OS_WINDOWS)
      {
        // Check to see if it is a file system path

        boolean validFileSystemPath = true;

        try
        {
          Paths.get(url);
        }
        catch (InvalidPathException e)
        {
          validFileSystemPath = false;
        }

        if (validFileSystemPath)
        {
          launchFile(new FilePath(url));
          return;
        }
      }
    }
    else if (url.startsWith("\\\\"))
      url = "file:" + url.replace('\\', '/');  // Universal Naming Convention (UNC) path
    else
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
      errorPopup("An error occurred while trying to browse to: " + url + ". " + getThrowableMessage(e));
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
      {
        // The deprecated method has to be used because otherwise, if any of the array elements contain a space
        // (which they will if any path components contain a space), the command will not work right. The
        // deprecated method uses spaces to separate the string into array elements.

        Runtime.getRuntime().exec("explorer.exe /select,\"" + filePath + '"').waitFor();
      }

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
      errorPopup("An error occurred while trying to show the file: " + filePath + ". " + getThrowableMessage(e));
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

  public static void findAvailablePorts(int numToFind, List<Integer> ports) throws IOException
  {
    if (numToFind < 1) return;

    try (ServerSocket socket = new ServerSocket(0))
    {
      ports.add(socket.getLocalPort());
      findAvailablePorts(numToFind - 1, ports);
    }
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
        output = execReadToString(new String[] {"wmic", "csproduct", "get", "UUID"});

        SplitString ss = new SplitString(output, '\n');
        ss.next();
        uuid = ss.next();
      }
      else if (SystemUtils.IS_OS_MAC)
      {
        output = execReadToString(new String[] {"system_profiler", "SPHardwareDataType"});

        String[] arr = output.split("UUID: ", 2);
        uuid = arr.length < 2 ? "" : new SplitString(arr[1], '\n').next();
      }
      else if (SystemUtils.IS_OS_LINUX)
      {
        uuid = execReadToString(new String[] {"cat", "/etc/machine-id"});
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
      hostName = formatName(execReadToString(new String[] {"hostname"}));
      if (hostName.length() > 0) return hostName;
    }
    catch (IOException | InterruptedException e) { noOp(); }

    if (SystemUtils.IS_OS_WINDOWS == false) try
    {
      for (String line : FileUtils.readLines(new File("/etc/hostname"), Charset.defaultCharset()))
      {
        hostName = formatName(line);
        if (hostName.length() > 0) return hostName;
      }
    }
    catch (IOException e) { noOp(); }

    try { hostName = formatName(InetAddress.getLocalHost().getHostName()); }
    catch (UnknownHostException e) { return ""; }

    if (hostName.isBlank())
      errorPopup("Unable to determine computer name");

    return hostName;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String execReadToString(String[] execCommand) throws IOException, InterruptedException
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

  public static String detectDistro()
  {
    String distro = null;

    try
    {
      distro = readValueFromFile("/etc/os-release", "PRETTY_NAME");

      if (distro == null)
        distro = readValueFromFile("/etc/lsb-release", "DISTRIB_DESCRIPTION");

      if (distro == null)
        distro = readValueFromFile("/etc/redhat-release", "");

      if (distro == null)
        distro = readValueFromFile("/etc/debian_version", "");
    }
    catch (IOException e)
    {
      noOp();
    }

    return StringUtils.strip(safeStr(distro), "\"");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String readValueFromFile(String filePath, String key) throws IOException
  {
    File file = new File(filePath);
    if (file.exists() == false)
      return null;

    for (String line : FileUtils.readLines(file, Charset.defaultCharset()))
    {
      if (safeStr(key).isBlank())
        return line;

      SplitString ss = new SplitString(line, '=');
      if (ss.next().equals(key))
        return ss.next();
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
