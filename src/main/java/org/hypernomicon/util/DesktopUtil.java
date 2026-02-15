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

package org.hypernomicon.util;

import java.awt.Desktop;
import java.io.IOException;
import java.net.*;
import java.nio.file.InvalidPathException;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.function.BooleanSupplier;
import java.util.stream.Stream;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.OperatingSystem.*;
import static org.hypernomicon.util.PopupDialog.DialogResult.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.WebButton.WebButtonField.*;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;

import org.hypernomicon.HyperTask;
import org.hypernomicon.model.Exceptions.CancelledTaskException;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.settings.LaunchCommandsDlgCtrlr;
import org.hypernomicon.util.PopupDialog.DialogResult;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.http.HttpResponseException;
import org.hypernomicon.view.tabs.WorkTabCtrlr;

import com.google.common.collect.Lists;
import com.sun.javafx.PlatformUtil;

import javafx.concurrent.Worker.State;

//---------------------------------------------------------------------------

public final class DesktopUtil
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DesktopUtil() { throw new UnsupportedOperationException("Instantiation of utility class is not allowed."); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum OperatingSystem
  {
    WINDOWS (PlatformUtil::isWindows   ),
    MAC     (PlatformUtil::isMac       ),
    LINUX   (PlatformUtil::isLinux     ),
    OTHER_OS(Boolean.TRUE::booleanValue);

    private final BooleanSupplier matcher;

    OperatingSystem(BooleanSupplier matcher)
    {
      this.matcher = matcher;
    }
  }

  public static final OperatingSystem CURRENT_OS = Arrays.stream(OperatingSystem.values()).filter(os -> os.matcher.getAsBoolean()).findFirst().orElse(OTHER_OS);

  public static final boolean IS_OS_WINDOWS = CURRENT_OS == WINDOWS,
                              IS_OS_MAC     = CURRENT_OS == MAC,
                              IS_OS_LINUX   = CURRENT_OS == LINUX;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum UrlOpenAction
  {
    BROWSE_WEB,   // Open in browser via Desktop.browse or openSystemSpecific
    LAUNCH_FILE,  // Open as file via launchFile
    INVALID       // Do not proceed; show error if errorMessage is non-null
  }

  /**
   * Result of processing a URL/path string for opening.
   *
   * @param action       the action to take (BROWSE_WEB, LAUNCH_FILE, or INVALID)
   * @param urlString    the URL/path string (or file path for LAUNCH_FILE)
   * @param uri          the constructed URI for BROWSE_WEB (null for other actions)
   * @param errorMessage error message for INVALID action (null otherwise)
   */
  record UrlOpenResult(UrlOpenAction action, String urlString, URI uri, String errorMessage) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Processes a URL/path string and determines how it should be opened.
   * This method contains all the business logic for URL transformation and URI construction,
   * without any side effects, making it fully unit-testable.
   *
   * @param url the URL or path string to process
   * @param extPathAvailable whether the external file path is configured (db.extPath() != null)
   * @return a UrlOpenResult indicating the action to take, the URL string, constructed URI, and any error message
   */
  static UrlOpenResult processWebLink(String url, boolean extPathAvailable)
  {
    url = url.strip();

    if (url.isEmpty())
      return new UrlOpenResult(UrlOpenAction.INVALID, null, null, null);

    if (url.startsWith(EXT_1) && (extPathAvailable == false))
      return new UrlOpenResult(UrlOpenAction.INVALID, null, null, WorkTabCtrlr.NO_EXT_PATH_MESSAGE);

    // Check for Unix-style absolute paths on Mac/Linux (before checking for colons,
    // since colons are valid in Unix filenames like "/path/to/Meeting: Notes.txt")
    // Exclude "//" which could be a protocol-relative URL

    if ((IS_OS_MAC || IS_OS_LINUX) && url.startsWith("/") && (url.startsWith("//") == false))
      return new UrlOpenResult(UrlOpenAction.LAUNCH_FILE, url, null, null);

    // Check for tilde paths (home directory) on Mac/Linux

    if ((IS_OS_MAC || IS_OS_LINUX) && url.startsWith("~"))
    {
      String relativePart = url.startsWith("~/") ? url.substring(2) : url.substring(1),
             expandedPath = homeDir().resolve(relativePart).toString();

      return new UrlOpenResult(UrlOpenAction.LAUNCH_FILE, expandedPath, null, null);
    }

    if (url.contains(":"))
    {
      if (IS_OS_WINDOWS)
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
          return new UrlOpenResult(UrlOpenAction.LAUNCH_FILE, url, null, null);
      }
    }
    else if (url.startsWith("\\\\"))
      url = "file:" + url.replace('\\', '/');  // Universal Naming Convention (UNC) path
    else
      url = "http://" + url;

    try
    {
      URI uri = new URI(escapeIllegalUriChars(url));
      return new UrlOpenResult(UrlOpenAction.BROWSE_WEB, url, uri, null);
    }
    catch (URISyntaxException e)
    {
      String errorMsg = "An error occurred while trying to browse to: " + url + ". " + getThrowableMessage(e);
      return new UrlOpenResult(UrlOpenAction.INVALID, url, null, errorMsg);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void browseDesktop(String urlString, URI uri)
  {
    // Bypass AWT Desktop on Linux to avoid deadlock (JDK-8267572, JDK-8240572)

    if ((CURRENT_OS != WINDOWS) && (CURRENT_OS != MAC))
    {
      exec(false, "xdg-open", urlString);
      return;
    }

    try
    {
      if ( ! (Desktop.isDesktopSupported() && Desktop.getDesktop().isSupported(Desktop.Action.BROWSE)))
      {
        errorPopup("An error occurred while trying to browse to: " + urlString + '.');
        return;
      }

      Desktop.getDesktop().browse(uri);
    }
    catch (IOException e)
    {
      errorPopup("An error occurred while trying to browse to: " + urlString + ". " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * RFC 3986 Appendix B regex for parsing URI components
   */
  private static final Pattern URI_PATTERN = Pattern.compile("^(([^:/?#]+):)?(//([^/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?");

  /**
   * Escapes all illegal printable ASCII characters in URIs per RFC 3986.
   * <p>
   * RFC 3986 Section 2 defines valid URI characters as:
   * <ul>
   *   <li>unreserved: A-Z a-z 0-9 - . _ ~</li>
   *   <li>reserved: : / ? # [ ] @ ! $ &amp; ' ( ) * + , ; =</li>
   *   <li>percent-encoded: %XX</li>
   * </ul>
   * All other printable ASCII characters are illegal: {@code space " < > [ ] \ ^ ` { | }}
   * <p>
   * Note: {@code [ ]} are reserved and only legal in the host portion for IPv6 addresses.
   * They are escaped only in the path/query/fragment portion, not in the host.
   * <p>
   * Pre-escaping these allows the single-string URI constructor to succeed,
   * preserving already-encoded characters like %20 (avoiding double-encoding).
   */
  private static String escapeIllegalUriChars(String url)
  {
    // Escape characters that are illegal everywhere in URIs
    url = url.replace(" ", "%20")
             .replace("{", "%7B")
             .replace("}", "%7D")
             .replace("|", "%7C")
             .replace("\\", "%5C")
             .replace("^", "%5E")
             .replace("`", "%60")
             .replace("<", "%3C")
             .replace(">", "%3E")
             .replace("\"", "%22");

    // Brackets are only legal in the host portion (for IPv6).
    // Use RFC 3986 regex to properly parse URI structure and escape brackets only in path/query/fragment.

    Matcher m = URI_PATTERN.matcher(url);
    if (m.matches())
    {
      String scheme    = m.group(1) != null ? m.group(1) : "";  // "http:" or ""
      String authority = m.group(3) != null ? m.group(3) : "";  // "//host" or ""
      String path      = m.group(5) != null ? m.group(5) : "";  // "/path" or ""
      String query     = m.group(6) != null ? m.group(6) : "";  // "?query" or ""
      String fragment  = m.group(8) != null ? m.group(8) : "";  // "#fragment" or ""

      // Escape brackets only in path, query, and fragment, not in authority (IPv6)

      path     = path.replace("[", "%5B").replace("]", "%5D");
      query    = query.replace("[", "%5B").replace("]", "%5D");
      fragment = fragment.replace("[", "%5B").replace("]", "%5D");

      return scheme + authority + path + query + fragment;
    }

    // Fallback: escape brackets everywhere

    return url.replace("[", "%5B").replace("]", "%5D");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void openFile(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    // Bypass AWT Desktop on Linux to avoid deadlock (JDK-8267572, JDK-8240572)

    if ((CURRENT_OS != WINDOWS) && (CURRENT_OS != MAC))
    {
      exec(false, "xdg-open", filePath.toString());
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
    // Bypass AWT Desktop on Linux to avoid deadlock (JDK-8267572, JDK-8240572)

    if ((CURRENT_OS != WINDOWS) && (CURRENT_OS != MAC))
    {
      exec(false, "xdg-open", filePath.toString());
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

  public static boolean exec(boolean showErrMsg, String... parts)
  {
    return exec(showErrMsg, Lists.newArrayList(parts));
  }

  public static boolean exec(boolean showErrMsg, List<String> command)
  {
    ProcessBuilder pb = new ProcessBuilder(IS_OS_MAC ?
      Stream.concat(Stream.of("open", "-a"), command.stream()).toList()
    :
      command);

    try
    {
      pb.redirectError (ProcessBuilder.Redirect.DISCARD)
        .redirectOutput(ProcessBuilder.Redirect.DISCARD)
        .start();
    }
    catch (IOException e)
    {
      return falseWithErrPopupCond(showErrMsg, "An error occurred while trying to start application: " + getThrowableMessage(e));
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void searchORCID(String orcid, String first, String last)
  {
    if (strNotNullOrEmpty(orcid))
    {
      openWebLink("https://orcid.org/" + escapeURL(orcid, false));
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
    if (strNotNullOrBlank(doi))
      openWebLink("https://dx.doi.org/" + escapeURL(doi, false));
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  public static void openWebLink(String url)
  {
    UrlOpenResult result = processWebLink(url, db.extPath() != null);

    switch (result.action())
    {
      case INVALID ->
      {
        if (result.errorMessage() != null)
          warningPopup(result.errorMessage());
      }

      case LAUNCH_FILE -> launchFile(new FilePath(result.urlString()));

      case BROWSE_WEB -> browseDesktop(result.urlString(), result.uri());
    }
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

    boolean isPdf = "pdf".equalsIgnoreCase(filePath.getExtensionOnly());

    String readerPath = isPdf ? app.prefs.get(PrefKey.PDF_READER, "") : "";

    if ((isPdf == false) || readerPath.isEmpty())
    {
      openFile(filePath);
      return;
    }

    if (pageNum < 1) pageNum = 1;

    LaunchCommandsDlgCtrlr.launch(readerPath, filePath, PrefKey.PDF_READER_COMMANDS, PrefKey.PDF_READER_COMMAND_TYPE, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void highlightFileInExplorer(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath)) return;

    try
    {
      switch (CURRENT_OS)
      {
        case WINDOWS -> highlightInWindowsExplorer(filePath);

        case MAC     -> Runtime.getRuntime().exec(new String[] {"open", "-R", filePath.toString()}).waitFor();

        case LINUX   ->
        {
          if (exec(false, "nautilus", filePath.toString()) == false)
            launchFile(filePath.getDirOnly()); // won't highlight file in folder
        }

        // xdg-mime query default inode/directory

        default -> launchFile(filePath.getDirOnly());  // won't highlight file in folder
      }
    }
    catch (IOException | InterruptedException e)
    {
      errorPopup("An error occurred while trying to show the file: " + filePath + ". " + getThrowableMessage(e));
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  private static void highlightInWindowsExplorer(FilePath filePath) throws InterruptedException, IOException
  {
    // The deprecated method has to be used because otherwise, if any of the array elements contain a space
    // (which they will if any path components contain a space), the command will not work right. The
    // deprecated method uses spaces to separate the string into array elements.

    Runtime.getRuntime().exec("explorer.exe /select,\"" + filePath + '"').waitFor();
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

  public static void findAvailablePorts(int numToFind, Collection<Integer> ports) throws IOException
  {
    while (numToFind-- > 0) try (ServerSocket socket = new ServerSocket(0))
    {
      ports.add(socket.getLocalPort());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String computerName = null;

  private static String formatName(String name)
  {
    return stripSafe(name).replaceAll("\\p{C}", "");
  }

  /**
   * Retrieves a stable identifier for the current computer, combining the host name
   * with a platform-specific UUID when available.
   * <p>
   * On first invocation, this method determines the UUID according to the detected
   * operating system and caches the result in {@code computerName}. Subsequent calls
   * return the cached value.
   * </p>
   *
   * <h4>Platform-specific behavior</h4>
   * <ul>
   *   <li><b>Windows</b> – Executes {@code wmic csproduct get UUID} and parses the UUID from the output.</li>
   *   <li><b>macOS</b> – Executes {@code system_profiler SPHardwareDataType} and extracts the UUID from the
   *       "Hardware UUID" field.</li>
   *   <li><b>Linux</b> – Reads the machine identifier from {@code /etc/machine-id}.</li>
   *   <li><b>Other</b> – No UUID is retrieved; only the host name is used.</li>
   * </ul>
   *
   * <p>
   * If a UUID cannot be obtained or is blank, the host name alone is returned. Otherwise,
   * the host name and UUID are concatenated with {@code "::::"} as a delimiter, with the
   * UUID converted to lower case. Any existing {@code "::::"} sequences in the host name
   * are removed before concatenation.
   * </p>
   *
   * @return a string identifier for the machine, in the form {@code hostName::::uuid} or just {@code hostName}
   *         if the UUID is unavailable
   */
  public static String getComputerName()
  {
    if (computerName != null) return computerName;

    String uuid = "",
           hostName = getHostName();

    try
    {
      switch (CURRENT_OS)
      {
        case WINDOWS ->
        {
          String output = execReadToString(new String[] {"wmic", "csproduct", "get", "UUID"});

          SplitString ss = new SplitString(output, '\n');
          ss.next();
          uuid = ss.next();
        }

        case MAC ->
        {
          String output = execReadToString(new String[] {"system_profiler", "SPHardwareDataType"});

          String[] arr = output.split("UUID: ", 2);
          uuid = arr.length < 2 ? "" : new SplitString(arr[1], '\n').next();
        }

        case LINUX -> uuid = execReadToString(new String[] {"cat", "/etc/machine-id"});

        default -> { }  // No UUID retrieval for OTHER_OS
      }
    }
    catch (IOException | InterruptedException e) { noOp(); }

    uuid = stripSafe(uuid);
    return computerName = uuid.isBlank() ? hostName : (hostName.replace("::::", "") + "::::" + uuid.toLowerCase());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getHostName()
  {
    String hostName = formatName(SystemUtils.getHostName());
    if (strNotNullOrBlank(hostName)) return hostName;

    hostName = formatName(System.getenv("HOSTNAME"));
    if (strNotNullOrBlank(hostName)) return hostName;

    hostName = formatName(System.getenv("COMPUTERNAME"));
    if (strNotNullOrBlank(hostName)) return hostName;

    try
    {
      hostName = formatName(execReadToString(new String[] {"hostname"}));
      if (strNotNullOrBlank(hostName)) return hostName;
    }
    catch (IOException | InterruptedException e) { noOp(); }

    if (IS_OS_WINDOWS == false) try
    {
      for (String line : new FilePath("/etc/hostname").readToStrList())
      {
        hostName = formatName(line);
        if (strNotNullOrBlank(hostName)) return hostName;
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

  private static String readValueFromFile(String filePathStr, String key) throws IOException
  {
    FilePath filePath = new FilePath(filePathStr);
    if (filePath.exists() == false)
      return null;

    for (String line : filePath.readToStrList())
    {
      if (strNullOrBlank(key))
        return line;

      SplitString ss = new SplitString(line, '=');
      if (ss.next().equals(key))
        return ss.next();
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean checkInternet()
  {
    DialogResult result = mrRetry;

    while (result == mrRetry)
    {
      HyperTask task = new HyperTask("CheckForInternet", "Checking for internet connection...", false) { @Override protected void call() throws HyperDataException, CancelledTaskException
      {
        AtomicReference<State> state = new AtomicReference<>(State.READY);
        AtomicReference<Throwable> throwable = new AtomicReference<>();

        HyperThread innerThread = new HyperThread("CheckForInternetInner", () ->
        {
          state.set(State.RUNNING);

          try
          {
            HttpURLConnection con = (HttpURLConnection) URI.create("https://www.google.com/").toURL().openConnection();
            //HttpURLConnection con = (HttpURLConnection) URI.create("http://10.255.255.1/").toURL().openConnection();

            con.setRequestMethod("HEAD");

            con.connect();

            if (con.getResponseCode() == HttpURLConnection.HTTP_OK)
            {
              state.set(State.SUCCEEDED);
            }
            else
            {
              state.set(State.FAILED);
              throwable.set(new HttpResponseException(con.getResponseCode()));
            }
          }
          catch (IOException e)
          {
            state.set(State.FAILED);
            throwable.set(e);
          }

        });

        innerThread.setDaemon(true);
        innerThread.start();

        try
        {
          while (innerThread.isAlive())
          {
            if (isCancelled())
            {
              innerThread.interrupt();
              throw new CancelledTaskException();
            }

            innerThread.join(100);
          }
        }
        catch (InterruptedException e)
        {
          innerThread.interrupt();
          throw new CancelledTaskException();
        }

        if (state.get() == State.SUCCEEDED)
          return;

        Throwable e = throwable.get();

        if (e instanceof HyperDataException hde) throw hde;

        if ((e instanceof UnknownHostException) || (e instanceof NoRouteToHostException))
          throw new CancelledTaskException();

        throw e == null ? new HyperDataException("Unknown error occurred") : new HyperDataException(e);

      }}.setSilent(true)
        .setSkippable(true)
        .setInterruptOnCancel(true)
        .addMessage("Press \"Skip\" if internet connection is not needed.");

      if (task.runWithProgressDialog() == State.SUCCEEDED)
        return true;

      Throwable e = task.getException();
      String msg = e instanceof HyperDataException ? getThrowableMessage(e.getCause()) : (e == null ? "" : getThrowableMessage(e));

      result = abortRetryIgnoreDialog("Warning: Internet connection check failed" + (strNullOrBlank(msg) ? '.' : ": " + msg));
    }

    return result == mrIgnore;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
