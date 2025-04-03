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

package org.hypernomicon;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.bib.BibManager;
import org.hypernomicon.dialogs.NewVersionDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.HyperDB;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.*;
import org.hypernomicon.util.*;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.MainCtrlr.ShutDownMode;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.json.simple.parser.ParseException;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.net.URLConnection;
import java.security.CodeSource;
import java.security.ProtectionDomain;

import static java.lang.management.ManagementFactory.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.jar.JarInputStream;
import java.util.jar.Manifest;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.IntStream;

import org.apache.commons.io.output.TeeOutputStream;
import org.apache.http.client.methods.HttpGet;
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.DefaultConfiguration;

import com.teamdev.jxbrowser.chromium.BrowserPreferences;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.stage.Stage;
import javafx.scene.text.Font;
import javafx.scene.text.Text;

//---------------------------------------------------------------------------

/**
 * Main application class for Hypernomicon
 *
 * @author  Jason Winning
 * @since   1.0
 */
public final class App extends Application
{
  public final Preferences prefs;
  public final boolean debugging;

  private boolean testMainTextEditing = false;

  private static int total, ctr, lastPercent;

  private static final double baseDisplayScale = 81.89306640625;

  public static App app;
  public static BibManager bibManagerDlg = null;
  public static ContentsWindow contentsWindow = null;
  public static FileManager fileManagerDlg = null;
  public static MainCtrlr ui;
  public static PreviewWindow previewWindow = null;

  public static double displayScale;
  public static boolean jxBrowserInitialized = false,
                        jxBrowserDisabled    = false;

  private static PrintStream logFileOut, teeOut, teeErr, origOut, origErr;
  private static FilePath logFilePath;

  public static final FolderTreeWatcher folderTreeWatcher = new FolderTreeWatcher();
  public static final String appTitle = "Hypernomicon";

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in FX application thread before init and before start

  public App()
  {
    super();

    synchronized(App.class)
    {
      if (app != null)
        throw new UnsupportedOperationException();

      app = this;
    }

    try (LoggerContext lc = Configurator.initialize(new DefaultConfiguration())) { Configurator.setRootLevel(Level.WARN); }

    String rtArgs = getRuntimeMXBean().getInputArguments().toString();
    debugging = rtArgs.contains("-agentlib:jdwp") || rtArgs.contains("-Xrunjdwp");

    BrowserPreferences.setChromiumSwitches("--disable-web-security", "--user-data-dir", "--allow-file-access-from-files", "--enable-local-file-accesses");

    Preferences appPrefs = null;

    try
    {
      appPrefs = Preferences.userNodeForPackage(App.class);
    }
    catch (SecurityException e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      prefs = null;
      Platform.exit();
      return;
    }

    prefs = appPrefs;
    origOut = System.out;
    origErr = System.err;

    try
    {
      if (prefs == null) throw new HDB_UnrecoverableInternalError(37546);

      FilePath newLogFilePath = new FilePath(prefs.get(PrefKey.LOG_PATH, null));

      if (FilePath.isEmpty(newLogFilePath) == false)
        setLogPath(newLogFilePath);

      HyperDB.create(folderTreeWatcher);

      //db.viewTestingInProgress = true;
      //testMainTextEditing = true;
    }
    catch (HDB_UnrecoverableInternalError e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      Platform.exit();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setLogPath(FilePath newLogFilePath)
  {
    if (FilePath.isEmpty(newLogFilePath))
    {
      if (logFileOut != null)
      {
        logFileOut.close();
        logFileOut = null;
      }

      if (teeOut != null)
      {
        teeOut = null;
        teeErr = null;
        logFilePath = null;

        System.setOut(origOut);
        System.setErr(origErr);
      }

      return;
    }

    if (newLogFilePath.equals(logFilePath))
      return;

    if (logFileOut != null)
    {
      logFileOut.close();
      logFileOut = null;
    }

    try
    {
      logFileOut = new PrintStream(newLogFilePath.toFile());
    }
    catch (IOException e)
    {
      errorPopup("Unable to log to file: " + getThrowableMessage(e));
      setLogPath(null);
      return;
    }

    // Create TeeOutputStream to write both to console and file
    teeOut = new PrintStream(new TeeOutputStream(origOut, logFileOut));
    teeErr = new PrintStream(new TeeOutputStream(origErr, logFileOut));

    logFilePath = newLogFilePath;

    System.setOut(teeOut);
    System.setErr(teeErr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in launcher thread (not FX application thread) after constructor and before start

  @Override public void init() throws Exception
  {
    super.init();

    // On Mac OS Chromium engine must be initialized outside of FX application thread
    if (Environment.isMac()) PDFJSWrapper.init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in FX application thread, only if start was called
  // Occurs if all windows are closed, or if Platform.exit is called

  @Override public void stop() throws Exception
  {
    super.stop();
    noOp();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Runs in FX application thread after init

  @Override public void start(Stage stage)
  {
    stage.setTitle(appTitle);

    try
    {
      initMainWindows(stage, prefs);
    }
    catch (IOException e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      if (ui != null)
        ui.shutDown(ShutDownMode.InitializationFailure);
      else
        Platform.exit();

      return;
    }

    String versionStr = manifestValue("Impl-Version");

    if ((safeStr(versionStr).isEmpty() == false) && (new VersionNumber(versionStr).equals(appVersion) == false))
    {
      internalErrorPopup(69698);
      ui.shutDown(ShutDownMode.InitializationFailure);
      return;
    }

    boolean hdbExists = false;
    String srcName = prefs.get(PrefKey.SOURCE_FILENAME, "");
    if (srcName.isBlank() == false)
    {
      String srcPath = prefs.get(PrefKey.SOURCE_PATH, "");
      if (srcPath.isBlank() == false)
      {
        FilePath hdbPath = new FilePath(srcPath).resolve(srcName);
        if (hdbPath.exists() && (InterProcClient.folderInUseByAnotherInstance(new FilePath(srcPath)) == false))
          hdbExists = true;
      }
    }

    List<String> args = new ArrayList<>(getParameters().getUnnamed());

    if (args.size() > 0)
    {
      FilePath filePath = new FilePath(args.get(0));

      if ("hdb".equalsIgnoreCase(filePath.getExtensionOnly()))
      {
        prefs.put(PrefKey.SOURCE_FILENAME, filePath.getNameOnly().toString());
        prefs.put(PrefKey.SOURCE_PATH    , filePath.getDirOnly ().toString());
        hdbExists = true;
        args.remove(0);
      }
    }

    if (hdbExists) ui.loadAllFromXmlAndResetUI(false);
    else           ui.startEmpty();

    if (args.size() > 0)
    {
      ui.handleArgs(args);
      return;
    }

    if (prefs.getBoolean(PrefKey.CHECK_FOR_NEW_VERSION, true)) checkForNewVersion(new AsyncHttpClient(), newVersion ->
    {
      if (newVersion.compareTo(appVersion) > 0)
        noOp(new NewVersionDlgCtrlr());

    }, Util::noOp);

    if (db.viewTestingInProgress && hdbExists)
      testUpdatingAllRecords(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String hypernomiconReleasesURL = "https://api.github.com/repos/jasonwinning/hypernomicon/releases";

  public static void checkForNewVersion(AsyncHttpClient httpClient, Consumer<VersionNumber> successHndlr, Runnable failHndlr)
  {
    JsonHttpClient.getArrayAsync(hypernomiconReleasesURL, httpClient,
      jsonArray -> processNewVersionJsonArray(jsonArray, successHndlr, failHndlr),
      e -> failHndlr.run());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void checkForNewVersionInThisThread(Consumer<VersionNumber> successHndlr, Runnable failHndlr)
  {
    try
    {
      JsonArray jsonArray = new JsonHttpClient().requestArrayInThisThread(new HttpGet(hypernomiconReleasesURL));

      processNewVersionJsonArray(jsonArray, successHndlr, failHndlr);
    }
    catch (UnsupportedOperationException | ParseException | IOException e)
    {
      failHndlr.run();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void processNewVersionJsonArray(JsonArray jsonArray, Consumer<VersionNumber> successHndlr, Runnable failHndlr)
  {
    VersionNumber updateNum = appVersion;

    if (jsonArray.isEmpty())
    {
      failHndlr.run();
      return;
    }

    Pattern p = Pattern.compile("([^\\d.]*)((?:\\d+\\.?)*\\d)$");

    for (JsonObj jsonObj : jsonArray.getObjs())
    {
      if ((jsonObj.getBoolean("prerelease", false) == false) &&
          (jsonObj.getBoolean("draft"     , false) == false))
      {
        Matcher m = p.matcher(jsonObj.getStrSafe("tag_name"));

        if (m.find())
        {
          VersionNumber curNum = new VersionNumber(m.group(2));
          if (curNum.compareTo(updateNum) > 0)
            updateNum = curNum;
        }
      }
    }

    successHndlr.accept(updateNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void testUpdatingAllRecords(int passes)
  {
    List<RecordType> types = List.of(hdtPerson,   hdtInstitution, hdtInvestigation, hdtDebate,   hdtPosition,
                                     hdtArgument, hdtWork,        hdtTerm,          hdtMiscFile, hdtNote);

    ctr = 0; lastPercent = 0;

    total = types.stream().mapToInt(type -> db.records(type).size()).sum() * passes;

    IntStream.rangeClosed(1, passes)
             .forEach(pass -> types.forEach(this::testUpdatingRecords));

  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void testUpdatingRecords(RecordType type)
  {
    db.records(type).forEach(record ->
    {
      ui.goToRecord(record, true);

      if (testMainTextEditing)
      {
        MainTextWrapper mainText = ui.activeTab().mainTextWrapper();

        if (mainText != null)
          mainText.beginEditing(false);
      }

      int curPercent = (ctr++ * 100) / total;

      if (curPercent > lastPercent)
      {
        System.out.println("Progress: " + curPercent + " %");
        lastPercent = curPercent;
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initMainWindows(Stage stage, Preferences prefs) throws IOException
  {
    setUserAgentStylesheet(STYLESHEET_MODENA);

    initScaling(prefs);

    MainCtrlr.create(stage);

    bibManagerDlg = new BibManager();
    bibManagerDlg.initBounds(PrefKey.BM_WINDOW_X, PrefKey.BM_WINDOW_Y, PrefKey.BM_WINDOW_WIDTH, PrefKey.BM_WINDOW_HEIGHT);

    db.addBibChangedHandler(() ->
    {
      bibManagerDlg.setLibrary(db.getBibLibrary());

      if ((db.bibLibraryIsLinked() == false) && bibManagerDlg.getStage().isShowing())
        bibManagerDlg.getStage().close();

      ui.updateBibImportMenus();

      if (db.isLoaded())
        ui.update();
    });

    fileManagerDlg = new FileManager();
    fileManagerDlg.initBounds(PrefKey.FM_WINDOW_X, PrefKey.FM_WINDOW_Y, PrefKey.FM_WINDOW_WIDTH, PrefKey.FM_WINDOW_HEIGHT);

    previewWindow = new PreviewWindow();
    previewWindow.initBounds(PrefKey.PREV_WINDOW_X, PrefKey.PREV_WINDOW_Y, PrefKey.PREV_WINDOW_WIDTH, PrefKey.PREV_WINDOW_HEIGHT);

    contentsWindow = new ContentsWindow();
    contentsWindow.initBounds(PrefKey.CONTENTS_WINDOW_X, PrefKey.CONTENTS_WINDOW_Y, PrefKey.CONTENTS_WINDOW_WIDTH, PrefKey.CONTENTS_WINDOW_HEIGHT);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initScaling(Preferences prefs)
  {
    Text text = new Text("Mac @Wow Cem");
    double fontSize = prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize > 0)
      text.setFont(new Font(fontSize));

    displayScale = text.getLayoutBounds().getWidth() / baseDisplayScale;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static String manifestValue(String key)
  {
    URL jarUrl;

    try
    {
      jarUrl = nullSwitch(nullSwitch(App.class.getProtectionDomain(), null, ProtectionDomain::getCodeSource), null, CodeSource::getLocation);

      if (jarUrl != null)
      {
        try (InputStream jarStream = jarUrl.openStream())
        {
          if (jarStream != null) try (JarInputStream jis = new JarInputStream(jarStream))
          {
            Manifest manifest = jis.getManifest();

            return manifest == null ? "" : nullSwitch(manifest.getMainAttributes(), "", attributes -> safeStr(attributes.getValue(key)));
          }
        }
      }
    }
    catch (SecurityException | IOException e)
    {
      noOp();
    }

    // Security exception likely happened so try less reliable method

    jarUrl = App.class.getResource(BLANK_DB_RESOURCE_NAME);  // Get the URL for a resource that will only exist in the Hypernomicon jar file.
                                                             // Most jar files will have META-INF/MANIFEST.MF so we are likely to get the
    if (jarUrl != null) try                                  // wrong URL if we search for that resource name (even though it is the one we
    {                                                        // really want, from the Hypernomicon jar file).
      URLConnection conn = jarUrl.openConnection();

      if (conn instanceof JarURLConnection jarURLConnection)
      {
        Manifest manifest = jarURLConnection.getManifest();

        return manifest == null ? "" : nullSwitch(manifest.getMainAttributes(), "", attributes -> safeStr(attributes.getValue(key)));
      }
    }
    catch (IOException e) { noOp(); }

    return "";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
