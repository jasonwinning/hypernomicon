/*
 * Copyright 2015-2024 Jason Winning
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
import org.hypernomicon.previewWindow.ContentsWindow;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.MediaUtil;
import org.hypernomicon.util.Util;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.json.simple.parser.ParseException;

import java.io.IOException;

import static java.lang.management.ManagementFactory.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

    MediaUtil.init();

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

    try
    {
      if (prefs == null) throw new HDB_InternalError(37546);

      HyperDB.create(folderTreeWatcher);
    }
    catch (HDB_InternalError e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      Platform.exit();
      return;
    }

    //db.viewTestingInProgress = true;
    //testMainTextEditing = true;
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
    catch(IOException e)
    {
      errorPopup("Initialization error: " + getThrowableMessage(e));

      if (ui != null)
        ui.shutDown(false, false, false);
      else
        Platform.exit();

      return;
    }

    String versionStr = manifestValue("Impl-Version");

    if ((safeStr(versionStr).isEmpty() == false) && (new VersionNumber(versionStr).equals(appVersion) == false))
    {
      internalErrorPopup(69698);
      ui.shutDown(false, false, false);
      return;
    }

    boolean hdbExists = false;
    String srcName = prefs.get(PREF_KEY_SOURCE_FILENAME, "");
    if (srcName.isBlank() == false)
    {
      String srcPath = prefs.get(PREF_KEY_SOURCE_PATH, "");
      if (srcPath.isBlank() == false)
      {
        FilePath hdbPath = new FilePath(srcPath).resolve(srcName);
        if (hdbPath.exists() && InterProcClient.checkFolder(new FilePath(srcPath)))
          hdbExists = true;
      }
    }

    List<String> args = new ArrayList<>(getParameters().getUnnamed());

    if (args.size() > 0)
    {
      FilePath filePath = new FilePath(args.get(0));

      if ("hdb".equalsIgnoreCase(filePath.getExtensionOnly()))
      {
        prefs.put(PREF_KEY_SOURCE_FILENAME, filePath.getNameOnly().toString());
        prefs.put(PREF_KEY_SOURCE_PATH    , filePath.getDirOnly ().toString());
        hdbExists = true;
        args.remove(0);
      }
    }

    if (hdbExists) ui.loadDB(false);
    else           ui.startEmpty();

    if (args.size() > 0)
    {
      ui.handleArgs(args);
      return;
    }

    if (prefs.getBoolean(PREF_KEY_CHECK_FOR_NEW_VERSION, true)) checkForNewVersion(new AsyncHttpClient(), newVersion ->
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
    JsonHttpClient.getArrayAsync(hypernomiconReleasesURL, httpClient, jsonArray ->
    {
      processNewVersionJsonArray(jsonArray, successHndlr, failHndlr);

    }, e -> failHndlr.run());
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

    total = 0; ctr = 0; lastPercent = 0;
    types.forEach(type -> total += db.records(type).size());

    total *= passes;

    for (int pass = 1; pass <= passes; pass++)
      types.forEach(this::testUpdatingRecords);
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
    bibManagerDlg.initBounds(PREF_KEY_BM_WINDOW_X, PREF_KEY_BM_WINDOW_Y, PREF_KEY_BM_WINDOW_WIDTH, PREF_KEY_BM_WINDOW_HEIGHT);

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
    fileManagerDlg.initBounds(PREF_KEY_FM_WINDOW_X, PREF_KEY_FM_WINDOW_Y, PREF_KEY_FM_WINDOW_WIDTH, PREF_KEY_FM_WINDOW_HEIGHT);

    previewWindow = new PreviewWindow();
    previewWindow.initBounds(PREF_KEY_PREV_WINDOW_X, PREF_KEY_PREV_WINDOW_Y, PREF_KEY_PREV_WINDOW_WIDTH, PREF_KEY_PREV_WINDOW_HEIGHT);

    contentsWindow = new ContentsWindow();
    contentsWindow.initBounds(PREF_KEY_CONTENTS_WINDOW_X, PREF_KEY_CONTENTS_WINDOW_Y, PREF_KEY_CONTENTS_WINDOW_WIDTH, PREF_KEY_CONTENTS_WINDOW_HEIGHT);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initScaling(Preferences prefs)
  {
    Text text = new Text("Mac @Wow Cem");
    double fontSize = prefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize > 0)
      text.setFont(new Font(fontSize));

    displayScale = text.getLayoutBounds().getWidth() / baseDisplayScale;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
