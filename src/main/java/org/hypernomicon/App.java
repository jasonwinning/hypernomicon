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

package org.hypernomicon;

import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.HyperTab.*;

import org.hypernomicon.bib.BibManager;
import org.hypernomicon.dialogs.NewVersionDlgCtrlr;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.Exceptions.*;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.ContentsWindow;
import org.hypernomicon.previewWindow.PDFJSWrapper;
import org.hypernomicon.previewWindow.PreviewWindow;
import org.hypernomicon.query.QueryTabCtrlr.QueryView;
import org.hypernomicon.util.AsyncHttpClient;
import org.hypernomicon.util.JsonHttpClient;
import org.hypernomicon.util.MediaUtil;
import org.hypernomicon.util.Util;
import org.hypernomicon.util.VersionNumber;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.util.json.JsonObj;
import org.hypernomicon.view.MainCtrlr;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.tabs.HyperTab;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ConnectException;
import java.net.Socket;

import static java.lang.management.ManagementFactory.*;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.prefs.Preferences;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.DefaultConfiguration;

import com.teamdev.jxbrowser.chromium.BrowserPreferences;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.stage.Stage;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.Region;
import javafx.scene.text.Font;
import javafx.scene.text.Text;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.input.KeyCombination;
import javafx.scene.input.KeyEvent;
import javafx.scene.input.TransferMode;

//---------------------------------------------------------------------------

/**
 * Main application class for Hypernomicon
 *
 * @author  Jason Winning
 * @since   1.0
 */
public final class App extends Application
{
  private VersionNumber version;
  private boolean testMainTextEditing = false;
  private static boolean isDebugging;
  private static int total, ctr, lastPercent;

  private static final double baseDisplayScale = 81.89306640625;

  public static App app;
  public static BibManager bibManagerDlg = null;
  public static ContentsWindow contentsWindow = null;
  public static FileManager fileManagerDlg = null;
  public static MainCtrlr ui;
  public static Preferences appPrefs;
  public static PreviewWindow previewWindow = null;
  public static QueryView curQV;
  public static boolean jxBrowserInitialized = false,
                        jxBrowserDisabled    = false;
  public static double displayScale;
  public static final FolderTreeWatcher folderTreeWatcher = new FolderTreeWatcher();
  public static final String appTitle = "Hypernomicon";

  public boolean debugging()        { return isDebugging; }
  public VersionNumber getVersion() { return version; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void init()
  {
    app = this;

    try (LoggerContext lc = Configurator.initialize(new DefaultConfiguration())) { Configurator.setRootLevel(Level.WARN); }

    String rtArgs = getRuntimeMXBean().getInputArguments().toString();
    isDebugging = rtArgs.contains("-agentlib:jdwp") || rtArgs.contains("-Xrunjdwp");

    try (Socket clientSocket = new Socket("localhost", InterProcDaemon.PORT);
         PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
         BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
    {
      List<String> args = getParameters().getUnnamed();
      out.println(args.size());
      args.forEach(out::println);
      for (String line = null; line == null; line = in.readLine());
      Platform.exit();
      return;
    }
    catch (ConnectException e) { new InterProcDaemon().start(); }
    catch (IOException e)      { Platform.exit(); return; }

    BrowserPreferences.setChromiumSwitches("--disable-web-security", "--user-data-dir", "--allow-file-access-from-files", "--enable-local-file-accesses");

    // On Mac OS X Chromium engine must be initialized in non-UI thread.
    if (Environment.isMac()) PDFJSWrapper.init();

    try
    {
      MediaUtil.init();

      appPrefs = Preferences.userNodeForPackage(App.class);
      db.init(appPrefs, folderTreeWatcher);
    }
    catch (SecurityException | HDB_InternalError e)
    {
      appPrefs = null;
      messageDialog("Initialization error: " + e.getMessage(), mtError, true);

      Platform.exit();
      return;
    }

    //db.viewTestingInProgress = true;
    //testMainTextEditing = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void start(Stage stage)
  {
    stage.setTitle(appTitle);

    if (appPrefs == null)
    {
      Platform.exit();
      return;
    }

    try
    {
      initMainWindows(stage);
    }
    catch(IOException e)
    {
      messageDialog("Initialization error: " + e.getMessage(), mtError);

      if (ui != null)
        ui.shutDown(false, false, false);
      else
        Platform.exit();

      return;
    }

    String versionStr = manifestValue("Impl-Version");

    if ((safeStr(versionStr).isEmpty() == false) && (new VersionNumber(versionStr).equals(dbVersion) == false))
    {
      messageDialog("Internal error #69698", mtError);
      ui.shutDown(false, false, false);
      return;
    }

    version = dbVersion;

    boolean hdbExists = false;
    String srcName = appPrefs.get(PREF_KEY_SOURCE_FILENAME, "");
    if (srcName.isBlank() == false)
    {
      String srcPath = appPrefs.get(PREF_KEY_SOURCE_PATH, "");
      if (srcPath.isBlank() == false)
      {
        FilePath hdbPath = new FilePath(srcPath).resolve(srcName);
        if (hdbPath.exists())
          hdbExists = true;
      }
    }

    List<String> args = new ArrayList<>(getParameters().getUnnamed());

    if (args.size() > 0)
    {
      FilePath filePath = new FilePath(args.get(0));

      if (filePath.getExtensionOnly().equalsIgnoreCase("hdb"))
      {
        appPrefs.put(PREF_KEY_SOURCE_FILENAME, filePath.getNameOnly().toString());
        appPrefs.put(PREF_KEY_SOURCE_PATH, filePath.getDirOnly().toString());
        hdbExists = true;
        args.remove(0);
      }
    }

    if (hdbExists) ui.loadDB();
    else           ui.startEmpty();

    if (args.size() > 0)
    {
      ui.handleArgs(args);
      return;
    }

    if (appPrefs.getBoolean(PREF_KEY_CHECK_FOR_NEW_VERSION, true)) checkForNewVersion(new AsyncHttpClient(), newVersion ->
    {
      if (newVersion.compareTo(app.getVersion()) > 0)
        NewVersionDlgCtrlr.build().showModal();
    }, Util::noOp);

    if (db.viewTestingInProgress && hdbExists)
      testUpdatingAllRecords(1);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void checkForNewVersion(AsyncHttpClient httpClient, Consumer<VersionNumber> successHndlr, Runnable failHndlr)
  {
    JsonHttpClient.getArrayAsync("https://api.github.com/repos/jasonwinning/hypernomicon/releases", httpClient, jsonArray ->
    {
      VersionNumber updateNum =  app.version;
      Pattern p = Pattern.compile("(\\A|\\D)(\\d(\\d|(\\.\\d))+)(\\z|\\D)");

      if (jsonArray.getObjs().hasNext() == false)
      {
        failHndlr.run();
        return;
      }

      for (JsonObj jsonObj : jsonArray.getObjs())
      {
        if (jsonObj.getBoolean("prerelease", false) == false)
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
    }, e -> failHndlr.run());
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
        MainTextWrapper mainText = record.getType() == hdtInvestigation ?
          ui.personHyperTab().getInvMainTextWrapper(record.getID())
        :
          ui.activeTab().mainTextWrapper();

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

  private void initMainWindows(Stage stage) throws IOException
  {
    Application.setUserAgentStylesheet(STYLESHEET_MODENA);

    FXMLLoader loader = new FXMLLoader(App.class.getResource("view/Main.fxml"));
    Region rootNode = loader.load();

    ui = loader.getController();
    ui.init(stage);

    Scene scene = new Scene(rootNode);

    scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

    scene.setOnKeyPressed(event -> { if (event.getCode() == KeyCode.ESCAPE)
    {
      ui.hideFindTable();
      event.consume();
    }});

    KeyCombination keyComb = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);
    scene.addEventHandler(KeyEvent.KEY_PRESSED, event ->
    {
      if (keyComb.match(event))
        ui.omniFocus();
    });

    scene.addEventFilter(DragEvent.DRAG_OVER, event ->
    {
      if (event.getDragboard().hasContent(HYPERNOMICON_DATA_FORMAT))
        return;

      if (event.getDragboard().hasFiles())
        event.acceptTransferModes(TransferMode.MOVE);

      event.consume();
    });

    scene.addEventFilter(DragEvent.DRAG_DROPPED, event ->
    {
      Dragboard board = event.getDragboard();

      if (board.hasContent(HYPERNOMICON_DATA_FORMAT))
        return;

      if (board.hasImage() && isDebugging)
        System.out.println("has image");

      if (board.hasFiles())
      {
        List<String> args = board.getFiles().stream().map(File::getAbsolutePath).collect(Collectors.toList());
        Platform.runLater(() -> ui.handleArgs(args));
        event.setDropCompleted(true);
      }

      event.consume();
    });

    stage.setScene(scene);

    stage.getIcons().addAll(Stream.of("16x16", "32x32", "48x48", "64x64", "128x128", "256x256")
                                  .map(str -> new Image(App.class.getResourceAsStream("resources/images/logo-" + str + ".png")))
                                  .collect(Collectors.toList()));
    ui.hideFindTable();

    initScaling(rootNode);

    double  x          = appPrefs.getDouble (PREF_KEY_WINDOW_X,          stage.getX()),
            y          = appPrefs.getDouble (PREF_KEY_WINDOW_Y,          stage.getY()),
            width      = appPrefs.getDouble (PREF_KEY_WINDOW_WIDTH,      stage.getWidth()),
            height     = appPrefs.getDouble (PREF_KEY_WINDOW_HEIGHT,     stage.getHeight());
    boolean fullScreen = appPrefs.getBoolean(PREF_KEY_WINDOW_FULLSCREEN, stage.isFullScreen()),
            maximized  = appPrefs.getBoolean(PREF_KEY_WINDOW_MAXIMIZED,  stage.isMaximized());

    stage.setX(x); // set X and Y first so that window gets full-screened or
    stage.setY(y); // maximized onto the correct monitor if there are more than one

    if      (fullScreen) stage.setFullScreen(true);
    else if (maximized)  stage.setMaximized(true);
    else
    {
      stage.setWidth(width);
      stage.setHeight(height);

      ensureVisible(stage, rootNode.getPrefWidth(), rootNode.getPrefHeight());
    }

    stage.show();

    scaleNodeForDPI(rootNode);
    MainTextWrapper.rescale();
    getHyperTab(personTabEnum).rescale();

    forEachHyperTab(HyperTab::setDividerPositions);

    bibManagerDlg = BibManager.build();    
    bibManagerDlg.initBounds(PREF_KEY_BM_WINDOW_X, PREF_KEY_BM_WINDOW_Y, PREF_KEY_BM_WINDOW_HEIGHT, PREF_KEY_BM_WINDOW_WIDTH);

    db.addBibChangedHandler(() ->
    {
      bibManagerDlg.setLibrary(db.getBibLibrary());

      if ((db.bibLibraryIsLinked() == false) && bibManagerDlg.getStage().isShowing())
        bibManagerDlg.getStage().close();

      ui.updateBibImportMenus();

      if (db.isLoaded())
        ui.update();
    });

    fileManagerDlg = FileManager.build();
    fileManagerDlg.initBounds(PREF_KEY_FM_WINDOW_X, PREF_KEY_FM_WINDOW_Y, PREF_KEY_FM_WINDOW_HEIGHT, PREF_KEY_FM_WINDOW_WIDTH);
    
    previewWindow = PreviewWindow.build();
    previewWindow.initBounds(PREF_KEY_PREV_WINDOW_X, PREF_KEY_PREV_WINDOW_Y, PREF_KEY_PREV_WINDOW_WIDTH, PREF_KEY_PREV_WINDOW_HEIGHT);

    contentsWindow = ContentsWindow.build();
    contentsWindow.initBounds(PREF_KEY_CONTENTS_WINDOW_X, PREF_KEY_CONTENTS_WINDOW_Y, PREF_KEY_CONTENTS_WINDOW_WIDTH, PREF_KEY_CONTENTS_WINDOW_HEIGHT);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initScaling(Region rootNode)
  {
    setFontSize(rootNode);

    Text text = new Text("Mac @Wow Cem");
    double fontSize = appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize > 0)
      text.setFont(new Font(fontSize));

    displayScale = text.getLayoutBounds().getWidth() / baseDisplayScale;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
