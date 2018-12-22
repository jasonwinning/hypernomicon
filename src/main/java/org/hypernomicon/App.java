/*
 * Copyright 2015-2018 Jason Winning
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
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;
import static org.hypernomicon.view.tabs.HyperTab.*;

import org.hypernomicon.bib.BibManager;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.MainController;
import org.hypernomicon.view.fileManager.FileManager;
import org.hypernomicon.view.mainText.MainTextWrapper;
import org.hypernomicon.view.previewWindow.ContentsWindow;
import org.hypernomicon.view.previewWindow.PreviewWindow;
import org.hypernomicon.view.tabs.PersonTabController;
import org.hypernomicon.view.tabs.HyperTab;
import org.hypernomicon.view.tabs.HyperTab.TabEnum;
import org.hypernomicon.view.tabs.QueriesTabController.QueryView;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ConnectException;
import java.net.Socket;

import static java.util.Objects.*;
import static java.lang.management.ManagementFactory.*;

import java.util.ArrayList;
import java.util.List;
import java.util.prefs.Preferences;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.tika.config.TikaConfig;
import org.apache.tika.exception.TikaException;

import com.teamdev.jxbrowser.chromium.BrowserCore;
import com.teamdev.jxbrowser.chromium.BrowserPreferences;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.event.EventHandler;
import javafx.fxml.FXMLLoader;
import javafx.stage.Stage;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.image.Image;
import javafx.scene.layout.BorderPane;
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
public class App extends Application
{
  private Stage primaryStage;
  private static final double baseDisplayScale = 81.89306640625;
  
  public static TikaConfig tika;
  public static final FolderTreeWatcher folderTreeWatcher = new FolderTreeWatcher();
    
  private static int total, ctr, lastPercent;
  private static boolean isDebugging;
  
  public static Preferences appPrefs;

  public static QueryView curQV;
  
  public static final String appTitle = "Hypernomicon";
  
  public static App app;
  public static MainController ui;
  public static BibManager bibManagerDlg = null;
  public static FileManager fileManagerDlg = null;
  public static PreviewWindow previewWindow = null;
  public static ContentsWindow contentsWindow = null;
  
  public static double displayScale;
  
  private double deltaX;
  private long swipeStartTime;
  private boolean testMainTextEditing = false;
  
  public static boolean jxBrowserDisabled = false;
  public static boolean browserCoreInitialized = false;
  public static String jxBrowserErrMsg = "";
  
  public final Stage getPrimaryStage() { return primaryStage; }
  public final boolean debugging()     { return isDebugging; }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
   
  public static final void main(String[] args) 
  {                    
    Logger.getLogger("org.apache").setLevel(Level.WARN);    
    
    String rtArgs = getRuntimeMXBean().getInputArguments().toString();
    isDebugging = rtArgs.contains("-agentlib:jdwp") || rtArgs.contains("-Xrunjdwp");
    
    launch(args);
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
  
  @Override public final void init() 
  {
    app = this;

    try (Socket clientSocket = new Socket("localhost", InterProcDaemon.PORT);
         PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
         BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream())))
    {
      List<String> args = getParameters().getUnnamed();      
      out.println(String.valueOf(args.size()));      
      for (String arg : args) out.println(arg);      
      String line = null;      
      while (isNull(line)) line = in.readLine();      
      Platform.exit();
      return;
    } 
    catch (ConnectException e) { new InterProcDaemon().start(); } 
    catch (IOException e)      { Platform.exit(); return; }    
    
    BrowserPreferences.setChromiumSwitches("--disable-web-security", "--user-data-dir", "--allow-file-access-from-files", "--enable-local-file-accesses");
    
    // On Mac OS X Chromium engine must be initialized in non-UI thread.
    if (Environment.isMac()) initJXBrowser();
    
    appPrefs = Preferences.userNodeForPackage(App.class);
    
    db.init(appPrefs, folderTreeWatcher);
    
    //db.viewTestingInProgress = true;
    //testMainTextEditing = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public static final void initJXBrowser()
  { 
    try
    {
      BrowserCore.initialize();
      browserCoreInitialized  = true;
    }
    catch (Exception e)
    {
      jxBrowserErrMsg = safeStr(e.getMessage());
      jxBrowserDisabled = true;     
    }
    catch (ExceptionInInitializerError e)
    {
      jxBrowserErrMsg = safeStr(e.getCause().getMessage());
      jxBrowserDisabled = true;
    }    
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  @Override public final void start(Stage primaryStage)
  {   
    this.primaryStage = primaryStage;
       
    primaryStage.setTitle(appTitle);
    
    if (!initRootLayout())
    {
      Platform.exit();
      return;
    }
       
    if (jxBrowserDisabled)
    {
      if (jxBrowserErrMsg.length() > 0)
        messageDialog("Unable to initialize preview window: " + jxBrowserErrMsg, mtError);
      else
        messageDialog("Unable to initialize preview window", mtError);
    }
    
    List<String> args = new ArrayList<>(getParameters().getUnnamed());
    
    if (args.size() > 0)
    {  
      FilePath filePath = new FilePath(args.get(0));
      
      if (filePath.getExtensionOnly().equalsIgnoreCase("hdb"))
      {
        appPrefs.put(PREF_KEY_SOURCE_FILENAME, filePath.getNameOnly().toString());
        appPrefs.put(PREF_KEY_SOURCE_PATH, filePath.getDirOnly().toString());
        args.remove(0);
      }
    }    
      
    ui.loadDB();
    
    if (args.size() > 0)
      ui.handleArgs(args);
    
    if (db.viewTestingInProgress)
      testUpdatingAllRecords();
  }
   
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
 
  private final void testUpdatingAllRecords()
  {   
    total = db.persons.size()   + db.institutions.size() + db.investigations.size() + db.debates.size() + 
            db.positions.size() + db.arguments.size()    + db.works.size()          + db.terms.size() + 
            db.miscFiles.size() + db.notes.size();

    ctr = 0;
    lastPercent = 0;
    
    testUpdatingRecords(hdtPerson);   testUpdatingRecords(hdtInstitution); testUpdatingRecords(hdtInvestigation); testUpdatingRecords(hdtDebate);    
    testUpdatingRecords(hdtPosition); testUpdatingRecords(hdtArgument);    testUpdatingRecords(hdtWork);          testUpdatingRecords(hdtTerm);
    testUpdatingRecords(hdtMiscFile); testUpdatingRecords(hdtNote);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final void testUpdatingRecords(HDT_RecordType type)
  {
    db.records(type).forEach(record ->
    {      
      ui.goToRecord(record, true);
      
      if (testMainTextEditing)
      {
        MainTextWrapper mainText = null;
        
        if (record.getType() == hdtInvestigation)
          mainText = PersonTabController.class.cast(getHyperTab(personTab)).getInvMainTextWrapper(record.getID());
        else      
          mainText = ui.currentTab().getMainTextWrapper();
        
        if (nonNull(mainText))
          mainText.beginEditing(false);
      }
      
      int curPercent = (ctr * 100) / total;
      ctr++;
      
      if (curPercent > lastPercent)      
      {
        System.out.println("Progress: " + curPercent + " %");
        lastPercent = curPercent;
      }
    });
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  
  
  public final boolean initRootLayout() 
  {
    Application.setUserAgentStylesheet(STYLESHEET_MODENA);
    
    try
    {
      tika = new TikaConfig();
      BasicConfigurator.configure();
      
      FXMLLoader loader = new FXMLLoader(App.class.getResource("view/Main.fxml"));     
      BorderPane rootLayout = loader.load();
                
      ui = loader.getController();
      ui.init();

      Scene scene = new Scene(rootLayout);
      
      scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());
      
      scene.setOnKeyPressed(event -> 
      {
        if (event.getCode() == KeyCode.ESCAPE) 
        {
          ui.hideFindTable();
          event.consume();
        }
      });
      
      final KeyCombination keyComb = new KeyCodeCombination(KeyCode.F, KeyCombination.SHORTCUT_DOWN);
      scene.addEventHandler(KeyEvent.KEY_PRESSED, event -> 
      {
        if (keyComb.match(event)) 
          ui.omniFocus();
      });
      
      scene.setOnScrollStarted(event ->
      {
        swipeStartTime = System.currentTimeMillis();
        deltaX = event.getDeltaX();
      });
      
      scene.setOnScroll(event -> deltaX = deltaX + event.getDeltaX());
      
      scene.setOnScrollFinished(event -> 
      {
        double swipeTime = System.currentTimeMillis() - swipeStartTime;
        
        if (swipeTime < 200)
        {
          if (deltaX > 500)       ui.btnBackClick();
          else if (deltaX < -500) ui.btnForwardClick();
        }
      });
      
      scene.setOnDragOver(new EventHandler<DragEvent>() 
      {
        @Override public void handle(DragEvent event) 
        {
          Dragboard board = event.getDragboard();

          if (board.hasFiles()) 
            event.acceptTransferModes(TransferMode.ANY);
          else 
            event.consume();
        }
      });
    
      // Dropping over surface
      scene.setOnDragDropped(new EventHandler<DragEvent>() 
      {
        @Override public void handle(DragEvent event) 
        {
          Dragboard board = event.getDragboard();
          boolean success = false;
          
          if (board.hasImage())
            if (isDebugging)
              System.out.println("has image");
          
          if (board.hasFiles()) 
          {
            success = true;
            List<String> args = new ArrayList<>();
            
            board.getFiles().forEach(file -> args.add(file.getAbsolutePath()));
            
            Platform.runLater(() -> ui.handleArgs(args));
          }
          event.setDropCompleted(success);
          event.consume();
        }
      });
      
      primaryStage.setScene(scene);
      
      primaryStage.getIcons().setAll(new Image(App.class.getResourceAsStream("resources/images/logo-16x16.png")),
                                     new Image(App.class.getResourceAsStream("resources/images/logo-32x32.png")),
                                     new Image(App.class.getResourceAsStream("resources/images/logo-48x48.png")),
                                     new Image(App.class.getResourceAsStream("resources/images/logo-64x64.png")),
                                     new Image(App.class.getResourceAsStream("resources/images/logo-128x128.png")),
                                     new Image(App.class.getResourceAsStream("resources/images/logo-256x256.png")));
      
      ui.hideFindTable();      
      
      initScaling(rootLayout);
      
      double x = appPrefs.getDouble(PREF_KEY_WINDOW_X, primaryStage.getX());
      double y = appPrefs.getDouble(PREF_KEY_WINDOW_Y, primaryStage.getY());
      double width = appPrefs.getDouble(PREF_KEY_WINDOW_WIDTH, primaryStage.getWidth());
      double height = appPrefs.getDouble(PREF_KEY_WINDOW_HEIGHT, primaryStage.getHeight());
      boolean fullScreen = appPrefs.getBoolean(PREF_KEY_WINDOW_FULLSCREEN, primaryStage.isFullScreen());
      boolean maximized = appPrefs.getBoolean(PREF_KEY_WINDOW_MAXIMIZED, primaryStage.isMaximized());

      primaryStage.setX(x); // set X and Y first so that window gets full-screened or
      primaryStage.setY(y); // maximized onto the correct monitor if there are more than one
      
      if (fullScreen)     primaryStage.setFullScreen(true);
      else if (maximized) primaryStage.setMaximized(true);
      else
      {
        primaryStage.setWidth(width);
        primaryStage.setHeight(height);
        
        ensureVisible(primaryStage, rootLayout.getPrefWidth(), rootLayout.getPrefHeight());
      }      
      
      primaryStage.show();
      
      rescale();      
      
      getHyperTabs().forEach(HyperTab::setDividerPositions);
            
      bibManagerDlg = BibManager.create();

      bibManagerDlg.getStage().setX(appPrefs.getDouble(PREF_KEY_BM_WINDOW_X, bibManagerDlg.getStage().getX()));
      bibManagerDlg.getStage().setY(appPrefs.getDouble(PREF_KEY_BM_WINDOW_Y, bibManagerDlg.getStage().getY()));
      
      bibManagerDlg.setInitHeight(PREF_KEY_BM_WINDOW_HEIGHT);
      bibManagerDlg.setInitWidth(PREF_KEY_BM_WINDOW_WIDTH);

      db.addBibChangedHandler(() -> 
      {
        bibManagerDlg.setLibrary(db.getBibLibrary());
        
        if (db.bibLibraryIsLinked() == false)
          if (bibManagerDlg.getStage().isShowing())
            bibManagerDlg.getStage().close();
        
        ui.updateBibImportMenus();
        
        if (db.isLoaded())
          ui.update();
      });
      
      fileManagerDlg = FileManager.create();
      
      fileManagerDlg.getStage().setX(appPrefs.getDouble(PREF_KEY_FM_WINDOW_X, fileManagerDlg.getStage().getX()));
      fileManagerDlg.getStage().setY(appPrefs.getDouble(PREF_KEY_FM_WINDOW_Y, fileManagerDlg.getStage().getY()));
      
      fileManagerDlg.setInitHeight(PREF_KEY_FM_WINDOW_HEIGHT);
      fileManagerDlg.setInitWidth(PREF_KEY_FM_WINDOW_WIDTH);
      
      previewWindow = PreviewWindow.create();
      
      previewWindow.getStage().setX(appPrefs.getDouble(PREF_KEY_PREV_WINDOW_X, previewWindow.getStage().getX()));
      previewWindow.getStage().setY(appPrefs.getDouble(PREF_KEY_PREV_WINDOW_Y, previewWindow.getStage().getY()));
      
      previewWindow.setInitWidth(PREF_KEY_PREV_WINDOW_WIDTH);
      previewWindow.setInitHeight(PREF_KEY_PREV_WINDOW_HEIGHT);
      
      contentsWindow = ContentsWindow.create();
      
      contentsWindow.getStage().setX(appPrefs.getDouble(PREF_KEY_CONTENTS_WINDOW_X, contentsWindow.getStage().getX()));
      contentsWindow.getStage().setY(appPrefs.getDouble(PREF_KEY_CONTENTS_WINDOW_Y, contentsWindow.getStage().getY()));
      
      contentsWindow.setInitWidth(PREF_KEY_CONTENTS_WINDOW_WIDTH);
      contentsWindow.setInitHeight(PREF_KEY_CONTENTS_WINDOW_HEIGHT);      
    } 
    catch(IOException | TikaException e) 
    {
      messageDialog("Unable to initialize. Reason: " + e.getMessage(), mtError);
      
      if (ui != null)
        ui.shutDown(false, false, false);
      
      return false;
    }
    
    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  private final void initScaling(Parent rootLayout)
  {
    setFontSize(rootLayout);
    
    Text text = new Text("Mac @Wow Cem");
    double fontSize = appPrefs.getDouble(PREF_KEY_FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize > 0)
      text.setFont(new Font(fontSize));
          
    displayScale = (text.getLayoutBounds().getWidth()) / baseDisplayScale;
  }
  
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------  

  public final void rescale()
  {   
    scaleNodeForDPI(primaryStage.getScene().getRoot());
    
    MainTextWrapper.rescale();
    
    getHyperTab(TabEnum.personTab).rescale();
  } 
 
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------  

}
