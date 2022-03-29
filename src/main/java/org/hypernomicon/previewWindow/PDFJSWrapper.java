/*
 * Copyright 2015-2022 Jason Winning
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

package org.hypernomicon.previewWindow;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

import com.teamdev.jxbrowser.chromium.Browser;
import com.teamdev.jxbrowser.chromium.BrowserContext;
import com.teamdev.jxbrowser.chromium.BrowserContextParams;
import com.teamdev.jxbrowser.chromium.BrowserCore;
import com.teamdev.jxbrowser.chromium.BrowserException;
import com.teamdev.jxbrowser.chromium.BrowserPreferences;
import com.teamdev.jxbrowser.chromium.DialogParams;
import com.teamdev.jxbrowser.chromium.JSArray;
import com.teamdev.jxbrowser.chromium.JSObject;
import com.teamdev.jxbrowser.chromium.JSValue;
import com.teamdev.jxbrowser.chromium.LoggerProvider;
import com.teamdev.jxbrowser.chromium.ProtocolService;
import com.teamdev.jxbrowser.chromium.URLResponse;
import com.teamdev.jxbrowser.chromium.events.ConsoleEvent.Level;
import com.teamdev.jxbrowser.chromium.events.FinishLoadingEvent;
import com.teamdev.jxbrowser.chromium.events.LoadAdapter;
import com.teamdev.jxbrowser.chromium.internal.Environment;
import com.teamdev.jxbrowser.chromium.internal.ipc.IPCException;
import com.teamdev.jxbrowser.chromium.javafx.BrowserView;
import com.teamdev.jxbrowser.chromium.javafx.DefaultDialogHandler;
import com.teamdev.jxbrowser.chromium.javafx.internal.dialogs.MessageDialog;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.util.Util.*;
import static java.util.logging.Level.*;

import org.apache.commons.io.FileUtils;
import org.hypernomicon.App;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;

import javafx.application.Platform;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

public class PDFJSWrapper
{
  private boolean ready = false, opened = false, pdfjsMode = true;
  private final PDFJSDoneHandler doneHndlr;
  private final Consumer<Integer> pageChangeHndlr;
  private final PDFJSRetrievedDataHandler retrievedDataHndlr;
  private int numPages = -1;
  private final JavascriptToJava javascriptToJava;
  private Browser browser = null, oldBrowser = null;
  private static BrowserContext browserContext = null;
  private BrowserView browserView = null;
  private static String viewerHTMLStr = null;
  private static final String basePlaceholder = "<!-- base placeholder -->";
  private final AnchorPane apBrowser;
  private Runnable postBrowserLoadCode = null;

  int getNumPages()    { return numPages; }
  void prepareToHide() { removeFromParent(browserView); }
  void prepareToShow() { addToParent(browserView, apBrowser); }

//---------------------------------------------------------------------------

  enum PDFJSCommand
  {
    pjsOpen,
    pjsClose,
    pjsNumPages
  }

//---------------------------------------------------------------------------

  @FunctionalInterface interface PDFJSDoneHandler {
    void handle(PDFJSCommand cmd, boolean success, String errMessage);
  }

//---------------------------------------------------------------------------

  @FunctionalInterface interface PDFJSRetrievedDataHandler {
    void handle(Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel, List<Integer> hilitePages);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PDFJSWrapper(AnchorPane apBrowser, PDFJSDoneHandler doneHndlr, Consumer<Integer> pageChangeHndlr, PDFJSRetrievedDataHandler retrievedDataHndlr)
  {
    this.doneHndlr = doneHndlr;
    this.pageChangeHndlr = pageChangeHndlr;
    this.retrievedDataHndlr = retrievedDataHndlr;
    this.apBrowser = apBrowser;

    javascriptToJava = new JavascriptToJava();

    reloadBrowser(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void switchToPdfMode()
  {
    if (pdfjsMode) return;

    loadViewerHtml(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String tempBrowserContextFolderName = "hnJxBrowserContext";

  private static FilePath tempContextFolder()
  {
    FilePath filePath = null;

    try { filePath = tempContextFolder(false); }
    catch (IOException e) { noOp(); }

    return filePath;
  }

  private static FilePath tempContextFolder(boolean create) throws IOException
  {
    FilePath filePath = DesktopUtil.tempDir().resolve(tempBrowserContextFolderName);

    if (create && (filePath.exists() == false))
      Files.createDirectory(filePath.toPath());

    return filePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearContextFolder()
  {
    FilePath filePath = tempContextFolder();
    if (filePath.exists() == false) return;

    try { FileUtils.cleanDirectory(filePath.toFile()); }
    catch (IOException e) { noOp(); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void init() { init(false); }

  private static Browser init(boolean createBrowserInstance)
  {
    Browser browser = null;

    try
    {
      if (createBrowserInstance)
      {
        if (browserContext == null)
        {
          browserContext = BrowserContext.defaultContext();
          LoggerProvider.setLevel(OFF);

          try
          {
            browser = new Browser(browserContext);
          }
          catch (BrowserException e) // Exception means the default Chrome data folder is already in use. See https://jxbrowser.support.teamdev.com/support/solutions/articles/9000012878-creating-browser
          {
            FilePath filePath = tempContextFolder(true).resolve(InterProcClient.getInstanceID());
            Files.createDirectory(filePath.toPath());

            LoggerProvider.setLevel(SEVERE);

            browserContext = new BrowserContext(new BrowserContextParams(filePath.toString()));
          }

          LoggerProvider.setLevel(SEVERE);
        }

        if (browser == null)
          browser = new Browser(browserContext);
      }
      else
        BrowserCore.initialize();

      jxBrowserInitialized  = true;
    }
    catch (Exception e)
    {
      String msg = safeStr(e.getMessage());
      messageDialog("Unable to initialize preview window" + (msg.length() > 0 ? (": " + msg) : ""), mtError, true);
      closeWindows();
      jxBrowserDisabled = true;
    }
    catch (ExceptionInInitializerError e)
    {
      String msg = safeStr(e.getCause().getMessage());
      messageDialog("Unable to initialize preview window" + (msg.length() > 0 ? (": " + msg) : ""), mtError, true);
      closeWindows();
      jxBrowserDisabled = true;
    }

    return jxBrowserDisabled ? null : browser;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Similar to MainCtrlr.closeWindows

  private static void closeWindows()
  {
    Platform.runLater(() ->
    {
      if ((previewWindow != null) && previewWindow.getStage().isShowing())
        previewWindow.getStage().close();

      if ((contentsWindow != null) && contentsWindow.getStage().isShowing())
        contentsWindow.getStage().close();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean reloadBrowser(Runnable stuffToDoAfterLoadingViewerHtml)
  {
    if (browser != null)
    {
      removeFromParent(browserView);
      oldBrowser = browser;
    }

    browser = init(true);
    if (browser == null)
    {
      dispose(oldBrowser, false);
      return false;
    }

    if (viewerHTMLStr == null)
    {
      try { initViewerHTML(); }
      catch (IOException e)
      {
        messageDialog("Unable to initialize preview window: Unable to read HTML file", mtError);
        dispose(oldBrowser, false);
        closeWindows();
        jxBrowserDisabled = true;
        return false;
      }
    }

    BrowserPreferences preferences = browser.getPreferences();

    preferences.setAllowRunningInsecureContent(true);
    preferences.setJavaScriptCanAccessClipboard(true);
    preferences.setLocalStorageEnabled(true);
    preferences.setAllowScriptsToCloseWindows(true);

    browser.setPreferences(preferences);

    browserView = new BrowserView(browser);

    setAnchors(browserView, 0.0, 0.0, 0.0, 0.0);

    addToParent(browserView, apBrowser);

    addCustomProtocolHandler("jar");

    apBrowser.setOnMouseEntered(event -> safeFocus(browserView));

    browser.setPopupHandler(new com.teamdev.jxbrowser.chromium.javafx.DefaultPopupHandler());

    browser.setDialogHandler(new DefaultDialogHandler(browserView)
    {
      @Override public void onAlert(DialogParams params) { MessageDialog.show(browserView, "Alert", params.getMessage()); }
    });

    if (debugging()) browser.addConsoleListener(event ->
    {
      String msg = event.getMessage();
      Level level = event.getLevel();

      if (level == Level.WARNING)
        return;

      if (level == Level.LOG)
      {
        if (msg.toLowerCase().contains("unrecognized link type"))
          return;
      }

      System.out.println("JS " + event.getLevel() + ": " + msg);
    });

    browser.addLoadListener(new LoadAdapter() { @Override public void onFinishLoadingFrame(FinishLoadingEvent event)
    {
      if (event.isMainFrame() == false) return;

      ready = true;

      JSValue window = browser.executeJavaScriptAndReturnValue("window");

      window.asObject().setProperty("javaApp", javascriptToJava);

      pdfjsMode = browser.executeJavaScriptAndReturnValue("'PDFViewerApplication' in window").getBooleanValue();

      if (postBrowserLoadCode == null) return;

      postBrowserLoadCode.run();
      postBrowserLoadCode = null;
    }});

    Runnable runnable = () ->
    {
      dispose(oldBrowser, false);

      if (stuffToDoAfterLoadingViewerHtml != null)
        stuffToDoAfterLoadingViewerHtml.run();
    };

    if (pdfjsMode)
      loadViewerHtml(runnable);
    else
      runnable.run();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void dispose(Browser browser, boolean wait)
  {
    if ((browser == null) || browser.isDisposed()) return;

    if (wait)
    {
      disposing = true;

      browser.addDisposeListener(event -> disposing = false);
    }

    Runnable runnable = () ->
    {
      try
      {
        browser.dispose();
      }
      catch (IPCException e)
      {
        disposing = false;
        messageDialog("An error occurred while disposing preview pane: " + e.getMessage(), mtError);
      }
    };

    if (Environment.isWindows())
      runOutsideFXThread(runnable);
    else
      runInFXThread(runnable);

    if (wait)
      while (disposing) sleepForMillis(30);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupPdfHtml()
  {
    if (pdfjsMode)
      browser.executeJavaScriptAndReturnValue("if ('PDFViewerApplication' in window) PDFViewerApplication.cleanup();");

    pdfjsMode = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadViewerHtml(Runnable stuffToDoAfterLoading)
  {
    cleanupPdfHtml();

    postBrowserLoadCode = stuffToDoAfterLoading;

    browser.loadHTML(viewerHTMLStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initViewerHTML() throws IOException
  {
    StringBuilder viewerHTMLSB = new StringBuilder();

    readResourceTextFile("resources/pdfjs/web/viewer.html", viewerHTMLSB, false);

    int ndx = viewerHTMLSB.indexOf(basePlaceholder);

    String pathStr = App.class.getResource("resources/pdfjs/web").toExternalForm();

    if (pathStr.contains("file:/") && (pathStr.contains("file:///") == false))
      pathStr = pathStr.replace("file:/", "file:///");

    String baseTag = "<base href=\"" + pathStr + "/\" />";

    viewerHTMLSB.replace(ndx, ndx + basePlaceholder.length(), baseTag);

    viewerHTMLStr = viewerHTMLSB.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addCustomProtocolHandler(String protocol)
  {
    ProtocolService protocolService = browser.getContext().getProtocolService();

    protocolService.setProtocolHandler(protocol, request ->
    {
      URLResponse response = new URLResponse();
      //response.getHeaders().setHeader("Access-Control-Allow-Origin", "*");
      URL path = null;

      try
      {
        String pathStr = request.getURL();

        while (pathStr.matches(".*file:/[^/].*"))
          pathStr = pathStr.replaceFirst("file:/", "file:///");

        path = new URL(pathStr);
      }
      catch (Exception e) { return null; }

      try (InputStream inputStream = path.openStream(); DataInputStream stream = new DataInputStream(inputStream))
      {
        byte[] data = new byte[stream.available()];
        stream.readFully(data);
        response.setData(data);
        String mimeType = getMimeType(path.toString());
        response.getHeaders().setHeader("Content-Type", mimeType);
        return response;
      }
      catch (Exception e) { noOp(); }

      return null;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String getMimeType(String path)
  {
    if (path.endsWith(".html")) return "text/html";
    if (path.endsWith(".css"))  return "text/css";
    if (path.endsWith(".css1")) return "text/css";
    if (path.endsWith(".js"))   return "text/javascript";
    return "text/html";
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public class JavascriptToJava
  {
    public void pageChange(int newPage)
    {
      if (pageChangeHndlr != null)
        pageChangeHndlr.accept(newPage);
    }

//---------------------------------------------------------------------------

    public void sidebarChange(int view)
    {
      appPrefs.putInt(PREF_KEY_PDFJS_SIDEBAR_VIEW, view);
    }

//---------------------------------------------------------------------------

    public void printVal(JSValue val)
    {
      printJSValue(val, 0);
    }

//---------------------------------------------------------------------------

    public void setData(JSObject obj)
    {
      if (retrievedDataHndlr == null) return;

      List<Integer> hilitePages = new ArrayList<>();

      JSArray annotPages = obj.getProperty("annotPages").asArray();

      for (int ndx = 0; ndx < annotPages.length(); ndx++)
      {
        int pageNum = annotPages.get(ndx).asNumber().getInteger();

        if (hilitePages.contains(pageNum) == false)
          addToSortedList(hilitePages, pageNum);
      }

      JSValue val = obj.getProperty("pageLabels");
      Map<String, Integer> labelToPage = new HashMap<>();
      Map<Integer, String> pageToLabel = new HashMap<>();

      if (val.isArray())
      {
        JSArray pageLabels = val.asArray();

        if (pageLabels.isNull() == false)
        {
          for (int page = 1; page <= pageLabels.length(); page++)
          {
            String label = pageLabels.get(page - 1).getStringValue();
            labelToPage.put(label, page);
            pageToLabel.put(page, label);
          }
        }
      }

      retrievedDataHndlr.handle(labelToPage, pageToLabel, hilitePages);
    }

//---------------------------------------------------------------------------

    public void openDone(Boolean success, JSObject errMessage)
    {
      ready = true;

      if (success)
      {
        numPages = browser.executeJavaScriptAndReturnValue("PDFViewerApplication.pagesCount").asNumber().getInteger();
        browser.executeJavaScript("getPdfData();");
        opened = true;
      }
      else
      {
        printVal(errMessage);
      }

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSCommand.pjsOpen, success, "");
    }

//---------------------------------------------------------------------------

    public void closeDone(Boolean success, JSObject errMessage)
    {
      ready = true;

      if (success)
      {
        opened = false;
        numPages = -1;
      }
      else
      {
        printVal(errMessage);
      }

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSCommand.pjsClose, success, "");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void close()
  {
    if (!opened)
    {
      if (doneHndlr != null) doneHndlr.handle(PDFJSCommand.pjsClose, false, "Unable to close because the viewer is already closed.");
      return;
    }

    browser.executeJavaScriptAndReturnValue("closePdfFile();");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void loadHtml(String html)
  {
    cleanupPdfHtml();
    browser.loadHTML(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void loadFile(FilePath file)
  {
    cleanupPdfHtml();
    browser.loadURL(file.toURLString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final int SidebarView_NONE = 0,
                   SidebarView_THUMBS = 1,
                   SidebarView_OUTLINE = 2,
                   SidebarView_ATTACHMENTS = 3;

  void loadPdf(FilePath file, int initialPage)
  {
    Runnable runnable = () ->
    {
      opened = false;
      boolean readyToOpen = false;

      for (int ndx = 0; (ndx < 20) && !readyToOpen; ndx++)
      {
        readyToOpen = browser.executeJavaScriptAndReturnValue("'openPdfFile' in window").getBooleanValue();
        if (!readyToOpen) sleepForMillis(100);
      }

      if (!readyToOpen)
      {
        messageDialog("An error occurred while trying to show PDF file preview.", mtError);
        return;
      }

      browser.executeJavaScript("openPdfFile(\"" + file.toURLString() + "\", " +
                                                   initialPage + ", " +
                                                   appPrefs.getInt(PREF_KEY_PDFJS_SIDEBAR_VIEW, SidebarView_NONE) + ");");
      ready = false;
    };

    if (pdfjsMode == false)
      loadViewerHtml(runnable);
    else
      runnable.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void goToPage(int pageNum)
  {
    if (!ready) return;

    browser.executeJavaScript("PDFViewerApplication.pdfViewer.currentPageNumber = " + pageNum + ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void cleanup()
  {
    cleanupPdfHtml();

    dispose(browser, false);
  }

  private static boolean disposing = false;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void cleanup(Runnable disposeHndlr)
  {
    cleanupPdfHtml();

    dispose(browser, true);

    disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void printIndented(String text, int indent)
  {
    for (int ndx = 0; ndx < indent; ndx++)
      text = ' ' + text;

    System.out.println(text);
  }

  private static void printJSValue(JSValue val, int indent)
  {
    if      (val.isNull        ()) { printIndented("NULL", indent); }
    else if (val.isNumberObject()) { printIndented(String.valueOf(val.asNumberObject().getNumberValue()), indent); }
    else if (val.isNumber      ()) { printIndented(String.valueOf(val.getNumberValue()), indent); }
    else if (val.isBoolean     ()) { printIndented(String.valueOf(val.getBooleanValue()), indent); }
    else if (val.isStringObject()) { printIndented('"' + val.asStringObject().getStringValue() + '"', indent); }
    else if (val.isString      ()) { printIndented('"' + val.asString().getStringValue() + '"', indent); }
    else if (val.isUndefined   ()) { printIndented("UNDEFINED", indent); }
    else if (val.isFunction    ()) { printIndented(val.asFunction().toJSONString(), indent); }

    else if (val.isArray())
    {
      JSArray array = val.asArray();

      for (int ndx = 0; ndx < array.length(); ndx++)
      {
        printIndented("[" + ndx + ']', indent);
        printJSValue(array.get(ndx), indent + 2);
      }
    }

    else if (val.isObject())
    {
      JSObject obj = val.asObject();

      obj.getPropertyNames().forEach(propName ->
      {
        printIndented(propName + ':', indent);
        printJSValue(obj.getProperty(propName), indent + 2);
      });
    }

    else if (val.isJavaObject())
    {
      Object obj = val.asJavaObject();

      printIndented(obj.getClass().getName() + ": " + obj, indent);
    }

    else printIndented("NONE OF THE ABOVE", indent);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
