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

package org.hypernomicon.view.previewWindow;

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.teamdev.jxbrowser.chromium.Browser;
import com.teamdev.jxbrowser.chromium.BrowserPreferences;
import com.teamdev.jxbrowser.chromium.DialogParams;
import com.teamdev.jxbrowser.chromium.JSArray;
import com.teamdev.jxbrowser.chromium.JSFunction;
import com.teamdev.jxbrowser.chromium.JSObject;
import com.teamdev.jxbrowser.chromium.JSValue;
import com.teamdev.jxbrowser.chromium.ProtocolHandler;
import com.teamdev.jxbrowser.chromium.ProtocolService;
import com.teamdev.jxbrowser.chromium.URLRequest;
import com.teamdev.jxbrowser.chromium.URLResponse;
import com.teamdev.jxbrowser.chromium.events.ConsoleEvent.Level;
import com.teamdev.jxbrowser.chromium.events.FinishLoadingEvent;
import com.teamdev.jxbrowser.chromium.events.LoadAdapter;
import com.teamdev.jxbrowser.chromium.javafx.BrowserView;
import com.teamdev.jxbrowser.chromium.javafx.DefaultDialogHandler;
import com.teamdev.jxbrowser.chromium.javafx.internal.dialogs.MessageDialog;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.App;
import org.hypernomicon.util.filePath.FilePath;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

// CTRL-click the following:  https://sites.google.com/a/teamdev.com/jxbrowser-support/documentation/programmer-s-guide

public class PDFJSWrapper
{
  private boolean ready = false, opened = false, pdfjsMode = true;
  private PDFJSDoneHandler doneHndlr;
  private PDFJSPageChangeHandler pageChangeHndlr;
  private PDFJSRetrievedDataHandler retrievedDataHndlr;
  private int numPages = -1, curPage = -1;
  private JavascriptToJava javascriptToJava;
  private Browser browser = null, oldBrowser = null;
  private BrowserView browserView = null;
  private static String viewerHTMLStr = null;
  private static final String basePlaceholder = "<!-- base placeholder -->";
  private AnchorPane apBrowser = null;
  private Runnable postBrowserLoadCode = null;

  private final boolean showJavascriptConsoleMessagesInJavaConsole = true;

  public int getNumPages() { return numPages; }
  public int getCurPage()  { return curPage; }

//---------------------------------------------------------------------------

  public static enum PDFJSCommand
  {
    pjsOpen,
    pjsClose,
    pjsNumPages
  }

//---------------------------------------------------------------------------

  @FunctionalInterface public interface PDFJSDoneHandler {
    void handle(PDFJSCommand cmd, boolean success, String errMessage);
  }

//---------------------------------------------------------------------------

  @FunctionalInterface public interface PDFJSPageChangeHandler {
    void handle(int newPage);
  }

//---------------------------------------------------------------------------

  @FunctionalInterface public interface PDFJSRetrievedDataHandler {
    void handle(Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel, List<Integer> hilitePages);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public PDFJSWrapper(AnchorPane apBrowser, PDFJSDoneHandler doneHndlr,
                                            PDFJSPageChangeHandler pageChangeHndlr,
                                            PDFJSRetrievedDataHandler retrievedDataHndlr)
  {
    this.doneHndlr = doneHndlr;
    this.pageChangeHndlr = pageChangeHndlr;
    this.retrievedDataHndlr = retrievedDataHndlr;
    this.apBrowser = apBrowser;

    javascriptToJava = new JavascriptToJava();

    pdfjsMode = true; // Setting this to true so that viewer html is loaded in reloadBrowser
    reloadBrowser(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean reloadBrowser(Runnable stuffToDoAfterLoadingViewerHtml)
  {
    if (browser != null)
    {
      removeFromAnchor(browserView);
      oldBrowser = browser;
    }

    try
    {
      browser = new Browser();
    }
    catch (ExceptionInInitializerError e)
    {
      String msg = safeStr(e.getCause().getMessage());

      messageDialog("Unable to initialize preview window" + (msg.length() == 0 ? "" : (": " + msg)), mtError);
      jxBrowserDisabled = true;
      return false;
    }
    catch (Exception e)
    {
      String msg = safeStr(e.getMessage());

      messageDialog("Unable to initialize preview window" + (msg.length() == 0 ? "" : (": " + msg)), mtError);
      jxBrowserDisabled = true;
      return false;
    }

    browserCoreInitialized  = true;

    if (viewerHTMLStr == null)
    {
      try { initViewerHTML(); }
      catch (IOException e)
      {
        messageDialog("Unable to initialize preview window: Unable to read HTML file", mtError);
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

    AnchorPane.setTopAnchor(browserView, 0.0);
    AnchorPane.setBottomAnchor(browserView, 0.0);
    AnchorPane.setLeftAnchor(browserView, 0.0);
    AnchorPane.setRightAnchor(browserView, 0.0);

    apBrowser.getChildren().add(browserView);

    addCustomProtocolHandler("jar");
    addViewerHTMLProtocolHandler();

    apBrowser.setOnMouseEntered(event ->
    {
      safeFocus(browserView);
    });

    browser.setPopupHandler(new com.teamdev.jxbrowser.chromium.javafx.DefaultPopupHandler());

    browser.setDialogHandler(new DefaultDialogHandler(browserView)
    {
      @Override public void onAlert(DialogParams params) { MessageDialog.show(browserView, "Alert", params.getMessage()); }
    });

    if (showJavascriptConsoleMessagesInJavaConsole) browser.addConsoleListener(event ->
    {
      String msg = event.getMessage();
      Level level = event.getLevel();

      if (level == Level.WARNING)
        return;

      if (level == Level.LOG)
      {
        if (app.debugging() == false) return;

        if (msg.toLowerCase().contains("unrecognized link type"))
          return;
      }

      System.out.println("JS " + event.getLevel() + ": " + msg);
    });

    browser.loadURL("viewerhtml:///html");

    while (browser.isLoading()) sleepForMillis(50);

    browser.addLoadListener(new LoadAdapter()
    {
      @Override public void onFinishLoadingFrame(FinishLoadingEvent event)
      {
        if (event.isMainFrame())
        {
          ready = true;

          JSValue window = browser.executeJavaScriptAndReturnValue("window");

          window.asObject().setProperty("javaApp", javascriptToJava);

          pdfjsMode = browser.executeJavaScriptAndReturnValue("'PDFViewerApplication' in window").getBooleanValue();

          if (postBrowserLoadCode != null)
          {
            postBrowserLoadCode.run();
            postBrowserLoadCode = null;
          }
        }
      }
    });

    Runnable runnable = () ->
    {
      if (oldBrowser != null)
      {
        oldBrowser.dispose();
        oldBrowser = null;
      }

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

  private void clearHtml()
  {
    browser.executeJavaScript("if ('PDFViewerApplication' in window) PDFViewerApplication.cleanup();");
    pdfjsMode = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadViewerHtml(Runnable stuffToDoAfterLoading)
  {
    clearHtml();

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

    if ((pathStr.indexOf("file:/") >= 0) && !(pathStr.indexOf("file:///") >= 0))
      pathStr = pathStr.replace("file:/", "file:///");

    String baseTag = "<base href=\"" + pathStr + "/\" />";

    viewerHTMLSB.replace(ndx, ndx + basePlaceholder.length(), baseTag);

    viewerHTMLStr = viewerHTMLSB.toString();
  }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addViewerHTMLProtocolHandler()
  {
    ProtocolService protocolService = browser.getContext().getProtocolService();

    protocolService.setProtocolHandler("viewerhtml", new ProtocolHandler()
    {
      @Override public URLResponse onRequest(URLRequest request)
      {
        URLResponse response = new URLResponse();

        response.setData(String.valueOf("<html><body>&nbsp;</body></html>").getBytes());

        response.getHeaders().setHeader("Content-Type", "text/html");
        return response;
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addCustomProtocolHandler(String protocol)
  {
    ProtocolService protocolService = browser.getContext().getProtocolService();

    protocolService.setProtocolHandler(protocol, new ProtocolHandler()
    {
      @Override public URLResponse onRequest(URLRequest request)
      {
        URLResponse response = new URLResponse();
        //response.getHeaders().setHeader("Access-Control-Allow-Origin", "*");
        URL path = null;

        try
        {
          String pathStr = request.getURL();

          while (pathStr.matches(".*file:\\/[^/].*"))
            pathStr = pathStr.replaceFirst("file:\\/", "file:///");

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
      }
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
      curPage = newPage;
      pageChangeHndlr.handle(newPage);
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
      ArrayList<Integer> hilitePages = new ArrayList<>();
      HashMap<String, Integer> labelToPage = new HashMap<>();
      HashMap<Integer, String> pageToLabel = new HashMap<>();

      JSArray annotPages = obj.getProperty("annotPages").asArray();

      for (int ndx = 0; ndx < annotPages.length(); ndx++)
      {
        int pageNum = annotPages.get(ndx).asNumber().getInteger();

        if (hilitePages.contains(pageNum) == false)
          hilitePages.add(pageNum);
      }

      hilitePages.sort(null);

      JSValue val = obj.getProperty("pageLabels");

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

      doneHndlr.handle(PDFJSCommand.pjsOpen, success, "");
    }

//---------------------------------------------------------------------------

    public void closeDone(Boolean success, JSObject errMessage)
    {
      ready = true;

      if (success)
      {
        opened = false;
        curPage = -1;
        numPages = -1;
      }
      else
      {
        printVal(errMessage);
      }

      doneHndlr.handle(PDFJSCommand.pjsClose, success, "");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void close()
  {
    if (!opened)
    {
      doneHndlr.handle(PDFJSCommand.pjsClose, false, "Unable to close because the viewer is already closed.");
      return;
    }

    browser.executeJavaScript("closePdfFile();");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadHtml(String html)
  {
    clearHtml();

    browser.loadHTML(html);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void loadFile(FilePath file)
  {
    clearHtml();

    browser.loadURL(file.toURLString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static final int SidebarView_NONE = 0,
                          SidebarView_THUMBS = 1,
                          SidebarView_OUTLINE = 2,
                          SidebarView_ATTACHMENTS = 3;

  public void loadPdf(FilePath file, int initialPage)
  {
    Runnable runnable = () ->
    {
      opened = false;
      boolean readyToOpen = false;

      for (int ndx = 0; (ndx < 100) && !readyToOpen; ndx++)
      {
        readyToOpen = browser.executeJavaScriptAndReturnValue("'openPdfFile' in window").getBooleanValue();
        if (!readyToOpen) sleepForMillis(50);
      }

      if (!readyToOpen)
      {
        messageDialog("An error occurred while trying to show PDF file preview.", mtError);
        return;
      }

      browser.executeJavaScript("openPdfFile(\"" + file.toURLString() + "\", " +
                                                   String.valueOf(initialPage) + ", " +
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

  public void goToPage(int pageNum)
  {
    if (!ready) return;

    browser.executeJavaScript("PDFViewerApplication.pdfViewer.currentPageNumber = " + pageNum + ";");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void cleanup(Runnable disposeHndlr)
  {
    clearHtml();

    browser.addDisposeListener(event -> disposeHndlr.run());
    browser.dispose();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void printIndented(String text, int indent)
  {
    for (int ndx = 0; ndx < indent; ndx++)
      text = " " + text;

    System.out.println(text);
  }

  private void printJSValue(JSValue val, int indent)
  {
    if (val.isNull())              { printIndented("NULL", indent); }
    else if (val.isNumberObject()) { printIndented(String.valueOf(val.asNumberObject().getNumberValue()), indent); }
    else if (val.isNumber())       { printIndented(String.valueOf(val.getNumberValue()), indent); }
    else if (val.isBoolean())      { printIndented(String.valueOf(val.getBooleanValue()), indent); }
    else if (val.isStringObject()) { printIndented("\"" + val.asStringObject().getStringValue() + "\"", indent); }
    else if (val.isString())       { printIndented("\"" + val.asString().getStringValue() + "\"", indent); }
    else if (val.isUndefined())    { printIndented("UNDEFINED", indent); }
    else if (val.isFunction())     { printIndented(JSFunction.class.cast(val.asFunction()).toJSONString(), indent); }

    else if (val.isArray())
    {
      JSArray array = val.asArray();

      for (int ndx = 0; ndx < array.length(); ndx++)
      {
        printIndented("[" + ndx + "]", indent);
        printJSValue(array.get(ndx), indent + 2);
      }
    }

    else if (val.isObject())
    {
      JSObject obj = val.asObject();

      for (String propName : obj.getPropertyNames())
      {
        printIndented(propName + ":", indent);
        printJSValue(obj.getProperty(propName), indent + 2);
      }
    }

    else if (val.isJavaObject())
    {
      Object obj = val.asJavaObject();

      printIndented(obj.getClass().getName() + ": " + obj.toString(), indent);
    }

    else  { printIndented("NONE OF THE ABOVE", indent); }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToHide()
  {
    removeFromAnchor(browserView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToShow()
  {
    apBrowser.getChildren().add(browserView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
