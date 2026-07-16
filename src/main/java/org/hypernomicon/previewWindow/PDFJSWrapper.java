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

package org.hypernomicon.previewWindow;

import java.io.*;
import java.net.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import com.teamdev.jxbrowser.chromium.*;
import com.teamdev.jxbrowser.chromium.LoadParams.LoadType;
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
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import static java.util.logging.Level.*;

import org.hypernomicon.App;
import org.hypernomicon.InterProcClient;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.file.deletion.FileDeletion;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

//---------------------------------------------------------------------------

public class PDFJSWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum PDFJSOperation { pjsOpen, pjsClose }

//---------------------------------------------------------------------------

  @FunctionalInterface interface PDFJSDoneHandler
  {
    void handle(PDFJSOperation operation, boolean success, String errMessage);
  }

//---------------------------------------------------------------------------

  @FunctionalInterface interface PDFJSRetrievedDataHandler
  {
    void handle(Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel, List<Integer> hilitePages);
  }

//---------------------------------------------------------------------------

  private final AnchorPane apBrowser;
  private final Consumer<Integer> pageChangeHndlr;
  private final GridPane gpAltDisplay;
  private final JavascriptToJava javascriptToJava;
  private final PDFJSDoneHandler doneHndlr;
  private final PDFJSRetrievedDataHandler retrievedDataHndlr;

  private static final String basePlaceholder = "<!-- base placeholder -->";

  private static BrowserContext browserContext = null;
  private static String viewerHTMLStr = null, directContentHighlightJS = null;

  private Browser browser = null, oldBrowser = null;
  private BrowserView browserView = null;
  private PreviewAltDisplayCtrlr altDisplay = null;
  private Runnable postBrowserLoadCode = null;

  /**
   * Whether the content slated for the viewer is direct browser content (HTML, plain text, XML,
   * media loaded straight into the WebView). <b>Declared</b> by the load path as intent for what we
   * are about to show; not read back from the browser. Used to route pending FTS hits to the
   * direct-content highlighter rather than the pdf.js one.
   * <p>
   * False means "not direct", which is not the same as "PDF": the alternative is a pdf.js-rendered
   * PDF <i>or</i> nothing (an unpreviewable file). Whether a PDF is actually up is the separate
   * {@link #pdfjsViewerLoaded}, of which this is <i>not</i> the complement; the two can disagree
   * during a transition (the old pdf.js viewer still up while direct content has been declared),
   * and both can be false.
   */
  private boolean contentToShowIsDirect = false;

  /**
   * Whether the pdf.js viewer ({@code PDFViewerApplication}) is actually live in the browser right
   * now. <b>Discovered</b>, not declared: read back from the DOM after each load, and used to gate
   * calls into the pdf.js JS API, which exist only while the viewer is present.
   * <p>
   * <i>Not</i> the complement of {@link #contentToShowIsDirect}; see that field for why the two can
   * disagree.
   */
  private boolean pdfjsViewerLoaded = true;

  private String pendingDirectContentHits = null, pendingPdfHits = null;
  private int numPages = -1;
  private boolean ready = false, hiding = false, showingAlt = false;

  private volatile boolean opened = false;

//---------------------------------------------------------------------------

  public PDFJSWrapper(AnchorPane apBrowser)
  {
    this(apBrowser, null, null, null);
  }

  PDFJSWrapper(AnchorPane apBrowser, PDFJSDoneHandler doneHndlr, Consumer<Integer> pageChangeHndlr, PDFJSRetrievedDataHandler retrievedDataHndlr)
  {
    this.doneHndlr = doneHndlr;
    this.pageChangeHndlr = pageChangeHndlr;
    this.retrievedDataHndlr = retrievedDataHndlr;
    this.apBrowser = apBrowser;

    GridPane tempGridPane = null;
    FXMLLoader loader = new FXMLLoader(App.class.getResource("previewWindow/PreviewAltDisplay.fxml"));
    try { tempGridPane = loader.load(); } catch (IOException e) { noOp(); }
    gpAltDisplay = tempGridPane;
    altDisplay = loader.getController();

    javascriptToJava = new JavascriptToJava();

    reloadBrowser(null);
  }

//---------------------------------------------------------------------------

  int getNumPages()      { return numPages; }
  boolean isShowingAlt() { return showingAlt; }

  /** Declares whether the content for the next preview is direct browser content; set by the load
   *  path. See {@link #contentToShowIsDirect}. */
  void setContentToShowIsDirect(boolean contentToShowIsDirect) { this.contentToShowIsDirect = contentToShowIsDirect; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToHide()
  {
    removeFromParent(showingAlt ? gpAltDisplay : browserView);

    hiding = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToShow()
  {
    if (hiding == false) return;

    addToParent(showingAlt ? gpAltDisplay : browserView, apBrowser);

    hiding = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchToAltDisplay()
  {
    runInFXThread(() ->
    {
      if (browserView == null) return;

      if (hiding == false)
      {
        removeFromParent(browserView);
        addToParent(gpAltDisplay, apBrowser);
      }

      showingAlt = true;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchToPreviewDisplay()
  {
    runInFXThread(() ->
    {
      if (browserView == null) return;

      if (hiding == false)
      {
        removeFromParent(gpAltDisplay);
        addToParent(browserView, apBrowser);
      }

      showingAlt = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setGenerating(FilePath filePath, boolean dontRestartProgressIfSamePreview)
  {
    runInFXThread(() ->
    {
      altDisplay.setGenerating(filePath, dontRestartProgressIfSamePreview);
      switchToAltDisplay();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setStartingConverter()
  {
    runInFXThread(() ->
    {
      altDisplay.setStartingConverter();
      switchToAltDisplay();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setUnable(FilePath filePath)
  {
    runInFXThread(() ->
    {
      altDisplay.setUnable(filePath);
      switchToAltDisplay();
    });
  }

  public void setUnable(String pathStr)
  {
    runInFXThread(() ->
    {
      altDisplay.setUnable(pathStr);
      switchToAltDisplay();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setNoOfficeInstallation()
  {
    runInFXThread(() ->
    {
      altDisplay.setNoOfficeInstallation();
      switchToAltDisplay();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
  {
    switchToPreviewDisplay();

    if (pdfjsViewerLoaded)
    {
      if (opened)
        close();
    }
    else
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
    FilePath filePath = tempDir().resolve(tempBrowserContextFolderName);

    if (create && (filePath.exists() == false))
      filePath.createDirectory();

    return filePath;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearContextFolder()
  {
    FilePath filePath = tempContextFolder();
    if (filePath.exists() == false) return;

    FileDeletion.ofDirContentsOnly(filePath).nonInteractiveFailureOK().execute();
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
            filePath.createDirectory();

            LoggerProvider.setLevel(SEVERE);

            browserContext = new BrowserContext(new BrowserContextParams(filePath.toString()));
          }

          LoggerProvider.setLevel(SEVERE);
        }

        if (browser == null)
          browser = new Browser(browserContext);

        BrowserTracker.register(browser, "PDFJSWrapper");
      }
      else
        BrowserCore.initialize();

      jxBrowserInitialized  = true;
    }
    catch (ExceptionInInitializerError e)
    {
      errorPopup("Unable to initialize preview window: " + getThrowableMessage(e.getCause()));
      disable();
    }
    catch (IOException | LinkageError e)
    {
      errorPopup("Unable to initialize preview window: " + getThrowableMessage(e));
      disable();
    }

    return jxBrowserDisabled ? null : browser;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Similar to MainCtrlr.closeWindows

  private static void disable()
  {
    Platform.runLater(() ->
    {
      PreviewWindow .close(false);
      ContentsWindow.close(false);
    });

    jxBrowserDisabled = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean zoom(boolean zoomingIn)
  {
    if (pdfjsViewerLoaded) return false;

    if (zoomingIn)
      browser.zoomIn();
    else
      browser.zoomOut();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reloadBrowser(Runnable stuffToDoAfterLoadingViewerHtml)
  {
    switchToPreviewDisplay();

    if (browser != null)
    {
      removeFromParent(browserView);

      // Dispose any prior reload's browser that hasn't been cleaned up yet, so this overwrite doesn't
      // orphan it. A never-disposed Browser leaks its native channel and non-daemon IPC threads, so
      // disposing it here is leak hygiene. (This is not what guarantees process exit; the primary
      // hang is a post-dispose channel wedge, backstopped by ExitWatchdog.)

      dispose(oldBrowser, false);
      oldBrowser = browser;
    }

    browser = init(true);
    if (browser == null)
    {
      dispose(oldBrowser, false);
      return;
    }

    if (viewerHTMLStr == null)
    {
      try { initViewerHTML(); }
      catch (IOException e)
      {
        errorPopup("Unable to initialize preview window: Unable to read HTML file");
        dispose(oldBrowser, false);
        disable();
        return;
      }
    }

    BrowserPreferences preferences = browser.getPreferences();

    preferences.setAllowRunningInsecureContent(true);
    preferences.setJavaScriptCanAccessClipboard(true);
    preferences.setLocalStorageEnabled(true);
    preferences.setAllowScriptsToCloseWindows(true);

    browser.setPreferences(preferences);

    browser.setDownloadHandler(downloadItem ->
    {
      try
      {
        setUnable(Paths.get(new URI(downloadItem.getURL())).toString());
      }
      catch (URISyntaxException e)
      {
        setUnable(downloadItem.getURL());
      }

      return false;
    });

    browser.setLoadHandler(new DefaultLoadHandler()
    {
      @Override public boolean onLoad(LoadParams params)
      {
        if (params.isRedirect())
          return true;

        if (params.getType() == LoadType.LinkClicked)
        {
          openWebLink(params.getURL());
          return true;
        }

        return false;
      }
    });

    browserView = new BrowserView(browser);

    setAnchors(browserView, 0.0, 0.0, 0.0, 0.0);

    addToParent(browserView, apBrowser);

    addCustomProtocolHandler(browser, "jar");

    apBrowser.setOnMouseEntered(event ->
    {
      if (showingAlt == false)
        safeFocus(browserView);
    });

    browser.setPopupHandler(new com.teamdev.jxbrowser.chromium.javafx.DefaultPopupHandler());

    browser.setDialogHandler(new DefaultDialogHandler(browserView)
    {
      @Override public void onAlert(DialogParams params) { MessageDialog.show(browserView, "Alert", params.getMessage()); }
    });

    if (app.debugging) browser.addConsoleListener(event ->
    {
      String msg = event.getMessage();
      Level level = event.getLevel();

      if (level == Level.WARNING)
        return;

      if (level == Level.LOG)
      {
        // pdf.js emits its warnings via console.log with a "Warning:" prefix, so they arrive
        // at LOG level. parseDestDictionary fires per malformed outline/link entry and can
        // dominate a debug log (observed at ~95% of the log during a large indexing run).

        String msgLower = msg.toLowerCase();

        if (msgLower.contains("unrecognized link type") || msgLower.contains("parsedestdictionary"))
          return;
      }

      System.out.println("JS " + event.getLevel() + ": " + msg);
    });

    browser.addLoadListener(new LoadAdapter() { @Override public void onFinishLoadingFrame(FinishLoadingEvent event)
    {
      if (event.isMainFrame() == false) return;

      ready = true;

      JSValue window = browser.executeJavaScriptAndReturnValue("window");

      try
      {
        window.asObject().setProperty("javaApp", javascriptToJava);
      }
      catch (IllegalStateException e)
      {
        noOp();
      }

      pdfjsViewerLoaded = browser.executeJavaScriptAndReturnValue("'PDFViewerApplication' in window").getBooleanValue();

      if (contentToShowIsDirect && (pendingDirectContentHits != null))
        applyDirectContentHits();

      if (pdfjsViewerLoaded && (pendingPdfHits != null))
        applyPdfHits();

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

    if (pdfjsViewerLoaded)
      loadViewerHtml(runnable);
    else
      runnable.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Disposes a browser and waits for disposal to complete. Uses the correct
   *  threading (off FX thread on Windows, on FX thread elsewhere). Also used by
   *  PreviewWindow's shutdown sweep of undisposed Browser instances. */
  static void dispose(Browser browser, boolean wait)
  {
    if ((browser == null) || browser.isDisposed()) return;

    CountDownLatch latch = wait ? new CountDownLatch(1) : null;

    if (wait)
      browser.addDisposeListener(event -> latch.countDown());

    Runnable runnable = () ->
    {
      try
      {
        browser.dispose();
      }
      catch (IPCException e)
      {
        if (latch != null) latch.countDown();
        errorPopup("An error occurred while disposing preview pane: " + getThrowableMessage(e));
      }
    };

    if (Environment.isWindows())
      runOutsideFXThread(runnable);
    else
      runInFXThread(runnable);

    if (wait)
    {
      try
      {
        if (latch.await(10, TimeUnit.SECONDS) == false)
          System.out.println("PDFJSWrapper.dispose: timed out after 10s waiting for browser disposal to complete.");
      }
      catch (InterruptedException e) { Thread.currentThread().interrupt(); }
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupPdfHtml()
  {
    if (pdfjsViewerLoaded)
    {
      browser.executeJavaScript("if ('PDFViewerApplication' in window) PDFViewerApplication.cleanup();");
      sleepForMillis(200);
    }

    pdfjsViewerLoaded = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadViewerHtml(Runnable stuffToDoAfterLoading)
  {
    switchToPreviewDisplay();

    cleanupPdfHtml();

    postBrowserLoadCode = stuffToDoAfterLoading;

    browser.loadHTML(viewerHTMLStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void initViewerHTML() throws IOException
  {
    StringBuilder viewerHTMLSB = new StringBuilder();

    readResourceTextFile("resources/pdfjs/web/viewer.html", viewerHTMLSB);

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

  private static void initDirectContentHighlightJS() throws IOException
  {
    StringBuilder sb = new StringBuilder();
    readResourceTextFile("resources/pdfjs/web/directContentHighlight.js", sb);
    directContentHighlightJS = sb.toString();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("deprecation")
  public static void addCustomProtocolHandler(Browser browser, String protocol)
  {
    ProtocolService protocolService = browser.getContext().getProtocolService();

    protocolService.setProtocolHandler(protocol, request ->
    {
      URLResponse response = new URLResponse();
      //response.getHeaders().setHeader("Access-Control-Allow-Origin", "*");
      URL path;

      try
      {
        String pathStr = request.getURL();

        while (pathStr.matches(".*file:/[^/].*"))
          pathStr = pathStr.replaceFirst("file:/", "file:///");

        path = new URL(pathStr);
      }
      catch (MalformedURLException e) { return null; }

      try (InputStream inputStream = path.openStream(); DataInputStream stream = new DataInputStream(inputStream))
      {
        byte[] data = new byte[stream.available()];
        stream.readFully(data);
        response.setData(data);
        String mimeType = getMimeType(path.toString());
        response.getHeaders().setHeader("Content-Type", mimeType);
        return response;
      }
      catch (IOException e) { noOp(); }

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
      app.prefs.putInt(PrefKey.PDFJS_SIDEBAR_VIEW, view);
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

        // Drain any setAllHits queued during the PDF swap. In the PDF->PDF
        // case there's no browser navigation, so onFinishLoadingFrame doesn't
        // fire; this is the only point where we know the new PDF is ready
        // for the viewer's setAllHits JS to be called.

        if (pendingPdfHits != null)
          applyPdfHits();
      }
      else
      {
        printVal(errMessage);
      }

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsOpen, success, "");
    }

//---------------------------------------------------------------------------

    public void closeDone(Boolean success, JSObject errMessage)
    {
      ready = true;

      if (success)
      {
        numPages = -1;
        opened = false;
      }
      else
      {
        printVal(errMessage);
      }

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsClose, success, "");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void close()
  {
    if (opened == false)
    {
      if (doneHndlr != null) doneHndlr.handle(PDFJSOperation.pjsClose, false, "Unable to close because the viewer is already closed.");
      return;
    }

    browser.executeJavaScript("closePdfFile();");

    for (int ndx = 0; (ndx < 5) && opened; ndx++)
      sleepForMillis(100);

    if (opened)
      errorPopup("An error occurred while closing the PDF file preview.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void loadFile(FilePath filePath, boolean isHtml) throws IOException
  {
    switchToPreviewDisplay();

    cleanupPdfHtml();

    ready = false;
    resetHitState();

    if (isHtml)
    {
      // Parse with charset auto-detection (BOM, then the document's own charset
      // declaration, defaulting to UTF-8) rather than decoding with the JVM
      // default charset.

      Document doc = Jsoup.parse(filePath.toFile());

      doc.getElementsByTag("script").forEach(Element::remove);

      // browser.loadHTML transmits the string to Chromium as UTF-8. Serialize as
      // UTF-8 and drop the document's now-stale charset declaration so it can't
      // tell Chromium to re-decode the UTF-8 byte stream as windows-1252.

      doc.outputSettings().charset(StandardCharsets.UTF_8);

      doc.getElementsByTag("meta").forEach(meta ->
      {
        if (meta.hasAttr("charset") || "Content-Type".equalsIgnoreCase(meta.attr("http-equiv")))
          meta.remove();
      });

      browser.loadHTML(doc.html());
    }
    else
      browser.loadURL(filePath.toURLString());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final int SidebarView_NONE = 0,
                   SidebarView_THUMBS = 1,
                   SidebarView_OUTLINE = 2,
                   SidebarView_ATTACHMENTS = 3;

  void loadPdf(FilePath file, int initialPage)
  {
    final boolean wasPdfjsViewerLoaded = pdfjsViewerLoaded;

    // Reset ready synchronously so any cross-thread setAllHits / goToPage call
    // queued before the deferred runnable runs sees a not-ready state and
    // buffers (or no-ops) instead of dispatching to the previous page's JS.
    // Mirrors what loadFile does at the start of its body.

    ready = false;
    resetHitState();

    Runnable runnable = () ->
    {
      opened = false;

      if (wasPdfjsViewerLoaded == false)
      {
        boolean readyToOpen = false;

        for (int ndx = 0; (ndx < 20) && (readyToOpen == false); ndx++)
        {
          readyToOpen = browser.executeJavaScriptAndReturnValue("'openPdfFile' in window").getBooleanValue();
          if (readyToOpen == false)
            sleepForMillis(100);
        }

        if (readyToOpen == false)
        {
          errorPopup("An error occurred while trying to show PDF file preview.");
          return;
        }
      }

      browser.executeJavaScript("openPdfFile(\"" + file.toURLString() + "\", " +
                                                   initialPage + ", " +
                                                   app.prefs.getInt(PrefKey.PDFJS_SIDEBAR_VIEW, SidebarView_NONE) + ");");
    };

    switchToPreviewDisplay();

    if (pdfjsViewerLoaded == false)
      loadViewerHtml(runnable);
    else
      Platform.runLater(runnable);  // This helps to prevent JxBrowser from crashing when quickly removing and re-adding it to the scene graph, then executing a script
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void goToPage(int pageNum)
  {
    if (ready == false) return;

    browser.executeJavaScript("PDFViewerApplication.pdfViewer.currentPageNumber = " + pageNum + ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void applyPdfHits()
  {
    if ((pendingPdfHits == null) || (browser == null) || browser.isDisposed()) return;

    // Confirm the viewer's setAllHits function is available before consuming
    // pendingPdfHits. If we're called prematurely (e.g., before loadViewerHtml
    // has navigated away from a previous DIRECT_CONTENT page), leave the buffer
    // intact so onFinishLoadingFrame can drain it once pdf.js is loaded.

    boolean fnExists = browser.executeJavaScriptAndReturnValue("typeof setAllHits === 'function'").getBooleanValue();

    if (fnExists == false) return;

    String json = pendingPdfHits;
    pendingPdfHits = null;

    browser.executeJavaScript("setAllHits('" + json.replace("'", "\\'") + "');");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Send all hit data for the current file. The viewer stores it and applies
   * highlights lazily as each page's text layer finishes rendering.
   *
   * @param allHitsJson JSON object mapping 1-based page numbers to arrays of
   *                    [startOffset, endOffset] pairs (page-relative offsets).
   *                    Example: {"1":[[10,20],[50,60]],"3":[[5,15]]}
   */
  void setAllHits(String allHitsJson)
  {
    if (contentToShowIsDirect)
    {
      // For non-PDF content, store hits and apply after content finishes loading
      pendingDirectContentHits = allHitsJson;

      if (ready)
        applyDirectContentHits();

      return;
    }

    // For PDF content, store hits if not ready yet; apply when PDF finishes loading
    pendingPdfHits = allHitsJson;

    if (ready == false) return;

    applyPdfHits();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scroll to the highlight with the given match index (stored in data-match-ndx attribute).
   */
  void scrollToHighlightByMatchNdx(int matchNdx)
  {
    if ((ready == false) || (browser == null) || browser.isDisposed()) return;

    browser.executeJavaScript
    (
      "(function() {" +
      "  var el = document.querySelector('.fts-highlight[data-match-ndx=\"" + matchNdx + "\"]');" +
      "  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });" +
      "})();"
    );
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Drops any buffered FTS hit data so a newly-loading file cannot inherit the
   * previous file's hits. Called at the start of every load ({@link #loadPdf},
   * {@link #loadFile}); the JS viewer separately resets its own stored hits when
   * a new document opens (see openPdfFile in javaapp.js). Together these make
   * "loading a file clears prior hits" an invariant, independent of any caller.
   *
   * <p>Only the Java-side buffers are touched here; the browser is not, because
   * the in-progress load is replacing the document (and its highlight DOM)
   * anyway. Hits intended for the file being loaded are pushed afterward, so
   * this reset never clobbers them.
   * <p>Also reused by {@link #clearAllHits}, which drops these same buffers and
   * then clears the browser-side highlights.
   */
  private void resetHitState()
  {
    pendingDirectContentHits = null;
    pendingPdfHits = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearAllHits()
  {
    resetHitState();

    if (ready == false) return;

    if (contentToShowIsDirect)
    {
      browser.executeJavaScript
      (
        "var hl = document.querySelectorAll('.fts-highlight');" +
        "for (var i = 0; i < hl.length; i++) {" +
        "  var parent = hl[i].parentNode;" +
        "  parent.replaceChild(document.createTextNode(hl[i].textContent), hl[i]);" +
        "  parent.normalize();" +
        '}'
      );

      return;
    }

    if (browser.executeJavaScriptAndReturnValue("typeof clearAllHits === 'function'").getBooleanValue())
      browser.executeJavaScript("clearAllHits();");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Inject JavaScript to highlight text at the stored character offsets in
   * directly-loaded content (HTML, text, XML, etc.). Walks DOM text nodes,
   * maps character offsets, and wraps matching ranges in highlight spans.
   */
  private void applyDirectContentHits()
  {
    if ((pendingDirectContentHits == null) || (browser == null) || browser.isDisposed()) return;

    if (directContentHighlightJS == null)
    {
      try { initDirectContentHighlightJS(); }
      catch (IOException e)
      {
        System.out.println("PDFJSWrapper.applyDirectContentHits: failed to load JS resource: " + getThrowableMessage(e));
        return;
      }
    }

    String json = pendingDirectContentHits;
    pendingDirectContentHits = null;

    // The JS resource is a function expression "function (data) { ... }" that
    // we wrap in parens and immediately invoke with the parsed JSON data.
    // The JSON format is: {"matches":[{"ctx":"...context...","s":20,"e":27},...]}
    // Each entry has context text from stored content, plus the start/end offsets
    // of the matched word within the context. The JS searches for the context in
    // the rendered DOM and wraps the match portion in a highlight span.

    browser.executeJavaScript('(' + directContentHighlightJS + ")(" + json + ");");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void cleanup()
  {
    cleanupPdfHtml();

    dispose(oldBrowser, false);  // see cleanup(Runnable): a deferred reload disposal may not have run yet
    dispose(browser, false);
  }


//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void cleanup(Runnable disposeHndlr)
  {
    cleanupPdfHtml();

    // reloadBrowser() disposes the prior browser only via a deferred callback once the replacement
    // finishes loading. If the app is closed before that runs, oldBrowser is still alive; dispose it
    // here too so it doesn't leak (a never-disposed Browser holds its native channel and non-daemon
    // IPC threads). ExitWatchdog is the backstop that guarantees exit if a channel wedges regardless.

    dispose(oldBrowser, true);
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
