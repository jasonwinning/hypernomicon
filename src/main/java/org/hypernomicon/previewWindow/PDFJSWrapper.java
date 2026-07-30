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

import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Consumer;

import com.teamdev.jxbrowser.browser.Browser;
import com.teamdev.jxbrowser.browser.callback.*;
import com.teamdev.jxbrowser.browser.callback.input.MoveMouseWheelCallback;
import com.teamdev.jxbrowser.browser.event.ConsoleMessageReceived;
import com.teamdev.jxbrowser.js.JsAccessible;
import com.teamdev.jxbrowser.js.JsObject;
import com.teamdev.jxbrowser.navigation.callback.StartNavigationCallback;
import com.teamdev.jxbrowser.navigation.event.FrameLoadFinished;
import com.teamdev.jxbrowser.ui.event.MouseWheel;
import com.teamdev.jxbrowser.view.javafx.BrowserView;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.App;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.json.simple.parser.ParseException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

import javafx.application.Platform;
import javafx.fxml.FXMLLoader;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.GridPane;

//---------------------------------------------------------------------------

final class PDFJSWrapper
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  enum PDFJSOperation { pjsOpen, pjsDirectLoad, pjsClose }

//---------------------------------------------------------------------------

  @FunctionalInterface interface PDFJSDoneHandler
  {
    /**
     * @param operation which viewer operation completed: a pdf.js document open,
     *                  a direct-content navigation finishing, or a document close
     * @param file      the file the operation was for (open: the file whose open
     *                  completed, which may already be superseded by a newer
     *                  request; direct load: the navigated file; close: null).
     *                  Consumers confirming loads must match this against what
     *                  they issued rather than trusting arrival order.
     */
    void handle(PDFJSOperation operation, FilePath file, boolean success, String errMessage);
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

  private static String directContentHighlightJS = null;

  private Browser browser = null;
  private BrowserView browserView = null;
  private PreviewAltDisplayCtrlr altDisplay = null;
  private Runnable postBrowserLoadCode = null;

  /** Guards the {@link #viewerHtmlLoadInFlight}/{@link #postBrowserLoadCode} pair:
   *  chaining work onto an in-flight viewer load (FX thread) and the load-finished
   *  event consuming that work (JxBrowser thread) must be atomic, or a runnable
   *  chained in the gap is never triggered. */
  private final Object loadLock = new Object();

  /**
   * Whether the content slated for the viewer is direct browser content (HTML, plain text, XML,
   * media loaded straight into the browser). <b>Declared</b> by the load path as intent for what we
   * are about to show; not read back from the browser. Used to route FTS hits and scroll targets
   * to the direct-content highlighter rather than the pdf.js one.
   * <p>
   * False means "not direct", which is not the same as "PDF": the alternative is a pdf.js-rendered
   * PDF <i>or</i> nothing (an unpreviewable file). Whether a PDF is actually up is the separate
   * {@link #pdfjsViewerLoaded}, of which this is <i>not</i> the complement; the two can disagree
   * during a transition (the old pdf.js viewer still up while direct content has been declared),
   * and both can be false.
   */
  private boolean contentToShowIsDirect = false;

  /**
   * Whether the pdf.js viewer page is the browser's current document. Under JxBrowser 6 this was
   * discovered by probing the DOM after each load; now the main-frame load-finished event commits
   * it from the finished navigation's URL (attribution by evidence: two navigations can be in
   * flight at once and commit in either order, so the load paths' intent cannot be trusted here).
   */
  private volatile boolean pdfjsViewerLoaded = true;

  /** True from the moment loadViewerHtml starts the navigation until the viewer page's
   *  load-finished event; anything wanting to run JS against the viewer page in that
   *  window must chain onto {@link #postBrowserLoadCode} instead of executing
   *  immediately (the script would run in the old or half-loaded document). Cleared
   *  by the navigations that supersede a pending viewer load (loadFile, reloadBrowser).
   */
  private volatile boolean viewerHtmlLoadInFlight = false;

  private FilePath lastDirectFilePath = null;

  /** The exact URL the most recent direct-content load was issued under (the
   *  self-minted {@code data:} URL for HTML, the file URL otherwise), written
   *  before the navigation is started. The main-frame load-finished handler
   *  confirms a direct load only when the finished URL is this one: a
   *  superseded direct load can finish after its successor was issued, and
   *  attributing that late finish to {@link #lastDirectFilePath} confirmed the
   *  new document's load while the old document was still on screen (the FTS
   *  hits then went into the wrong DOM and were never re-applied). Volatile:
   *  written from load paths, read on the browser event thread. */
  private volatile String expectedDirectUrl = null;

  private int numPages = -1;
  private boolean ready = false, hiding = false, showingAlt = false;

  private volatile boolean opened = false;

  /**
   * Open coordination (writes are FX-confined; volatile because the openDone
   * bridge callback reads {@link #openInFlightFile} and viewer-driving threads
   * read the others): at most one {@code openPdfFile} call is in flight at a
   * time. A request made while one is loading replaces any previously waiting
   * request (latest wins, never a queue) and is issued when the in-flight open
   * reports {@code openDone}, success or failure. Concurrent
   * {@code openPdfFile} calls race inside pdf.js (null-document errors, a
   * nondeterministic final document) and under rapid selection can destabilize
   * the engine.
   */
  private volatile boolean openInFlight = false;
  private volatile FilePath pendingOpenFile = null, openInFlightFile = null;
  private int pendingOpenPage = 1;

  /** Page correction that arrived while an open was in flight (see
   *  {@link #goToPage(int)}); applied by the coordinator's release, cleared by
   *  each new load. Volatile: written from viewer-driving threads, drained on FX. */
  private volatile int pendingGoToPage = -1;

  /** Identifies the newest issued open (FX-confined); a deferred open dispatch
   *  (see {@link #issueOpen}) fires only if it is still the newest and nothing
   *  navigated away during its deferral. */
  private long issueSeq = 0;

  /** Render pulses between issuing an open and dispatching it to the viewer:
   *  a document open dispatched in the same pulse that re-shows the
   *  hardware-accelerated surface (alt display clearing, e.g.) can leave the
   *  surface blank while the document renders in Chromium; giving the show a
   *  couple of presented frames first avoids that window. */
  private static final int OPEN_DEFER_PULSES = 2;

//---------------------------------------------------------------------------

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

    // The alt display overlays the always-attached browser view (see
    // switchToAltDisplay), so its root needs an opaque background; in the FXML
    // only the centered message box has one.

    if (gpAltDisplay != null)
      gpAltDisplay.setStyle("-fx-background-color: -fx-background;");

    javascriptToJava = new JavascriptToJava();

    reloadBrowser(null);
  }

//---------------------------------------------------------------------------

  int getNumPages() { return numPages; }

  /** Declares whether the content for the next preview is direct browser content; set by the load
   *  path. See {@link #contentToShowIsDirect}. */
  void setContentToShowIsDirect(boolean contentToShowIsDirect) { this.contentToShowIsDirect = contentToShowIsDirect; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToHide()
  {
    if (app.debugging)
      System.out.println("PDFJSWrapper.prepareToHide: showingAlt=" + showingAlt);

    removeFromParent(browserView);
    removeFromParent(gpAltDisplay);

    hiding = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToShow()
  {
    if (app.debugging)
      System.out.println("PDFJSWrapper.prepareToShow: hiding=" + hiding + " showingAlt=" + showingAlt);

    if (hiding == false) return;

    addToParent(browserView, apBrowser);
    browserView.setVisible(showingAlt == false);

    if (showingAlt)
      addToParent(gpAltDisplay, apBrowser);  // back on top of the (hidden) browser view

    hiding = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The browser view stays attached while the alt display shows; it is hidden
  // via setVisible(false) instead. Two constraints force this design: detaching
  // the view mid-flow (the original design) could leave JxBrowser 9 not
  // painting after the re-attach (a document would open and render in Chromium
  // while the re-attached view stayed blank until the next open), and the
  // hardware-accelerated view is a native surface that paints over sibling
  // JavaFX nodes regardless of z-order, so an overlay alone never shows. With
  // the surface hidden, the alt display (attached on top, opaque root) renders.
  // The view is detached only in prepareToHide (tab-level hide, long-proven).

  private void switchToAltDisplay()
  {
    runInFXThread(() ->
    {
      if (app.debugging)
        System.out.println("PDFJSWrapper.switchToAltDisplay: browserView=" + (browserView != null) + " hiding=" + hiding);

      if (browserView == null) return;

      if (hiding == false)
      {
        addToParent(gpAltDisplay, apBrowser);
        browserView.setVisible(false);
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
      if (app.debugging)
        System.out.println("PDFJSWrapper.switchToPreviewDisplay: browserView=" + (browserView != null) + " hiding=" + hiding);

      if (browserView == null) return;

      if (hiding == false)
      {
        removeFromParent(gpAltDisplay);
        browserView.setVisible(true);  // attached all along; just unhide the surface
      }

      showingAlt = false;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setGenerating(FilePath filePath)
  {
    runInFXThread(() ->
    {
      altDisplay.setGenerating(filePath);
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

    // Drop any open still waiting its turn; a reset means nothing should load.
    // An in-flight open is left alone: its page survives, so its openDone still
    // arrives and releases the coordinator normally.

    runInFXThread(() -> pendingOpenFile = null);

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

  /**
   * Zooms whatever the viewer is showing, through the mechanism proper to it:
   * document scale for a pdf.js document (the same path as the viewer's own
   * toolbar buttons), Chromium page zoom for direct content (where scaling
   * the page is scaling the content). Serves the gestures the browser surface
   * never sees: wheel and key events over the Preview Window's own controls.
   *
   * <p>Known and deliberate: for a PDF, Ctrl+wheel here zooms in smaller
   * increments than the same gesture over the document itself. Over the
   * document, pdf.js's own wheel handler runs a different algorithm: it
   * converts the wheel delta to zoom steps (roughly three 1.1x steps per
   * wheel notch) and zooms toward the cursor position.
   *
   * @return whether a zoom was issued (false leaves the triggering event
   *         unconsumed: no document is open, or the viewer page is up with
   *         nothing to zoom)
   */
  boolean zoom(boolean zoomingIn)
  {
    if (pdfjsViewerLoaded)
    {
      if (opened == false) return false;  // the viewer page is up but holds no document; nothing to zoom

      execJS("PDFViewerApplication." + (zoomingIn ? "zoomIn" : "zoomOut") + "();");
      return true;
    }

    if (zoomingIn)
      browser.zoom().in();
    else
      browser.zoom().out();

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reloadBrowser(Runnable stuffToDoAfterLoadingViewerHtml)
  {
    switchToPreviewDisplay();

    // The browser (and with it any in-flight or waiting PDF open, and any
    // viewer-page load) is being replaced; clear the open coordination and
    // viewer-load state so neither can wedge on completions that never come.

    pendingOpenFile = null;
    openInFlight = false;
    issueSeq++;

    synchronized (loadLock)
    {
      viewerHtmlLoadInFlight = false;
      postBrowserLoadCode = null;
    }

    if (browser != null)
    {
      removeFromParent(browserView);

      Browser toClose = browser;
      browser = null;

      // close() blocks and can need the FX thread (view detachment), so it must not
      // run on it; reloadBrowser is called from FX-thread refresh flows.

      runOutsideFXThread(() ->
      {
        try
        {
          if (toClose.isClosed() == false)
            toClose.close();
        }
        catch (RuntimeException e)
        {
          System.out.println("PDFJSWrapper: error closing browser during reload: " + getThrowableMessage(e));
        }
      });
    }

    browser = BrowserEngine.newBrowser();
    if (browser == null)
      return;

    // Inject the bridge before page scripts run, so javaApp already exists when the
    // viewer page's scripts execute.

    browser.set(InjectJsCallback.class, params ->
    {
      JsObject window = params.frame().executeJavaScript("window");

      if (window != null)
        window.putProperty("javaApp", javascriptToJava);

      return InjectJsCallback.Response.proceed();
    });

    // Reject all downloads; the preview pane never saves files. A download here means
    // Chromium could not display what the pane just navigated to (e.g. a .mov file):
    // the navigation becomes a download instead of committing, so without intervention
    // the previous content would silently stay up. Cancel it and show the unable
    // display for the file the load path was attempting.

    browser.set(StartDownloadCallback.class, (params, tell) ->
    {
      tell.cancel();

      FilePath filePath = lastDirectFilePath;

      if (FilePath.isEmpty(filePath))
        setUnable("");
      else
        setUnable(filePath);
    });

    // Navigation policy: the preview pane may only navigate to content this application
    // serves; any attempted navigation to an external URL (link click, JS redirect, meta
    // refresh) is cancelled and routed to the system browser instead.

    browser.navigation().set(StartNavigationCallback.class, params ->
    {
      String url = params.url();

      if (isInternalUrl(url))
        return StartNavigationCallback.Response.start();

      openWebLink(url);
      return StartNavigationCallback.Response.ignore();
    });

    // PDF links open with target=_blank (a popup) in the stock viewer, and page JS can
    // call window.open; both become system-browser opens.

    browser.set(CreatePopupCallback.class, params ->
    {
      String url = params.targetUrl();

      if (isInternalUrl(url) == false)
        openWebLink(url);

      return CreatePopupCallback.Response.suppress();
    });

    // External-protocol links (mailto: and the like). Since Chromium 151
    // (JxBrowser 9.4.0 and later) these route through this callback instead of
    // launching the external application directly, and with no callback
    // registered the link does nothing at all. Approving restores the earlier
    // direct-launch behavior: the content is the user's own document and the
    // click is the user's own gesture, so the OS handoff (typically the mail
    // client) is exactly what was asked for.

    browser.set(OpenExternalAppCallback.class, (params, tell) -> tell.open());

    // Zoom direct content on Ctrl+wheel (Cmd+wheel also accepted, matching the
    // pdf.js viewer's own gesture). Under JxBrowser 6 the wheel event reached
    // the JavaFX stage, so PreviewWindow's scroll filter implemented this; the
    // version 9 hardware-accelerated surface takes wheel input natively, the
    // event never becomes a JavaFX ScrollEvent over the preview, and embedded
    // Chromium implements no zoom gesture of its own. The pdf.js viewer page
    // is forwarded untouched: its own script scales the document on Ctrl+wheel.
    // (The stage filter stays for wheel events over the window's own controls.)

    browser.set(MoveMouseWheelCallback.class, params ->
    {
      MouseWheel event = params.event();

      if ((event.keyModifiers().isControlDown() || event.keyModifiers().isMetaDown())
          && (event.deltaY() != 0) && (pdfjsViewerLoaded == false))
      {
        boolean zoomingIn = event.deltaY() > 0;

        runInFXThread(() -> zoom(zoomingIn));  // zoom() must not run on this callback thread

        return MoveMouseWheelCallback.Response.suppress();
      }

      return MoveMouseWheelCallback.Response.proceed();
    });

    if (app.debugging) browser.on(ConsoleMessageReceived.class, event ->
    {
      var msg = event.consoleMessage();
      String level = msg.level().toString(),
             text  = msg.message();

      if (level.contains("WARNING"))
        return;

      if (level.contains("LOG"))
      {
        // pdf.js emits its warnings via console.log with a "Warning:" prefix, so they arrive
        // at LOG level. parseDestDictionary fires per malformed outline/link entry and can
        // dominate a debug log (observed at ~95% of the log during a large indexing run).

        String textLower = text.toLowerCase();

        if (textLower.contains("unrecognized link type") || textLower.contains("parsedestdictionary"))
          return;
      }

      System.out.println("JS " + level + ": " + text);
    });

    browser.navigation().on(FrameLoadFinished.class, event ->
    {
      if (event.frame().isMain() == false) return;

      // Attribute this completion by what actually loaded, not by which load
      // path ran last: two navigations can be in flight at once (a direct-
      // content load superseded by a viewer-page load carrying a PDF open's
      // dispatch, or one direct-content load superseded by another), and
      // Chromium can commit the superseded one first, or finish it late.
      // Trusting the load-path flags here executed the open's dispatch in the
      // dying direct-content page, and the open then wedged the coordinator
      // forever (observed under rapid mixed-type selection). The event's own
      // URL identifies the document this finish belongs to; reading the
      // browser's current URL instead would race a newer commit.

      String url = event.url();
      boolean isViewerPage = url.regionMatches(true, 0, ResourceServer.viewerUrl(), 0, ResourceServer.viewerUrl().length());

      ready = true;

      pdfjsViewerLoaded = isViewerPage;

      Runnable toRun = null;
      boolean viewerLoadStillInFlight, hadPostLoadCode;

      synchronized (loadLock)
      {
        // The post-load work belongs to the viewer-page load; a different
        // navigation finishing must leave it (and the in-flight marker) for
        // the viewer load still on its way.

        hadPostLoadCode = postBrowserLoadCode != null;

        if (isViewerPage)
        {
          viewerHtmlLoadInFlight = false;
          toRun = postBrowserLoadCode;
          postBrowserLoadCode = null;
        }

        viewerLoadStillInFlight = viewerHtmlLoadInFlight;
      }

      if (app.debugging)
        System.out.println("PDFJSWrapper: main frame load finished; isViewerPage=" + isViewerPage +
                           " hadPostLoadCode=" + hadPostLoadCode +
                           " url=" + (url.length() <= 100 ? url : (url.substring(0, 100) + "...")));

      // A direct-content navigation finishing IS the load confirmation for
      // that content kind (there is no openDone; the document is the content),
      // but only the finish of the load most recently issued, matched by URL.
      // A superseded direct load can finish after its successor was issued
      // (Chromium does not cancel the in-flight one), and reporting that late
      // finish here confirmed the successor's load while the superseded
      // document was still on screen: the FTS hits were injected into the
      // wrong DOM (0 matches found), and when the intended document finished,
      // the reconciler believed its hits were already applied. A stale finish
      // is dropped; the intended load's own finish arrives later and confirms.

      if ((isViewerPage == false) && contentToShowIsDirect && isExpectedDirectUrl(url) && (doneHndlr != null))
        doneHndlr.handle(PDFJSOperation.pjsDirectLoad, lastDirectFilePath, true, "");

      // A finished navigation that neither carries the in-flight open's
      // dispatch nor precedes a viewer load that will (reset's bare viewer
      // reload, an external navigation) has replaced the page that open lived
      // in, so its openDone can never arrive. Apply the supersession rule
      // loadFile applies explicitly: release the coordinator, so the newest
      // waiting open issues instead of every later open wedging behind a
      // release that never comes.

      if (openInFlight && (toRun == null) && (viewerLoadStillInFlight == false))
      {
        FilePath releasedFile = openInFlightFile;

        System.out.println("PDFJSWrapper: navigation superseded the in-flight open of " + releasedFile + "; releasing the coordinator");

        Platform.runLater(() ->
        {
          openInFlight = false;
          pumpOpenQueue();

          // If the pump issued a waiting request, the newest open is now under way and
          // recovery is unnecessary (latest wins). With nothing waiting, the released
          // open would otherwise vanish silently: JxBrowser can deliver a duplicate
          // main-frame load-finished event for the same viewer navigation (observed),
          // which lands in this branch after the first finish consumed the open's
          // dispatch; the reconciler still believes the document is issued, so
          // nothing re-issues and the viewer sits empty until a manual refresh.
          // Report the released open as a failed open through the normal completion
          // channel instead: the pane's identity and generation gates drop the report
          // if intent has moved on, and otherwise its bounded retry re-issues the
          // document from intent (by then the viewer page is loaded, so the re-issued
          // open dispatches directly without another navigation).

          if ((openInFlight == false) && (doneHndlr != null))
            doneHndlr.handle(PDFJSOperation.pjsOpen, releasedFile, false, "The open was superseded by another navigation");
        });
      }

      if (toRun != null)
        toRun.run();
    });

    browserView = BrowserView.newInstance(browser);

    setAnchors(browserView, 0.0, 0.0, 0.0, 0.0);

    addToParent(browserView, apBrowser);

    if (showingAlt)
    {
      browserView.setVisible(false);         // the alt display is up; the new view starts hidden

      addToParent(gpAltDisplay, apBrowser);  // no-op if attached; covers a reload while hidden
      gpAltDisplay.toFront();                // the overlay must stay on top of the just-appended view
    }

    apBrowser.setOnMouseEntered(event ->
    {
      if (showingAlt == false)
        safeFocus(browserView);
    });

    Runnable runnable = () ->
    {
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

  /** Whether the given URL is content this application serves to the preview pane
   *  (as opposed to an external URL, which must open in the system browser). data:
   *  covers the data URLs {@link #loadFile} mints for sanitized HTML. */
  private static boolean isInternalUrl(String url)
  {
    if (url == null) return true;

    String urlLower = url.toLowerCase();

    return urlLower.startsWith(ResourceServer.SCHEME_NAME + ':')
      ||   urlLower.startsWith("file:")
      ||   urlLower.startsWith("about:")
      ||   urlLower.startsWith("data:")
      ||   urlLower.startsWith("chrome");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Runs a script in the browser's main frame, asynchronously; safe to call
   * from any thread, including the FX thread. The empty result callback is
   * what selects JxBrowser's asynchronous overload (the no-callback overload
   * blocks on a full IPC round-trip into the renderer, which the FX thread
   * must never wait on).
   *
   * <p>Discarding the result is by design, not neglect: this class talks to
   * the viewer over two one-way channels. Commands go down through here as
   * fire-and-forget strings; results and events come back through the injected
   * {@code window.javaApp} bridge ({@link JavascriptToJava}), fired by the
   * page when the outcome actually exists. A script statement's own completion
   * value could not serve that purpose anyway: the interesting outcomes
   * (a document open, a close) complete asynchronously in pdf.js, long after
   * the statement evaluates to {@code undefined}. Script errors are not lost
   * either; they surface through the {@code ConsoleMessageReceived} handler.
   */
  private void execJS(String script)
  {
    Browser curBrowser = browser;

    if ((curBrowser == null) || curBrowser.isClosed())
    {
      if (app.debugging)
        System.out.println("PDFJSWrapper.execJS dropped (browser closed): " + scriptHead(script));

      return;
    }

    curBrowser.mainFrame().ifPresentOrElse(
      frame -> frame.executeJavaScript(script, result -> {}),
      ()    -> System.out.println("PDFJSWrapper.execJS dropped (no main frame): " + scriptHead(script)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static String scriptHead(String script)
  {
    return (script.length() <= 60) ? script : (script.substring(0, 60) + "...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void cleanupPdfHtml()
  {
    if (pdfjsViewerLoaded)
      execJS("if (typeof PDFViewerApplication !== 'undefined') PDFViewerApplication.close();");

    pdfjsViewerLoaded = false;
    opened = false;  // the page's document goes with it; a stale true here would let setAllHits, scrollToHighlight, and zoom address a document that is gone
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadViewerHtml(Runnable stuffToDoAfterLoading)
  {
    switchToPreviewDisplay();

    synchronized (loadLock)
    {
      postBrowserLoadCode = stuffToDoAfterLoading;

      if (viewerHtmlLoadInFlight)
      {
        // A viewer-page load is already under way (e.g. the constructor's, when
        // loadPdf arrives during window construction). Navigating again would wipe
        // whatever the in-flight load's completion is about to do (observed: the
        // second load blanking a just-opened PDF), so just replace the post-load
        // work and let the in-flight load deliver it.

        if (app.debugging)
          System.out.println("PDFJSWrapper.loadViewerHtml: joining in-flight viewer load");

        return;
      }

      viewerHtmlLoadInFlight = true;
    }

    cleanupPdfHtml();

    browser.navigation().loadUrl(ResourceServer.viewerUrl());
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

  /**
   * Bridge object exposed to viewer-page JavaScript as {@code window.javaApp}.
   * JS numbers arrive as double per the JxBrowser type mapping; structured data
   * arrives as JSON strings (walking live JS objects from Java is avoided).
   */
  @JsAccessible
  public class JavascriptToJava
  {
    public void pageChange(double newPage)
    {
      if (pageChangeHndlr != null)
        pageChangeHndlr.accept((int) newPage);
    }

//---------------------------------------------------------------------------

    public void sidebarChange(double view)
    {
      app.prefs.putInt(PrefKey.PDFJS_SIDEBAR_VIEW, (int) view);
    }

//---------------------------------------------------------------------------

    /**
     * Receives page labels and annotation pages after a document opens.
     * @param json {@code {"annotPages":["3","7",...], "pageLabels":["i","ii","1",...] or null}}
     *             (annotation pages as strings for uniform array handling)
     */
    public void setData(String json)
    {
      if (retrievedDataHndlr == null) return;

      List<Integer> hilitePages = new ArrayList<>();

      Map<String, Integer> labelToPage = new HashMap<>();
      Map<Integer, String> pageToLabel = new HashMap<>();

      try
      {
        JsonObj obj = JsonObj.parseJsonObj(json);

        JsonArray annotPages = obj.getArraySafe("annotPages");

        for (int ndx = 0; ndx < annotPages.size(); ndx++)
        {
          int pageNum = Integer.parseInt(annotPages.getStr(ndx));

          if (hilitePages.contains(pageNum) == false)
            addToSortedList(hilitePages, pageNum);
        }

        JsonArray pageLabels = obj.getArray("pageLabels");

        if (pageLabels != null)
        {
          for (int page = 1; page <= pageLabels.size(); page++)
          {
            String label = pageLabels.getStr(page - 1);
            labelToPage.put(label, page);
            pageToLabel.put(page, label);
          }
        }
      }
      catch (ParseException | NumberFormatException e)
      {
        System.out.println("PDFJSWrapper.setData: malformed data from viewer: " + getThrowableMessage(e));
        return;
      }

      retrievedDataHndlr.handle(labelToPage, pageToLabel, hilitePages);
    }

//---------------------------------------------------------------------------

    public void openDone(boolean success, double pagesCount, String errMessage)
    {
      ready = true;

      if (app.debugging)
        System.out.println("PDFJSWrapper.openDone: success=" + success);

      if (success)
      {
        numPages = (int) pagesCount;
        execJS("getPdfData();");
        opened = true;
      }
      else
      {
        System.out.println("PDFJSWrapper: open failed: " + errMessage);
      }

      // The file identifies which open this was: a newer request may already be
      // waiting (latest-wins coalescing), in which case this event describes a
      // superseded document and consumers must not treat it as confirming the
      // newest one.

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsOpen, openInFlightFile, success, errMessage);

      // This open is finished (success or failure); release the coordinator and
      // issue the latest request that arrived while it was loading, if any. The
      // coordination state is FX-confined; this callback arrives on a JxBrowser
      // thread.

      Platform.runLater(() ->
      {
        openInFlight = false;
        pumpOpenQueue();

        // Drain a buffered page correction (whether queued during the swap or in
        // the gap between openDone and this release); if the pump started a
        // newer open, openInFlight is true again and the newest open's release
        // drains instead. In the PDF->PDF case there is no browser navigation,
        // so no load-finished event fires; this is the point where the new PDF
        // is known ready for the viewer's JS to be called.

        if ((openInFlight == false) && ready && opened && (pendingGoToPage > 0))
        {
          goToPage(pendingGoToPage);
          pendingGoToPage = -1;
        }
      });
    }

//---------------------------------------------------------------------------

    public void closeDone(boolean success, String errMessage)
    {
      ready = true;

      if (success)
      {
        numPages = -1;
        opened = false;
      }
      else
      {
        System.out.println("PDFJSWrapper: close failed: " + errMessage);
      }

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsClose, null, success, "");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void close()
  {
    if (opened == false)
    {
      if (doneHndlr != null) doneHndlr.handle(PDFJSOperation.pjsClose, null, false, "Unable to close because the viewer is already closed.");
      return;
    }

    execJS("closePdfFile();");

    for (int ndx = 0; (ndx < 5) && opened; ndx++)
      sleepForMillis(100);

    if (opened)
      errorPopup("An error occurred while closing the PDF file preview.");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Remove the browser view from the scene graph without closing the browser.
   * Called on the FX thread during application shutdown, before the hosting stage
   * closes: JxBrowser's SceneTracker reacts to a closing window that still contains
   * a BrowserView with Platform.runLater callbacks that otherwise run after the
   * native window peer is destroyed ("Failed to get native widget ID"). The
   * browser itself is closed later by the PreviewWindow.cleanup() dispose chain.
   */
  void detachBrowserView()
  {
    if (browserView != null)
      removeFromParent(browserView);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void loadFile(FilePath filePath, boolean isHtml) throws IOException
  {
    switchToPreviewDisplay();

    // The navigation below replaces the whole document (viewer.html and the PDF
    // open in it included), so there is no need to close the pdf.js app first.
    // Doing so would empty the viewer for a frame before the new content loads,
    // a visible flash on a pdf.js-to-direct-content switch. Just drop the flag.

    pdfjsViewerLoaded = false;
    opened = false;  // the open document (if any) goes with the page; see cleanupPdfHtml

    ready = false;
    pendingGoToPage = -1;

    // Navigating away destroys the page any in-flight PDF open lives in (its
    // openDone will never arrive), and this direct content supersedes any PDF
    // open still waiting its turn; clear the open coordination state so the
    // coordinator is not wedged and no stale open issues after the navigation.

    runInFXThread(() ->
    {
      pendingOpenFile = null;
      openInFlight = false;
      issueSeq++;  // a deferred open dispatch waiting on pulses must not fire into this navigation
    });

    // This navigation also supersedes any viewer-page load still in flight,
    // along with whatever work was chained onto it: that load either aborts or
    // its page is immediately replaced, so the chained work must not run, and
    // a later viewer load must not "join" a navigation that no longer exists.

    synchronized (loadLock)
    {
      viewerHtmlLoadInFlight = false;
      postBrowserLoadCode = null;
    }

    lastDirectFilePath = filePath;

    String url;

    if (isHtml)
    {
      // Jsoup parses with charset auto-detection (BOM, then the document's own charset declaration,
      // defaulting to UTF-8); better than decoding with the JVM default charset.

      Document doc = Jsoup.parse(filePath.toFile());

      doc.getElementsByTag("script").forEach(Element::remove);

      // Script preload/prefetch hints would make Chromium fetch (and CORS-reject, from
      // the data URL's opaque origin) the scripts the line above just stripped; removing them
      // silences the resulting console-error spam and the pointless network chatter.
      // Iframes go too: an embedded external frame (ads, videos) would otherwise trip
      // the external-navigation policy and open the system browser unprompted.

      doc.select("link[rel=modulepreload], link[rel=preload], link[rel=prefetch], iframe").forEach(Element::remove);

      // The data URL minted below carries the document as UTF-8 and says so in
      // its media type. Serialize as UTF-8 and drop the document's now-stale
      // charset declaration so it can't tell Chromium to re-decode the UTF-8
      // byte stream as windows-1252.

      doc.outputSettings().charset(StandardCharsets.UTF_8);

      doc.getElementsByTag("meta").forEach(meta ->
      {
        if (meta.hasAttr("charset") || "Content-Type".equalsIgnoreCase(meta.attr("http-equiv")))
          meta.remove();
      });

      // Mint the data URL here rather than through loadHtml (which mints an
      // equivalent one internally) so the exact committed URL is known and the
      // load-finished handler can attribute completions to this load; see
      // isExpectedDirectUrl.

      url = "data:text/html;charset=utf-8;base64,"
        + Base64.getEncoder().encodeToString(doc.html().getBytes(StandardCharsets.UTF_8));
    }
    else
    {
      url = filePath.toURLString();
    }

    expectedDirectUrl = url;

    browser.navigation().loadUrl(url);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether a finished main-frame URL is the one the most recent direct-content
   * load was issued under. Exact comparison suffices for the {@code data:} URLs
   * {@link #loadFile} mints for HTML; file URLs additionally compare as paths,
   * since Chromium's canonicalized commit can differ from the Java-built form
   * in percent-encoding.
   */
  private boolean isExpectedDirectUrl(String url)
  {
    String expected = expectedDirectUrl;

    if (expected == null) return false;
    if (expected.equals(url)) return true;

    if (expected.startsWith("file:") && url.startsWith("file:"))
    {
      // Raw Path rather than FilePath on purpose: FilePath equality resolves
      // real paths on disk, and this runs per load-finished event on a browser
      // thread; the comparison here is purely syntactic.

      try { return Paths.get(URI.create(expected)).equals(Paths.get(URI.create(url))); }
      catch (RuntimeException e) { return false; }
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Loads a file as direct browser content if its kind can be shown that way:
   * HTML is sanitized (scripts and frames stripped); text, images, media,
   * XML/JSON, and other ASCII files load as-is.
   *
   * @return {@code false} if the file kind cannot be shown as direct content
   *         (nothing was loaded); the caller decides how to surface that
   */
  boolean loadDirectContent(FilePath displayPath) throws IOException
  {
    String mimetypeStr = getMediaType(displayPath).toString();

    if (mimetypeStr.contains("html"))
    {
      setContentToShowIsDirect(true);
      loadFile(displayPath, true);
      return true;
    }

    if (mimetypeStr.contains("image") || mimetypeStr.contains("plain") || mimetypeStr.contains("video") || mimetypeStr.contains("audio") ||
        "application/xml".equalsIgnoreCase(mimetypeStr)  ||
        "application/json".equalsIgnoreCase(mimetypeStr) ||
        isAsciiFile(displayPath))
    {
      setContentToShowIsDirect(true);
      loadFile(displayPath, false);
      return true;
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static final int SidebarView_NONE = 0,
                   SidebarView_THUMBS = 1,
                   SidebarView_OUTLINE = 2,
                   SidebarView_ATTACHMENTS = 3;

  void loadPdf(FilePath file, int initialPage)
  {
    // Reset ready synchronously so a cross-thread goToPage call queued before
    // the open actually issues sees a not-ready state and buffers instead of
    // dispatching to the previous page's JS. Mirrors what loadFile does at the
    // start of its body. A buffered page correction from the previous document
    // dies with it: this load's initialPage supersedes.

    ready = false;
    pendingGoToPage = -1;

    switchToPreviewDisplay();

    runInFXThread(() ->
    {
      pendingOpenFile = file;  // Latest wins; a request superseded before it issues is never opened
      pendingOpenPage = initialPage;

      pumpOpenQueue();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Issues the waiting open request, if there is one and no open is already in
   * flight; otherwise does nothing (the in-flight open's {@code openDone} pumps
   * again). FX thread only.
   */
  private void pumpOpenQueue()
  {
    if (openInFlight)
    {
      // The waiting request is issued when the in-flight open's openDone
      // releases the coordinator; if that never happens, every later open
      // parks here and the viewer sits empty, so make the wait visible.

      if (app.debugging && (pendingOpenFile != null))
        System.out.println("PDFJSWrapper.pumpOpenQueue: waiting on in-flight open of " + openInFlightFile + "; queued " + pendingOpenFile.getNameOnly());

      return;
    }

    if (pendingOpenFile == null) return;

    FilePath file = pendingOpenFile;
    int initialPage = pendingOpenPage;

    pendingOpenFile = null;
    openInFlight = true;
    openInFlightFile = file;

    issueOpen(file, initialPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Dispatches an {@code openPdfFile} call for the given file, loading the
   *  viewer page first if necessary. FX thread only; callers go through
   *  {@link #pumpOpenQueue()} so opens never overlap. */
  private void issueOpen(FilePath file, int initialPage)
  {
    final long mySeq = ++issueSeq;

    String fileUrl = ResourceServer.urlForFile(file);

    Runnable runnable = () ->
    {
      opened = false;

      // javaapp.js's openPdfFile retries internally until PDFViewerApplication finishes
      // initializing, so no Java-side polling is needed once the viewer page's scripts
      // have parsed. The typeof guard covers the residual case where this executes
      // before javaapp.js has parsed: the arguments are buffered and javaapp.js opens
      // the file as soon as it loads.

      String args = '"' + fileUrl + "\", " + initialPage + ", " + app.prefs.getInt(PrefKey.PDFJS_SIDEBAR_VIEW, SidebarView_NONE);

      execJS("if (typeof openPdfFile === 'function') openPdfFile(" + args + "); else window.__hnPendingOpen = [" + args + "];");
    };

    if (pdfjsViewerLoaded == false)
    {
      if (app.debugging)
        System.out.println("PDFJSWrapper.loadPdf: viewer not loaded; loading viewer first");

      loadViewerHtml(runnable);
      return;
    }

    boolean chained;

    synchronized (loadLock)
    {
      chained = viewerHtmlLoadInFlight;

      if (chained)
        postBrowserLoadCode = runnable;  // The viewer page is still loading (e.g. right after construction); run this when it finishes
    }

    if (app.debugging)
      System.out.println("PDFJSWrapper.loadPdf: " + (chained ? "chained onto in-flight viewer load" : "executing directly"));

    if (chained == false)
    {
      // Deferred by a couple of render pulses (not dispatched immediately): the
      // surface un-hide queued by loadPdf's switchToPreviewDisplay then gets
      // presented frames before the navigation reaches the surface; see
      // OPEN_DEFER_PULSES. The guard drops the dispatch if something navigated
      // away (loadFile, browser reload) while it waited.

      runInFXThreadAfterPulses(OPEN_DEFER_PULSES, () ->
      {
        if ((mySeq != issueSeq) || (openInFlight == false))
        {
          if (app.debugging)
            System.out.println("PDFJSWrapper.issueOpen: deferred dispatch dropped (superseded=" + (mySeq != issueSeq) + " openInFlight=" + openInFlight + ')');

          return;
        }

        runnable.run();
      });
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void goToPage(int pageNum)
  {
    if (ready == false)
    {
      // An open is under way (or the viewer is mid-load): a page correction
      // issued now targets the document being opened, e.g. steering to the
      // first-match page when search hits arrive while the document is still
      // loading. Dropping it would leave the document on its initial page, so
      // buffer it; the open coordinator's release drains it.

      pendingGoToPage = pageNum;
      return;
    }

    execJS("PDFViewerApplication.pdfViewer.currentPageNumber = " + pageNum + ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Send all hit data for the current file. The viewer stores it and applies
   * highlights lazily as each page's text layer finishes rendering (the JS
   * injection is render-idempotent, so hits arriving after pages have rendered
   * still highlight). Callers push hits only after the load of the intended
   * document has been confirmed (the reconciler's contract), so no Java-side
   * buffering is needed; a call against an unready viewer is a caller bug and
   * is dropped with a log rather than misapplied.
   *
   * @param allHitsJson JSON object mapping 1-based page numbers to arrays of
   *                    [startOffset, endOffset] pairs (page-relative offsets).
   *                    Example: {"1":[[10,20],[50,60]],"3":[[5,15]]}
   */
  void setAllHits(String allHitsJson)
  {
    if (contentToShowIsDirect)
    {
      if (ready == false)
      {
        System.out.println("PDFJSWrapper.setAllHits: dropped (direct content not loaded)");
        return;
      }

      applyDirectContentHits(allHitsJson);
      return;
    }

    if ((ready == false) || (opened == false))
    {
      System.out.println("PDFJSWrapper.setAllHits: dropped (ready=" + ready + " opened=" + opened + ')');
      return;
    }

    if (app.debugging)
      System.out.println("PDFJSWrapper.setAllHits: sending " + allHitsJson.length() + " chars");

    execJS("setAllHits('" + allHitsJson.replace("'", "\\'") + "');");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scroll to the highlight for a passage. Direct content is addressed by the
   * global match index (highlight spans carry data-match-ndx attributes, applied
   * in matches-list order by directContentHighlight.js); the PDF viewer is
   * addressed by page number plus index within that page. The reconciler
   * delivers a scroll only after the document's load is confirmed and its hits
   * have been issued, so like {@link #setAllHits} this applies directly.
   */
  void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage)
  {
    if (ready == false) return;

    if (contentToShowIsDirect)
    {
      execJS
      (
        "(function() {" +
        "  var el = document.querySelector('.fts-highlight[data-match-ndx=\"" + matchNdx + "\"]');" +
        "  if (el) el.scrollIntoView({ behavior: 'smooth', block: 'center' });" +
        "})();"
      );

      return;
    }

    if (pageNum >= 1)
      execJS("scrollToMatchOnPage(" + pageNum + ", " + ndxOnPage + ");");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearAllHits()
  {
    if (ready == false) return;

    if (contentToShowIsDirect)
    {
      execJS(
        "var hl = document.querySelectorAll('.fts-highlight');" +
        "for (var i = 0; i < hl.length; i++) {" +
        "  var parent = hl[i].parentNode;" +
        "  parent.replaceChild(document.createTextNode(hl[i].textContent), hl[i]);" +
        "  parent.normalize();" +
        '}');

      return;
    }

    execJS("if (typeof clearAllHits === 'function') clearAllHits();");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Inject JavaScript to highlight text at the stored character offsets in
   * directly-loaded content (HTML, text, XML, etc.). Walks DOM text nodes,
   * maps character offsets, and wraps matching ranges in highlight spans.
   */
  private void applyDirectContentHits(String json)
  {
    if (directContentHighlightJS == null)
    {
      try { initDirectContentHighlightJS(); }
      catch (IOException e)
      {
        System.out.println("PDFJSWrapper.applyDirectContentHits: failed to load JS resource: " + getThrowableMessage(e));
        return;
      }
    }

    // The JS resource is a function expression "function (data) { ... }" that
    // we wrap in parens and immediately invoke with the parsed JSON data.
    // The JSON format is: {"matches":[{"ctx":"...context...","s":20,"e":27},...]}
    // Each entry has context text from stored content, plus the start/end offsets
    // of the matched word within the context. The JS searches for the context in
    // the rendered DOM and wraps the match portion in a highlight span.

    execJS('(' + directContentHighlightJS + ")(" + json + ");");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Closes this wrapper's browser. Safe to call from the FX thread: the blocking
   *  close is dispatched to a background thread. */
  public void cleanup()
  {
    runOutsideFXThread(() -> cleanup(null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ConversionSession leasedArtifactSession = null;

  /**
   * Records that this viewer is displaying the given session's artifact,
   * leasing it against cache eviction and releasing the lease on whatever
   * artifact it displayed before. Called on the FX thread (display callbacks);
   * used by dialog-hosted previews, which have no PreviewWrapper.
   */
  void leaseArtifact(ConversionSession session)
  {
    if (leasedArtifactSession == session) return;

    if (leasedArtifactSession != null)
      leasedArtifactSession.release();

    leasedArtifactSession = session;
    session.lease();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Closes this wrapper's browser and then runs the handler. Blocking; must be
   *  called OFF the FX thread (the shutdown dispose chain runs on a background
   *  thread; see {@link PreviewWindow#cleanup()}). */
  void cleanup(Runnable disposeHndlr)
  {
    if (leasedArtifactSession != null)
    {
      leasedArtifactSession.release();
      leasedArtifactSession = null;
    }

    // No cleanupPdfHtml() here: the browser close below tears down the pdf.js
    // app regardless, and firing PDFViewerApplication.close() (which returns a
    // Promise) immediately before that close races JxBrowser's RPC thread as it
    // marshals the Promise result against the by-then-destroyed page context.

    Browser toClose = browser;
    browser = null;

    if (toClose != null)
    {
      try
      {
        if (toClose.isClosed() == false)
          toClose.close();
      }
      catch (RuntimeException e)
      {
        System.out.println("PDFJSWrapper: error closing browser: " + getThrowableMessage(e));
      }
    }

    if (disposeHndlr != null)
      disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
