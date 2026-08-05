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
import java.util.concurrent.atomic.AtomicInteger;
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
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.util.json.JsonArray;
import org.hypernomicon.util.json.JsonObj;

import org.json.simple.parser.ParseException;

import org.jsoup.Jsoup;
import org.jsoup.nodes.*;

import javafx.application.Platform;
import javafx.scene.layout.AnchorPane;

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

  /** Receives the page-label maps after a document opens. Annotated pages are
   *  not part of this channel: they are scanned Java-side straight from the
   *  file ({@link PDFAnnotationScanner}), not collected through the viewer. */
  @FunctionalInterface interface PDFJSRetrievedDataHandler
  {
    void handle(Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel);
  }

//---------------------------------------------------------------------------

  private final AnchorPane apBrowser;
  private final Consumer<Integer> pageChangeHndlr;
  private final JavascriptToJava javascriptToJava;
  private final PDFJSDoneHandler doneHndlr;
  private final PDFJSRetrievedDataHandler retrievedDataHndlr;

  private static String directContentHighlightJS = null;

  private Browser browser = null;
  private BrowserView browserView = null;
  private Runnable postBrowserLoadCode = null;

  /** The status kinds the in-viewer overlay can display; see {@link #showStatus}. */
  private enum StatusKind { PROGRESS, NOTICE }

  /** A status the in-viewer overlay displays: conversion progress, a notice
   *  (unable to preview, office installation missing), or the bare idle panel
   *  (NOTICE with an empty message). */
  private record Status(StatusKind kind, String message) { }

  /** What the status overlay is currently showing, or null when no status is up.
   *  Writes are FX-confined; volatile because diagnostics read it from browser
   *  threads. Also the re-issue source after {@link #reloadBrowser}. */
  private volatile Status currentStatus = null;

  /** Guards the {@link #viewerHtmlLoadInFlight}/{@link #postBrowserLoadCode} pair:
   *  chaining work onto an in-flight viewer load (FX thread) and the load-finished
   *  event consuming that work (JxBrowser thread) must be atomic, or a runnable
   *  chained in the gap is never triggered. */
  private final Object loadLock = new Object();

  /**
   * Whether the content slated for the viewer is direct browser content (HTML, plain text, XML,
   * media loaded straight into the browser). <b>Declared</b> by the load path as intent for what we
   * are about to show; not read back from the browser. Also declared false by a status display
   * ({@link #showStatus}), which supersedes any direct declaration. Used to route FTS hits and
   * scroll targets to the direct-content highlighter rather than the pdf.js one.
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

  /** Diagnostic: counts navigations this wrapper has initiated (viewer page loads and
   *  direct-content loads). Echoed by the load-finished and supersession logs, so a
   *  trailing viewer-page finish can be classified as a duplicate finish of the same
   *  navigation (seq unchanged) vs. the finish of a newly initiated one (seq advanced). */
  private final AtomicInteger navSeq = new AtomicInteger(0);

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
  private boolean ready = false, hiding = false;

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

  /** True when the status overlay was shown while an open was already in flight:
   *  that open predates the status, so its completion must not clear the overlay
   *  (observed on Linux: a superseded slow open finishing stripped the progress
   *  overlay for the rest of a conversion). Set by {@link #showStatus}, cleared
   *  when the coordinator issues a new open, which is then newer than any
   *  displayed status. FX-confined. */
  private boolean openSupersededByStatus = false;

  /** Page correction that arrived while an open was in flight (see
   *  {@link #goToPage(int)}); applied by the coordinator's release, cleared by
   *  each new load. Volatile: written from viewer-driving threads, drained on FX. */
  private volatile int pendingGoToPage = -1;

//---------------------------------------------------------------------------

  PDFJSWrapper(AnchorPane apBrowser, PDFJSDoneHandler doneHndlr, Consumer<Integer> pageChangeHndlr, PDFJSRetrievedDataHandler retrievedDataHndlr)
  {
    this.doneHndlr = doneHndlr;
    this.pageChangeHndlr = pageChangeHndlr;
    this.retrievedDataHndlr = retrievedDataHndlr;
    this.apBrowser = apBrowser;

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
      System.out.println("PDFJSWrapper.prepareToHide: status=" + currentStatus);

    removeFromParent(browserView);

    hiding = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToShow()
  {
    if (app.debugging)
      System.out.println("PDFJSWrapper.prepareToShow: hiding=" + hiding + " status=" + currentStatus);

    if (hiding == false) return;

    addToParent(browserView, apBrowser);

    hiding = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Status display lives INSIDE the viewer page, as a DOM overlay toggled via
  // executeJavaScript (javaapp.js showStatusOverlay): the BrowserView is a
  // native hardware surface that ignores JavaFX visibility and z-order until
  // its first real presentation (observed on Linux as a window-scale black
  // rectangle, desynchronized from the node's geometry, while a JavaFX overlay
  // should have covered it), so no JavaFX node can reliably cover the browser,
  // and hiding the surface is what desynchronized it in the first place. The
  // view therefore stays attached and visible at all times, and viewer.html is
  // the pane's status home: when a status must display and the current page is
  // direct content (or nothing yet), the wrapper navigates home first.

  private void showStatus(StatusKind kind, String message)
  {
    runInFXThread(() ->
    {
      currentStatus = new Status(kind, message);

      if (openInFlight)
        openSupersededByStatus = true;  // that open predates this status; its success must not clear it

      if (browser == null) return;  // engine unavailable; the pane shows its static fallback instead

      boolean viewerLoadInFlight;

      synchronized (loadLock) { viewerLoadInFlight = viewerHtmlLoadInFlight; }

      if (pdfjsViewerLoaded && (viewerLoadInFlight == false))
      {
        execStatusOverlay(currentStatus);
        return;
      }

      // The current page is not (or is about to stop being) the viewer: make
      // viewer.html the status home. This supersedes any open whose dispatch is
      // chained to a pending viewer load (intent has moved to a status), so
      // release the coordinator the way loadFile's supersession does; a late
      // openDone report for the superseded open fails the pane's identity gate
      // (the non-document views null issuedDisplayPath).

      pendingOpenFile = null;
      openInFlight = false;

      // The status also supersedes any direct-content declaration: what is
      // about to show is the status home, not direct content. This ordered
      // FX-side write is what lets a superseded direct load's late finish know
      // not to clear this status (see the confirmation branch's re-check).

      contentToShowIsDirect = false;

      Status status = currentStatus;
      loadViewerHtml(() -> execStatusOverlay(status));
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Sends the given status to the overlay. The viewer page buffers a call that
   *  arrives before javaapp.js has parsed and replays it when ready, so this can
   *  ride a viewer load as its post-load work. */
  private void execStatusOverlay(Status status)
  {
    JsonObj obj = new JsonObj();
    obj.put("kind", status.kind() == StatusKind.PROGRESS ? "progress" : "notice");
    obj.put("message", status.message());

    // The overlay text follows the application font-size preference (same pref,
    // default, and at-least-1 guard as UIUtil.setFontSize), plus 2: the status
    // panel is a single short message in a large empty area, and matching the
    // control-font size exactly reads too small there.

    double fontSize = app.prefs.getDouble(PrefKey.FONT_SIZE, DEFAULT_FONT_SIZE);
    if (fontSize >= 1)
      obj.put("fontSize", fontSize + 2);

    execJS("if (typeof showStatusOverlay === 'function') showStatusOverlay(" + obj + "); else window.__hnPendingStatus = " + obj + ';');
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Clears the displayed status. Called only from content-confirmation points
   *  (a successful document open; a finished direct-content load), never from
   *  load initiation, so the overlay stays up until real content is visible. */
  private void clearStatusOverlay()
  {
    currentStatus = null;

    execJS("if (typeof hideStatusOverlay === 'function') hideStatusOverlay(); else window.__hnPendingStatus = null;");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void setGenerating(FilePath filePath)
  {
    // Dialog previews can show a file being imported from outside the database, which
    // does not relativize; those fall back to the full path, left in native form so it
    // can be copied and pasted. Database-relative paths are shown with forward slashes
    // regardless of platform.

    FilePath relPath = db.getRootPath().relativize(filePath);

    String pathStr = relPath != null ? relPath.toString().replace('\\', '/') : filePath.toString();

    showStatus(StatusKind.PROGRESS, "Generating preview for file: " + pathStr);
  }

  void setStartingConverter()
  {
    showStatus(StatusKind.PROGRESS, "Starting office document previewer...");
  }

  public void setUnable(FilePath filePath)
  {
    setUnable(filePath.toString());
  }

  private void setUnable(String pathStr)
  {
    showStatus(StatusKind.NOTICE, "Unable to preview the file: " + pathStr);
  }

  void setNoOfficeInstallation()
  {
    showStatus(StatusKind.NOTICE, "To preview this type of file, enter the installation path for LibreOffice or OpenOffice in the Settings dialog.");
  }

  /** The idle look of a warmed or emptied pane: the bare neutral panel, never
   *  the viewer's own chrome with no document. */
  void showIdle()
  {
    showStatus(StatusKind.NOTICE, "");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
  {
    // Drop any open still waiting its turn; a reset means nothing should load.
    // An in-flight open is left alone: its page survives, so its openDone still
    // arrives and releases the coordinator normally.

    runInFXThread(() -> pendingOpenFile = null);

    // The idle overlay goes up first (navigating home if the current page is
    // direct content), so the document close below happens under it.

    showIdle();

    if (pdfjsViewerLoaded && opened)
      close();
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
    // The browser (and with it any in-flight or waiting PDF open, and any
    // viewer-page load) is being replaced; clear the open coordination and
    // viewer-load state so neither can wedge on completions that never come.

    pendingOpenFile = null;
    openInFlight = false;

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
                           " navSeq=" + navSeq.get() +
                           " url=" + describeUrl(url));

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
      //
      // A matching finish is also a status-clearing point: any overlay died
      // with the page this navigation replaced. The clear hops to the FX
      // thread and re-checks the direct declaration there: a status shown
      // meanwhile has declared the content non-direct (see showStatus), and
      // that FX-side write is ordered ahead of this runnable, so it cannot
      // null the very status that superseded it. This also keeps every status
      // write FX-confined.

      if ((isViewerPage == false) && contentToShowIsDirect)
      {
        if (isExpectedDirectUrl(url))
        {
          Platform.runLater(() ->
          {
            if (contentToShowIsDirect)
              currentStatus = null;
          });

          if (doneHndlr != null)
            doneHndlr.handle(PDFJSOperation.pjsDirectLoad, lastDirectFilePath, true, "");
        }
        else if (app.debugging)
        {
          System.out.println("PDFJSWrapper: stale direct-content finish dropped; finished=" + describeUrl(url)
            + " expected=" + (expectedDirectUrl == null ? "none" : describeUrl(expectedDirectUrl)));
        }
      }

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

        System.out.println("PDFJSWrapper: navigation superseded the in-flight open of " + releasedFile
          + "; releasing the coordinator. Superseding content: " + describeUrl(url)
          + "; navSeq=" + navSeq.get() + "; pane " + paneStateStr());

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
          {
            if (app.debugging)
              System.out.println("PDFJSWrapper: after supersession release, queue empty; reporting failed open of "
                + releasedFile.getNameOnly() + "; pane " + paneStateStr());

            doneHndlr.handle(PDFJSOperation.pjsOpen, releasedFile, false, "The open was superseded by another navigation");
          }
        });
      }

      if (toRun != null)
        toRun.run();
    });

    browserView = BrowserView.newInstance(browser);

    setAnchors(browserView, 0.0, 0.0, 0.0, 0.0);

    addToParent(browserView, apBrowser);

    // No focus grab while a status shows: the viewer under the overlay must not
    // gain the keyboard (the overlay also swallows keys JS-side).

    apBrowser.setOnMouseEntered(event ->
    {
      if (currentStatus == null)
        safeFocus(browserView);
    });

    Runnable runnable = () ->
    {
      // The new browser starts blank; re-show whatever status the old one was
      // displaying (the caller then re-issues the content display).

      Status status = currentStatus;
      if (status != null)
        execStatusOverlay(status);

      if (stuffToDoAfterLoadingViewerHtml != null)
        stuffToDoAfterLoadingViewerHtml.run();
    };

    if (pdfjsViewerLoaded || (currentStatus != null))
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

    navSeq.incrementAndGet();

    if (app.debugging)
      System.out.println("PDFJSWrapper.loadViewerHtml: initiating viewer navigation navSeq=" + navSeq.get());

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
     * Receives page labels after a document opens.
     * @param json {@code {"pageLabels":["i","ii","1",...] or null}}
     */
    public void setData(String json)
    {
      if (retrievedDataHndlr == null) return;

      Map<String, Integer> labelToPage = new HashMap<>();
      Map<Integer, String> pageToLabel = new HashMap<>();

      try
      {
        JsonObj obj = JsonObj.parseJsonObj(json);

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
      catch (ParseException e)
      {
        System.out.println("PDFJSWrapper.setData: malformed data from viewer: " + getThrowableMessage(e));
        return;
      }

      retrievedDataHndlr.handle(labelToPage, pageToLabel);
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
        // A successful open is a content-confirmation point: the document is
        // loaded, so the status overlay (conversion progress, typically) comes
        // down. Only for an open that postdates the status, though: a superseded
        // open completing late must not strip a newer status (observed as the
        // progress overlay vanishing for the rest of a conversion when an older
        // slow open finished). A failed open leaves the overlay up; the
        // reconciler decides what shows next (a retry re-issue, eventually the
        // unable notice).

        if (success && (openSupersededByStatus == false))
          clearStatusOverlay();

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
    navSeq.incrementAndGet();

    if (app.debugging)
      System.out.println("PDFJSWrapper.loadFile: " + (isHtml ? "html" : "direct") + ' ' + filePath.getNameOnly()
        + " navSeq=" + navSeq.get()
        + "; supersedes in-flight open=" + (openInFlight ? openInFlightFile : "none")
        + "; issued via: " + loadCallChain());

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

      doc.getElementsByTag("meta").stream().filter(meta -> meta.hasAttr("charset") || "Content-Type".equalsIgnoreCase(meta.attr("http-equiv")))
                                           .forEach(Node::remove);

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

    if (app.debugging)
      System.out.println("PDFJSWrapper.loadFile: issuing direct navigation navSeq=" + navSeq.get() + " url=" + describeUrl(url));

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
   * Diagnostic rendering of a navigation URL. {@code data:} URLs (minted by
   * {@code loadFile} for direct HTML content) are otherwise opaque and huge, and
   * the plain truncation that used to apply hid which content they carried; here
   * they are summarized by media type, payload byte length, and a stable hash so
   * two log lines referring to the same content can be correlated. Other URLs are
   * shown in full (they are short: {@code hnres://}, {@code file://}). Used only
   * under {@code app.debugging}.
   */
  private static String describeUrl(String url)
  {
    if (url.startsWith("data:") == false)
      return url;

    int commaNdx = url.indexOf(',');

    String header  = commaNdx < 0 ? url : url.substring(0, commaNdx),  // e.g. "data:text/html;charset=utf-8;base64"
           payload = commaNdx < 0 ? "" : url.substring(commaNdx + 1);

    return header + ",<" + payload.length() + " bytes, hash=" + Integer.toHexString(payload.hashCode()) + '>';
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Compact summary of the application frames on the current stack, innermost
   *  first: which preview code path issued a load. Diagnostic only, for tracing
   *  the source of a navigation that superseded another. */
  private static String loadCallChain()
  {
    return appCallChain(PDFJSWrapper.class);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Diagnostic snapshot of what the pane has actually settled on, for
   *  distinguishing a genuinely blank pane from one showing unexpected content. */
  private String paneStateStr()
  {
    return "pdfjsViewerLoaded=" + pdfjsViewerLoaded + " status=" + currentStatus
         + " hiding=" + hiding + " opened=" + opened
         + " browserViewAttached=" + ((browserView != null) && (browserView.getParent() != null))
         + " lastDirect=" + (lastDirectFilePath == null ? "null" : lastDirectFilePath.getNameOnly());
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
    if (app.debugging)
      System.out.println("PDFJSWrapper.loadPdf: paged " + file.getNameOnly() + " page " + initialPage
        + "; supersedes in-flight open=" + (openInFlight ? openInFlightFile : "none")
        + "; lastDirect=" + (lastDirectFilePath == null ? "null" : lastDirectFilePath.getNameOnly())
        + "; issued via: " + loadCallChain());

    // Reset ready synchronously so a cross-thread goToPage call queued before
    // the open actually issues sees a not-ready state and buffers instead of
    // dispatching to the previous page's JS. Mirrors what loadFile does at the
    // start of its body. A buffered page correction from the previous document
    // dies with it: this load's initialPage supersedes.

    ready = false;
    pendingGoToPage = -1;

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
    openSupersededByStatus = false;  // this open is newer than any displayed status

    issueOpen(file, initialPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Dispatches an {@code openPdfFile} call for the given file, loading the
   *  viewer page first if necessary. FX thread only; callers go through
   *  {@link #pumpOpenQueue()} so opens never overlap. */
  private void issueOpen(FilePath file, int initialPage)
  {
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
      runnable.run();
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
