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
import java.util.concurrent.*;
import java.util.function.BiConsumer;

import com.teamdev.jxbrowser.browser.Browser;
import com.teamdev.jxbrowser.browser.callback.*;
import com.teamdev.jxbrowser.browser.callback.input.MoveMouseWheelCallback;
import com.teamdev.jxbrowser.browser.event.ConsoleMessageReceived;
import com.teamdev.jxbrowser.js.JsAccessible;
import com.teamdev.jxbrowser.js.JsObject;
import com.teamdev.jxbrowser.navigation.callback.StartNavigationCallback;
import com.teamdev.jxbrowser.navigation.event.*;
import com.teamdev.jxbrowser.ui.event.MouseWheel;
import com.teamdev.jxbrowser.view.javafx.BrowserView;

import static org.hypernomicon.App.*;
import static org.hypernomicon.Const.*;
import static org.hypernomicon.model.HyperDB.*;
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.util.Util;
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

  enum PDFJSOperation { pjsOpen, pjsDirectLoad }

//---------------------------------------------------------------------------

  /** Receives the terminal report of each document load. A document close
   *  reports through the future {@link #close()} returns instead. */
  @FunctionalInterface interface PDFJSDoneHandler
  {
    /**
     * @param operation which viewer operation completed: a pdf.js document open
     *                  or a direct-content navigation finishing
     * @param file      the file the operation was for (open: the file whose open
     *                  completed, which may already be superseded by a newer
     *                  request; direct load: the navigated file). Consumers
     *                  confirming loads must match this against what they
     *                  issued rather than trusting arrival order.
     */
    void handle(PDFJSOperation operation, FilePath file, boolean success, String errMessage);
  }

//---------------------------------------------------------------------------

  /** Receives the page-label maps after a document opens. {@code file} is the
   *  document the labels belong to; the consumer must match it against what it
   *  issued, as with every other viewer report. Annotated pages are not part of
   *  this channel: they are scanned Java-side straight from the file
   *  ({@link PDFAnnotationScanner}), not collected through the viewer. */
  @FunctionalInterface interface PDFJSRetrievedDataHandler
  {
    void handle(FilePath file, Map<String, Integer> labelToPage, Map<Integer, String> pageToLabel);
  }

//---------------------------------------------------------------------------

  private final AnchorPane apBrowser;
  private final BiConsumer<FilePath, Integer> pageChangeHndlr;
  /** The bridge injected into the current browser; one per browser, see {@link #reloadBrowser}. */
  private JavascriptToJava javascriptToJava = null;
  private final PDFJSDoneHandler doneHndlr;
  private final PDFJSRetrievedDataHandler retrievedDataHndlr;

  private static String directContentHighlightJS = null;

  /** One daemon thread serving every pane's open watchdog; each check is a
   *  rare, cheap hop to the FX thread (see {@link OpenCoordinator}). */
  private static final ScheduledExecutorService watchdogScheduler = Executors.newSingleThreadScheduledExecutor(runnable ->
  {
    HyperThread hyperThread = new HyperThread("Preview-OpenWatchdog", runnable);
    hyperThread.setDaemon(true);
    return hyperThread;
  });

  private Browser browser = null;
  private BrowserView browserView = null;

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

  private FilePath lastDirectFilePath = null;

  /** The exact URL the most recent direct-content load was issued under (the
   *  self-minted {@code data:} URL for HTML, the file URL otherwise), written
   *  before the navigation is started. The document-load handler confirms a
   *  direct load only when the document that loaded is this one: a superseded
   *  direct load can finish after its successor was issued, and attributing
   *  that late finish to {@link #lastDirectFilePath} confirmed the new
   *  document's load while the old document was still on screen (the FTS hits
   *  then went into the wrong DOM and were never re-applied). Volatile:
   *  written from load paths, read on the browser event thread. */
  private volatile String expectedDirectUrl = null;

  /** The URL of the main frame's most recently committed navigation, recorded
   *  from the navigation event that carries it for the document-load event
   *  that follows and carries none. Browser event thread only. */
  private volatile String committedMainUrl = null;

  private int numPages = -1;
  private boolean ready = false, hiding = false;

  private volatile boolean opened = false;

  /** How long a document close may go without the viewer's report before it
   *  is given up on; see {@link #close()}. */
  private static final long CLOSE_TIMEOUT_MILLIS = 5_000;

  /** The document close awaiting the viewer's report, or null. Issued on the
   *  FX thread, completed on the bridge thread; volatile for that handoff.
   *  Cleared once it settles, whichever way. */
  private volatile CompletableFuture<Boolean> pendingClose = null;

  /** Serializes document opens and joins them onto viewer-page loads; the
   *  {@link OpenAdapter} below supplies its browser-side effects. Its executor
   *  runs immediately when already on the FX thread, so calls from the load
   *  paths (FX) act synchronously and reports from browser threads hop. */
  private final OpenCoordinator opens = new OpenCoordinator(new OpenAdapter(), Util::runInFXThread,
    (task, delayMillis) -> watchdogScheduler.schedule(task, delayMillis, TimeUnit.MILLISECONDS));

  /** Page correction that arrived while an open was in flight (see
   *  {@link #goToPage(int)}); applied by the coordinator's release, cleared by
   *  each new load. Volatile: written from viewer-driving threads, drained on FX. */
  private volatile int pendingGoToPage = -1;

//---------------------------------------------------------------------------

  PDFJSWrapper(AnchorPane apBrowser, PDFJSDoneHandler doneHndlr, BiConsumer<FilePath, Integer> pageChangeHndlr, PDFJSRetrievedDataHandler retrievedDataHndlr)
  {
    this.doneHndlr = doneHndlr;
    this.pageChangeHndlr = pageChangeHndlr;
    this.retrievedDataHndlr = retrievedDataHndlr;
    this.apBrowser = apBrowser;

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
    removeFromParent(browserView);

    hiding = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void prepareToShow()
  {
    if (hiding == false) return;

    addToParent(browserView, apBrowser);

    hiding = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Status display lives INSIDE the viewer page, as a DOM overlay toggled via
  // executeJavaScript (javaapp.js showStatusOverlay): on Windows and Linux the
  // BrowserView is a native hardware surface that ignores JavaFX visibility and
  // z-order until its first real presentation (observed on Linux as a
  // window-scale black rectangle, desynchronized from the node's geometry,
  // while a JavaFX overlay should have covered it), so no JavaFX node can
  // reliably cover the browser, and hiding the surface is what desynchronized
  // it in the first place. The view therefore stays attached and visible at all
  // times, and viewer.html is the pane's status home: when a status must
  // display and the current page is direct content (or nothing yet), the
  // wrapper navigates home first. macOS renders off-screen (see BrowserEngine),
  // where a JavaFX overlay would work, but the page-internal display is kept
  // identical on every platform.

  private void showStatus(StatusKind kind, String message)
  {
    runInFXThread(() ->
    {
      currentStatus = new Status(kind, message);

      opens.statusShown();  // an open already in flight predates this status; its success must not clear it

      if (browser == null) return;  // engine unavailable; the pane shows its static fallback instead

      if (pdfjsViewerLoaded && (opens.viewerLoadInFlight() == false))
      {
        execStatusOverlay(currentStatus);
        return;
      }

      // The current page is not (or is about to stop being) the viewer: make
      // viewer.html the status home. This supersedes any open whose dispatch is
      // chained to a pending viewer load (intent has moved to a status), so
      // release the coordinator the way loadFile's supersession does; the
      // pending viewer load itself survives, and the status work below replaces
      // the open's dispatch on it. A late openDone report for the superseded
      // open fails the pane's identity gate (the non-document views null
      // issuedDisplayPath).

      opens.supersedeOpens(false);

      // The status also supersedes any direct-content declaration: what is
      // about to show is the status home, not direct content. This ordered
      // FX-side write is what lets a superseded direct load's late finish know
      // not to clear this status (see the confirmation branch's re-check).

      contentToShowIsDirect = false;

      Status status = currentStatus;
      opens.loadViewerPage(() -> execStatusOverlay(status));
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

    execJS(jsCallWhenDefined("showStatusOverlay", obj.toString(), "window.__hnPendingStatus = " + obj));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Clears the displayed status. Called only from content-confirmation points
   *  (a successful document open; a finished direct-content load), never from
   *  load initiation, so the overlay stays up until real content is visible. */
  private void clearStatusOverlay()
  {
    currentStatus = null;

    execJS(jsCallWhenDefined("hideStatusOverlay", "", "window.__hnPendingStatus = null"));
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

    opens.dropWaiting();

    // The idle overlay goes up first (navigating home if the current page is
    // direct content), so the document close below happens under it.

    showIdle();

    if (pdfjsViewerLoaded && opened)
    {
      // Nothing waits on the close (the idle overlay is already up, and the next
      // open replaces the document regardless); only its silence is worth noting.

      close().whenComplete((success, e) ->
      {
        if (e != null)
          System.out.println("PDFJSWrapper: no report of the document close within " + (CLOSE_TIMEOUT_MILLIS / 1000) + " seconds; the document may still be open");
      });
    }
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

    opens.supersedeOpens(true);

    if (browser != null)
    {
      removeFromParent(browserView);

      Browser toClose = browser;
      browser = null;

      javascriptToJava.retired = true;  // its reports now belong to a browser this wrapper has moved on from

      settlePendingClose();  // the document goes with the browser; no report is coming

      // close() blocks and can need the FX thread (view detachment), so it must not
      // run on it; reloadBrowser is called from FX-thread refresh flows.

      runOutsideFXThread(() -> BrowserEngine.closeQuietly(toClose, "the replaced preview browser"));
    }

    browser = BrowserEngine.newBrowser();
    if (browser == null)
      return;

    // A fresh bridge per browser (the previous one was retired above). A replaced
    // browser keeps running until its close completes, and anything it reports in
    // that window would otherwise be taken as this browser's: an open completing
    // would confirm the re-issued document early and release the open coordinator
    // under the real open. Document identity cannot catch that, since a refresh
    // re-issues the same document; the retired bridge just drops every later report.

    JavascriptToJava bridge = new JavascriptToJava();
    javascriptToJava = bridge;

    // Inject the bridge before page scripts run, so javaApp already exists when the
    // viewer page's scripts execute.

    browser.set(InjectJsCallback.class, params ->
    {
      JsObject window = params.frame().executeJavaScript("window");

      if (window != null)
        window.putProperty("javaApp", bridge);

      return InjectJsCallback.Response.proceed();
    });

    // Reject all downloads; the preview pane never saves files. A download here means
    // Chromium could not display what the pane just navigated to (e.g. a .mov file):
    // the navigation becomes a download instead of committing, so without intervention
    // the previous content would silently stay up. Cancel it and show the unable
    // display for the file the load path was attempting. Only the direct load most
    // recently issued can fail this way, though (the same URL match as the
    // load-finished handler): a download belonging to anything else, a superseded
    // navigation whose decision arrived late or a download link inside the displayed
    // content, is cancelled without disturbing what is showing.

    browser.set(StartDownloadCallback.class, (params, tell) ->
    {
      tell.cancel();

      String url = params.download().target().url();

      if ((url != null) && isExpectedDirectUrl(url))
        setUnable(lastDirectFilePath);
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

      if (app.debugging)
        System.out.println("PDFJSWrapper: main frame load finished; isViewerPage=" + isViewerPage
          + " url=" + describeUrl(url) + "; pane " + paneStateStr());

      // Direct content is not confirmed here; see the document-load handler below.

      // The coordinator runs the work chained onto the viewer-page load this
      // finish completes, or releases the open whose page this finish replaced.

      opens.navigationFinished(isViewerPage);
    });

    // Direct content is confirmed when its document has loaded (the
    // DOMContentLoaded-level event), not when the page's load event fires: the
    // document is complete at that point, which is all the hit injection needs,
    // and Chromium's media pages hold the load event back until the media has
    // data to render, which with autoplay blocked and only metadata preloaded
    // is never until the user presses play (observed: no load-finished event at
    // all for an mp3 or an mp4). The document-load event carries no URL, so the
    // document is identified by the main frame's most recent commit, taken from
    // the navigation event that precedes it; both arrive in order on the
    // browser event thread.

    browser.navigation().on(NavigationFinished.class, event ->
    {
      if ((event.isInMainFrame() == false) || (event.hasCommitted() == false) || event.isSameDocument()) return;

      String url = event.url();

      if (event.isErrorPage() == false)
      {
        committedMainUrl = url;
        return;
      }

      // Chromium committed its own error page in place of the content (the file
      // unreadable, or gone by the time it was fetched). That page is a document
      // too, and since the finished URL is still the content's, its document-load
      // event would confirm the direct load as a success with a browser error on
      // screen. Withhold it from the document-load handler and fail the load
      // instead: this is its terminal report (see the liveness clause in
      // ViewerPort).

      committedMainUrl = null;

      if ((contentToShowIsDirect == false) || (isExpectedDirectUrl(url) == false))
      {
        if (app.debugging)
          System.out.println("PDFJSWrapper: error page for a navigation other than the expected direct load: " + event.error() + "; url=" + describeUrl(url));

        return;
      }

      System.out.println("PDFJSWrapper: direct content failed to load: " + event.error() + "; url=" + describeUrl(url));

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsDirectLoad, lastDirectFilePath, false, "The browser could not load the file (" + event.error() + ')');
    });

    browser.navigation().on(FrameDocumentLoadFinished.class, event ->
    {
      if (event.frame().isMain() == false) return;

      String url = committedMainUrl;

      if ((url == null) || (contentToShowIsDirect == false)) return;

      // Only the load most recently issued, matched by URL. A superseded direct
      // load can finish after its successor was issued (Chromium does not cancel
      // the in-flight one), and reporting that late finish confirmed the
      // successor's load while the superseded document was still on screen: the
      // FTS hits were injected into the wrong DOM (0 matches found), and when
      // the intended document finished, the reconciler believed its hits were
      // already applied. A stale finish is dropped; the intended load's own
      // finish arrives later and confirms.

      if (isExpectedDirectUrl(url) == false)
      {
        if (app.debugging)
          System.out.println("PDFJSWrapper: stale direct-content finish dropped; finished=" + describeUrl(url)
            + " expected=" + (expectedDirectUrl == null ? "none" : describeUrl(expectedDirectUrl)));

        return;
      }

      ready = true;

      if (app.debugging)
        System.out.println("PDFJSWrapper: direct content loaded; url=" + describeUrl(url));

      // A matching finish is also a status-clearing point: any overlay died
      // with the page this navigation replaced. The clear hops to the FX
      // thread and re-checks the direct declaration there: a status shown
      // meanwhile has declared the content non-direct (see showStatus), and
      // that FX-side write is ordered ahead of this runnable, so it cannot
      // null the very status that superseded it. This also keeps every status
      // write FX-confined.

      Platform.runLater(() ->
      {
        if (contentToShowIsDirect)
          currentStatus = null;
      });

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsDirectLoad, lastDirectFilePath, true, "");
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
      opens.loadViewerPage(runnable);
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
   *
   * @return whether the script was handed to a page. The one caller that
   *         needs to know is the open dispatch: an open no page received
   *         will never be reported on, so it must be failed at once.
   */
  private boolean execJS(String script)
  {
    Browser curBrowser = browser;

    if ((curBrowser == null) || curBrowser.isClosed())
    {
      if (app.debugging)
        System.out.println("PDFJSWrapper.execJS dropped (browser closed): " + scriptHead(script));

      return false;
    }

    var frame = curBrowser.mainFrame();

    if (frame.isEmpty())
    {
      System.out.println("PDFJSWrapper.execJS dropped (no main frame): " + scriptHead(script));
      return false;
    }

    frame.get().executeJavaScript(script, result -> {});
    return true;
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
   * One instance per browser: when the browser it was injected into is
   * replaced or closed, the instance is retired and drops every report that
   * still arrives from that browser (see {@link #reloadBrowser}).
   */
  @JsAccessible
  public class JavascriptToJava
  {
    /** Set when this bridge's browser is replaced or closed; reports arriving
     *  after that come from a browser this wrapper has moved on from. */
    private volatile boolean retired = false;

    /**
     * Receives the viewer's page changes. {@code url} is the viewer's own URL
     * at the moment the event fired, naming the document the page belongs to:
     * the event reaches Java after the fact, by which time a newer open may
     * have been issued, and only the document identity lets a consumer tell a
     * superseded document's late page jump from the current document's. Mapped
     * back to the file the URL was minted for; {@code null} if it is not one of
     * this application's file URLs.
     */
    public void pageChange(double newPage, String url)
    {
      if (retired) return;

      if (pageChangeHndlr != null)
        pageChangeHndlr.accept(ResourceServer.fileForUrl(url), (int) newPage);
    }

//---------------------------------------------------------------------------

    public void sidebarChange(double view)
    {
      if (retired) return;

      app.prefs.putInt(PrefKey.PDFJS_SIDEBAR_VIEW, (int) view);
    }

//---------------------------------------------------------------------------

    /**
     * Receives page labels after a document opens. Like {@link #pageChange},
     * the report names its document: {@code url} is the viewer's URL when the
     * labels were requested, since they resolve asynchronously and the viewer
     * may hold the next document by the time they arrive.
     * @param json {@code {"pageLabels":["i","ii","1",...] or null}}
     * @param url  the document the labels belong to
     */
    public void setData(String json, String url)
    {
      if (retired || (retrievedDataHndlr == null)) return;

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

      retrievedDataHndlr.handle(ResourceServer.fileForUrl(url), labelToPage, pageToLabel);
    }

//---------------------------------------------------------------------------

    /**
     * The viewer's report that an open finished. {@code token} names the open
     * (see {@link OpenCoordinator}): the coordinator drops reports for opens
     * it has already closed out and forwards the rest to
     * {@link OpenAdapter#openReported}, then releases and issues the latest
     * request that arrived while the open was loading, if any.
     */
    public void openDone(boolean success, double pagesCount, String errMessage, double token)
    {
      if (retired) return;

      opens.openFinished((int) token, success, (int) pagesCount, errMessage);
    }

//---------------------------------------------------------------------------

    /** A data-progress callback for the loading document; see {@link OpenCoordinator#progress()}. */
    public void openProgress()
    {
      if (retired) return;

      opens.progress();
    }

//---------------------------------------------------------------------------

    /** The viewer put up its password prompt; see {@link OpenCoordinator#waitingOnUser()}. */
    public void openWaitingOnUser()
    {
      if (retired) return;

      opens.waitingOnUser();
    }

//---------------------------------------------------------------------------

    public void closeDone(boolean success, String errMessage)
    {
      if (retired) return;

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

      CompletableFuture<Boolean> pending = pendingClose;

      if (pending != null)
        pending.complete(success);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Closes the open document. The returned future completes with the viewer's
   * report (true: closed; false: the viewer reported a failure, already
   * logged), or exceptionally with a {@link TimeoutException} if no report
   * arrives within {@link #CLOSE_TIMEOUT_MILLIS}: the same conversion of
   * silence into failure the open coordinator applies to opens. A close
   * already under way is shared, and with no document open the result is an
   * already-completed true. Never blocks: the previous implementation polled
   * {@link #opened} on the FX thread for up to half a second, freezing the UI
   * for every reset that closed a document.
   */
  private CompletableFuture<Boolean> close()
  {
    if (opened == false)
      return CompletableFuture.completedFuture(Boolean.TRUE);

    CompletableFuture<Boolean> pending = pendingClose;

    if (pending != null)
      return pending;

    CompletableFuture<Boolean> issued = new CompletableFuture<Boolean>().orTimeout(CLOSE_TIMEOUT_MILLIS, TimeUnit.MILLISECONDS);

    pendingClose = issued;

    issued.whenComplete((success, e) ->
    {
      if (pendingClose == issued)
        pendingClose = null;
    });

    if (execJS("closePdfFile();") == false)
      issued.complete(Boolean.FALSE);  // no page to close in; execJS logged it

    return issued;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Settles a pending close as done: the page the document lived in is gone,
   *  so the document is closed and no report is coming. */
  private void settlePendingClose()
  {
    CompletableFuture<Boolean> pending = pendingClose;

    if (pending != null)
      pending.complete(Boolean.TRUE);
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
    // For the diagnostic line issued once the URL is known (below): the open this
    // navigation supersedes, read before the coordination state is cleared.

    FilePath supersededOpenFile = opens.inFlightFile();

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
    // open still waiting its turn and any viewer-page load still in flight.

    opens.supersedeOpens(true);

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
      System.out.println("PDFJSWrapper.loadFile: " + (isHtml ? "html" : "direct") + ' ' + filePath.getNameOnly()
        + " url=" + describeUrl(url)
        + "; supersedes in-flight open=" + (supersededOpenFile == null ? "none" : supersededOpenFile));

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
        + "; supersedes in-flight open=" + (opens.isOpenInFlight() ? opens.inFlightFile() : "none")
        + "; lastDirect=" + (lastDirectFilePath == null ? "null" : lastDirectFilePath.getNameOnly()));

    // Reset ready synchronously so a cross-thread goToPage call queued before
    // the open actually issues sees a not-ready state and buffers instead of
    // dispatching to the previous page's JS. Mirrors what loadFile does at the
    // start of its body. A buffered page correction from the previous document
    // dies with it: this load's initialPage supersedes.

    ready = false;
    pendingGoToPage = -1;

    opens.requestOpen(file, initialPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The browser side of {@link OpenCoordinator}: navigating to the viewer
   *  page, dispatching an open against it, and what a settled open means for
   *  this wrapper. */
  private final class OpenAdapter implements OpenCoordinator.Adapter
  {
    @Override public boolean viewerPageLoaded() { return pdfjsViewerLoaded; }

//---------------------------------------------------------------------------

    @Override public void navigateToViewerPage()
    {
      cleanupPdfHtml();

      if (app.debugging)
        System.out.println("PDFJSWrapper: initiating viewer navigation");

      browser.navigation().loadUrl(ResourceServer.viewerUrl());
    }

//---------------------------------------------------------------------------

    @Override public boolean dispatchOpen(FilePath file, int initialPage, int token)
    {
      opened = false;

      // javaapp.js's openPdfFile retries internally until PDFViewerApplication finishes
      // initializing, so no Java-side polling is needed once the viewer page's scripts
      // have parsed. jsCallWhenDefined's guard covers the residual case where this
      // executes before javaapp.js has parsed: the arguments are buffered and
      // javaapp.js opens the file as soon as it loads.

      String args = '"' + ResourceServer.urlForFile(file) + "\", " + initialPage + ", " + app.prefs.getInt(PrefKey.PDFJS_SIDEBAR_VIEW, SidebarView_NONE) + ", " + token;

      return execJS(jsCallWhenDefined("openPdfFile", args, "window.__hnPendingOpen = [" + args + ']'));
    }

//---------------------------------------------------------------------------

    @Override public void openReported(FilePath file, boolean success, int pageCount, String errMessage)
    {
      ready = true;

      if (success)
      {
        numPages = pageCount;
        execJS("getPdfData();");
        opened = true;
      }
      else
      {
        System.out.println("PDFJSWrapper: open failed: " + errMessage);
      }

      // The file identifies which open this was: a newer request may already be
      // waiting (latest-wins coalescing), in which case this report describes a
      // superseded document and consumers must not treat it as confirming the
      // newest one.

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsOpen, file, success, errMessage);
    }

//---------------------------------------------------------------------------

    /** The document is loaded, so the status overlay (conversion progress,
     *  typically) comes down. */
    @Override public void openConfirmed() { clearStatusOverlay(); }

//---------------------------------------------------------------------------

    @Override public void openQueueIdle()
    {
      // Drain a buffered page correction (whether queued during the swap or in
      // the gap between openDone and the coordinator's release). In the
      // PDF-to-PDF case there is no browser navigation, so no load-finished
      // event fires; this is the point where the new PDF is known ready for
      // the viewer's JS to be called.

      if (ready && opened && (pendingGoToPage > 0))
      {
        goToPage(pendingGoToPage);
        pendingGoToPage = -1;
      }
    }

//---------------------------------------------------------------------------

    @Override public void openFailed(FilePath file, String cause)
    {
      // Reported as a failed open through the normal completion channel: the
      // pane's identity and generation gates drop the report if intent has
      // moved on, and otherwise its bounded retry re-issues the document from
      // intent (for a superseded open the viewer page is loaded by then, so the
      // re-issued open dispatches directly without another navigation).

      if (app.debugging)
        System.out.println("PDFJSWrapper: reporting failed open of " + file.getNameOnly() + " (" + cause + "); pane " + paneStateStr());

      if (doneHndlr != null)
        doneHndlr.handle(PDFJSOperation.pjsOpen, file, false, cause);
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

    execJS(jsCallWhenDefined("clearAllHits", "", null));
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

    // The JS resource is a parenthesized function expression "(function (data) { ... })"
    // that we immediately invoke with the parsed JSON data.
    // The JSON format is: {"matches":[{"ctx":"...context...","s":20,"e":27},...]}
    // Each entry has context text from stored content, plus the start/end offsets
    // of the matched word within the context. The JS searches for the context in
    // the rendered DOM and wraps the match portion in a highlight span.

    execJS(directContentHighlightJS + '(' + json + ");");
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

    if (javascriptToJava != null)
      javascriptToJava.retired = true;

    settlePendingClose();

    if (toClose != null)
      BrowserEngine.closeQuietly(toClose, "the preview browser");

    if (disposeHndlr != null)
      disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
