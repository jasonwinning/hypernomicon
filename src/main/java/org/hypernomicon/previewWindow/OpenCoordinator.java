/*
 * Copyright 2026 Jason Winning
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

import static org.hypernomicon.App.*;

import java.util.concurrent.Executor;

import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Serializes pdf.js document opens, joins them onto viewer-page loads, and
 * guarantees each open a terminal report, on behalf of {@link PDFJSWrapper}.
 * <p>
 * At most one {@code openPdfFile} call is in flight at a time. A request made
 * while one is loading replaces any previously waiting request (latest wins,
 * never a queue) and is issued when the in-flight open reports done, success
 * or failure. Concurrent {@code openPdfFile} calls race inside pdf.js
 * (null-document errors, a nondeterministic final document) and under rapid
 * selection can destabilize the engine.
 * <p>
 * An open can only dispatch against the viewer page, so an open issued while
 * the browser shows something else navigates to the viewer page first and
 * chains its dispatch onto that load. A viewer-page load already under way is
 * joined rather than repeated: navigating again would wipe whatever the
 * in-flight load's completion is about to do (observed as a second load
 * blanking a just-opened PDF), so a later request just replaces the chained
 * work and lets the in-flight load deliver it.
 * <p>
 * <b>Liveness.</b> Every issued open reaches exactly one terminal report. The
 * viewer's own report is the normal one; it carries the token the open was
 * issued with, and a report for an open this coordinator has already closed
 * out is dropped (a duplicate terminal report, or one against whatever open
 * replaced it). Silence is converted into failure here: a dispatch the viewer
 * page never received fails at once, an open whose page a navigation replaced
 * fails when that navigation finishes, and an open that stops reporting
 * progress fails after {@link #NO_PROGRESS_TIMEOUT_MILLIS} of silence. The
 * watchdog is progress-based, never a bound on total time: a huge document on
 * a slow machine takes as long as it takes and reports progress all the
 * while, and a viewer waiting on the user (a password prompt) is not stalled.
 * <p>
 * Threading: the open state is confined to {@code fxExecutor} tasks (the FX
 * thread in production, a direct executor in tests); reports that arrive on
 * browser threads hop there. The viewer-load pair is guarded by its own lock
 * instead: chaining work onto an in-flight load (FX thread) and the
 * load-finished event consuming that work (JxBrowser thread) must be atomic,
 * or a runnable chained in the gap is never triggered.
 * <p>
 * Every browser-side effect goes through {@link Adapter} and every delay
 * through {@link Scheduler}, so the rules here run without a browser or a
 * clock; {@code OpenCoordinatorTest} pins them.
 */
final class OpenCoordinator
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The browser-side effects the coordinator drives; {@link PDFJSWrapper} implements it. */
  interface Adapter
  {
    /** Whether the pdf.js viewer page is the browser's current document. */
    boolean viewerPageLoaded();

    /** Starts the navigation to the viewer page. Called once per load; a
     *  request arriving while that load is in flight joins it instead. */
    void navigateToViewerPage();

    /**
     * Runs the {@code openPdfFile} dispatch for the file against the viewer
     * page. Runs on the FX thread when the page is up, otherwise on the
     * load-finished thread of the viewer-page load it was chained onto.
     * @param token identifies this open; the viewer echoes it in its report
     * @return false if the dispatch could not be delivered (no page to run it
     *         in); no report will ever come for it, so the coordinator fails
     *         the open at once
     */
    boolean dispatchOpen(FilePath file, int initialPage, int token);

    /** The viewer's own terminal report for the in-flight open, forwarded on
     *  the thread it arrived on before the coordinator releases; {@code meta}
     *  describes the document on success and is null on failure. Never called
     *  for an open already closed out (see the class comment). */
    void openReported(FilePath file, boolean success, ViewerMeta meta, String errMessage);

    /** A successful open that postdates every displayed status finished: a
     *  content-confirmation point. Not called for a failed open, nor for a
     *  superseded open completing late (see {@link #statusShown()}). */
    void openConfirmed();

    /** The in-flight open finished and no newer request was issued in its
     *  place, so anything buffered for "the open under way" now targets the
     *  document that just finished. */
    void openQueueIdle();

    /** The coordinator's synthesized terminal report for an open the viewer
     *  will never report on: its dispatch was undeliverable, a navigation
     *  replaced its page, or it stopped reporting progress. Only made when no
     *  newer request issued in the open's place (latest wins). */
    void openFailed(FilePath file, String cause);
  }

//---------------------------------------------------------------------------

  /** Runs a task after a delay: the watchdog's one need from the clock,
   *  injected so the timeout rules run in tests against hand-driven time. */
  @FunctionalInterface interface Scheduler
  {
    void schedule(Runnable task, long delayMillis);
  }

//---------------------------------------------------------------------------

  /** How long the in-flight open may go without a progress report before it
   *  is failed as stalled; see the class comment. Measured from the last
   *  report (or from the issue, for an open that never reports), never from
   *  the open's start. */
  static final long NO_PROGRESS_TIMEOUT_MILLIS = 30_000;

  private final Adapter adapter;
  private final Executor fxExecutor;
  private final Scheduler scheduler;

  /** Guards the {@link #viewerLoadInFlight}/{@link #postLoadWork} pair; see the class comment. */
  private final Object loadLock = new Object();

  /** True from the moment a viewer-page navigation starts until the viewer
   *  page's load-finished event; anything wanting to run against the viewer
   *  page in that window must chain onto {@link #postLoadWork} instead of
   *  executing immediately (it would run in the old or half-loaded document).
   *  Cleared by the navigations that supersede a pending viewer load. */
  private boolean viewerLoadInFlight = false;
  private Runnable postLoadWork = null;

  /**
   * Open coordination (writes are FX-confined; volatile because the viewer's
   * report reads {@link #inFlightToken} and {@link #inFlightFile} on its
   * arrival thread, and viewer-driving threads read the others).
   * {@code inFlightFile} is the file of the open in flight, or null when none
   * is; whether an open is in flight is read from it and recorded nowhere
   * else. {@code waitingFile} is the request that issues when the in-flight
   * open finishes, or null when none is waiting. The token is fresh per issued
   * open: the viewer echoes it, and a report whose token is not the in-flight
   * one describes an open already closed out.
   * <p>
   * Code that only needs to know whether an open is in flight asks
   * {@link #isOpenInFlight()}, or {@link #isInFlight(int)} when it holds the
   * open's token. Code that also uses the file, off the FX thread, reads
   * {@code inFlightFile} once into a local and tests the local: asking first
   * and reading afterward would pair an answer with a file from a different
   * moment.
   */
  private volatile FilePath waitingFile = null, inFlightFile = null;
  private volatile int inFlightToken = 0;
  private int waitingPage = 1, nextToken = 0;

  /** True when a status was shown while an open was already in flight: that
   *  open predates the status, so its completion must not clear the overlay
   *  (observed on Linux: a superseded slow open finishing stripped the progress
   *  overlay for the rest of a conversion). Set by {@link #statusShown()},
   *  cleared when a new open issues, which is then newer than any displayed
   *  status. FX-confined. */
  private boolean inFlightPredatesStatus = false;

  /** Watchdog state, FX-confined. A check armed for the in-flight open fires
   *  after the timeout and compares {@link #progressCount} with its value at
   *  arming: unchanged means a whole window of silence. {@link #armCount}
   *  identifies the latest armed check so an older one that fires no-ops;
   *  {@link #waitingOnUser} suspends the watch until progress resumes. */
  private long progressCount = 0;
  private int armCount = 0;
  private boolean waitingOnUser = false;

//---------------------------------------------------------------------------

  /**
   * @param adapter    the browser side to drive
   * @param fxExecutor executor the open state is confined to; the FX thread
   *                   in production (run immediately when already on it), a
   *                   direct executor in tests
   * @param scheduler  runs the watchdog's delayed checks
   */
  OpenCoordinator(Adapter adapter, Executor fxExecutor, Scheduler scheduler)
  {
    this.adapter = adapter;
    this.fxExecutor = fxExecutor;
    this.scheduler = scheduler;
  }

//---------------------------------------------------------------------------

  /** Whether an open is in flight. */
  boolean isOpenInFlight() { return inFlightFile != null; }

  /** Whether the open issued under the token is the one in flight: false once
   *  that open has been closed out, whatever is in flight now. FX thread only,
   *  where the pair it reads cannot change between the reads. */
  private boolean isInFlight(int token) { return isOpenInFlight() && (token == inFlightToken); }

  /** The file of the in-flight open, or null when none is in flight. */
  FilePath inFlightFile() { return inFlightFile; }

  /** Whether a viewer-page load is in flight. */
  boolean viewerLoadInFlight() { synchronized (loadLock) { return viewerLoadInFlight; } }

  /** Records that a status went up: an open already in flight predates it. FX thread only. */
  void statusShown() { if (isOpenInFlight()) inFlightPredatesStatus = true; }

  /** Drops any open still waiting its turn. An in-flight open is left alone:
   *  its page survives, so its completion still arrives and releases the
   *  coordinator normally. */
  void dropWaiting() { fxExecutor.execute(() -> waitingFile = null); }

  /** The viewer is waiting on the user (a password prompt) for the in-flight
   *  open: not a stall. Suspends the watchdog until progress resumes. Any thread. */
  void waitingOnUser() { fxExecutor.execute(() -> { if (isOpenInFlight()) waitingOnUser = true; }); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Requests that the file be opened at the page. Latest wins: a request
   *  superseded before it issues is never opened. Any thread. */
  void requestOpen(FilePath file, int initialPage)
  {
    fxExecutor.execute(() ->
    {
      waitingFile = file;
      waitingPage = initialPage;

      pump();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The viewer reported progress on the loading document: a sign of life for
   * the watchdog. Any thread.
   */
  void progress()
  {
    fxExecutor.execute(() ->
    {
      progressCount++;

      if (waitingOnUser && isOpenInFlight())
      {
        waitingOnUser = false;
        armWatchdog();
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The viewer's report that an open finished (success or failure), from the
   * browser thread the report arrives on. The token says which open: a report
   * for any open but the in-flight one is dropped, because that open has
   * already been closed out (it settled through an earlier report, this
   * coordinator failed it, or a navigation superseded it) and its terminal
   * report, if one was due, went out then. Otherwise the report is forwarded,
   * the coordinator releases, and the latest request that arrived while the
   * open was loading issues, if any.
   */
  void openFinished(int token, boolean success, ViewerMeta meta, String errMessage)
  {
    // Read once each, file first: this runs on the report's arrival thread, and
    // the FX thread can release or replace the in-flight open between the
    // reads. The pump writes the token before the file, so a token that still
    // matches after the file was read means that file is this open's.

    FilePath file = inFlightFile;

    if ((file == null) || (token != inFlightToken))
    {
      debugLog("OpenCoordinator.openFinished: dropped report for closed-out open (token " + token + ", success=" + success + ')');

      return;
    }

    adapter.openReported(file, success, meta, errMessage);

    fxExecutor.execute(() ->
    {
      if (isInFlight(token) == false) return;  // closed out between the report's arrival and this hop

      // A successful open is a content-confirmation point. Only for an open
      // that postdates the displayed status, though: a superseded open
      // completing late must not strip a newer status. A failed open leaves
      // the status up; the reconciler decides what shows next (a retry
      // re-issue, eventually the unable notice).

      if (success && (inFlightPredatesStatus == false))
        adapter.openConfirmed();

      release();
      pump();

      // If the pump issued a waiting request, the newest open is now under way
      // and its own release is the one that matters.

      if (isOpenInFlight() == false)
        adapter.openQueueIdle();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Drops any waiting open and releases any in-flight one: the caller is
   * navigating the browser somewhere that destroys the page the in-flight open
   * lives in (its completion will never arrive) and that supersedes the waiting
   * one, so without this the coordinator wedges, or a stale open issues after
   * the navigation. With {@code alsoViewerLoad}, the navigation also supersedes
   * any viewer-page load still in flight, along with whatever work was chained
   * onto it: that load either aborts or its page is immediately replaced, so
   * the chained work must not run, and a later viewer load must not join a
   * navigation that no longer exists. Without it (the caller is itself about
   * to load the viewer page), the pending load and its join survive and the
   * caller's chained work replaces the superseded open's dispatch.
   */
  void supersedeOpens(boolean alsoViewerLoad)
  {
    fxExecutor.execute(() ->
    {
      waitingFile = null;
      release();
    });

    if (alsoViewerLoad == false) return;

    synchronized (loadLock)
    {
      viewerLoadInFlight = false;
      postLoadWork = null;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Loads the viewer page, then runs the work; joins a viewer-page load
   *  already in flight, replacing its chained work with this. Any thread. */
  void loadViewerPage(Runnable postLoadWork)
  {
    synchronized (loadLock)
    {
      this.postLoadWork = postLoadWork;

      if (viewerLoadInFlight)
      {
        debugLog("OpenCoordinator.loadViewerPage: joining in-flight viewer load");

        return;
      }

      viewerLoadInFlight = true;
    }

    adapter.navigateToViewerPage();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Called when a main-frame navigation has finished, from the browser thread
   * the event arrives on. The finished navigation is attributed by evidence
   * carried in the event (whether it was the viewer page), never by which load
   * path ran last: two navigations can be in flight at once and Chromium can
   * commit the superseded one first, or finish it late.
   * <p>
   * The chained work belongs to the viewer-page load; a different navigation
   * finishing must leave it (and the in-flight marker) for the viewer load
   * still on its way.
   * <p>
   * A finished navigation that neither carries the in-flight open's dispatch
   * nor precedes a viewer load that will (a bare viewer reload, an external
   * navigation) has replaced the page that open lived in, so its completion
   * can never arrive. The rule {@link #supersedeOpens} applies explicitly
   * applies here too: release the coordinator, so the newest waiting open
   * issues instead of every later open wedging behind a release that never
   * comes, and fail the released open if nothing was waiting. JxBrowser can
   * deliver a duplicate main-frame load-finished event for the same viewer
   * navigation (observed), which lands here after the first finish consumed
   * the open's dispatch; without the failure report nothing would re-issue
   * the document.
   */
  void navigationFinished(boolean isViewerPage)
  {
    Runnable toRun = null;
    boolean viewerLoadStillInFlight, hadPostLoadWork;

    synchronized (loadLock)
    {
      hadPostLoadWork = postLoadWork != null;

      if (isViewerPage)
      {
        viewerLoadInFlight = false;
        toRun = postLoadWork;
        postLoadWork = null;
      }

      viewerLoadStillInFlight = viewerLoadInFlight;
    }

    debugLog("OpenCoordinator.navigationFinished: isViewerPage=" + isViewerPage + " hadPostLoadWork=" + hadPostLoadWork);

    // Read once each, token first: this runs on a browser thread, and the FX
    // thread can release or replace the in-flight open between the reads. A
    // token read first can only be stale, and a stale token fails nothing; a
    // token read after the file could belong to a newer open, which would then
    // be failed in the superseded open's place.

    int token = inFlightToken;
    FilePath supersededFile = inFlightFile;

    if ((supersededFile != null) && (toRun == null) && (viewerLoadStillInFlight == false))
    {
      debugLog("OpenCoordinator: navigation superseded the in-flight open of " + supersededFile + "; releasing the coordinator");

      fxExecutor.execute(() ->
      {
        if (isInFlight(token))
          failInFlightOpen("The open was superseded by another navigation");
      });
    }

    if (toRun != null)
      toRun.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Issues the waiting request, if there is one and no open is already in
   * flight; otherwise does nothing (the in-flight open's completion pumps
   * again). FX thread only.
   */
  private void pump()
  {
    if (isOpenInFlight())
    {
      // The waiting request is issued when the in-flight open's completion
      // releases the coordinator; if that never happens, every later open
      // parks here and the viewer sits empty, so make the wait visible.

      if (waitingFile != null)
        debugLog("OpenCoordinator.pump: waiting on in-flight open of " + inFlightFile + "; queued " + waitingFile.getNameOnly());

      return;
    }

    if (waitingFile == null) return;

    FilePath file = waitingFile;
    int initialPage = waitingPage;

    waitingFile = null;

    // The file write goes last: it alone publishes the open to the
    // browser-thread readers, so a reader that sees this file is guaranteed
    // this token or a newer one, never an older one.

    inFlightToken = ++nextToken;
    inFlightFile = file;
    inFlightPredatesStatus = false;  // this open is newer than any displayed status

    armWatchdog();
    issue(file, initialPage, inFlightToken);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Dispatches the open, loading the viewer page first if necessary. FX
   *  thread only; callers go through {@link #pump()} so opens never overlap. */
  private void issue(FilePath file, int initialPage, int token)
  {
    Runnable dispatch = () ->
    {
      if (adapter.dispatchOpen(file, initialPage, token)) return;

      // The viewer page never received the open, so nothing will ever report
      // on it; fail it now rather than leave it to the watchdog.

      fxExecutor.execute(() ->
      {
        if (isInFlight(token))
          failInFlightOpen("The open could not be delivered to the viewer");
      });
    };

    if (adapter.viewerPageLoaded() == false)
    {
      loadViewerPage(dispatch);
      return;
    }

    boolean chained;

    synchronized (loadLock)
    {
      chained = viewerLoadInFlight;

      if (chained)
        postLoadWork = dispatch;  // The viewer page is still loading (e.g. right after construction); run this when it finishes
    }

    if (chained == false)
      dispatch.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Takes the in-flight open out of flight, standing its watchdog down. FX thread only. */
  private void release()
  {
    inFlightFile = null;
    waitingOnUser = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Closes out the in-flight open with a synthesized failure: releases the
   * coordinator, issues the latest waiting request if any, and otherwise
   * reports the failure, so the open cannot vanish silently (the reconciler
   * would still believe the document issued, nothing would re-issue it, and
   * the viewer would sit empty until a manual refresh). FX thread only.
   */
  private void failInFlightOpen(String cause)
  {
    FilePath file = inFlightFile;

    release();
    pump();

    if (isOpenInFlight() == false)
      adapter.openFailed(file, cause);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Arms a liveness check for the in-flight open one timeout from now. FX thread only. */
  private void armWatchdog()
  {
    int token = inFlightToken, arm = ++armCount;
    long progressAtArm = progressCount;

    scheduler.schedule(() -> fxExecutor.execute(() -> checkLiveness(token, arm, progressAtArm)), NO_PROGRESS_TIMEOUT_MILLIS);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void checkLiveness(int token, int arm, long progressAtArm)
  {
    if ((isInFlight(token) == false) || (arm != armCount)) return;  // that open is closed out, or a newer check took over

    if (waitingOnUser) return;  // a wait on the user; the next progress report re-arms

    if (progressCount != progressAtArm)
    {
      armWatchdog();  // alive; watch the next window
      return;
    }

    System.out.println("OpenCoordinator: no progress reported for " + (NO_PROGRESS_TIMEOUT_MILLIS / 1000) + " seconds on the open of " + inFlightFile + "; failing it");

    failInFlightOpen("The viewer stopped reporting progress");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
