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

import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * Serializes pdf.js document opens and joins them onto viewer-page loads, on
 * behalf of {@link PDFJSWrapper}.
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
 * Threading: the open state is confined to {@code fxExecutor} tasks (the FX
 * thread in production, a direct executor in tests); reports that arrive on
 * browser threads hop there. The viewer-load pair is guarded by its own lock
 * instead: chaining work onto an in-flight load (FX thread) and the
 * load-finished event consuming that work (JxBrowser thread) must be atomic,
 * or a runnable chained in the gap is never triggered.
 * <p>
 * Every browser-side effect goes through {@link Adapter}, so the rules here
 * run without a browser; {@code OpenCoordinatorTest} pins them.
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

    /** Runs the {@code openPdfFile} dispatch for the file against the viewer
     *  page. Runs on the FX thread when the page is up, otherwise on the
     *  load-finished thread of the viewer-page load it was chained onto. */
    void dispatchOpen(FilePath file, int initialPage);

    /** A successful open that postdates every displayed status finished: a
     *  content-confirmation point. Not called for a failed open, nor for a
     *  superseded open completing late (see {@link #statusShown()}). */
    void openConfirmed();

    /** The in-flight open finished and no newer request was issued in its
     *  place, so anything buffered for "the open under way" now targets the
     *  document that just finished. */
    void openQueueIdle();

    /** A navigation replaced the page the in-flight open lived in, nothing
     *  was waiting to issue in its place, and the open's own completion can
     *  therefore never arrive; see {@link #navigationFinished(boolean)}. */
    void reportSupersededOpen(FilePath file);
  }

//---------------------------------------------------------------------------

  private final Adapter adapter;
  private final Executor fxExecutor;

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
   * Open coordination (writes are FX-confined; volatile because the openDone
   * bridge callback reads {@link #lastIssuedFile} and viewer-driving threads
   * read the others). {@code inFlightFile} is the file of the open in flight,
   * or null when none is; whether an open is in flight is read from it and
   * recorded nowhere else. {@code waitingFile} is the request that issues when
   * the in-flight open finishes, or null when none is waiting.
   * {@code lastIssuedFile} is never cleared: an open can be released before its
   * done report arrives, and the late report still has to be attributed.
   * <p>
   * Code that only needs to know whether an open is in flight asks
   * {@link #isOpenInFlight()}. Code that also uses the file, off the FX thread,
   * reads {@code inFlightFile} once into a local and tests the local: asking
   * first and reading afterward would pair an answer with a file from a
   * different moment.
   */
  private volatile FilePath waitingFile = null, inFlightFile = null, lastIssuedFile = null;
  private int waitingPage = 1;

  /** True when a status was shown while an open was already in flight: that
   *  open predates the status, so its completion must not clear the overlay
   *  (observed on Linux: a superseded slow open finishing stripped the progress
   *  overlay for the rest of a conversion). Set by {@link #statusShown()},
   *  cleared when a new open issues, which is then newer than any displayed
   *  status. FX-confined. */
  private boolean inFlightPredatesStatus = false;

//---------------------------------------------------------------------------

  /**
   * @param adapter    the browser side to drive
   * @param fxExecutor executor the open state is confined to; the FX thread
   *                   in production (run immediately when already on it), a
   *                   direct executor in tests
   */
  OpenCoordinator(Adapter adapter, Executor fxExecutor)
  {
    this.adapter = adapter;
    this.fxExecutor = fxExecutor;
  }

//---------------------------------------------------------------------------

  /** Whether an open is in flight. */
  boolean isOpenInFlight() { return inFlightFile != null; }

  /** The file of the in-flight open, or null when none is in flight. */
  FilePath inFlightFile() { return inFlightFile; }

  /** The file of the most recently issued open, in flight or not: what an
   *  {@code openDone} report describes. A newer request may already be waiting
   *  (latest-wins coalescing), in which case the report describes a superseded
   *  document and consumers must not treat it as confirming the newest one. */
  FilePath lastIssuedFile() { return lastIssuedFile; }

  /** Whether a viewer-page load is in flight. */
  boolean viewerLoadInFlight() { synchronized (loadLock) { return viewerLoadInFlight; } }

  /** Records that a status went up: an open already in flight predates it. FX thread only. */
  void statusShown() { if (isOpenInFlight()) inFlightPredatesStatus = true; }

  /** Drops any open still waiting its turn. An in-flight open is left alone:
   *  its page survives, so its completion still arrives and releases the
   *  coordinator normally. */
  void dropWaiting() { fxExecutor.execute(() -> waitingFile = null); }

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
   * Called when the in-flight open reports done (success or failure), from
   * the browser thread the report arrives on: releases the coordinator and
   * issues the latest request that arrived while the open was loading, if any.
   */
  void openFinished(boolean success)
  {
    fxExecutor.execute(() ->
    {
      // A successful open is a content-confirmation point. Only for an open
      // that postdates the displayed status, though: a superseded open
      // completing late must not strip a newer status. A failed open leaves
      // the status up; the reconciler decides what shows next (a retry
      // re-issue, eventually the unable notice).

      if (success && (inFlightPredatesStatus == false))
        adapter.openConfirmed();

      inFlightFile = null;
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
      inFlightFile = null;
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
        if (debugging())
          System.out.println("OpenCoordinator.loadViewerPage: joining in-flight viewer load");

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
   * applies here too: release the coordinator, so the newest waiting
   * open issues instead of every later open wedging behind a release that
   * never comes. With nothing waiting, the released open would otherwise
   * vanish silently: JxBrowser can deliver a duplicate main-frame load-finished
   * event for the same viewer navigation (observed), which lands here after the
   * first finish consumed the open's dispatch, and nothing would re-issue the
   * document. It is reported to the adapter as a superseded open instead.
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

    if (debugging())
      System.out.println("OpenCoordinator.navigationFinished: isViewerPage=" + isViewerPage + " hadPostLoadWork=" + hadPostLoadWork);

    // Read once: this runs on a browser thread, and the FX thread can release
    // or replace the in-flight open between two reads.

    FilePath releasedFile = inFlightFile;

    if ((releasedFile != null) && (toRun == null) && (viewerLoadStillInFlight == false))
    {
      if (debugging())
        System.out.println("OpenCoordinator: navigation superseded the in-flight open of " + releasedFile + "; releasing the coordinator");

      fxExecutor.execute(() ->
      {
        inFlightFile = null;
        pump();

        if (isOpenInFlight() == false)
          adapter.reportSupersededOpen(releasedFile);
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

      if (debugging() && (waitingFile != null))
        System.out.println("OpenCoordinator.pump: waiting on in-flight open of " + inFlightFile + "; queued " + waitingFile.getNameOnly());

      return;
    }

    if (waitingFile == null) return;

    FilePath file = waitingFile;
    int initialPage = waitingPage;

    waitingFile = null;

    // The in-flight write goes last: it alone publishes the open to the
    // browser-thread readers, so what they pair with it is already in place.

    lastIssuedFile = file;
    inFlightFile = file;
    inFlightPredatesStatus = false;  // this open is newer than any displayed status

    issue(file, initialPage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Dispatches the open, loading the viewer page first if necessary. FX
   *  thread only; callers go through {@link #pump()} so opens never overlap. */
  private void issue(FilePath file, int initialPage)
  {
    Runnable dispatch = () -> adapter.dispatchOpen(file, initialPage);

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

}
