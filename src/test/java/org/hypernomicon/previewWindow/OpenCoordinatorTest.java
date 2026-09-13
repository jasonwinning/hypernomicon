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

import static org.junit.jupiter.api.Assertions.*;

import java.util.ArrayList;
import java.util.List;

import org.hypernomicon.util.file.FilePath;

import org.junit.jupiter.api.Test;

//---------------------------------------------------------------------------

/**
 * Contract tests for {@link OpenCoordinator}: the latest-wins open queue, the
 * viewer-page load join, status supersession, the release rules for
 * navigations that replace the page an open lives in, and the liveness
 * guarantees (token-attributed reports, undeliverable dispatches, the
 * no-progress watchdog). Run against a recording adapter, a direct executor,
 * and hand-driven time (all mutation synchronous; no browser, no JavaFX, no
 * clock). These pin the rules the preview's debug traces used to be the only
 * record of; a change to any of them updates these tests in the same commit.
 */
class OpenCoordinatorTest
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Records every effect in order; the viewer-page flag is set by the tests
   *  the way the wrapper sets it, from the evidence in each finished
   *  navigation, so the coordinator's decisions can be checked against it. */
  private static final class RecordingAdapter implements OpenCoordinator.Adapter
  {
    boolean viewerPageLoaded = true, deliverable = true;
    int lastToken = -1;

    final List<String> calls = new ArrayList<>();

    @Override public boolean viewerPageLoaded() { return viewerPageLoaded; }

    @Override public void navigateToViewerPage() { calls.add("navigate"); }

    @Override public boolean dispatchOpen(FilePath file, int initialPage, int token)
    {
      lastToken = token;
      calls.add("open:" + file.getNameOnly() + '@' + initialPage);
      return deliverable;
    }

    @Override public void openReported(FilePath file, boolean success, int pageCount, String errMessage)
    {
      calls.add("reported:" + file.getNameOnly() + ':' + (success ? "ok" : "fail"));
    }

    @Override public void openConfirmed() { calls.add("confirmed"); }

    @Override public void openQueueIdle() { calls.add("idle"); }

    @Override public void openFailed(FilePath file, String cause) { calls.add("failed:" + file.getNameOnly()); }
  }

//---------------------------------------------------------------------------

  /** Holds every armed check until the test elapses a window. */
  private static final class FakeScheduler implements OpenCoordinator.Scheduler
  {
    final List<Runnable> armed = new ArrayList<>();
    long lastDelay = -1;

    @Override public void schedule(Runnable task, long delayMillis)
    {
      armed.add(task);
      lastDelay = delayMillis;
    }

    /** One full window passes: every check armed so far fires, in order.
     *  Checks those fires arm stay for the next window. */
    void elapse()
    {
      List<Runnable> due = new ArrayList<>(armed);
      armed.clear();
      due.forEach(Runnable::run);
    }
  }

//---------------------------------------------------------------------------

  private static final FilePath A = FilePath.of("a.pdf"),
                                B = FilePath.of("b.pdf"),
                                C = FilePath.of("c.pdf");

  private final RecordingAdapter adapter = new RecordingAdapter();
  private final FakeScheduler scheduler = new FakeScheduler();
  private final OpenCoordinator coordinator = new OpenCoordinator(adapter, Runnable::run, scheduler);

//---------------------------------------------------------------------------

  /** A main-frame navigation finished; the wrapper commits the viewer-page
   *  flag from the event's URL before handing the finish to the coordinator. */
  private void finishNavigation(boolean isViewerPage)
  {
    adapter.viewerPageLoaded = isViewerPage;
    coordinator.navigationFinished(isViewerPage);
  }

  /** The viewer reports on the most recently dispatched open. */
  private void finishOpen(boolean success)
  {
    coordinator.openFinished(adapter.lastToken, success, 10, success ? "" : "Invalid PDF structure");
  }

  private List<String> calls() { return adapter.calls; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Test void openDispatchesImmediatelyWhenTheViewerPageIsUp()
  {
    coordinator.requestOpen(A, 3);

    assertEquals(List.of("open:a.pdf@3"), calls());
    assertTrue(coordinator.isOpenInFlight());
    assertEquals(A, coordinator.inFlightFile());
  }

//---------------------------------------------------------------------------

  @Test void openLoadsTheViewerPageFirstWhenItIsNotUp()
  {
    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);

    assertEquals(List.of("navigate"), calls());
    assertTrue(coordinator.viewerLoadInFlight());

    finishNavigation(true);

    assertEquals(List.of("navigate", "open:a.pdf@1"), calls());
    assertFalse(coordinator.viewerLoadInFlight());
    assertTrue(coordinator.isOpenInFlight());
  }

//---------------------------------------------------------------------------

  @Test void requestWhileAnOpenIsInFlightWaitsForItToFinish()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 2);

    assertEquals(List.of("open:a.pdf@1"), calls());
    assertEquals(A, coordinator.inFlightFile());

    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "open:b.pdf@2"), calls());
    assertEquals(B, coordinator.inFlightFile());
  }

//---------------------------------------------------------------------------

  @Test void latestRequestWinsWhileWaiting()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 2);
    coordinator.requestOpen(C, 3);

    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "open:c.pdf@3"), calls());
  }

//---------------------------------------------------------------------------

  @Test void finishedOpenWithNothingWaitingConfirmsAndReportsIdle()
  {
    coordinator.requestOpen(A, 1);
    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle"), calls());
    assertFalse(coordinator.isOpenInFlight());
    assertNull(coordinator.inFlightFile());
  }

//---------------------------------------------------------------------------

  @Test void failedOpenReleasesWithoutConfirming()
  {
    coordinator.requestOpen(A, 1);
    finishOpen(false);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:fail", "idle"), calls());
    assertFalse(coordinator.isOpenInFlight());

    coordinator.requestOpen(B, 1);

    assertEquals("open:b.pdf@1", calls().get(calls().size() - 1));
  }

//---------------------------------------------------------------------------

  @Test void openThatPredatesAStatusDoesNotConfirmOnSuccess()
  {
    coordinator.requestOpen(A, 1);
    coordinator.statusShown();
    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "idle"), calls());
  }

//---------------------------------------------------------------------------

  @Test void statusShownWhileIdleDoesNotAffectTheNextOpen()
  {
    coordinator.statusShown();
    coordinator.requestOpen(A, 1);
    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle"), calls());
  }

//---------------------------------------------------------------------------

  @Test void openIssuedAfterAStatusConfirmsEvenThoughItsPredecessorPredatedIt()
  {
    coordinator.requestOpen(A, 1);
    coordinator.statusShown();
    coordinator.requestOpen(B, 1);

    finishOpen(true);   // A: predates the status, must not confirm

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "open:b.pdf@1"), calls());

    finishOpen(true);   // B: newer than the status

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "open:b.pdf@1", "reported:b.pdf:ok", "confirmed", "idle"), calls());
  }

//---------------------------------------------------------------------------

  @Test void supersedeOpensDropsTheWaitingOpenAndReleasesTheInFlightOne()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 1);

    coordinator.supersedeOpens(false);

    assertFalse(coordinator.isOpenInFlight());
    assertNull(coordinator.inFlightFile());

    coordinator.requestOpen(C, 1);

    assertEquals(List.of("open:a.pdf@1", "open:c.pdf@1"), calls());  // B never issues; C is not wedged behind A
  }

//---------------------------------------------------------------------------

  @Test void dropWaitingLeavesTheInFlightOpenAlone()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 1);

    coordinator.dropWaiting();

    assertTrue(coordinator.isOpenInFlight());

    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle"), calls());  // A settles normally; B never issues
  }

//---------------------------------------------------------------------------

  @Test void viewerPageFinishRunsTheChainedDispatchWithoutReleasing()
  {
    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);
    finishNavigation(true);

    assertEquals(List.of("navigate", "open:a.pdf@1"), calls());
    assertTrue(coordinator.isOpenInFlight());

    coordinator.requestOpen(B, 1);

    assertEquals(List.of("navigate", "open:a.pdf@1"), calls());  // A is still in flight; B waits
  }

//---------------------------------------------------------------------------

  @Test void foreignNavigationFailsTheInFlightOpenWhenNothingWaits()
  {
    coordinator.requestOpen(A, 1);

    finishNavigation(false);  // e.g. direct content committed over the viewer page

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());

    coordinator.requestOpen(B, 1);  // nothing is wedged; the viewer page is gone, so B navigates first

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf", "navigate"), calls());
  }

//---------------------------------------------------------------------------

  @Test void foreignNavigationReleasesTheInFlightOpenAndIssuesTheWaitingOne()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 2);

    finishNavigation(false);

    assertEquals(List.of("open:a.pdf@1", "navigate"), calls());  // B issues (latest wins), so A is not reported
    assertEquals(B, coordinator.inFlightFile());

    finishNavigation(true);

    assertEquals(List.of("open:a.pdf@1", "navigate", "open:b.pdf@2"), calls());
  }

//---------------------------------------------------------------------------

  @Test void foreignNavigationDoesNotReleaseWhileAViewerLoadCarryingTheDispatchIsPending()
  {
    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);

    finishNavigation(false);  // a superseded direct-content load finishing late

    assertEquals(List.of("navigate"), calls());
    assertTrue(coordinator.isOpenInFlight());
    assertTrue(coordinator.viewerLoadInFlight());

    finishNavigation(true);

    assertEquals(List.of("navigate", "open:a.pdf@1"), calls());
  }

//---------------------------------------------------------------------------

  @Test void duplicateViewerPageFinishFailsTheOpenItsPredecessorDispatched()
  {
    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);
    finishNavigation(true);
    finishNavigation(true);  // JxBrowser can deliver the same finish twice

    assertEquals(List.of("navigate", "open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());
  }

//---------------------------------------------------------------------------

  @Test void loadViewerPageJoinsAnInFlightLoadAndReplacesItsWork()
  {
    List<String> ran = new ArrayList<>();

    coordinator.loadViewerPage(() -> ran.add("first"));
    coordinator.loadViewerPage(() -> ran.add("second"));

    assertEquals(List.of("navigate"), calls());  // one navigation, not two

    finishNavigation(true);

    assertEquals(List.of("second"), ran);
    assertFalse(coordinator.viewerLoadInFlight());
  }

//---------------------------------------------------------------------------

  @Test void openIssuedWhileAViewerLoadIsPendingChainsOntoIt()
  {
    List<String> ran = new ArrayList<>();

    coordinator.loadViewerPage(() -> ran.add("warm-up"));

    // The page flag can still read as loaded while the load is in flight (right
    // after construction); the dispatch must wait for the load all the same.

    coordinator.requestOpen(A, 1);

    assertEquals(List.of("navigate"), calls());

    finishNavigation(true);

    assertEquals(List.of("navigate", "open:a.pdf@1"), calls());
    assertTrue(ran.isEmpty());  // replaced by the dispatch
  }

//---------------------------------------------------------------------------

  @Test void supersedingTheViewerLoadDiscardsItsChainedWorkAndItsJoin()
  {
    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);
    coordinator.supersedeOpens(true);

    assertFalse(coordinator.viewerLoadInFlight());

    finishNavigation(true);  // the dead load finishing anyway

    assertEquals(List.of("navigate"), calls());  // A's dispatch never runs, and nothing is released or reported

    coordinator.loadViewerPage(() -> { });

    assertEquals(List.of("navigate", "navigate"), calls());  // a later load does not join the dead one
  }

//---------------------------------------------------------------------------

  @Test void supersedingOpensAloneKeepsTheViewerLoadForTheCallersOwnWork()
  {
    List<String> ran = new ArrayList<>();

    adapter.viewerPageLoaded = false;

    coordinator.requestOpen(A, 1);      // navigates; dispatch chained
    coordinator.supersedeOpens(false);  // a status is about to become the viewer page's content

    assertTrue(coordinator.viewerLoadInFlight());
    assertFalse(coordinator.isOpenInFlight());

    coordinator.loadViewerPage(() -> ran.add("status"));

    assertEquals(List.of("navigate"), calls());  // joined the pending load

    finishNavigation(true);

    assertEquals(List.of("status"), ran);
    assertEquals(List.of("navigate"), calls());  // the superseded open's dispatch was replaced, and nothing is reported
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Liveness: token-attributed reports

  @Test void eachIssuedOpenGetsAFreshToken()
  {
    coordinator.requestOpen(A, 1);
    int tokenA = adapter.lastToken;

    finishOpen(true);
    coordinator.requestOpen(B, 1);

    assertNotEquals(tokenA, adapter.lastToken);
  }

//---------------------------------------------------------------------------

  @Test void reportWithAnUnknownTokenIsDropped()
  {
    coordinator.requestOpen(A, 1);

    coordinator.openFinished(adapter.lastToken + 100, true, 10, "");

    assertEquals(List.of("open:a.pdf@1"), calls());  // neither forwarded nor released
    assertTrue(coordinator.isOpenInFlight());
  }

//---------------------------------------------------------------------------

  @Test void lateReportForASupersededOpenDoesNotReleaseItsSuccessor()
  {
    coordinator.requestOpen(A, 1);
    int tokenA = adapter.lastToken;

    coordinator.supersedeOpens(false);
    coordinator.requestOpen(B, 1);

    coordinator.openFinished(tokenA, true, 10, "");  // A's promise settling after all

    assertEquals(List.of("open:a.pdf@1", "open:b.pdf@1"), calls());
    assertEquals(B, coordinator.inFlightFile());
  }

//---------------------------------------------------------------------------

  @Test void duplicateReportForASettledOpenIsDropped()
  {
    coordinator.requestOpen(A, 1);
    finishOpen(true);
    finishOpen(true);

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle"), calls());
  }

//---------------------------------------------------------------------------

  @Test void reportArrivingAfterTheWatchdogFailedTheOpenIsDropped()
  {
    coordinator.requestOpen(A, 1);
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());

    finishOpen(true);  // the viewer got there eventually; its terminal report already went out

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Liveness: undeliverable dispatch

  @Test void undeliverableDispatchFailsTheOpenAtOnce()
  {
    adapter.deliverable = false;

    coordinator.requestOpen(A, 1);

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());

    adapter.deliverable = true;

    coordinator.requestOpen(B, 1);

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf", "open:b.pdf@1"), calls());  // not wedged behind A
  }

//---------------------------------------------------------------------------

  @Test void undeliverableChainedDispatchFailsTheOpenWhenTheLoadFinishes()
  {
    adapter.viewerPageLoaded = false;
    adapter.deliverable = false;

    coordinator.requestOpen(A, 1);

    assertEquals(List.of("navigate"), calls());

    finishNavigation(true);

    assertEquals(List.of("navigate", "open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Liveness: the no-progress watchdog

  @Test void issuingAnOpenArmsOneCheckForTheTimeoutWindow()
  {
    coordinator.requestOpen(A, 1);

    assertEquals(1, scheduler.armed.size());
    assertEquals(OpenCoordinator.NO_PROGRESS_TIMEOUT_MILLIS, scheduler.lastDelay);
  }

//---------------------------------------------------------------------------

  @Test void openWithNoProgressForAWholeWindowIsFailed()
  {
    coordinator.requestOpen(A, 1);
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());
    assertFalse(coordinator.isOpenInFlight());
    assertTrue(scheduler.armed.isEmpty());  // nothing left to watch
  }

//---------------------------------------------------------------------------

  @Test void progressWithinTheWindowKeepsTheOpenAliveForAnotherWindow()
  {
    coordinator.requestOpen(A, 1);
    coordinator.progress();
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1"), calls());
    assertTrue(coordinator.isOpenInFlight());
    assertEquals(1, scheduler.armed.size());  // re-armed

    scheduler.elapse();  // a full window with no further progress

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());
  }

//---------------------------------------------------------------------------

  @Test void theWatchStandsDownWhenTheOpenSettles()
  {
    coordinator.requestOpen(A, 1);
    finishOpen(true);
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle"), calls());
    assertTrue(scheduler.armed.isEmpty());
  }

//---------------------------------------------------------------------------

  @Test void aCheckArmedForAnEarlierOpenDoesNotJudgeTheCurrentOne()
  {
    coordinator.requestOpen(A, 1);
    finishOpen(true);              // A's check is still armed
    coordinator.requestOpen(B, 1); // arms B's check
    coordinator.progress();
    scheduler.elapse();            // A's check: stale by token; B's check: progress seen, re-arms

    assertEquals(List.of("open:a.pdf@1", "reported:a.pdf:ok", "confirmed", "idle", "open:b.pdf@1"), calls());
    assertTrue(coordinator.isOpenInFlight());
    assertEquals(1, scheduler.armed.size());
  }

//---------------------------------------------------------------------------

  @Test void aStallWithARequestWaitingIssuesItInsteadOfReporting()
  {
    coordinator.requestOpen(A, 1);
    coordinator.requestOpen(B, 2);
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1", "open:b.pdf@2"), calls());
    assertEquals(B, coordinator.inFlightFile());
  }

//---------------------------------------------------------------------------

  @Test void waitingOnTheUserSuspendsTheWatchUntilProgressResumes()
  {
    coordinator.requestOpen(A, 1);
    coordinator.waitingOnUser();  // a password prompt is up
    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1"), calls());  // not a stall
    assertTrue(scheduler.armed.isEmpty());          // and nothing re-armed: the user takes as long as they take

    coordinator.progress();  // the password was accepted and loading resumed

    assertEquals(1, scheduler.armed.size());

    scheduler.elapse();

    assertEquals(List.of("open:a.pdf@1", "failed:a.pdf"), calls());  // silence after that counts again
  }

//---------------------------------------------------------------------------

  @Test void progressFromABygoneOpenNeverArmsAWatchWhileIdle()
  {
    coordinator.requestOpen(A, 1);
    coordinator.waitingOnUser();
    finishOpen(false);   // the prompt was cancelled
    scheduler.elapse();  // the check armed at issue fires on a settled open: no-op

    coordinator.progress();

    assertTrue(scheduler.armed.isEmpty());
  }

//---------------------------------------------------------------------------

}
