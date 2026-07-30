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

import java.util.Objects;
import java.util.concurrent.Executor;

import org.hypernomicon.previewWindow.DesiredView.*;
import org.hypernomicon.previewWindow.DocumentArtifactService.ConverterState;
import org.hypernomicon.previewWindow.PipelineSnapshot.ArtifactStatus;
import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.PreviewIntent.ContentKind;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * The per-pane reconciler at the center of the intent-based preview architecture:
 * a preview pane's display is a derived view of (intent, pipeline state),
 * closed over by {@link #reconcile}, a total function from those inputs to a
 * {@link DesiredView}. The reconciler diffs the desired view against the last
 * view it issued commands for and sends the minimal command set through the
 * {@link ViewerPort}; because every input state maps to a display, stranded
 * UI (the eternal spinner family) is unrepresentable by construction.
 * <p>
 * <b>Inputs.</b> Initiators set a {@link PreviewIntent}; service adapters
 * push {@link PipelineSnapshot}s; the viewer reports lifecycle events. All
 * three arrive from arbitrary threads and are marshalled onto the pane's
 * executor (the FX thread in production, a direct executor in tests), so all
 * mutation is single-threaded and reconcile always evaluates a consistent
 * snapshot.
 * <p>
 * <b>Diffing is against issued, not confirmed.</b> {@code issuedView} is the
 * last DesiredView commands were sent for; equal desired and issued means
 * no-op. Confirmation state (the viewer's {@code documentLoaded}) gates only
 * hit and scroll-target delivery: hits are pushed once the load of the
 * current generation is confirmed, by reconcile observing the confirmation
 * event, and the intent's clicked-match scroll target follows the hits (see
 * {@link #issueScrollTargetIfReady}).
 * <p>
 * <b>Generations.</b> Each {@code showDocument}/{@code showContent}
 * establishes a new document generation; events carrying any other generation
 * are dropped (supersession, not cancellation handshakes).
 * <p>
 * <b>Failure escalation is bounded.</b> A viewer error clears
 * {@code issuedView} so reconcile re-issues, at most
 * {@link #MAX_VIEWER_RETRIES} times per desired document; on exhaustion the
 * desired view itself escalates to {@code Unable}, a terminal state exited
 * only by an intent change or a user refresh ({@link #refreshDisplay}).
 * <p>
 * <b>The intent back-edge.</b> User scrolling ({@code pageChanged}) is both a
 * confirmation and an intent update: the new page is folded into the current
 * intent and the issued view, so the next reconcile does not fight the user
 * with a {@code goToPage} back to the stale page. This is the one sanctioned
 * place viewer state writes intent.
 */
final class PreviewPane
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Re-issues after a viewer error, per desired document, before escalating to Unable. */
  static final int MAX_VIEWER_RETRIES = 2;

  private static final String VIEWER_FAILURE_CAUSE = "The viewer was unable to display the file";

  private final ViewerPort viewer;
  private final Executor paneExecutor;

  // All fields below are confined to paneExecutor tasks, except the two
  // volatile read-back fields.

  private PreviewIntent intent = null;
  private PipelineSnapshot snapshot = null;

  private DesiredView issuedView = null;
  private String issuedHitsJson = null;
  private ScrollTarget issuedScrollTarget = null;
  private long generation = 0;
  private boolean loadConfirmed = false;

  private FilePath retryDocPath = null, escalatedDocPath = null;
  private int retryCount = 0;

  private volatile FilePath confirmedFile = null;
  private volatile int confirmedPage = -1;

//---------------------------------------------------------------------------

  /**
   * @param viewer       the command surface to drive
   * @param paneExecutor executor all mutation and reconciliation runs on; the
   *                     FX thread in production ({@code Platform::runLater}),
   *                     a direct executor in tests
   */
  PreviewPane(ViewerPort viewer, Executor paneExecutor)
  {
    this.viewer = viewer;
    this.paneExecutor = paneExecutor;
  }

//---------------------------------------------------------------------------

  // Read-back API: the sanctioned way features read viewer state (launch at
  // current page, ContentsWindow Set button). Reads never mutate.

  FilePath currentFile()   { return confirmedFile; }
  int      currentPage()   { return confirmedPage; }

  private void reconcile() { issue(derive()); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets what this pane should show; {@code null} clears the pane. The only
   * sanctioned display-mutation path. Resets viewer-failure escalation, which
   * is how a terminal Unable(viewer) state is exited.
   */
  void setIntent(PreviewIntent newIntent)
  {
    paneExecutor.execute(() ->
    {
      intent = newIntent;
      retryDocPath = null;
      escalatedDocPath = null;
      retryCount = 0;
      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Chrome-driven page navigation: re-sets the current intent's page (a
   * derived page becomes explicit). The document, highlight, and scroll-target
   * facets of the intent are untouched, and viewer-failure escalation is not
   * reset (the document is unchanged). No-op when the pane has no intent.
   */
  void setIntentPage(int pageNum)
  {
    paneExecutor.execute(() ->
    {
      if (intent == null) return;

      intent = intent.withPage(pageNum);
      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Replaces the pipeline snapshot this pane derives from. Called by service adapters on any thread. */
  void updatePipeline(PipelineSnapshot newSnapshot)
  {
    paneExecutor.execute(() ->
    {
      snapshot = newSnapshot;
      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets intent and pipeline snapshot together in a single reconcile. Used when
   * the initiator already knows the snapshot for the new intent (an
   * instant-ready file, or a conversion just enqueued), so the pane does not
   * transiently score the new intent against the previous file's snapshot and
   * flash an intermediate Progress. Resets viewer-failure escalation, like
   * {@link #setIntent}.
   */
  void setIntentAndPipeline(PreviewIntent newIntent, PipelineSnapshot newSnapshot)
  {
    paneExecutor.execute(() ->
    {
      intent = newIntent;
      snapshot = newSnapshot;
      retryDocPath = null;
      escalatedDocPath = null;
      retryCount = 0;
      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Viewer events. Arrive on the bridge thread; marshalled like every other
  // input. Any event whose generation is not the current one is stale by
  // definition and dropped.

  @SuppressWarnings("unused")
  void onDocumentLoaded(long gen, ViewerMeta meta)
  {
    // meta (page count, labels, hilite pages) is reserved for the window-chrome
    // metadata channel, wired in a later phase; not consumed here yet.

    paneExecutor.execute(() ->
    {
      if (gen != generation) return;

      loadConfirmed = true;

      confirmedFile = sourceFileOf(issuedView);
      confirmedPage = (issuedView instanceof PagedDoc pagedDoc) ? pagedDoc.pageNum() : -1;

      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void onPageChanged(long gen, int pageNum)
  {
    paneExecutor.execute(() ->
    {
      if (gen != generation) return;

      confirmedPage = pageNum;

      // Intent back-edge: fold the viewer-originated page into intent and the
      // issued view so reconcile doesn't re-issue a goToPage to the old page.
      // Only when the event describes the intended document, though: during a
      // document switch, the outgoing document's page events are still current
      // by generation (the new document has not issued yet, so the generation
      // has not moved), and folding one into the new intent would turn the old
      // document's page into an explicit page for the new one. Observed as a
      // hit-derived preview (intent page -1) landing on the previous document's
      // page instead of the first-match page: the stale fold made the intent
      // explicit, which also skipped the withhold-until-hits derivation.

      if ((intent != null) && intent.sourceFile().equals(sourceFileOf(issuedView)))
        intent = intent.withPage(pageNum);

      if (issuedView instanceof PagedDoc pagedDoc)
        issuedView = new PagedDoc(pagedDoc.sourceFile(), pagedDoc.displayPath(), pageNum, pagedDoc.hitsJson());

      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  void onViewerError(long gen, String cause)
  {
    // cause is not surfaced yet: every viewer error currently collapses to a
    // generic Unable after the retry budget. Reserved for per-cause display or
    // retry-policy variation.

    paneExecutor.execute(() ->
    {
      if (gen != generation) return;

      FilePath docPath = displayPathOf(issuedView);
      if (docPath == null) return;

      if (docPath.equals(retryDocPath) == false)
      {
        retryDocPath = docPath;
        retryCount = 0;
      }

      retryCount++;

      if (retryCount > MAX_VIEWER_RETRIES)
        escalatedDocPath = docPath;

      // Mark dirty: clearing the issued view makes reconcile re-issue the
      // desired document (with a fresh generation), or the escalated Unable

      issuedView = null;
      issuedHitsJson = null;
      issuedScrollTarget = null;
      loadConfirmed = false;

      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Transitional-hybrid escape hatch: a legacy writer (record navigation from
   * a non-FTS query tab) has taken over this pane's viewer outside the intent
   * path. Drops intent, snapshot, issued/confirmed state, and escalation
   * WITHOUT issuing any command, so the external writer's display is left
   * alone and the next intent starts from a clean diff. Dissolves when every
   * writer goes through intent.
   */
  void yieldDisplay()
  {
    paneExecutor.execute(() ->
    {
      intent = null;
      snapshot = null;
      issuedView = null;
      issuedHitsJson = null;
      issuedScrollTarget = null;
      loadConfirmed = false;
      retryDocPath = null;
      escalatedDocPath = null;
      retryCount = 0;
      confirmedFile = null;
      confirmedPage = -1;
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * User-driven refresh: drops the issued view and confirmation state so
   * reconcile re-issues the full display (a fresh generation) for the current
   * intent, and resets viewer-failure escalation, making this the second exit
   * (besides an intent change) from a terminal viewer-failure display.
   */
  void refreshDisplay()
  {
    paneExecutor.execute(() ->
    {
      issuedView = null;
      issuedHitsJson = null;
      issuedScrollTarget = null;
      loadConfirmed = false;
      retryDocPath = null;
      escalatedDocPath = null;
      retryCount = 0;
      reconcile();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The total function from (intent, pipeline snapshot, escalation state) to
   * the display this pane should present.
   */
  private DesiredView derive()
  {
    if (intent == null)
      return new Empty();

    FilePath sourceFile = intent.sourceFile();

    // A snapshot describing a different file than the intent's is stale by
    // definition and treated as absent, exactly like a stale-generation
    // viewer event; the pipeline for the intended file has not reported yet

    if ((snapshot == null) || (snapshot.artifact() == null) || (sourceFile.equals(snapshot.sourceFile()) == false))
      return new Progress(sourceFile, progressVariant());

    DesiredView desired = switch (snapshot.artifact())
    {
      case ArtifactStatus.Queued     _     -> new Progress(sourceFile, progressVariant());
      case ArtifactStatus.Converting _     -> new Progress(sourceFile, progressVariant());
      case ArtifactStatus.Failed     f     -> new Unable(sourceFile, f.cause());
      case ArtifactStatus.Ready      ready -> deriveDocView(sourceFile, ready.displayPath());
    };

    // Bounded viewer-failure escalation: a document that exhausted its
    // re-issues maps to a terminal Unable until the intent changes

    if ((escalatedDocPath != null) && escalatedDocPath.equals(displayPathOf(desired)))
      return new Unable(sourceFile, VIEWER_FAILURE_CAUSE);

    return desired;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private DesiredView deriveDocView(FilePath sourceFile, FilePath displayPath)
  {
    if (intent.kind() == ContentKind.DIRECT)
    {
      String hitsJson = (intent.wantsHighlights() && (snapshot.hits() instanceof HitsStatus.ReadyDirect(String json)))
        ? json
        : null;

      return new DirectDoc(sourceFile, displayPath, hitsJson);
    }

    // Paged content whose page derives from the hit set (the FTS case): the
    // display is deliberately withheld until the first-match page is known,
    // but a failed hit computation degrades to page 1 with no highlights;
    // degraded display beats withheld display

    if (intent.wantsHighlights() && intent.derivesPage())
    {
      return switch (snapshot.hits())
      {
        case HitsStatus.ReadyPaged ready -> new PagedDoc(sourceFile, displayPath, Math.max(1, ready.firstMatchPage()), ready.hitsJson());
        case HitsStatus.Failed     _     -> new PagedDoc(sourceFile, displayPath, 1, null);
        case null, default               -> new Progress(sourceFile, ProgressVariant.GENERATING);
      };
    }

    // Explicit page (record navigation, nav history): show immediately;
    // hits apply when they arrive

    int pageNum = intent.derivesPage() ? 1 : intent.pageNum();

    String hitsJson = (intent.wantsHighlights() && (snapshot.hits() instanceof HitsStatus.ReadyPaged(String json, _)))
      ? json
      : null;

    return new PagedDoc(sourceFile, displayPath, pageNum, hitsJson);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Starting-vs-generating is converter-lifecycle state, not request state. */
  private ProgressVariant progressVariant()
  {
    return ((snapshot != null) && (snapshot.converterState() == ConverterState.RUNNING))
      ? ProgressVariant.GENERATING
      : ProgressVariant.STARTING_CONVERTER;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Issues the minimal command set to move the viewer from the issued view to
   * the desired one. Alt displays diff by value equality; documents diff along
   * the hierarchy (document identity, then page, then hits), with hit delivery
   * additionally gated on the current generation's load confirmation.
   */
  private void issue(DesiredView desired)
  {
    switch (desired)
    {
      case Empty _ ->
      {
        if (desired.equals(issuedView) == false)
          viewer.showEmpty();
      }

      case Progress progress ->
      {
        if (desired.equals(issuedView) == false)
          viewer.showProgress(progress.sourceFile(), progress.variant());
      }

      case Unable unable ->
      {
        if (desired.equals(issuedView) == false)
          viewer.showUnable(unable.sourceFile());
      }

      case PagedDoc pagedDoc ->
      {
        if (((issuedView instanceof PagedDoc prev) && prev.displayPath().equals(pagedDoc.displayPath())) == false)
        {
          generation++;
          loadConfirmed = false;
          issuedHitsJson = null;
          issuedScrollTarget = null;
          viewer.showDocument(generation, pagedDoc.displayPath(), pagedDoc.pageNum());
        }
        else if (((PagedDoc) issuedView).pageNum() != pagedDoc.pageNum())
        {
          viewer.goToPage(generation, pagedDoc.pageNum());
        }

        issueHitsIfConfirmed(pagedDoc.hitsJson());
        issueScrollTargetIfReady();
      }

      case DirectDoc directDoc ->
      {
        if (((issuedView instanceof DirectDoc prev) && prev.displayPath().equals(directDoc.displayPath())) == false)
        {
          generation++;
          loadConfirmed = false;
          issuedHitsJson = null;
          issuedScrollTarget = null;
          viewer.showContent(generation, directDoc.displayPath());
        }

        issueHitsIfConfirmed(directDoc.hitsJson());
        issueScrollTargetIfReady();
      }
    }

    issuedView = desired;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Hit delivery is gated on the current generation's confirmed load: pushing
   * hits into a viewer that has not finished loading the document would race
   * the text layer. Reconcile re-runs on the confirmation event, which is when
   * pending hits actually ship.
   */
  private void issueHitsIfConfirmed(String hitsJson)
  {
    if (loadConfirmed == false) return;

    if (Objects.equals(issuedHitsJson, hitsJson)) return;

    if (hitsJson == null)
      viewer.clearHits(generation);
    else
      viewer.setHits(generation, hitsJson);

    issuedHitsJson = hitsJson;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scroll-target delivery: the intent's clicked-match target is forwarded
   * once per target, after the current generation's load is confirmed and its
   * highlights have been issued (a scroll target addresses highlight spans,
   * so without hits there is nothing to scroll to). A repeat click arrives as
   * a fresh target (new serial) and delivers again; the back-edge folding a
   * user scroll into intent preserves the delivered target, so reconcile
   * never re-fires it against the user. A reload of the same document (a
   * viewer-error re-issue or a user refresh) resets the issued target with
   * the rest of the generation's sub-state and re-delivers.
   */
  private void issueScrollTargetIfReady()
  {
    if ((loadConfirmed == false) || (issuedHitsJson == null)) return;

    ScrollTarget target = intent.scrollTarget();

    if ((target == null) || target.equals(issuedScrollTarget)) return;

    viewer.scrollToMatch(generation, target.matchNdx(), target.pageNum(), target.ndxOnPage());
    issuedScrollTarget = target;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static FilePath displayPathOf(DesiredView view)
  {
    return switch (view)
    {
      case PagedDoc  pagedDoc  -> pagedDoc .displayPath();
      case DirectDoc directDoc -> directDoc.displayPath();
      case null, default       -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static FilePath sourceFileOf(DesiredView view)
  {
    return switch (view)
    {
      case PagedDoc  pagedDoc  -> pagedDoc .sourceFile();
      case DirectDoc directDoc -> directDoc.sourceFile();
      case null, default       -> null;
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
