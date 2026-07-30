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

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.PreviewIntent.ContentKind;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.SettleGate;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;

//---------------------------------------------------------------------------

/**
 * Hosts the {@link PreviewPane} reconciler for one preview pane; one
 * instance per {@link PreviewWindow.PreviewSource}, obtained via
 * {@link PreviewWindow#hostFor}. This host supplies the reconciler's inputs
 * (the artifact side of the {@link PipelineSnapshot}, observed from the
 * conversion session, and viewer lifecycle events forwarded by the wrapper)
 * and implements the {@link ViewerPort} over the {@link PreviewWrapper},
 * whose load methods keep the window controls, nav history, and work-page
 * bookkeeping fed.
 * <p>
 * Record-navigation panes use {@link #setPreviewAuto} (no highlighting, content
 * kind derived from the mimetype). The queries pane additionally drives the FTS
 * flow: {@code FTSQueryCtrlr} sets intent via {@link #setPreview} and pushes
 * hit-set results (the {@code updateHits*} methods) through the
 * {@link PreviewWindow} facade.
 * <p>
 * Transitional-hybrid note: some legacy writers (file-level nav buttons, and
 * whatever has not converted yet) still drive the wrapper directly; those
 * paths call {@link #yieldToExternalWriter} so the pane relinquishes the
 * viewer without disturbing the external display. The yield dissolves as the
 * last legacy writers convert.
 * <p>
 * Threading: everything here runs on the FX thread (controller calls, session
 * display callbacks, and pane executor tasks all marshal there), except the
 * wrapper's event sink, which arrives on browser threads and only reads the
 * two volatile fields before handing off to the pane's own marshalling.
 */
final class PreviewPaneHost
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final PreviewWindow.PreviewSource src;

  /**
   * Settle gate for this pane's intents: rapid intent changes (key-repeat
   * selection reaching this host per selection) must not each set an intent,
   * subscribe to a conversion, and cycle the display; only the file the
   * selection settles on does. A quiet-selection intent proceeds immediately,
   * so gated upstream callers (the FTS controller's own settle gate) and
   * deliberate single selections never wait here.
   */
  private final SettleGate settleGate = new SettleGate(150);

  /** The artifact side of the pipeline snapshot; completed artifacts are
   *  leased through the wrapper, which releases its previous lease. */
  private final ArtifactTracker artifacts = new ArtifactTracker(session -> wrapper().leaseArtifact(session), this::pushSnapshot);

  private PreviewPane pane = null;

  private volatile FilePath intentFile = null;
  private volatile long issuedGen = 0;

  private HDT_Record intentRecord = null;
  private HitsStatus hitsStatus = null;

  /**
   * The file of the most recent {@link #setPreview} request, recorded before
   * the settle gate, and hit results that arrived for it while its intent was
   * still gated. The FTS initiator pushes hits synchronously right after
   * requesting an intent, so when the gate defers that intent, the hits reach
   * {@link #updateHits} while {@link #intentFile} still names the previous
   * file; they are early, not stale, and dropping them left the deferred
   * intent stuck on a Pending hit status forever (nothing re-pushes).
   * {@link #setPreviewNow} consumes the stash in place of Pending. Both
   * FX-confined, like the gate that makes them necessary.
   */
  private FilePath requestedFile = null;
  private HitsStatus requestedFileHits = null;

  /** True while {@code trackNewFile} runs inside {@code setPreviewNow}: the new
   *  subscription's immediate display callback would otherwise queue a pipeline
   *  update built against the PREVIOUS intent one pane-executor task ahead of the
   *  atomic {@code setIntentAndPipeline} (observed as a spurious Empty/Progress
   *  issue plus a wasted viewer reset at first preview). The atomic set that
   *  follows carries the same artifact state, so nothing is lost. FX-confined. */
  private boolean suppressSnapshotPush = false;

//---------------------------------------------------------------------------

  PreviewPaneHost(PreviewWindow.PreviewSource src)
  {
    this.src = src;
  }

//---------------------------------------------------------------------------

  private PreviewWrapper wrapper() { return PreviewWindow.wrapperForSource(src); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets this pane's intent, through the settle gate. For office documents the
   * host starts (or joins) the conversion and feeds its status to the
   * reconciler; for natively-viewable files the artifact is the source file
   * itself.
   *
   * @param filePath        the file to preview
   * @param record          record associated with the file, or {@code null}
   * @param paged           pdf.js mode vs direct browser content
   * @param pageNum         1-based explicit page, or -1 to derive from the hit set
   * @param wantsHighlights whether hit results will be pushed for this view
   * @param scrollTarget    clicked-match target to scroll to once the document and
   *                        its highlights are in place, or {@code null}
   */
  void setPreview(FilePath filePath, HDT_Record record, boolean paged, int pageNum, boolean wantsHighlights, ScrollTarget scrollTarget)
  {
    // Record the request target before gating, so hits arriving for it while
    // the intent waits in the gate are stashed rather than dropped as stale.
    // A request for a different file invalidates any stash (latest wins, like
    // the gate itself).

    if (filePath.equals(requestedFile) == false)
    {
      requestedFile = filePath;
      requestedFileHits = null;
    }

    settleGate.request(() -> setPreviewNow(filePath, record, paged, pageNum, wantsHighlights, scrollTarget));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreviewNow(FilePath filePath, HDT_Record record, boolean paged, int pageNum, boolean wantsHighlights, ScrollTarget scrollTarget)
  {
    PreviewWrapper wrapper = wrapper();
    if ((wrapper == null) || (wrapper.ensureInitialized() == false)) return;

    ensurePane();

    // Hits that arrived while this intent was gated; only meaningful for the
    // execution of the request they were stashed under, so always consumed

    HitsStatus earlyHits = filePath.equals(requestedFile) ? requestedFileHits : null;
    requestedFileHits = null;

    boolean sameFile = filePath.equals(intentFile);

    intentFile = filePath;
    intentRecord = record;

    if (sameFile == false)
    {
      hitsStatus = wantsHighlights
        ? (earlyHits != null ? earlyHits : new HitsStatus.Pending())
        : null;

      suppressSnapshotPush = true;
      try     { artifacts.trackNewFile(filePath, wrapper); }
      finally { suppressSnapshotPush = false; }
    }

    // Set intent and snapshot atomically: an instant-ready file (native PDF,
    // direct content) then goes straight to its document instead of flashing a
    // one-cycle Progress from the staleness guard scoring the new intent against
    // the previous file's snapshot. Later conversion-state changes still arrive
    // as separate pushSnapshot() updates.

    pane.setIntentAndPipeline(
      new PreviewIntent(filePath, paged ? ContentKind.PAGED : ContentKind.DIRECT, pageNum, wantsHighlights, scrollTarget),
      new PipelineSnapshot(filePath, artifacts.status(), DocumentArtifactService.converterState(), hitsStatus));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets this pane's intent for a record-navigation preview (no highlighting),
   * with the content kind derived from the file's mimetype. Rapid selection
   * coalesces in the settle gate.
   */
  void setPreviewAuto(FilePath filePath, HDT_Record record, int pageNum)
  {
    boolean paged = PreviewIntent.kindFor(filePath) == ContentKind.PAGED;

    setPreview(filePath, record, paged, paged ? Math.max(pageNum, 1) : 1, false, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void updateHitsPaged(FilePath filePath, String hitsJson, int firstMatchPage)
  {
    updateHits(filePath, new HitsStatus.ReadyPaged(hitsJson, firstMatchPage));
  }

  void updateHitsDirect(FilePath filePath, String hitsJson)
  {
    updateHits(filePath, new HitsStatus.ReadyDirect(hitsJson));
  }

  void updateHitsFailed(FilePath filePath)
  {
    updateHits(filePath, new HitsStatus.Failed());
  }

//---------------------------------------------------------------------------

  private void updateHits(FilePath filePath, HitsStatus newStatus)
  {
    if (filePath.equals(intentFile) == false)
    {
      if (filePath.equals(requestedFile))
      {
        // Early, not stale: computed for a request whose intent is still
        // waiting in the settle gate. Stash it for setPreviewNow to consume;
        // dropping it would leave that intent Pending forever.

        if (app.debugging)
          System.out.println("PreviewPaneHost[" + src + "].updateHits: STASHED " + newStatus.getClass().getSimpleName() +
                             " for gated request " + filePath.getNameOnly() +
                             " (intent=" + (intentFile == null ? "null" : intentFile.getNameOnly()) + ')');

        requestedFileHits = newStatus;
        return;
      }

      // Stale by value; a different file is intended now

      if (app.debugging)
        System.out.println("PreviewPaneHost[" + src + "].updateHits: DROPPED for " + filePath.getNameOnly() +
                           " (intent=" + (intentFile == null ? "null" : intentFile.getNameOnly()) + ')');

      return;
    }

    if (app.debugging)
      System.out.println("PreviewPaneHost[" + src + "].updateHits: " + newStatus.getClass().getSimpleName() + " for " + filePath.getNameOnly());

    hitsStatus = newStatus;
    pushSnapshot();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Clears the queries pane's FTS preview (intent = none; the viewer empties). */
  void clear()
  {
    settleGate.cancel();

    if (pane == null) return;

    artifacts.drop();
    hitsStatus = null;
    intentFile = null;
    intentRecord = null;

    pane.setIntent(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A legacy writer (record navigation from a non-FTS query sub-tab,
   * file-level nav buttons) has taken over this pane's viewer. Relinquish
   * without touching the display; see the class comment.
   */
  void yieldToExternalWriter()
  {
    settleGate.cancel();

    requestedFile = null;      // the stash dies with the cancelled gate request
    requestedFileHits = null;

    if (pane == null) return;

    artifacts.drop();
    hitsStatus = null;
    intentFile = null;
    intentRecord = null;

    pane.yieldDisplay();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pushSnapshot()
  {
    if (suppressSnapshotPush) return;  // the atomic setIntentAndPipeline immediately after trackNewFile carries this state

    FilePath sourceFile = intentFile;
    if ((pane == null) || (sourceFile == null)) return;

    pane.updatePipeline(new PipelineSnapshot(sourceFile, artifacts.status(), DocumentArtifactService.converterState(), hitsStatus));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void ensurePane()
  {
    if (pane != null) return;

    pane = new PreviewPane(new WrapperPort(), Platform::runLater);

    wrapper().setPaneEventSink(new PreviewWrapper.PaneEventSink()
    {
      // Arrives on browser threads; the intentFile gate keeps legacy-driven
      // loads (after a yield) from reaching the pane, and the pane marshals
      // and generation-checks everything else.

      @Override public void onOpened(boolean success)
      {
        if (intentFile == null) return;

        if (success)
          pane.onDocumentLoaded(issuedGen, new ViewerMeta(-1));
        else
          pane.onViewerError(issuedGen, "The viewer could not open the document");
      }

      @Override public void onPageChanged(int pageNum)
      {
        if (intentFile == null) return;

        pane.onPageChanged(issuedGen, pageNum);
      }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The {@link ViewerPort} over the {@link PreviewWrapper}. Every command
   * corresponds to the current intent (the pane is single-threaded on the FX
   * thread), so the wrapper's source-file tracking is read from the host
   * fields.
   * <p>
   * Both load kinds self-confirm: the wrapper buffers hits (pendingPdfHits for
   * paged, pendingDirectContentHits for direct) and applies them when the
   * viewer is ready, so it accepts hits the moment the load command is issued.
   * Deferring confirmation to the wrapper's open event would deliver paged
   * hits only after the page had rendered, too late to highlight. The open
   * event remains the source of load-failure reporting (onViewerError).
   */
  private final class WrapperPort implements ViewerPort
  {

  //---------------------------------------------------------------------------

    @Override public void showEmpty()
    {
      wrapper().clearPreview();
    }

  //---------------------------------------------------------------------------

    @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
    {
      if (variant == ProgressVariant.STARTING_CONVERTER)
        wrapper().setStartingConverter();
      else
        wrapper().setGenerating(sourceFile, true);
    }

  //---------------------------------------------------------------------------

    @Override public void showUnable(FilePath sourceFile)
    {
      if (artifacts.noOfficeInstallation())
        wrapper().setNoOfficeInstallation();
      else
        wrapper().setUnable(sourceFile);
    }

  //---------------------------------------------------------------------------

    @Override public void showDocument(long gen, FilePath documentPath, int pageNum)
    {
      // The host may have been cleared (or yielded) after the pane task that
      // issues this command was queued; the intent fields are already null and
      // a setIntent(null) is queued right behind. Drop the command.

      FilePath sourceFile = intentFile;
      if (sourceFile == null) return;

      issuedGen = gen;
      wrapper().paneShowPaged(sourceFile, documentPath, pageNum, intentRecord);

      // Self-confirm, like showContent. The wrapper buffers PDF hits
      // (pendingPdfHits) and drains them inside openDone, before the page
      // renders. Waiting for the actual open event to confirm would deliver
      // hits only after the page had already rendered, too late for the viewer
      // to highlight them. Genuine load failures still surface through the open
      // event as onViewerError.

      pane.onDocumentLoaded(gen, new ViewerMeta(-1));
    }

  //---------------------------------------------------------------------------

    @Override public void showContent(long gen, FilePath contentPath)
    {
      FilePath sourceFile = intentFile;  // See showDocument; a cleared host drops the command
      if (sourceFile == null) return;

      issuedGen = gen;

      if (wrapper().paneShowDirect(sourceFile, contentPath, intentRecord))
        pane.onDocumentLoaded(gen, new ViewerMeta(1));
      else
        pane.onViewerError(gen, "The file kind cannot be shown as direct content");
    }

  //---------------------------------------------------------------------------

    @Override public void setHits(long gen, String hitsJson)
    {
      wrapper().setAllHits(hitsJson);
    }

  //---------------------------------------------------------------------------

    @Override public void clearHits(long gen)
    {
      wrapper().clearAllHits();
    }

  //---------------------------------------------------------------------------

    @Override public void goToPage(long gen, int pageNum)
    {
      wrapper().paneGoToPage(pageNum);
    }

  //---------------------------------------------------------------------------

    @Override public void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage)
    {
      wrapper().scrollToHighlight(matchNdx, pageNum, ndxOnPage);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
