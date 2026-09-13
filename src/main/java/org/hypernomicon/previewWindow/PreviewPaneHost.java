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

import static org.hypernomicon.App.*;

import java.util.concurrent.Executor;
import java.util.function.Supplier;

import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.PreviewIntent.ContentKind;
import org.hypernomicon.previewWindow.PreviewWrapper.PaneEventSink;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.RequestGate;
import org.hypernomicon.util.SettleGate;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;

//---------------------------------------------------------------------------

/**
 * Hosts the {@link PreviewPane} reconciler for one preview pane; one
 * instance per {@link PreviewWindow.PreviewSource}, obtained via
 * {@link PreviewWindow#hostFor}. The reconciler, the settle gate, the artifact
 * tracking, and the attribution of viewer reports are the {@link PreviewHostCore}
 * this host owns; what it adds is the record behind the intent, the FTS hit
 * flow, and the {@link ViewerPort} over the pane's {@link PaneViewer} (the
 * {@link PreviewWrapper}, whose load methods keep the window controls, nav
 * history, and work-page bookkeeping fed).
 * <p>
 * Record-navigation panes use {@link #setPreviewAuto} (no highlighting, content
 * kind derived from the mimetype). The queries pane additionally drives the FTS
 * flow: {@code FTSQueryCtrlr} sets intent via {@link #setPreview} and pushes
 * hit-set results (the {@code updateHits*} methods) through the
 * {@link PreviewWindow} facade. Window-chrome actions (page navigation,
 * refresh) arrive as intent updates too ({@link #navigateToPage},
 * {@link #refresh}); nothing displays anything except by setting intent.
 * <p>
 * Threading: everything here runs on the FX thread (controller calls, session
 * display callbacks, and pane executor tasks all marshal there), except the
 * viewer's event sink, which arrives on browser threads and hands straight to
 * the core's identity gate.
 * <p>
 * The gate, the pane's executor, and the viewer surface are collaborators
 * (the production constructor supplies the settle gate, the FX thread, and the
 * pane's {@link PreviewWrapper}), so {@code PreviewPaneHostTest} runs the
 * host's rules with a held gate, a direct executor, and a recording viewer.
 */
final class PreviewPaneHost
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * What this host drives: the pane's viewer surface, which loads documents
   * and status displays, applies hits, and reports the viewer's events back
   * through the sink it is given. {@link PreviewWrapper} in production; a
   * recording fake in the host's contract tests.
   */
  interface PaneViewer
  {
    /** Creates the viewer if it does not exist yet; false if it cannot (the browser engine is unavailable). */
    boolean ensureInitialized();

    void setPaneEventSink(PaneEventSink sink);

    void clearPreview();
    void paneShowProgress(FilePath sourceFile, HDT_Record record, ProgressVariant variant);
    void paneShowUnable(FilePath sourceFile, HDT_Record record, boolean noOfficeInstallation);
    void paneShowPaged(FilePath sourceFile, FilePath displayPath, int pageNum, HDT_Record record);

    /** @return false if the file kind cannot be shown as direct content (nothing was loaded) */
    boolean paneShowDirect(FilePath sourceFile, FilePath displayPath, HDT_Record record);

    void paneGoToPage(int pageNum);
    void setAllHits(String hitsJson);
    void clearAllHits();
    void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage);

    /** Leases a completed conversion's artifact against cache eviction while displayed. */
    void leaseArtifact(ConversionSession session);

    /** Reloads the embedded browser, then runs {@code done} (from a browser thread). */
    void reloadViewer(Runnable done);
  }

//---------------------------------------------------------------------------

  private final PreviewWindow.PreviewSource src;
  private final PreviewHostCore core;
  private final Executor paneExecutor;
  private final Supplier<PaneViewer> viewerSupplier;

  private HDT_Record intentRecord = null;
  private HitsStatus hitsStatus = null;

  /**
   * The file of the most recent {@link #setPreview} request, recorded before
   * the settle gate, and hit results that arrived for it while its intent was
   * still gated. The FTS initiator pushes hits synchronously right after
   * requesting an intent, so when the gate defers that intent, the hits reach
   * {@link #updateHits} while the intent still names the previous file; they
   * are early, not stale, and dropping them left the deferred intent stuck on
   * a Pending hit status forever (nothing re-pushes). {@link #setPreviewNow}
   * consumes the stash in place of Pending. Both FX-confined, like the gate
   * that makes them necessary.
   */
  private FilePath requestedFile = null;
  private HitsStatus requestedFileHits = null;

  /** The viewer's events, handed to the core's identity gate. Installed on
   *  the viewer at every intent set: cheap, and a viewer that the window
   *  replaced gets it too. */
  private final PaneEventSink eventSink = new PaneEventSink()
  {
    @Override public void onOpened(FilePath file, boolean success, ViewerMeta meta) { core.onOpened(file, success, meta, null); }
    @Override public void onPageChanged(FilePath file, int pageNum)                 { core.onPageChanged(file, pageNum); }
  };

//---------------------------------------------------------------------------

  PreviewPaneHost(PreviewWindow.PreviewSource src)
  {
    this(src, new SettleGate(150), Platform::runLater, () -> PreviewWindow.wrapperForSource(src));
  }

  /**
   * @param settleGate     gates this pane's intents: rapid intent changes
   *                       (key-repeat selection reaching this host per
   *                       selection) must not each set an intent, subscribe to
   *                       a conversion, and cycle the display; only the file
   *                       the selection settles on does. A quiet-selection
   *                       intent proceeds immediately, so gated upstream
   *                       callers (the FTS controller's own settle gate) and
   *                       deliberate single selections never wait here
   * @param paneExecutor   executor the reconciler runs on: the FX thread in
   *                       production, a direct executor in tests
   * @param viewerSupplier the pane's viewer surface, or null while the window
   *                       that owns it does not exist
   */
  PreviewPaneHost(PreviewWindow.PreviewSource src, RequestGate settleGate, Executor paneExecutor, Supplier<PaneViewer> viewerSupplier)
  {
    this.src = src;
    this.paneExecutor = paneExecutor;
    this.viewerSupplier = viewerSupplier;

    core = new PreviewHostCore(new WrapperPort(), paneExecutor, settleGate, session -> viewer().leaseArtifact(session), () -> hitsStatus);
  }

//---------------------------------------------------------------------------

  private PaneViewer viewer() { return viewerSupplier.get(); }

  /** The file whose load the viewer last confirmed for this pane, or
   *  {@code null}; the sanctioned read of "what is this pane showing". */
  FilePath confirmedFile() { return core.confirmedFile(); }

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

    core.gate().request(() -> setPreviewNow(filePath, record, paged, pageNum, wantsHighlights, scrollTarget));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreviewNow(FilePath filePath, HDT_Record record, boolean paged, int pageNum, boolean wantsHighlights, ScrollTarget scrollTarget)
  {
    if (debugging())
      System.out.println("PreviewPaneHost[" + src + "].setPreviewNow EXECUTE: " + filePath.getNameOnly()
        + " (replacing intent=" + (core.intentFile() == null ? "null" : core.intentFile().getNameOnly()) + ')');

    PaneViewer viewer = viewer();
    if ((viewer == null) || (viewer.ensureInitialized() == false)) return;

    viewer.setPaneEventSink(eventSink);

    // Hits that arrived while this intent was gated; only meaningful for the
    // execution of the request they were stashed under, so always consumed

    HitsStatus earlyHits = filePath.equals(requestedFile) ? requestedFileHits : null;
    requestedFileHits = null;

    intentRecord = record;

    if (filePath.equals(core.intentFile()) == false)
      hitsStatus = wantsHighlights ? (earlyHits != null ? earlyHits : HitsStatus.PENDING) : null;

    core.setIntent(new PreviewIntent(filePath, paged ? ContentKind.PAGED : ContentKind.DIRECT, pageNum, wantsHighlights, scrollTarget), viewer);
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
    updateHits(filePath, HitsStatus.FAILED);
  }

//---------------------------------------------------------------------------

  private void updateHits(FilePath filePath, HitsStatus newStatus)
  {
    FilePath intentFile = core.intentFile();

    if (filePath.equals(intentFile) == false)
    {
      if (filePath.equals(requestedFile))
      {
        // Early, not stale: computed for a request whose intent is still
        // waiting in the settle gate. Stash it for setPreviewNow to consume;
        // dropping it would leave that intent Pending forever.

        if (debugging())
          System.out.println("PreviewPaneHost[" + src + "].updateHits: STASHED " + newStatus.getClass().getSimpleName() +
                             " for gated request " + filePath.getNameOnly() +
                             " (intent=" + (intentFile == null ? "null" : intentFile.getNameOnly()) + ')');

        requestedFileHits = newStatus;
        return;
      }

      // Stale by value; a different file is intended now

      if (debugging())
        System.out.println("PreviewPaneHost[" + src + "].updateHits: DROPPED for " + filePath.getNameOnly() +
                           " (intent=" + (intentFile == null ? "null" : intentFile.getNameOnly()) + ')');

      return;
    }

    hitsStatus = newStatus;
    core.pushSnapshot();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * User-driven refresh: reloads the embedded browser, then re-issues the
   * current display from intent (fresh generation; hits re-ship after the
   * reloaded document confirms). No-op without an active intent.
   */
  void refresh()
  {
    if (core.intentFile() == null) return;

    viewer().reloadViewer(() -> paneExecutor.execute(core.pane()::refreshDisplay));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Chrome-driven page navigation within the currently intended document
   * (slider, page buttons, page field, nav history, ContentsWindow): re-sets
   * the intent's page. Deliberate single actions, so the settle gate is
   * bypassed. No-op without an active intent.
   */
  void navigateToPage(int pageNum)
  {
    if ((core.intentFile() == null) || (pageNum < 1)) return;

    core.pane().setIntentPage(pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Clears the pane's preview (intent = none; the viewer empties). */
  void clear()
  {
    core.gate().cancel();

    requestedFile = null;      // the stash dies with the cancelled gate request
    requestedFileHits = null;
    hitsStatus = null;
    intentRecord = null;

    core.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The {@link ViewerPort} over the {@link PaneViewer}. Every command
   * corresponds to the current intent (the pane is single-threaded on the FX
   * thread), so the viewer's source-file tracking is read from the host
   * fields.
   * <p>
   * Both load kinds are confirmed by the viewer's real completion events
   * (openDone for paged documents, the direct navigation finishing for direct
   * content), matched by document identity in the core; hits and scroll
   * targets are then pushed by the reconciler observing the confirmation, and
   * apply directly (the JS injection is render-idempotent, so hits arriving
   * after pages render still highlight).
   */
  private final class WrapperPort implements ViewerPort
  {

  //---------------------------------------------------------------------------

    // The non-document views tell the core no document is issued (see
    // PreviewHostCore.issuingStatus), and a cleared host drops them like the
    // document views: the intent fields are already null and a setIntent(null)
    // is queued right behind. The status displays track the file like the
    // document displays do, so the window's controls name the intended file
    // from the moment it is issued.

    @Override public void showEmpty()
    {
      core.issuingStatus();

      PaneViewer viewer = viewer();  // a host cleared before its window ever existed has nothing to empty

      if (viewer != null)
        viewer.clearPreview();
    }

  //---------------------------------------------------------------------------

    @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
    {
      if (core.issuingStatus() == false) return;

      viewer().paneShowProgress(sourceFile, intentRecord, variant);
    }

  //---------------------------------------------------------------------------

    @Override public void showUnable(FilePath sourceFile)
    {
      if (core.issuingStatus() == false) return;

      viewer().paneShowUnable(sourceFile, intentRecord, core.noOfficeInstallation());
    }

  //---------------------------------------------------------------------------

    @Override public void showDocument(long gen, FilePath documentPath, int pageNum)
    {
      if (core.issuingDocument(gen, documentPath) == false) return;

      viewer().paneShowPaged(core.intentFile(), documentPath, pageNum, intentRecord);
    }

  //---------------------------------------------------------------------------

    @Override public void showContent(long gen, FilePath contentPath)
    {
      if (core.issuingDocument(gen, contentPath) == false) return;

      if (viewer().paneShowDirect(core.intentFile(), contentPath, intentRecord) == false)
        core.pane().onViewerError(gen, "The file kind cannot be shown as direct content");
    }

  //---------------------------------------------------------------------------

    @Override public void setHits(long gen, String hitsJson)
    {
      viewer().setAllHits(hitsJson);
    }

  //---------------------------------------------------------------------------

    @Override public void clearHits(long gen)
    {
      viewer().clearAllHits();
    }

  //---------------------------------------------------------------------------

    @Override public void goToPage(long gen, int pageNum)
    {
      viewer().paneGoToPage(pageNum);
    }

  //---------------------------------------------------------------------------

    @Override public void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage)
    {
      viewer().scrollToHighlight(matchNdx, pageNum, ndxOnPage);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
