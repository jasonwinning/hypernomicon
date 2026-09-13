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

import static org.hypernomicon.util.StringUtil.*;

import java.util.concurrent.Executor;
import java.util.function.Consumer;
import java.util.function.Supplier;

import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.RequestGate;
import org.hypernomicon.util.file.FilePath;

//---------------------------------------------------------------------------

/**
 * What every preview host has in common, owned by composition: the
 * {@link PreviewPane} reconciler, the gate its intents pass through, the
 * {@link ArtifactTracker} that turns the intent file into the artifact side of
 * the pipeline snapshot, and the bookkeeping that attributes the viewer's
 * reports to the document they describe. A host adds what it alone knows (the
 * {@link PreviewPaneHost} its records, hits, and window chrome; the
 * {@link DialogPreviewHost} its deferred viewer creation), supplies the
 * {@link ViewerPort} the pane drives, and calls back here from that port when
 * it issues a document or a status display.
 * <p>
 * Threading: everything runs on the pane's executor (the FX thread in
 * production), except {@link #onOpened} and {@link #onPageChanged}, which
 * arrive on browser threads and only read the volatile fields before handing
 * off to the pane's own marshalling.
 */
final class PreviewHostCore
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final String VIEWER_OPEN_FAILURE = "The viewer could not open the document";

  private final PreviewPane pane;
  private final RequestGate gate;
  private final ArtifactTracker artifacts;
  private final Supplier<HitsStatus> hits;

  private volatile FilePath intentFile = null, issuedDisplayPath = null;
  private volatile long issuedGen = 0;

  /** True while the artifact tracker takes up a new file inside
   *  {@link #setIntent}: the new subscription's immediate display callback
   *  would otherwise queue a pipeline update built against the PREVIOUS intent
   *  one pane-executor task ahead of the atomic intent-plus-snapshot set
   *  (observed as a spurious Empty/Progress issue plus a wasted viewer reset at
   *  first preview). The atomic set that follows carries the same artifact
   *  state, so nothing is lost. FX-confined. */
  private boolean suppressSnapshotPush = false;

//---------------------------------------------------------------------------

  /**
   * @param port           the host's viewer port, which the pane drives
   * @param paneExecutor   executor the pane runs on
   * @param gate           gates the host's intents
   * @param artifactLeaser leases a completed conversion's artifact against
   *                       cache eviction while the host displays it
   * @param hits           the host's current hit status for the snapshot
   *                       (always null for hosts without hit sets)
   */
  PreviewHostCore(ViewerPort port, Executor paneExecutor, RequestGate gate, Consumer<ConversionSession> artifactLeaser, Supplier<HitsStatus> hits)
  {
    pane = new PreviewPane(port, paneExecutor);
    this.gate = gate;
    this.hits = hits;

    artifacts = new ArtifactTracker(artifactLeaser, this::pushSnapshot);
  }

//---------------------------------------------------------------------------

  PreviewPane pane()              { return pane; }
  RequestGate gate()              { return gate; }

  /** The intended source file, or null when the host has no intent. Volatile: read by viewer reports on browser threads. */
  FilePath intentFile()           { return intentFile; }

  /** The file whose load the viewer last confirmed, or null; the sanctioned read of "what is this host showing". */
  FilePath confirmedFile()        { return pane.currentFile(); }

  boolean noOfficeInstallation()  { return artifacts.noOfficeInstallation(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Makes {@code intent} the host's intent. When its file differs from the
   * current intent's, the artifact tracker takes up the new file first (a
   * conversion starts or is joined; a natively viewable file is its own
   * artifact). Intent and snapshot then reach the pane together in a single
   * reconcile: an instant-ready file goes straight to its document instead of
   * flashing a one-cycle Progress from the staleness guard scoring the new
   * intent against the previous file's snapshot. Later conversion-state
   * changes arrive as separate {@link #pushSnapshot} updates.
   *
   * @param consumerKey the host's display-slot key on a conversion session
   */
  void setIntent(PreviewIntent intent, Object consumerKey)
  {
    FilePath filePath = intent.sourceFile();

    boolean sameFile = filePath.equals(intentFile);

    intentFile = filePath;

    if (sameFile == false)
    {
      suppressSnapshotPush = true;
      try     { artifacts.trackNewFile(filePath, consumerKey); }
      finally { suppressSnapshotPush = false; }
    }

    pane.setIntentAndPipeline(intent, snapshot());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Pushes a fresh pipeline snapshot to the pane (artifact or hit status changed). */
  void pushSnapshot()
  {
    if (suppressSnapshotPush || (intentFile == null)) return;

    pane.updatePipeline(snapshot());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PipelineSnapshot snapshot()
  {
    return new PipelineSnapshot(intentFile, artifacts.status(), DocumentArtifactService.converterState(), hits.get());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Clears the intent: the artifact is released and the pane empties the viewer. */
  void clear()
  {
    drop();
    pane.setIntent(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Releases the artifact and forgets the intent without touching the pane
   *  (the host is being torn down, or has no viewer to empty). */
  void drop()
  {
    artifacts.drop();
    intentFile = null;
    issuedDisplayPath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The host's port is about to issue a document. Records which, so the
   * viewer's reports can be attributed to it.
   * @return false if the host has no intent (it was cleared after the pane
   *         task issuing this command was queued; a {@code setIntent(null)}
   *         is right behind), in which case the port must drop the command
   */
  boolean issuingDocument(long gen, FilePath displayPath)
  {
    if (intentFile == null) return false;

    issuedGen = gen;
    issuedDisplayPath = displayPath;

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The host's port is about to issue a non-document view (empty, progress,
   * unable). No document is issued then, so a late report for the previously
   * issued one must fail the identity gate rather than match a stale path.
   * @return whether the host has an intent (see {@link #issuingDocument})
   */
  boolean issuingStatus()
  {
    issuedDisplayPath = null;

    return intentFile != null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The viewer's load report, from a browser thread. Confirmed by document
   * identity, not arrival order: latest-wins open coalescing means a superseded
   * document's open can complete (and report here) after a newer document was
   * issued, and it must not confirm the newer generation.
   * @param errMessage the viewer's own failure message, or null/blank for the generic one
   */
  void onOpened(FilePath file, boolean success, ViewerMeta meta, String errMessage)
  {
    if (intentFile == null) return;

    if ((file == null) || (file.equals(issuedDisplayPath) == false)) return;

    if (success)
      pane.onDocumentLoaded(issuedGen, meta);
    else
      pane.onViewerError(issuedGen, strNullOrBlank(errMessage) ? VIEWER_OPEN_FAILURE : errMessage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The viewer's page-change report, from a browser thread, gated like
   * {@link #onOpened}. The generation stamped is the current one, read at
   * arrival, so it cannot tell a late page event from the outgoing document
   * apart from one belonging to the document issued after it; the document
   * named by the event can.
   */
  void onPageChanged(FilePath file, int pageNum)
  {
    if (intentFile == null) return;

    if ((file == null) || (file.equals(issuedDisplayPath) == false)) return;

    pane.onPageChanged(issuedGen, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
