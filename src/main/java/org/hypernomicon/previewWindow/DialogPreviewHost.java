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
import static org.hypernomicon.util.Util.*;

import java.io.IOException;

import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PDFJSWrapper.PDFJSOperation;
import org.hypernomicon.previewWindow.PipelineSnapshot.ArtifactStatus;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.SettleGate;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.view.controls.CollapsibleSplitPane;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Window;
import javafx.stage.WindowEvent;

//---------------------------------------------------------------------------

/**
 * Hosts a transient {@link PreviewPane} reconciler for a dialog-owned preview
 * (WorkDlgCtrlr, SelectWorkDlgCtrlr, MergeWorksDlgCtrlr): the dialog sets a
 * file as intent and the display (document, conversion progress, unable) is
 * derived, never commanded. The host owns the dialog's {@link PDFJSWrapper}
 * outright; there is no {@link PreviewWrapper} and no window chrome, so the
 * {@link ViewerPort} drives the viewer directly, and document loads are
 * confirmed by the viewer's real open event rather than self-confirmed
 * (dialog previews carry no hit sets, so there is no hit-timing reason to
 * confirm early).
 * <p>
 * The viewer is not created along with the host: it waits until the dialog's
 * preview pane is in a scene and laid out (see
 * {@link #createViewerWhenPaneIsLaidOut}), and a file set as intent before then
 * is held until it is.
 * <p>
 * The artifact side (conversion session subscription mapped to
 * {@link ArtifactStatus}) is shared with {@link PreviewPaneHost} through
 * {@link ArtifactTracker}, as is the settle gate.
 * <p>
 * Threading: dialog calls and session display callbacks arrive on the FX
 * thread; the viewer's done handler arrives on a browser thread and only reads
 * the two volatile fields before handing off to the pane's own marshalling.
 */
public final class DialogPreviewHost
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final AnchorPane apPreview;
  private final PreviewPane pane;

  /**
   * The dialog's viewer, created only once {@link #apPreview} is in a scene and
   * has been laid out; see {@link #createViewerWhenPaneIsLaidOut}. Null until
   * then, so every entry point that can reach the {@link Port} checks it first
   * (the {@code Port} methods themselves are unreachable while it is null,
   * because nothing calls into the reconciler before the viewer exists).
   * FX-confined.
   */
  private PDFJSWrapper jsWrapper = null;

  /** The artifact side of the pipeline snapshot; completed artifacts are
   *  leased through the dialog's own viewer. */
  private final ArtifactTracker artifacts;

  private volatile FilePath intentFile = null;

  /** See {@code PreviewPaneHost.suppressSnapshotPush}. FX-confined. */
  private boolean suppressSnapshotPush = false;
  private volatile long issuedGen = 0;

  /** The file the dialog asked for before the viewer existed; the newest one
   *  wins and becomes intent as soon as it does. FX-confined. */
  private FilePath pendingFile = null;

  /** Whether the dialog closed before the viewer was ever created, which cancels
   *  the pending creation (there is nothing to display into). FX-confined. */
  private boolean cleanedUp = false;

  /**
   * Settle gate for this dialog's intents, the same pattern as
   * {@link PreviewPaneHost}. Dialogs preview a single file, so there is no
   * selection to burst through; the gate coalesces repeated updatePreview
   * calls (dialog setup, the preview pane being toggled visible, WorkDlg's
   * source file changing) so only the file they settle on loads or converts.
   * A quiet single call proceeds immediately.
   */
  private final SettleGate settleGate = new SettleGate(150);

//---------------------------------------------------------------------------

  public DialogPreviewHost(AnchorPane apPreview)
  {
    this.apPreview = apPreview;

    // This host's viewer is always dialog-hosted, so creating it is itself the modal attach
    // that BrowserEngine would otherwise prime for; record it so no throwaway is ever made.

    BrowserEngine.noteModalAttach();

    pane = new PreviewPane(new Port(), Platform::runLater);

    // Deliberately a lambda, not jsWrapper::leaseArtifact: a method reference
    // would capture the field eagerly, and it is null until the deferred
    // viewer creation runs. The lambda reads it at lease time, after that.

    artifacts = new ArtifactTracker(session -> jsWrapper.leaseArtifact(session), this::pushSnapshot);

    createViewerWhenPaneIsLaidOut();

    // Detach the browser view just before the hosting dialog's window closes
    // (WINDOW_HIDING fires while the native peer still exists). The dialogs run
    // cleanup() only after showModal() returns, which is after their window has
    // closed, and a closing window that still contains a live BrowserView
    // triggers JxBrowser SceneTracker callbacks that run after the peer is
    // destroyed ("Failed to get native widget ID"); see
    // PDFJSWrapper.detachBrowserView. Registering here rather than in the
    // dialogs also covers application exit while the dialog is open. The scene
    // and window may not exist yet during construction, so hook through the
    // property chain.

    apPreview.sceneProperty().addListener((ob, oldScene, newScene) -> hookWindowHiding(newScene));
    hookWindowHiding(apPreview.getScene());

    // Note on scene exits and re-entries: JxBrowser re-attaches the native
    // surface when its view rejoins a scene, and the attach carries only a
    // size, no position; the surface parks at the window origin and moves
    // only when a later change event (view transform or size, window location)
    // triggers a bounds recompute. A pane that rejoins at exactly its former
    // position and size fires no such event, stranding the surface at the
    // origin. WorkDlgCtrlr and MergeWorksDlgCtrlr therefore keep this pane in
    // the scene permanently (CollapsibleSplitPane, collapsed to a sliver when
    // toggled off); SelectWorkDlgCtrlr does detach and re-add its pane, but
    // moves and resizes its stage on every toggle, and the window-location
    // events from that (applied asynchronously by the window manager, hence
    // reliably after the native re-attach) push correct bounds. Any future
    // host must preserve one of those two properties.
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void hookWindowHiding(Scene scene)
  {
    if (scene == null) return;

    scene.windowProperty().addListener((ob, oldWindow, newWindow) -> hookWindowHiding(newWindow));
    hookWindowHiding(scene.getWindow());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void hookWindowHiding(Window window)
  {
    if (window == null) return;

    window.addEventHandler(WindowEvent.WINDOW_HIDING, event ->
    {
      if (jsWrapper != null)
        jsWrapper.detachBrowserView();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates the viewer once the preview pane is in a scene and has been laid
   * out at a real size, which is not the case when the dialogs construct this
   * host.
   * <p>
   * All three hosting dialogs build the host from a listener that fires the
   * moment the preview is toggled on, but the pane they hand over has no real
   * size yet: in {@code WorkDlgCtrlr} and {@code MergeWorksDlgCtrlr} it sits in
   * a {@link CollapsibleSplitPane}, still pinned to its collapsed 1px sliver
   * until the expansion's layout pass runs, and {@code SelectWorkDlgCtrlr} adds
   * the pane to its root a layout pass before it has any size. On top of that
   * the dialogs widen their stage in that same call, and the window resize is
   * asynchronous on Linux.
   * <p>
   * A {@code HARDWARE_ACCELERATED} browser view is a native window that does not
   * clip to JavaFX bounds and does not take part in JavaFX z-order (see
   * {@link BrowserEngine}), so one attached to a detached or sliver-sized pane
   * never acquires correct geometry: it was observed floating over the whole
   * dialog at the stage's pre-widen client size, covering the controls behind
   * it. Waiting for the pane to be real is what keeps the surface confined to
   * it.
   */
  private void createViewerWhenPaneIsLaidOut()
  {
    if (paneIsLaidOut())
    {
      createViewer();
      return;
    }

    apPreview.layoutBoundsProperty().addListener(paneReadyHndlr);
    apPreview.sceneProperty       ().addListener(paneReadyHndlr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * A {@code ChangeListener}, not an {@code InvalidationListener}: layout bounds
   * are a lazily validated property, so an invalidation listener that does not
   * read the new bounds is notified once and then stays silent through every
   * later change (observed as the viewer never being created at all). A change
   * listener validates the property on every notification.
   */
  private final ChangeListener<Object> paneReadyHndlr = new ChangeListener<>()
  {
    @Override public void changed(ObservableValue<?> ob, Object oldValue, Object newValue)
    {
      if ((jsWrapper != null) || cleanedUp || (paneIsLaidOut() == false)) return;

      apPreview.layoutBoundsProperty().removeListener(this);
      apPreview.sceneProperty       ().removeListener(this);

      // One more pulse, so the layout pass that gave the pane this size has been
      // applied in full before the native surface is created against it.

      runInFXThreadAfterPulses(1, DialogPreviewHost.this::createViewer);
    }
  };

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean paneIsLaidOut()
  {
    // Strictly above the collapsed-sliver width: in WorkDlgCtrlr and
    // MergeWorksDlgCtrlr the pane sits in a CollapsibleSplitPane, permanently
    // in the scene and pinned to a 1px sliver while the preview is toggled
    // off, and a viewer must not be created against that.

    return (apPreview.getScene() != null)
        && (apPreview.getWidth() > CollapsibleSplitPane.COLLAPSED_DETAIL_WIDTH)
        && (apPreview.getHeight() > 0.0);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void createViewer()
  {
    if (cleanedUp || (jsWrapper != null)) return;

    // The preview can be toggled back off within the pulse this waited out,
    // collapsing the pane back to its sliver (or detaching it, in
    // SelectWorkDlgCtrlr); wait for the next time it is real rather than
    // attaching to nothing.

    if (paneIsLaidOut() == false)
    {
      createViewerWhenPaneIsLaidOut();
      return;
    }

    jsWrapper = new PDFJSWrapper(apPreview, this::onViewerDone, null, null);

    // Whatever the dialog asked for while the viewer did not exist becomes intent now

    if (pendingFile == null) return;

    FilePath filePath = pendingFile;
    pendingFile = null;

    setPreviewNow(filePath);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets the file this dialog's preview should show, through the settle gate;
   * an empty path clears the preview immediately. For office documents the
   * host starts (or joins) the conversion and feeds its status to the
   * reconciler; for natively-viewable files the artifact is the source file
   * itself.
   */
  public void setPreview(FilePath filePath)
  {
    if (FilePath.isEmpty(filePath))
    {
      settleGate.cancel();
      clearNow();
      return;
    }

    settleGate.request(() -> setPreviewNow(filePath));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Releases the conversion subscription and disposes the viewer. Call when the dialog closes. */
  public void cleanup()
  {
    cleanedUp = true;  // cancels the pending viewer creation, if the dialog closed before it ran

    apPreview.layoutBoundsProperty().removeListener(paneReadyHndlr);
    apPreview.sceneProperty       ().removeListener(paneReadyHndlr);

    settleGate.cancel();
    artifacts.drop();
    intentFile = null;
    pendingFile = null;

    if (jsWrapper != null)
      jsWrapper.cleanup();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void clearNow()
  {
    pendingFile = null;
    artifacts.drop();
    intentFile = null;

    if (jsWrapper == null) return;  // nothing was ever issued; see the field's note

    pane.setIntent(null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setPreviewNow(FilePath filePath)
  {
    if (jsWrapper == null)
    {
      pendingFile = filePath;  // createViewer picks it up; a later file replaces it first
      return;
    }

    boolean sameFile = filePath.equals(intentFile);

    intentFile = filePath;

    if (sameFile == false)
    {
      // Suppress the subscription's immediate display callback; see
      // PreviewPaneHost.suppressSnapshotPush (the atomic set below carries the state)

      suppressSnapshotPush = true;

      try     { artifacts.trackNewFile(filePath, this); }
      finally { suppressSnapshotPush = false; }
    }

    // Intent and snapshot together in a single reconcile, like the pane hosts:
    // an instant-ready file goes straight to its document.

    pane.setIntentAndPipeline(
      new PreviewIntent(filePath, PreviewIntent.kindFor(filePath), 1, false),
      new PipelineSnapshot(filePath, artifacts.status(), DocumentArtifactService.converterState(), null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pushSnapshot()
  {
    if (suppressSnapshotPush || (jsWrapper == null)) return;  // see setPreviewNow

    FilePath sourceFile = intentFile;
    if (sourceFile == null) return;

    pane.updatePipeline(new PipelineSnapshot(sourceFile, artifacts.status(), DocumentArtifactService.converterState(), null));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The viewer's open event; the pane confirms or fails the current generation from it. */
  private void onViewerDone(PDFJSOperation operation, boolean success, String errMessage)
  {
    if ((operation != PDFJSOperation.pjsOpen) || (intentFile == null)) return;

    if (success)
      pane.onDocumentLoaded(issuedGen, new ViewerMeta(-1));
    else
      pane.onViewerError(issuedGen, strNullOrBlank(errMessage) ? "The viewer could not open the document" : errMessage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The {@link ViewerPort} directly over the dialog's {@link PDFJSWrapper}.
   * Paged loads are confirmed by the viewer's open event (see
   * {@link #onViewerDone}); direct content self-confirms on a successful
   * navigation, since navigations report no completion to the host.
   */
  private final class Port implements ViewerPort
  {
    @Override public void showEmpty()
    {
      jsWrapper.reset();
    }

    @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
    {
      if (variant == ProgressVariant.STARTING_CONVERTER)
        jsWrapper.setStartingConverter();
      else
        jsWrapper.setGenerating(sourceFile, true);
    }

    @Override public void showUnable(FilePath sourceFile)
    {
      if (artifacts.noOfficeInstallation())
        jsWrapper.setNoOfficeInstallation();
      else
        jsWrapper.setUnable(sourceFile);
    }

    @Override public void showDocument(long gen, FilePath documentPath, int pageNum)
    {
      if (intentFile == null) return;  // cleared after the command was queued; a setIntent(null) is right behind

      issuedGen = gen;

      jsWrapper.setContentToShowIsDirect(false);
      jsWrapper.loadPdf(documentPath, pageNum);
    }

    @Override public void showContent(long gen, FilePath contentPath)
    {
      if (intentFile == null) return;  // see showDocument

      issuedGen = gen;

      try
      {
        if (jsWrapper.loadDirectContent(contentPath))
          pane.onDocumentLoaded(gen, new ViewerMeta(1));
        else
          pane.onViewerError(gen, "The file kind cannot be shown as direct content");
      }
      catch (IllegalStateException | IOException e)
      {
        pane.onViewerError(gen, "The file could not be loaded");
      }
    }

    @Override public void setHits(long gen, String hitsJson)
    {
      // Dialog previews carry no hit sets
    }

    @Override public void clearHits(long gen)
    {
      // Dialog previews carry no hit sets
    }

    @Override public void goToPage(long gen, int pageNum)
    {
      jsWrapper.goToPage(pageNum);
    }

    @Override public void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage)
    {
      // Dialog previews carry no hit sets
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
