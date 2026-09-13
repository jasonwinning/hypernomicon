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

import java.io.IOException;

import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PDFJSWrapper.PDFJSOperation;
import org.hypernomicon.previewWindow.PreviewPaneHost.PaneViewer;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.file.FilePath;

import javafx.scene.control.Tab;
import javafx.scene.control.ToggleButton;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

/**
 * One preview pane's viewer in the Preview Window: owns the pane's
 * {@link PDFJSWrapper}, drives it with the display commands its
 * {@link PreviewPaneHost} issues, and forwards the viewer's events to the
 * host. Nothing is decided here, and nothing about records, history, or the
 * window's controls lives here; that is the host's. The window creates one
 * per source and attaches it to the source's host.
 */
final class PreviewWrapper implements PaneViewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Viewer lifecycle events forwarded to the host driving this wrapper; the
   * host attributes them to the document they describe.
   */
  interface PaneEventSink
  {
    /**
     * A document load completed (a pdf.js open, or a direct-content navigation
     * finishing). {@code file} is the document the load was for; the consumer
     * must match it against what it issued, because a superseded open still
     * reports here before the newest request's load has run. {@code meta}
     * describes the loaded document on success and is null on failure.
     */
    void onOpened(FilePath file, boolean success, ViewerMeta meta);

    /**
     * The viewer's current page changed (user scrolling, or a page the viewer
     * was told to show). {@code file} is the document the viewer reported the
     * page for; the consumer must match it against what it issued, because a
     * document's page events can still arrive after a newer document was
     * issued in its place (its pages finished loading and the viewer jumped
     * to the requested page just as the next selection superseded it).
     */
    void onPageChanged(FilePath file, int pageNum);
  }

//---------------------------------------------------------------------------

  private final ToggleButton btn;
  private final AnchorPane ap;
  private boolean initialized = false;
  private PDFJSWrapper jsWrapper;
  private PaneEventSink paneEventSink = null;
  private ConversionSession leasedArtifactSession = null;

//---------------------------------------------------------------------------

  PreviewWrapper(AnchorPane ap, Tab tab, ToggleButton btn, PreviewWindow window)
  {
    this.btn = btn;
    this.ap = ap;

    btn.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue)) window.tpPreview.getSelectionModel().select(tab);
    });
  }

//---------------------------------------------------------------------------

  @Override public void setPaneEventSink(PaneEventSink sink) { paneEventSink = sink; }
  @Override public void markSelected()                       { btn.setSelected(true); }
  @Override public boolean zoom(boolean zoomingIn)           { return (jsWrapper != null) && jsWrapper.zoom(zoomingIn); }
  @Override public void showEmpty()                          { if (initialized) jsWrapper.reset(); }
  @Override public void goToPage(int pageNum)                { if (initialized) jsWrapper.goToPage(pageNum); }
  @Override public void clearAllHits()                       { if (initialized) jsWrapper.clearAllHits(); }

  @Override public void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage) { if (initialized) jsWrapper.scrollToHighlight(matchNdx, pageNum, ndxOnPage); }

  void prepareToHide()                                       { if (initialized) jsWrapper.prepareToHide(); }
  void prepareToShow()                                       { if (initialized) jsWrapper.prepareToShow(); }

  /** Shutdown-only: detach the browser view from the scene graph before the
   *  preview stage closes. See {@link PDFJSWrapper#detachBrowserView()}. */
  void detachBrowserView()                                   { if (initialized) jsWrapper.detachBrowserView(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void doneHndlr(PDFJSOperation operation, FilePath file, boolean success, String errMessage, ViewerMeta meta)
  {
    if (paneEventSink != null)
      paneEventSink.onOpened(file, success, meta);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pageChangeHndlr(FilePath file, int newPageNum)
  {
    if (paneEventSink != null)
      paneEventSink.onPageChanged(file, newPageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void initJS()
  {
    if (jxBrowserDisabled) return;

    // This pane's viewer attaches into the (non-modal) Preview Window. On macOS the
    // process's first BrowserView attach has to happen in a modal window or a later attach
    // in a modal dialog can kill the JVM, so make a throwaway modal attach first if nothing
    // else has. No-op elsewhere, and after the first time.

    BrowserEngine.primeModalAttach();

    jsWrapper = new PDFJSWrapper(ap, this::doneHndlr, this::pageChangeHndlr);

    if (jxBrowserDisabled) return;

    initialized = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Lazy-initializes the underlying jsWrapper if it hasn't been created yet.
   *  Returns {@code true} if the wrapper is initialized after the call (which
   *  is always the case unless JxBrowser is disabled). */
  @Override public boolean ensureInitialized()
  {
    if (initialized == false)
      initJS();

    return initialized;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Creates and warms this pane's browser if it has never been initialized: the
   * surface paints viewer.html with the idle overlay before any preview intent
   * arrives, so no never-painted (black) surface is ever user-visible. No-op
   * once initialized, so an active pane's content is never disturbed; browsers
   * are created only for panes the user actually activates (renderer memory
   * stays bounded on small machines).
   */
  @Override public void warmUp()
  {
    if (initialized) return;

    if (ensureInitialized())
      jsWrapper.showIdle();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Reloads the embedded browser (recreating the viewer page), then runs
   *  {@code done}; the caller re-issues the display afterward. */
  @Override public void reloadViewer(Runnable done)
  {
    if (initialized)
      jsWrapper.reloadBrowser(done);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Loads a paged document (a native PDF, or the PDF output of an office
   *  conversion) into the pdf.js viewer at the given page. */
  @Override public void showPaged(FilePath displayPath, int pageNum)
  {
    if (ensureInitialized() == false) return;

    jsWrapper.setContentToShowIsDirect(false);
    jsWrapper.loadPdf(displayPath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Loads {@code displayPath} as direct browser content (HTML, plain text,
   * image, media).
   *
   * @return true if content was loaded; false if the file kind cannot be
   *         previewed or loading failed (the unable indicator names
   *         {@code sourceFile})
   */
  @Override public boolean showDirect(FilePath sourceFile, FilePath displayPath)
  {
    if (ensureInitialized() == false) return false;

    try
    {
      if (jsWrapper.loadDirectContent(displayPath))
        return true;

      jsWrapper.setContentToShowIsDirect(false);
      jsWrapper.setUnable(sourceFile);
    }
    catch (IllegalStateException | IOException e)
    {
      jsWrapper.setUnable(sourceFile);
    }

    return false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
  {
    if (ensureInitialized() == false) return;

    if (variant == ProgressVariant.STARTING_CONVERTER)
      jsWrapper.setStartingConverter();
    else
      jsWrapper.setGenerating(sourceFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void showUnable(FilePath sourceFile, boolean noOfficeInstallation)
  {
    if (ensureInitialized() == false) return;

    if (noOfficeInstallation)
      jsWrapper.setNoOfficeInstallation();
    else
      jsWrapper.setUnable(sourceFile);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Push FTS hit JSON to the underlying jsWrapper. The reconciler delivers
   *  hits only after the intended document's load is confirmed, which implies
   *  the wrapper is initialized; a push before then is a caller bug. */
  @Override public void setAllHits(String allHitsJson)
  {
    if (initialized)
      jsWrapper.setAllHits(allHitsJson);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records that this pane is displaying the given session's artifact, leasing
   * it against cache eviction and releasing the lease on whatever artifact this
   * pane displayed before. Called on the FX thread (display callbacks).
   */
  @Override public void leaseArtifact(ConversionSession session)
  {
    if (leasedArtifactSession == session) return;

    if (leasedArtifactSession != null)
      leasedArtifactSession.release();

    leasedArtifactSession = session;
    session.lease();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void cleanup(Runnable disposeHndlr)
  {
    if (leasedArtifactSession != null)
    {
      leasedArtifactSession.release();
      leasedArtifactSession = null;
    }

    OfficePreviewer.cleanup();

    if (initialized)
      jsWrapper.cleanup(disposeHndlr);
    else
      disposeHndlr.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
