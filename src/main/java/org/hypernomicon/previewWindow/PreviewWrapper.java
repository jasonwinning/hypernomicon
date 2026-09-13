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

import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.App.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.io.IOException;
import java.util.*;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PDFJSWrapper.PDFJSOperation;
import org.hypernomicon.previewWindow.PreviewNavHistory.Entry;
import org.hypernomicon.previewWindow.PreviewPaneHost.PaneViewer;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;

//---------------------------------------------------------------------------

/**
 * One preview pane's viewer host and window-facing state: owns the pane's
 * {@link PDFJSWrapper}, executes the load/page/hit commands its
 * {@link PreviewPaneHost} issues, and keeps everything the Preview Window's
 * controls read (current file and record, page and page-label metadata,
 * annotated pages, work start/end pages) plus the pane's navigation history
 * (the file back/forward list and each file's page history), fed by viewer
 * events and intent-driven loads. What to DISPLAY is never decided here; that
 * is the reconciler's job, and setting a pane's intent is the only mutation
 * path.
 */
final class PreviewWrapper implements PaneViewer
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private int pageNum = -1, workStartPageNum = -1, workEndPageNum = -1;
  private final PreviewSource src;
  private final PreviewWindow window;
  private final Tab tab;
  private boolean initialized = false;
  private PDFJSWrapper jsWrapper;

  /** The displayed document's metadata (page count, labels), taken from its
   *  load confirmation; {@link ViewerMeta#PAGELESS} for direct content, for a
   *  status display, and for a document whose load has not confirmed yet. */
  private ViewerMeta meta = ViewerMeta.PAGELESS;

  private List<Integer> hilitePages;
  private final PreviewNavHistory history = new PreviewNavHistory();
  private final ToggleButton btn;
  private final AnchorPane ap;

  /** The file the viewer was last told to load (the source itself, or the
   *  converted artifact of an office document): what the viewer's own events
   *  name their document by. Written by the pane-driven loads on the FX
   *  thread, read by the page-change callback on a browser thread. */
  private volatile FilePath displayPath = null;

  PreviewSource getSource()             { return src; }
  int getPageNum()                      { return pageNum; }
  int getNumPages()                     { return meta.pageCount(); }
  Tab getTab()                          { return tab; }
  FilePath getFilePath()                { return history.currentFile(); }
  int getWorkStartPageNum()             { return workStartPageNum; }
  int getWorkEndPageNum()               { return workEndPageNum; }
  HDT_RecordWithPath getRecord()        { return history.currentRecord(); }
  void prepareToHide()                  { if (initialized) jsWrapper.prepareToHide(); }
  void prepareToShow()                  { if (initialized) jsWrapper.prepareToShow(); }
  @Override public void clearAllHits()  { if (initialized) jsWrapper.clearAllHits(); }

  /** Shutdown-only: detach the browser view from the scene graph before the
   *  preview stage closes. See {@link PDFJSWrapper#detachBrowserView()}. */
  void detachBrowserView()              { if (initialized) jsWrapper.detachBrowserView(); }

  int lowestHilitePage()                { return collEmpty(hilitePages) ? -1 : hilitePages.getFirst(); }
  int highestHilitePage()               { return collEmpty(hilitePages) ? -1 : hilitePages.getLast(); }
  int getPageByLabel(String label)      { return meta.pageForLabel(label); }
  String getLabelByPage(int page)       { return meta.labelForPage(page); }
  boolean zoom(boolean zoomingIn)       { return (jsWrapper != null) && jsWrapper.zoom(zoomingIn); }

  @Override public void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage) { if (initialized) jsWrapper.scrollToHighlight(matchNdx, pageNum, ndxOnPage); }
  boolean enableFileNavButton(boolean isForward) { return history.canStepFile(isForward); }
  boolean enableNavButton    (boolean isForward) { return history.canStepPage(isForward); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  PreviewWrapper(PreviewSource src, AnchorPane ap, Tab tab, ToggleButton btn, PreviewWindow window)
  {
    this.src = src;
    this.tab = tab;
    this.window = window;
    this.btn = btn;
    this.ap = ap;

    btn.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue)) window.tpPreview.getSelectionModel().select(tab);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Viewer lifecycle events forwarded to the reconciler driving this wrapper;
   * the wrapper's own bookkeeping (nav history, window controls) runs
   * alongside, fed by the same events.
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

  private PaneEventSink paneEventSink = null;

  @Override public void setPaneEventSink(PaneEventSink sink) { paneEventSink = sink; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @SuppressWarnings("unused")
  private void doneHndlr(PDFJSOperation operation, FilePath file, boolean success, String errMessage, ViewerMeta meta)
  {
    if (paneEventSink != null)
      paneEventSink.onOpened(file, success, meta);

    // As in pageChangeHndlr: only the displayed document's completion feeds
    // this pane's bookkeeping (metadata, first history entry, controls). The
    // metadata is the report's own: attributing it by reading the viewer's
    // current state here would race the next document's load.

    if ((file == null) || (file.equals(displayPath) == false)) return;

    if (history.current() == null) return;

    if (success)
      this.meta = meta;

    Platform.runLater(() ->
    {
      // The first page of a freshly tracked file enters its page history on
      // load; later pages enter through the page-change event or a chrome jump.

      Entry entry = history.current();

      if ((entry != null) && (entry.hasPages() == false))
        history.recordPage(pageNum);

      refreshControls();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void pageChangeHndlr(FilePath file, int newPageNum)
  {
    if (paneEventSink != null)
      paneEventSink.onPageChanged(file, newPageNum);

    // Only the displayed document's page events feed this pane's bookkeeping. A
    // superseded document's late page jump would otherwise be recorded against
    // the file that replaced it: shown in the page field, entered in its page
    // history, and offered to the start/end-page buttons.

    if ((file == null) || (file.equals(displayPath) == false)) return;

    // A change Java did not already know about is viewer-originated (the user
    // scrolled): it enters the page-nav history here. Java-initiated jumps
    // recorded at issue time (recordChromePageNav) and reconciler-driven ones
    // (which do not enter history) pre-set pageNum, so they only refresh the
    // window controls.

    if ((history.current() != null) && (pageNum != newPageNum))
    {
      pageNum = newPageNum;
      history.recordPage(pageNum);
    }

    if (window.curSource() == src)
      Platform.runLater(this::refreshControls);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Identifies the newest tracked file (FX-confined; bumped by
   *  {@link #trackFile}); a finishing annotation scan delivers its result only
   *  if this pane hasn't moved on to another file in the meantime. */
  private long annotScanSeq = 0;

  /** Whether an annotation scan for the currently tracked file is still running
   *  (FX-confined). The window shows a loading indicator on the annotation
   *  navigation buttons while this is true, so the not-yet-known state cannot be
   *  mistaken for "no annotations". */
  private boolean annotScanInProgress = false;

  boolean annotScanInProgress() { return annotScanInProgress; }

  /**
   * Scans the displayed document for annotated pages on a background thread
   * and applies the result to {@link #hilitePages}. The scan reads the PDF
   * directly with PDFBox ({@link PDFAnnotationScanner}) instead of walking
   * pages through the pdf.js worker, and it runs concurrently with the
   * viewer's own open, so the annotation-navigation buttons can enable without
   * waiting for the document to render.
   */
  private void startAnnotationScan(FilePath displayPath)
  {
    long seq = annotScanSeq;

    annotScanInProgress = true;

    HyperThread thread = new HyperThread("AnnotationScan", () ->
    {
      List<Integer> annotPages = PDFAnnotationScanner.scan(displayPath);

      Platform.runLater(() ->
      {
        if (seq != annotScanSeq) return;  // this pane has moved on to another file

        annotScanInProgress = false;
        hilitePages = annotPages;

        if (window.curSource() == src)
          refreshControls();
      });
    });

    thread.setDaemon(true);
    thread.start();
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
  void warmUp()
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

  /** Load a paged document (a native PDF, or the PDF output of an office
   *  conversion) into the pdf.js viewer. */
  private void loadPagedDocument(FilePath displayPath, int pageNum)
  {
    jsWrapper.setContentToShowIsDirect(false);
    jsWrapper.loadPdf(displayPath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records {@code sourceFile} as the file this pane is showing (see
   * {@link PreviewNavHistory#track}) and resets the per-document metadata the
   * previous document left behind.
   */
  private void trackFile(FilePath sourceFile, HDT_Record record)
  {
    history.track(sourceFile, record);

    meta = ViewerMeta.PAGELESS;  // until the newly tracked file's load confirms
    hilitePages = null;

    annotScanSeq++;               // a scan of the previous file must not deliver into this one
    annotScanInProgress = false;  // no scan is running for the newly tracked file yet; paneShowPaged starts one for paged content
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pane-driven paged load: displays {@code displayPath} in the pdf.js viewer
   * at the given page, tracking {@code sourceFile} as the file being shown
   * (they differ for LibreOffice-converted office documents).
   *
   * @param sourceFile  the file the user asked to preview (used for file tracking)
   * @param displayPath the file the viewer actually loads (source itself, or converted artifact)
   * @param pageNum     1-based page to open at
   * @param record      record associated with the source file, or {@code null}
   */
  @Override public void paneShowPaged(FilePath sourceFile, FilePath displayPath, int pageNum, HDT_Record record)
  {
    if (ensureInitialized() == false) return;

    trackFile(sourceFile, record);

    this.pageNum = pageNum;
    this.displayPath = displayPath;

    startAnnotationScan(displayPath);

    loadPagedDocument(displayPath, pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pane-driven direct-content load: displays {@code displayPath} as direct
   * browser content (HTML, plain text, image, media), with the same
   * source-file tracking as {@link #paneShowPaged}.
   *
   * @return true if content was loaded; false if the file kind cannot be
   *         previewed or loading failed (the unable indicator is shown)
   */
  @Override public boolean paneShowDirect(FilePath sourceFile, FilePath displayPath, HDT_Record record)
  {
    if (ensureInitialized() == false) return false;

    trackFile(sourceFile, record);

    pageNum = 1;
    meta = ViewerMeta.PAGELESS;
    this.displayPath = displayPath;

    try
    {
      if (jsWrapper.loadDirectContent(displayPath))
      {
        // The controls follow the issued display, not only the load's completion:
        // for Chromium's built-in media pages (an mp3 in its audio player) the
        // completion never brought them up to date.

        refreshControls();
        return true;
      }

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

  /**
   * Pane-driven progress display for a document on its way (an office
   * conversion). The pane is about {@code sourceFile} from this moment, so the
   * window's controls follow it now rather than when its document loads.
   */
  @Override public void paneShowProgress(FilePath sourceFile, HDT_Record record, ProgressVariant variant)
  {
    if (ensureInitialized() == false) return;

    trackStatusFile(sourceFile, record);

    if (variant == ProgressVariant.STARTING_CONVERTER)
      jsWrapper.setStartingConverter();
    else
      jsWrapper.setGenerating(sourceFile);

    refreshControls();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Pane-driven unable display. Nothing loads for {@code sourceFile}, so this
   * is the only point at which the window's controls can come to name it.
   */
  @Override public void paneShowUnable(FilePath sourceFile, HDT_Record record, boolean noOfficeInstallation)
  {
    if (ensureInitialized() == false) return;

    trackStatusFile(sourceFile, record);

    if (noOfficeInstallation)
      jsWrapper.setNoOfficeInstallation();
    else
      jsWrapper.setUnable(sourceFile);

    refreshControls();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Tracks the file a status display is about: no document, so a single
   *  notional page and no display path for viewer reports to match. */
  private void trackStatusFile(FilePath sourceFile, HDT_Record record)
  {
    trackFile(sourceFile, record);

    pageNum = 1;
    meta = ViewerMeta.PAGELESS;
    displayPath = null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Pane-driven page navigation within the currently-displayed document. */
  @Override public void paneGoToPage(int pageNum)
  {
    if (initialized == false) return;

    this.pageNum = pageNum;
    jsWrapper.goToPage(pageNum);
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

  void setWorkPageNums(int start, int end)
  {
    workStartPageNum = start;
    workEndPageNum = end;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void go()
  {
    Entry entry = history.current();
    if (entry == null) return;

    if (entry.record() != null)
      ui.goToRecord(entry.record(), true);
    else if (FilePath.isEmpty(entry.filePath()) == false)
      ui.goToRecord(HyperPath.getRecordFromFilePath(entry.filePath()), true);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private boolean addMenuItem(List<MenuItem> menu, int ndx)
  {
    menu.add(getMenuItemForNavNdx(ndx));

    return menu.size() == 15;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshNavMenu(List<MenuItem> menu, boolean isForward)
  {
    menu.clear();

    Entry entry = history.current();
    if (entry == null) return;

    if (isForward)
    {
      for (int ndx = entry.pageNdx() + 1; ndx < entry.pages().size(); ndx++)
        if (addMenuItem(menu, ndx)) return;
    }
    else
    {
      for (int ndx = entry.pageNdx() - 1; ndx >= 0; ndx--)
        if (addMenuItem(menu, ndx)) return;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private MenuItem getMenuItemForNavNdx(int ndx)
  {
    int page = history.current().pages().get(ndx);
    String pageLabel = safeStr(getLabelByPage(page)), pageStr = String.valueOf(page);

    MenuItem item = new MenuItem("Page " + (pageLabel.isEmpty() || pageLabel.equals(pageStr) ? pageStr : (pageLabel + " (" + pageStr + ')')));

    item.setOnAction(event -> jumpToHistoryPage(history.selectPage(ndx)));

    return item;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void navClick(boolean isForward)
  {
    int page = history.stepPage(isForward);

    if (page > 0)
      jumpToHistoryPage(page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Navigates to a page reached through the back/forward history: the history
   * index was already repositioned, so no entry is recorded, and
   * {@link #pageNum} is pre-set so the resulting page-change event does not
   * re-enter history either.
   */
  private void jumpToHistoryPage(int page)
  {
    pageNum = page;

    PreviewWindow.hostFor(src).navigateToPage(page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records a page jump made with the window's controls (slider, page buttons,
   * page field, ContentsWindow) in this file's page-nav history at issue time;
   * the jump itself is then issued through the pane's intent. Also pre-sets
   * {@link #pageNum} so the resulting page-change event is recognized as
   * Java-initiated rather than user scrolling.
   */
  void recordChromePageNav(int page)
  {
    if (history.current() == null) return;

    pageNum = page;
    history.recordPage(page);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Back/forward file navigation: steps the file history (the target entry is
   * current from this moment, so the controls, launch, and go reflect it) and
   * re-previews the entry through the pane's intent. The resulting load tracks
   * the entry's own file and record, which keeps the entry with its page
   * history and the forward file history (see {@link PreviewNavHistory#track}).
   */
  void fileNavClick(boolean isForward)
  {
    Entry entry = history.stepFile(isForward);
    if (entry == null) return;

    PreviewWindow.hostFor(src).setPreviewAuto(entry.filePath(), entry.record(), entry.currentPage());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void reset()
  {
    clearPreview();
    history.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override public void clearPreview()
  {
    pageNum = -1;
    workStartPageNum = -1;
    workEndPageNum = -1;
    displayPath = null;
    history.clearCurrent();

    if (window.curSource() == src) window.clearControls();

    if (initialized)
      jsWrapper.reset();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void refreshControls()
  {
    FilePath filePath = getFilePath();

    // A missing file is no reason to clear: the reconciler owns the display, and
    // an unable notice for a file that is gone is exactly when the controls
    // should still name it.

    if ((pageNum <= 0) || FilePath.isEmpty(filePath))
    {
      clearPreview();
      return;
    }

    window.refreshControls(pageNum, getNumPages(), this);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * This pane's tab became the active one. The display itself needs nothing
   * (each tab has its own browser, and loads deferred while this source was
   * not the active, showing one replay through the intent path below); only
   * the window controls are brought up to date.
   */
  void activate()
  {
    btn.setSelected(true);

    if (history.current() == null)
      window.clearControls();
    else
      refreshControls();

    // This source is now active and showing, so any preview work a caller
    // deferred while it was not (record navigation with the window closed, the
    // FTS hit pipeline) can now run; the deferred caller has the last word on
    // what is displayed.

    PreviewWindow.fireActivation(getSource());
  }

  //---------------------------------------------------------------------------
  //---------------------------------------------------------------------------

  void setWorkPageFromContentsWindow(int pageNum, boolean isStart)
  {
    if (isStart)
      workStartPageNum = pageNum;
    else
      workEndPageNum = pageNum;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  boolean setCurPageAsWorkPage(boolean isStart)
  {
    Entry entry = history.current();

    if ((entry == null) || (entry.record() == null) || (entry.record().getType() != hdtWork))
      return false;

    if (isStart)
      workStartPageNum = pageNum;
    else
      workEndPageNum = pageNum;

    HDT_Work work = (HDT_Work) entry.record();
    HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.getRecordFromFilePath(entry.filePath());

    if (isStart)
      work.setStartPageNum(workFile, pageNum);
    else
      work.setEndPageNum(workFile, pageNum);

    if ((ui.activeTabEnum() == workTabEnum) && (ui.activeTab().activeRecord() == work))
      ui.workHyperTab().setPageNum(workFile, pageNum, isStart);

    if (workFile == null)
      ContentsWindow.instance().update(entry.filePath(), pageNum);
    else
      ContentsWindow.instance().update(workFile, pageNum);

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int getPrevHilite(int curPage)
  {
    if (collEmpty(hilitePages)) return -1;

    int newPage = -1;

    for (Integer page : hilitePages)
      if ((page < curPage) && (page > newPage))
        newPage = page;

    return newPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  int getNextHilite(int curPage)
  {
    if (collEmpty(hilitePages)) return -1;

    int numPages = getNumPages(), newPage = numPages + 1;

    for (Integer page : hilitePages)
      if ((page > curPage) && (page < newPage))
        newPage = page;

    return newPage > numPages ? -1 : newPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ConversionSession leasedArtifactSession = null;

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
