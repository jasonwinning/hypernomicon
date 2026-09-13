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
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.util.StringUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.view.tabs.HyperTab.TabEnum.*;

import java.util.List;
import java.util.concurrent.Executor;
import java.util.function.Consumer;

import org.hypernomicon.HyperTask.HyperThread;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.previewWindow.DesiredView.ProgressVariant;
import org.hypernomicon.previewWindow.PipelineSnapshot.HitsStatus;
import org.hypernomicon.previewWindow.PreviewIntent.ContentKind;
import org.hypernomicon.previewWindow.PreviewNavHistory.Entry;
import org.hypernomicon.previewWindow.PreviewWindow.PreviewSource;
import org.hypernomicon.previewWindow.PreviewWrapper.PaneEventSink;
import org.hypernomicon.previewWindow.ViewerPort.ViewerMeta;
import org.hypernomicon.util.RequestGate;
import org.hypernomicon.util.SettleGate;
import org.hypernomicon.util.file.FilePath;

import javafx.application.Platform;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tab;

//---------------------------------------------------------------------------

/**
 * One preview pane: everything about a {@link PreviewSource} that is not the
 * viewer itself. One instance per source, obtained via
 * {@link PreviewWindow#hostFor}, created on first use, which can be before the
 * Preview Window exists (the search flow sets intent without opening it); the
 * window attaches the pane's viewer and tab when it is created.
 * <p>
 * The reconciler, the settle gate, the artifact tracking, and the attribution
 * of viewer reports are the {@link PreviewHostCore} this pane owns. On top of
 * that it holds what the window's chrome reads and what it navigates with:
 * the record behind the intent, the FTS hit flow, the navigation history
 * ({@link PreviewNavHistory}), the current page and the work start/end pages,
 * the displayed document's metadata, and the annotated-page scan. The viewer
 * is reached through {@link PaneViewer} ({@link PreviewWrapper} in production,
 * a recording fake in the contract tests).
 * <p>
 * <b>Two readings of "the current file", both owned here.</b>
 * {@link #intendedFile()} is the file the pane is about (the navigation
 * history's current entry), set the moment a display is issued, so the chrome
 * names it immediately. {@link #confirmedFile()} is the file whose load the
 * viewer last confirmed, so the lock gate never keeps a view that is not there.
 * They differ while a load is in flight; do not merge them.
 * <p>
 * Record-navigation panes use {@link #setPreviewAuto} (no highlighting, content
 * kind derived from the mimetype). The queries pane additionally drives the FTS
 * flow: {@code FTSQueryCtrlr} sets intent via {@link #setPreview} and pushes
 * hit-set results (the {@code updateHits*} methods) through the
 * {@link PreviewWindow} facade. Window-chrome actions (page navigation,
 * refresh, history) arrive as intent updates too; nothing displays anything
 * except by setting intent.
 * <p>
 * Threading: everything here runs on the FX thread (controller calls, session
 * display callbacks, and pane executor tasks all marshal there), except the
 * viewer's event sink, which arrives on browser threads, hands the report to
 * the core's identity gate, and marshals its own bookkeeping.
 * <p>
 * Lifetimes: the pane is static per source. Its viewer-bound half (the
 * attached viewer and tab) comes and goes with the window; its session-bound
 * half (history, work pages) is cleared when the database closes
 * ({@link PreviewWindow#clearAll}).
 */
final class PreviewPaneHost
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The pane's viewer surface: loads documents and status displays, applies
   * hits, and reports the viewer's events back through the sink it is given.
   * Knows nothing of records, history, or the window's controls.
   */
  interface PaneViewer
  {
    /** Creates the viewer if it does not exist yet; false if it cannot (the browser engine is unavailable). */
    boolean ensureInitialized();

    void setPaneEventSink(PaneEventSink sink);

    /** Paints the idle viewer if the viewer has never been created, so no never-painted surface is user-visible. */
    void warmUp();

    /** This pane's tab became the active one: reflect it in the pane's own controls. */
    void markSelected();

    /** Empties the viewer: the idle display, any document closed. */
    void showEmpty();

    void showProgress(FilePath sourceFile, ProgressVariant variant);
    void showUnable(FilePath sourceFile, boolean noOfficeInstallation);
    void showPaged(FilePath displayPath, int pageNum);

    /** @return false if the file kind cannot be shown as direct content (nothing was loaded; the unable display is up) */
    boolean showDirect(FilePath sourceFile, FilePath displayPath);

    void goToPage(int pageNum);
    void setAllHits(String hitsJson);
    void clearAllHits();
    void scrollToHighlight(int matchNdx, int pageNum, int ndxOnPage);

    /** @return whether a zoom was issued (false leaves the triggering event unconsumed) */
    boolean zoom(boolean zoomingIn);

    /** Leases a completed conversion's artifact against cache eviction while displayed. */
    void leaseArtifact(ConversionSession session);

    /** Reloads the embedded browser, then runs {@code done} (from a browser thread). */
    void reloadViewer(Runnable done);
  }

//---------------------------------------------------------------------------

  /**
   * Scans a document for annotated pages and delivers the result, on any
   * thread (the pane marshals it). In production the scan runs on a
   * background thread; the contract tests deliver synchronously.
   */
  @FunctionalInterface interface AnnotationScanner
  {
    void scan(FilePath displayPath, Consumer<List<Integer>> onResult);
  }

  /** The production scanner: {@link PDFAnnotationScanner} on a daemon thread. */
  static final AnnotationScanner BACKGROUND_SCANNER = (displayPath, onResult) ->
  {
    HyperThread thread = new HyperThread("AnnotationScan", () -> onResult.accept(PDFAnnotationScanner.scan(displayPath)));

    thread.setDaemon(true);
    thread.start();
  };

//---------------------------------------------------------------------------

  private final PreviewSource src;
  private final PreviewHostCore core;
  private final Executor paneExecutor;
  private final AnnotationScanner annotationScanner;
  private final PreviewNavHistory history = new PreviewNavHistory();

  /** The window's viewer and tab for this pane, attached while the window exists. FX-confined. */
  private PaneViewer viewer = null;
  private Tab tab = null;

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

  // Chrome state, FX-confined

  private int pageNum = -1, workStartPageNum = -1, workEndPageNum = -1;

  /** The displayed document's metadata (page count, labels), taken from its
   *  load confirmation; {@link ViewerMeta#PAGELESS} for direct content, for a
   *  status display, and for a document whose load has not confirmed yet. */
  private ViewerMeta meta = ViewerMeta.PAGELESS;

  private List<Integer> hilitePages = null;

  /** Identifies the newest tracked file (bumped by {@link #trackFile}); a
   *  finishing annotation scan delivers its result only if this pane has not
   *  moved on to another file in the meantime. */
  private long annotScanSeq = 0;

  /** Whether an annotation scan for the currently tracked file is still
   *  running. The window shows a loading indicator on the annotation
   *  navigation buttons while this is true, so the not-yet-known state cannot
   *  be mistaken for "no annotations". */
  private boolean annotScanInProgress = false;

  /** The viewer's events, handed to the core's identity gate and to this
   *  pane's own bookkeeping. Installed on the viewer at every intent set:
   *  cheap, and a viewer the window replaced gets it too. */
  private final PaneEventSink eventSink = new PaneEventSink()
  {
    @Override public void onOpened(FilePath file, boolean success, ViewerMeta meta) { PreviewPaneHost.this.onOpened(file, success, meta); }
    @Override public void onPageChanged(FilePath file, int pageNum)                 { PreviewPaneHost.this.onPageChanged(file, pageNum); }
  };

//---------------------------------------------------------------------------

  PreviewPaneHost(PreviewSource src)
  {
    this(src, new SettleGate(150), Platform::runLater, BACKGROUND_SCANNER);
  }

  /**
   * @param settleGate   gates this pane's intents: rapid intent changes
   *                     (key-repeat selection reaching this pane per
   *                     selection) must not each set an intent, subscribe to a
   *                     conversion, and cycle the display; only the file the
   *                     selection settles on does. A quiet-selection intent
   *                     proceeds immediately, so gated upstream callers (the
   *                     FTS controller's own settle gate) and deliberate
   *                     single selections never wait here
   * @param paneExecutor executor the reconciler and this pane's bookkeeping
   *                     run on: the FX thread in production, a direct
   *                     executor in tests
   * @param annotationScanner scans paged documents for annotated pages
   */
  PreviewPaneHost(PreviewSource src, RequestGate settleGate, Executor paneExecutor, AnnotationScanner annotationScanner)
  {
    this.src = src;
    this.paneExecutor = paneExecutor;
    this.annotationScanner = annotationScanner;

    core = new PreviewHostCore(new WrapperPort(), paneExecutor, settleGate, session -> viewer.leaseArtifact(session), () -> hitsStatus);
  }

//---------------------------------------------------------------------------

  PreviewSource source()                   { return src; }
  Tab tab()                                { return tab; }

  FilePath confirmedFile()                 { return core.confirmedFile(); }
  FilePath intendedFile()                  { return history.currentFile(); }
  HDT_RecordWithPath intendedRecord()      { return history.currentRecord(); }

  int pageNum()                            { return pageNum; }
  int numPages()                           { return meta.pageCount(); }
  int pageForLabel(String label)           { return meta.pageForLabel(label); }
  String labelForPage(int page)            { return meta.labelForPage(page); }

  int workStartPageNum()                   { return workStartPageNum; }
  int workEndPageNum()                     { return workEndPageNum; }

  boolean annotScanInProgress()            { return annotScanInProgress; }
  int lowestHilitePage()                   { return collEmpty(hilitePages) ? -1 : hilitePages.getFirst(); }
  int highestHilitePage()                  { return collEmpty(hilitePages) ? -1 : hilitePages.getLast(); }

  boolean canStepPage(boolean forward)     { return history.canStepPage(forward); }
  boolean canStepFile(boolean forward)     { return history.canStepFile(forward); }

  void warmUp()                            { if (viewer != null) viewer.warmUp(); }
  boolean zoom(boolean zoomingIn)          { return (viewer != null) && viewer.zoom(zoomingIn); }

  void setWorkPageNums(int start, int end) { workStartPageNum = start; workEndPageNum = end; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** The window attaches this pane's viewer and tab when it is created (a test attaches a fake). */
  void attach(PaneViewer viewer, Tab tab)
  {
    this.viewer = viewer;
    this.tab = tab;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Whether the viewer exists and is (or can now be) initialized. */
  boolean viewerReady()
  {
    return (viewer != null) && viewer.ensureInitialized();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Sets this pane's intent, through the settle gate. For office documents the
   * pane starts (or joins) the conversion and feeds its status to the
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

    if (viewerReady() == false) return;

    viewer.setPaneEventSink(eventSink);

    // Hits that arrived while this intent was gated; only meaningful for the
    // execution of the request they were stashed under, so always consumed

    HitsStatus earlyHits = filePath.equals(requestedFile) ? requestedFileHits : null;
    requestedFileHits = null;

    intentRecord = record;

    if (filePath.equals(core.intentFile()) == false)
      hitsStatus = wantsHighlights ? (earlyHits != null ? earlyHits : HitsStatus.PENDING) : null;

    core.setIntent(new PreviewIntent(filePath, paged ? ContentKind.PAGED : ContentKind.DIRECT, pageNum, wantsHighlights, scrollTarget), this);
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
    if ((core.intentFile() == null) || (viewer == null)) return;

    viewer.reloadViewer(() -> paneExecutor.execute(core.pane()::refreshDisplay));
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

  /** Clears the pane's preview (intent = none; the viewer empties). The
   *  navigation history stays; {@link #reset} forgets it. */
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

  /** Forgets everything session-bound: the preview, and the navigation history
   *  with its record references (the database is closing). */
  void reset()
  {
    clear();
    history.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Empties the pane's chrome state and viewer; the history's entries stay navigable. */
  private void clearPreview()
  {
    pageNum = -1;
    workStartPageNum = -1;
    workEndPageNum = -1;
    history.clearCurrent();

    PreviewWindow.clearControlsIfShowing(src);

    if (viewer != null)
      viewer.showEmpty();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // Viewer events (browser threads)

  private void onOpened(FilePath file, boolean success, ViewerMeta meta)
  {
    core.onOpened(file, success, meta, null);

    // As with page changes: only the issued document's completion feeds this
    // pane's bookkeeping (metadata, first history entry, controls). The
    // metadata is the report's own: attributing it by reading the viewer's
    // current state would race the next document's load.

    if (core.isIssuedDocument(file) == false) return;

    paneExecutor.execute(() ->
    {
      Entry entry = history.current();
      if (entry == null) return;

      if (success)
        this.meta = meta;

      // The first page of a freshly tracked file enters its page history on
      // load; later pages enter through the page-change event or a chrome jump.

      if (entry.hasPages() == false)
        history.recordPage(pageNum);

      refreshControls();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void onPageChanged(FilePath file, int newPageNum)
  {
    core.onPageChanged(file, newPageNum);

    // Only the issued document's page events feed this pane's bookkeeping. A
    // superseded document's late page jump would otherwise be recorded against
    // the file that replaced it: shown in the page field, entered in its page
    // history, and offered to the start/end-page buttons.

    if (core.isIssuedDocument(file) == false) return;

    paneExecutor.execute(() ->
    {
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

      refreshControls();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Records {@code sourceFile} as the file this pane is showing (see
   * {@link PreviewNavHistory#track}) and resets the per-document state the
   * previous document left behind.
   */
  private void trackFile(FilePath sourceFile)
  {
    history.track(sourceFile, intentRecord);

    meta = ViewerMeta.PAGELESS;  // until the newly tracked file's load confirms
    hilitePages = null;

    annotScanSeq++;               // a scan of the previous file must not deliver into this one
    annotScanInProgress = false;  // no scan is running for the newly tracked file yet; a paged load starts one
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Tracks the file a status display is about: no document, so a single notional page. */
  private void trackStatusFile(FilePath sourceFile)
  {
    trackFile(sourceFile);

    pageNum = 1;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Scans the displayed document for annotated pages and applies the result
   * to {@link #hilitePages}. The scan reads the PDF directly with PDFBox
   * ({@link PDFAnnotationScanner}) instead of walking pages through the pdf.js
   * worker, and it runs concurrently with the viewer's own open, so the
   * annotation-navigation buttons can enable without waiting for the document
   * to render.
   */
  private void startAnnotationScan(FilePath displayPath)
  {
    long seq = annotScanSeq;

    annotScanInProgress = true;

    annotationScanner.scan(displayPath, annotPages -> paneExecutor.execute(() ->
    {
      if (seq != annotScanSeq) return;  // this pane has moved on to another file

      annotScanInProgress = false;
      hilitePages = annotPages;

      refreshControls();
    }));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Brings the window's controls up to date with this pane, if it is the pane the window is showing. */
  void refreshControls()
  {
    // A missing file is no reason to clear: the reconciler owns the display, and
    // an unable notice for a file that is gone is exactly when the controls
    // should still name it.

    if ((pageNum <= 0) || FilePath.isEmpty(intendedFile()))
    {
      clearPreview();
      return;
    }

    PreviewWindow.refreshControlsIfShowing(this, pageNum, numPages());
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
    if (viewer != null)
      viewer.markSelected();

    if (history.current() == null)
      PreviewWindow.clearControlsIfShowing(src);
    else
      refreshControls();

    // This source is now active and showing, so any preview work a caller
    // deferred while it was not (record navigation with the window closed, the
    // FTS hit pipeline) can now run; the deferred caller has the last word on
    // what is displayed.

    PreviewWindow.fireActivation(src);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Opens the record (or the record owning the file) this pane is about. */
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

  private boolean addMenuItem(List<MenuItem> menu, int ndx)
  {
    int page = history.current().pages().get(ndx);
    String pageLabel = safeStr(labelForPage(page)), pageStr = String.valueOf(page);

    MenuItem item = new MenuItem("Page " + (pageLabel.isEmpty() || pageLabel.equals(pageStr) ? pageStr : (pageLabel + " (" + pageStr + ')')));

    item.setOnAction(event -> jumpToHistoryPage(history.selectPage(ndx)));

    menu.add(item);

    return menu.size() == 15;
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
   * cursor was already moved, so no entry is recorded, and {@link #pageNum}
   * is pre-set so the resulting page-change event does not re-enter history
   * either.
   */
  private void jumpToHistoryPage(int page)
  {
    pageNum = page;

    navigateToPage(page);
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

    setPreviewAuto(entry.filePath(), entry.record(), entry.currentPage());
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

    int numPages = numPages(), newPage = numPages + 1;

    for (Integer page : hilitePages)
      if ((page > curPage) && (page < newPage))
        newPage = page;

    return newPage > numPages ? -1 : newPage;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * The {@link ViewerPort} over the {@link PaneViewer}. Every command
   * corresponds to the current intent (the pane is single-threaded on the FX
   * thread). Each display command first records what the pane is about
   * (tracking the file in the history, resetting per-document state) and
   * then drives the viewer, so the window's controls name the intended file
   * from the moment it is issued.
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
    // PreviewHostCore.issuingStatus); a cleared pane drops them like the
    // document views, since a setIntent(null) is queued right behind.

    @Override public void showEmpty()
    {
      core.issuingStatus();

      clearPreview();
    }

  //---------------------------------------------------------------------------

    /** The pane is about {@code sourceFile} from this moment, so the controls follow it now rather than when its document loads. */
    @Override public void showProgress(FilePath sourceFile, ProgressVariant variant)
    {
      if ((core.issuingStatus() == false) || (viewerReady() == false)) return;

      trackStatusFile(sourceFile);

      viewer.showProgress(sourceFile, variant);

      refreshControls();
    }

  //---------------------------------------------------------------------------

    /** Nothing loads for {@code sourceFile}, so this is the only point at which the controls can come to name it. */
    @Override public void showUnable(FilePath sourceFile)
    {
      if ((core.issuingStatus() == false) || (viewerReady() == false)) return;

      trackStatusFile(sourceFile);

      viewer.showUnable(sourceFile, core.noOfficeInstallation());

      refreshControls();
    }

  //---------------------------------------------------------------------------

    @Override public void showDocument(long gen, FilePath documentPath, int pageNum)
    {
      if ((core.issuingDocument(gen, documentPath) == false) || (viewerReady() == false)) return;

      trackFile(core.intentFile());

      PreviewPaneHost.this.pageNum = pageNum;

      startAnnotationScan(documentPath);

      viewer.showPaged(documentPath, pageNum);
    }

  //---------------------------------------------------------------------------

    @Override public void showContent(long gen, FilePath contentPath)
    {
      if ((core.issuingDocument(gen, contentPath) == false) || (viewerReady() == false)) return;

      FilePath sourceFile = core.intentFile();

      trackFile(sourceFile);

      pageNum = 1;

      if (viewer.showDirect(sourceFile, contentPath) == false)
      {
        core.pane().onViewerError(gen, "The file kind cannot be shown as direct content");
        return;
      }

      // The controls follow the issued display, not only the load's completion:
      // for Chromium's built-in media pages (an mp3 in its audio player) the
      // completion never brought them up to date.

      refreshControls();
    }

  //---------------------------------------------------------------------------

    @Override public void setHits(long gen, String hitsJson)
    {
      viewer.setAllHits(hitsJson);
    }

  //---------------------------------------------------------------------------

    @Override public void clearHits(long gen)
    {
      viewer.clearAllHits();
    }

  //---------------------------------------------------------------------------

    @Override public void goToPage(long gen, int pageNum)
    {
      PreviewPaneHost.this.pageNum = pageNum;

      viewer.goToPage(pageNum);
    }

  //---------------------------------------------------------------------------

    @Override public void scrollToMatch(long gen, int matchNdx, int pageNum, int ndxOnPage)
    {
      viewer.scrollToHighlight(matchNdx, pageNum, ndxOnPage);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
