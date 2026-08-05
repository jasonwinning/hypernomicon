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
import static org.hypernomicon.util.DesktopUtil.*;
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.model.HyperDB.db;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;

import java.util.*;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.ExitWatchdog;
import org.hypernomicon.bib.BibManager;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutAction;
import org.hypernomicon.settings.shortcuts.Shortcut.ShortcutContext;
import org.hypernomicon.util.file.FilePath;
import org.hypernomicon.view.controls.LoadingDots;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.*;
import javafx.scene.layout.*;

//---------------------------------------------------------------------------

public final class PreviewWindow extends NonmodalWindow
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML TabPane tpPreview;
  @FXML private AnchorPane apManager, apOther, apPerson, apQuery, apTree, apWork;
  @FXML private Button btnContents, btnEndPage, btnFileBack, btnFileForward, btnGoToMain, btnGoToManager, btnHiliteNext, btnHilitePrev,
                       btnLaunch, btnPreviewBack, btnPreviewForward, btnPreviewNext, btnPreviewPrev, btnRefresh, btnSetEnd, btnSetStart, btnStartPage;
  @FXML private Label lblPreviewPages, lblRecord;
  @FXML private Pane paneType;
  @FXML private Slider sldPreview;
  @FXML private Tab tabManager, tabOther, tabPerson, tabQuery, tabTree, tabWork;
  @FXML private TextField tfPath, tfPreviewPage;
  @FXML private ToggleButton btnLock, btnManager, btnOther, btnPerson, btnQueries, btnTree, btnWorks;

  private static PreviewWindow instance;

  private static final String dialogTitle = "Preview Work/File",
                              TEXT_TO_SHOW_IF_NONE = "(none)";

  private static final Object LOCK = new Object();
  private static final Map<Tab, PreviewWrapper> tabToWrapper = new HashMap<>();

  /**
   * One reconciler host per preview source, created on first use (which can be
   * before the window exists: the search flow sets intent without opening it).
   * Per-source state is thereby split between the hosts and the wrappers, which
   * are created with the window: the host decides what a pane shows (intent and
   * pipeline state, through its reconciler); the wrapper owns the viewer and
   * still holds the chrome's metadata (labels, page counts, work pages) and the
   * navigation history, which is why {@link #getFilePath} answers from the
   * wrapper while the lock gate asks the host. Collapsing the two into one
   * per-source object waits on chrome metadata moving into the typed viewer
   * protocol and the navigation history being redesigned behind the host; until
   * then the wrapper is the only home for both.
   */
  private static final Map<PreviewSource, PreviewPaneHost> srcToHost = new EnumMap<>(PreviewSource.class);

  private final Map<PreviewSource, PreviewWrapper> srcToWrapper = new EnumMap<>(PreviewSource.class);

  /** Previews requested while the Lock button deferred them, one replay per
   *  source (latest wins); run and cleared on unlock. Initiators with pipeline
   *  context of their own (the FTS flow) register via {@link #runWhenUnlocked};
   *  record navigation is stashed internally by {@link #doSetPreview}. */
  private final Map<PreviewSource, Runnable> srcToLockedReplay = new EnumMap<>(PreviewSource.class);

  // Work deferred by a caller that declined to generate a preview while its source was not the active,
  // showing one (see isSourceActiveAndShowing / runWhenSourceActivates). Keyed by source, at most one
  // entry each; fired and removed by fireActivation when that source next activates. Static so it can be
  // registered before the window instance exists (the preview window may never have been opened).

  private static final Map<PreviewSource, Runnable> pendingActivation = new EnumMap<>(PreviewSource.class);

  private final LoadingDots hiliteDotsPrev = new LoadingDots(),
                            hiliteDotsNext = new LoadingDots();

  /** The annotation-navigation buttons' normal graphics (from the FXML),
   *  restored when the scan loading indicator comes down. */
  private final Node hiliteGraphicPrev, hiliteGraphicNext;

  /** Batch suppression for initiators: the File Manager sets this around bulk
   *  table updates so the selection churn they cause does not set previews. */
  public static boolean disablePreviewUpdating = false;

  /** Re-entrancy guard: true while {@link #refreshControls(int, int, PreviewWrapper)}
   *  updates the window's controls (and the ContentsWindow) programmatically, so
   *  their change listeners do not navigate or write back. */
  private boolean refreshingControls = false;

  /** Whether the Preview Window is currently updating its controls programmatically. */
  static boolean isRefreshingControls()
  {
    return (instance != null) && instance.refreshingControls;
  }

//---------------------------------------------------------------------------

  public FilePath getFilePath(PreviewSource src) { return srcToWrapper.get(src).getFilePath(); }
  private PreviewWrapper curWrapper()            { return tabToWrapper.get(tpPreview.getSelectionModel().getSelectedItem()); }
  PreviewSource curSource()                      { return curWrapper().getSource(); }
  int curPage()                                  { return (int) sldPreview.getValue(); }
  int getMax()                                   { return (int) sldPreview.getMax(); }

  @Override protected void getDividerPositions() { }
  @Override protected void setDividerPositions() { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum PreviewSource { pvsPersonTab, pvsWorkTab, pvsQueriesTab, pvsManager, pvsTreeTab, pvsOther }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static PreviewWindow instance()
  {
    if (instance == null) instance = new PreviewWindow();

    return instance;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private PreviewWindow()
  {
    super("previewWindow/PreviewWindow", dialogTitle, PrefKey.PREV_WINDOW_X, PrefKey.PREV_WINDOW_Y, PrefKey.PREV_WINDOW_WIDTH, PrefKey.PREV_WINDOW_HEIGHT);

    addWrapper(pvsPersonTab , apPerson , tabPerson , btnPerson );
    addWrapper(pvsWorkTab   , apWork   , tabWork   , btnWorks  );
    addWrapper(pvsQueriesTab, apQuery  , tabQuery  , btnQueries);
    addWrapper(pvsManager   , apManager, tabManager, btnManager);
    addWrapper(pvsOther     , apOther  , tabOther  , btnOther  );
    addWrapper(pvsTreeTab   , apTree   , tabTree   , btnTree   );

    forceToggleSelection(btnPerson.getToggleGroup());

    tabToWrapper.values().forEach(PreviewWrapper::clearPreview);

    lblRecord.setOnMouseClicked(event -> curWrapper().go());
    paneType.setOnMouseClicked (event -> curWrapper().go());

    btnGoToMain   .setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnGoToManager.setOnAction(event -> FileManager.show());

    btnRefresh.setOnAction(event -> hostFor(curSource()).refresh());

    btnLaunch.setOnAction(event ->
    {
      FilePath filePath = curWrapper().getFilePath();

      if (FilePath.isEmpty(filePath) == false)
        launchWorkFile(filePath, curWrapper().getPageNum());
    });

    btnLock.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        btnLock.setGraphic(imgViewFromRelPath("resources/images/lock.png"));
      else
      {
        btnLock.setGraphic(imgViewFromRelPath("resources/images/lock_open.png"));

        List<Runnable> replays = List.copyOf(srcToLockedReplay.values());
        srcToLockedReplay.clear();
        replays.forEach(Runnable::run);
      }
    });

    sldPreview.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if (tfPreviewPage.isDisabled() == false)
      {
        tfPreviewPage.setText(curWrapper().getLabelByPage(newValue.intValue()));

        lblPreviewPages.setText(newValue.intValue() + " / " + curWrapper().getNumPages());

        if (sldPreview.isValueChanging() == false)
          navigateToPage(newValue.intValue());
      }
    });

    sldPreview.valueChangingProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(oldValue) && Boolean.FALSE.equals(newValue) && (tfPreviewPage.isDisabled() == false))
        navigateToPage((int) sldPreview.getValue());
    });

    hiliteGraphicPrev = btnHilitePrev.getGraphic();
    hiliteGraphicNext = btnHiliteNext.getGraphic();

    btnHilitePrev.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled() == false)
        navigateToPage(curWrapper().getPrevHilite((int) sldPreview.getValue()));
    });

    btnHiliteNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled() == false)
        navigateToPage(curWrapper().getNextHilite((int) sldPreview.getValue()));
    });

    ClickHoldButton chbBack    = new ClickHoldButton(btnPreviewBack   , Side.BOTTOM);
    ClickHoldButton chbForward = new ClickHoldButton(btnPreviewForward, Side.BOTTOM);

    chbBack   .setMenuFactory(menu -> curWrapper().refreshNavMenu(menu, false));
    chbForward.setMenuFactory(menu -> curWrapper().refreshNavMenu(menu, true ));

    chbBack.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      curWrapper().navClick(false);
    });

    chbForward.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      curWrapper().navClick(true);
    });

    setToolTip(btnPerson        , "Preview content selected on Persons tab");
    setToolTip(btnWorks         , "Preview content selected on Works tab");
    setToolTip(btnQueries       , "Preview content selected on Queries tab");
    setToolTip(btnManager       , "Preview content selected in File Manager");
    setToolTip(btnTree          , "Preview content selected on Tree tab");

    setToolTip(btnGoToMain      , "Go to main window");
    setToolTip(btnGoToManager   , "Go to File Manager");

    setToolTip(btnLock          , "Don't change the current view when a different record is selected in another window");
    setToolTip(btnPreviewNext   , "Go forward 1 page");
    setToolTip(btnPreviewPrev   , "Go back 1 page");
    setToolTip(btnPreviewBack   , "Click to go back, hold to see history");
    setToolTip(btnPreviewForward, "Click to go forward, hold to see history");
    setToolTip(btnFileBack      , "Go to the file that was viewed before this one");
    setToolTip(btnFileForward   , "Go to the file that was viewed after this one");
    setToolTip(btnHilitePrev    , "Go to previous annotated page");
    setToolTip(btnHiliteNext    , "Go to next annotated page");
    setToolTip(btnRefresh       , "Refresh current view");
    setToolTip(btnContents      , "Show list of works and page numbers assigned to this work file");
    setToolTip(btnStartPage     , "Jump to this page");
    setToolTip(btnEndPage       , "Jump to this page");
    setToolTip(btnSetStart      , "Set start page to page currently showing");
    setToolTip(btnSetEnd        , "Set end page to page currently showing");
    setToolTip(sldPreview       , "Navigate to different page");

    btnFileBack   .setOnAction(event -> curWrapper().fileNavClick(false));
    btnFileForward.setOnAction(event -> curWrapper().fileNavClick(true ));

    btnPreviewPrev.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      navigateToPage(((int) sldPreview.getValue()) - 1);
    });

    btnPreviewNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      navigateToPage(((int) sldPreview.getValue()) + 1);
    });

    btnStartPage.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      int workPage = curWrapper().getWorkStartPageNum();

      navigateToPage(workPage < 0 ? 1 : workPage);
    });

    btnEndPage.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      int workPage = curWrapper().getWorkEndPageNum();

      navigateToPage(workPage < 0 ? (int) sldPreview.getMax() : workPage);
    });

    btnSetStart.setOnAction(event ->
    {
      if (curWrapper().setCurPageAsWorkPage(true))
        updateStartBtn(curWrapper().getWorkStartPageNum());
    });

    btnSetEnd.setOnAction(event ->
    {
      if (curWrapper().setCurPageAsWorkPage(false))
        updateEndBtn(curWrapper().getWorkEndPageNum());
    });

    tpPreview.getSelectionModel().selectedItemProperty().addListener((ob, oldValue, newValue) ->
    {
      if (newValue == null) return;

      PreviewWrapper wrapper = tabToWrapper.get(newValue);

      wrapper.warmUp();  // first activation of an unused pane: paint the idle viewer before any intent arrives
      wrapper.activate();
    });

    tfPreviewPage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      int pageNum = curWrapper().getPageNum();

      tfPreviewPage.setText(Boolean.TRUE.equals(newValue) ? "" : (pageNum == -1 ? "" : curWrapper().getLabelByPage(pageNum)));
    });

    tfPreviewPage.setOnAction(event -> navigateToPage(curWrapper().getPageByLabel(tfPreviewPage.getText())));

    // The browser views are detached from the scene graph while this window is hidden, and
    // re-attached only once it is showing again. Both halves must straddle the stage's native
    // peer, which JavaFX destroys on hide and recreates on show: JxBrowser's SceneTracker
    // reacts to a BrowserView joining or leaving a scene by resolving the window's native id,
    // so re-attaching while hidden throws ("Failed to get native widget ID") and leaves the
    // view unattached (a blank pane on the next show). Hence prepareToShow runs here, on
    // shown, and not from onHidden.

    onShown = () ->
    {
      srcToWrapper.values().forEach(PreviewWrapper::prepareToShow);

      curWrapper().warmUp();  // the visible pane paints the idle viewer even before its first preview

      runDelayedInFXThread(1, 300, () -> curWrapper().activate());
    };

    stage.setOnHiding(event -> srcToWrapper.values().forEach(PreviewWrapper::prepareToHide));

    btnContents.setOnAction(event -> ContentsWindow.show());

    stage.addEventFilter(ScrollEvent.SCROLL, event ->
    {
      double deltaY = event.getDeltaY();

      // Ctrl or Cmd, matching the browser-side wheel callback in PDFJSWrapper
      // (this filter sees only wheel events over the window's own controls;
      // the browser view's native surface takes its wheel input directly)

      if (((event.isControlDown() || event.isMetaDown()) == false) || (deltaY == 0)) return;

      if (curWrapper().zoom(deltaY > 0))
        event.consume();
    });

    stage.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if (event.isShortcutDown())
      {
        if ((event.getCode() == KeyCode.PLUS    ) ||
            (event.getCode() == KeyCode.EQUALS  ) ||
            (event.getCode() == KeyCode.MINUS   ) ||
            (event.getCode() == KeyCode.SUBTRACT) ||
            (event.getCode() == KeyCode.ADD)    )
        {
          if (curWrapper().zoom((event.getCode() != KeyCode.MINUS) && (event.getCode() != KeyCode.SUBTRACT)))
            event.consume();
        }
      }
    });

    registerShortcuts();

    app.shortcuts.addListener((obs, ov, nv) -> registerShortcuts());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void registerShortcuts()
  {
    Scene scene = stage.getScene();

    // Clear old accelerators first

    scene.getAccelerators().clear();

  //---------------------------------------------------------------------------

    // Hard-coded shortcuts

  //---------------------------------------------------------------------------

    // User-defined shortcuts

    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToBibManager , () -> { if (db.bibLibraryIsLinked()) BibManager.show(true); });
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToMainWindow , () -> ui.windows.focusStage(ui.getStage()));
    assignShortcut(ShortcutContext.AllWindows, ShortcutAction.GoToFileManager, FileManager::show);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void goToPage(int pageNum)
  {
    navigateToPage(Math.max(pageNum, 1));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Page navigation from the window's controls (slider, page buttons, page
   * field, ContentsWindow): records the jump in the pane's page-nav history
   * and re-sets the pane's intent to the explicit page.
   */
  private void navigateToPage(int pageNum)
  {
    if (disablePreviewUpdating || refreshingControls) return;

    PreviewWrapper wrapper = curWrapper();

    if ((pageNum < 1) || (pageNum > wrapper.getNumPages()) || FilePath.isEmpty(wrapper.getFilePath()) || (pageNum == wrapper.getPageNum()))
      return;

    wrapper.recordChromePageNav(pageNum);
    hostFor(wrapper.getSource()).navigateToPage(pageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void addWrapper(PreviewSource src, AnchorPane ap, Tab tab, ToggleButton btn)
  {
    PreviewWrapper wrapper = new PreviewWrapper(src, ap, tab, btn, this);

    srcToWrapper.put(src, wrapper);
    tabToWrapper.put(tab, wrapper);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateStartBtn(int newVal)
  {
    btnStartPage.setText("Start p. " + (newVal < 0 ? TEXT_TO_SHOW_IF_NONE : String.valueOf(newVal)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateEndBtn(int newVal)
  {
    btnEndPage.setText("End p. " + (newVal < 0 ? TEXT_TO_SHOW_IF_NONE : String.valueOf(newVal)));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void resetNavBtns()
  {
    updateStartBtn(-1);
    updateEndBtn(-1);

    disableAll(btnSetStart, btnStartPage, btnSetEnd, btnEndPage, btnContents);

    btnContents.setText("No other records...");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearPreview(PreviewSource src)
  {
    instance.doSetPreview(src, null, -1, -1, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, HDT_WorkFile workFile, HDT_Work work)
  {
    instance.doSetPreview(src, workFile.filePath(), work.getStartPageNum(workFile), work.getEndPageNum(workFile), work);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath)
  {
    setPreview(src, filePath, null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath, HDT_Record record)
  {
    if (record instanceof HDT_Work work)
      instance.doSetPreview(src, filePath, work.getStartPageNum(), work.getEndPageNum(), work);
    else
      instance.doSetPreview(src, filePath, -1, -1, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show(PreviewSource src, HDT_RecordWithPath record)
  {
    setPreview(src, record);
    show(src);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, HDT_RecordWithPath record)
  {
    if (record instanceof HDT_Work work)
      setPreview(src, work.filePathIncludeExt(), work);
    else
      setPreview(src, record.filePath(), record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
  {
    instance.doSetPreview(src, filePath, startPageNum, endPageNum, record);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether the Lock button currently defers new previews for {@code src}:
   * locked, {@code src} is the active tab, and it is showing something to
   * keep. Initiators outside {@link #doSetPreview} (the FTS flow) check this
   * and register their replay via {@link #runWhenUnlocked}.
   */
  public static boolean isPreviewLocked(PreviewSource src)
  {
    return (instance != null) && instance.btnLock.isSelected() && (instance.curSource() == src) && (hostFor(src).confirmedFile() != null);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Schedules {@code replay} to run when the Lock button is next unlocked,
   *  replacing any replay already scheduled for {@code src}. */
  public static void runWhenUnlocked(PreviewSource src, Runnable replay)
  {
    if (instance != null)
      instance.srcToLockedReplay.put(src, replay);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Whether a preview set for {@code src} would be shown immediately rather than deferred: the preview
   * window is open and {@code src} is its active tab. Mirrors the guard inside {@link #doSetPreview},
   * which defers a record-navigation intent until its source is the active, showing one; exposed so
   * initiators with their own pipeline (the FTS flow) can match the File Manager's laziness, avoiding
   * expensive work (LibreOffice conversion, pdf.js extraction) for a preview no one is looking at.
   */
  public static boolean isSourceActiveAndShowing(PreviewSource src)
  {
    return (instance != null) && instance.isShowing() && (instance.curSource() == src);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Schedules {@code action} to run the next time {@code src} becomes the active, showing preview source
   * (the window is opened on that tab, or that tab is selected while the window is showing). Replaces any
   * previously-scheduled action for {@code src}; pass {@code null} to cancel. Lets a caller that declined
   * to generate a preview while it was not visible (see {@link #isSourceActiveAndShowing}) kick off the
   * work once it will actually be seen, rather than eagerly on every row selection.
   */
  public static void runWhenSourceActivates(PreviewSource src, Runnable action)
  {
    if (action == null)
      pendingActivation.remove(src);
    else
      pendingActivation.put(src, action);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /** Runs and clears the action deferred for {@code src} via {@link #runWhenSourceActivates}, if any.
   *  Called from {@link PreviewWrapper#activate()} once {@code src} is the active, showing source. */
  static void fireActivation(PreviewSource src)
  {
    nullSwitch(pendingActivation.remove(src), Runnable::run);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // The FTS-driven preview flow of the queries tab runs through its
  // reconciler host (see PreviewPaneHost): the FTS controller sets intent and
  // pushes hit results; the display is derived, never commanded directly.

  /** The wrapper for {@code src}, or {@code null} if the preview system is
   *  unavailable; the pane's {@link PreviewPaneHost} drives it as the ViewerPort. */
  static PreviewWrapper wrapperForSource(PreviewSource src)
  {
    return (jxBrowserDisabled || (instance == null)) ? null : instance.srcToWrapper.get(src);
  }

//---------------------------------------------------------------------------

  /** The reconciler host for {@code src} (one per pane), created on first use. */
  static PreviewPaneHost hostFor(PreviewSource src)
  {
    return srcToHost.computeIfAbsent(src, PreviewPaneHost::new);
  }

//---------------------------------------------------------------------------

  /** Sets the queries pane's FTS preview intent; see {@link PreviewPaneHost#setPreview}. */
  public static void setQueriesFtsPreview(FilePath filePath, HDT_Record record, boolean paged, int pageNum, boolean wantsHighlights, ScrollTarget scrollTarget)
  {
    if (jxBrowserDisabled || (instance == null)) return;

    hostFor(pvsQueriesTab).setPreview(filePath, record, paged, pageNum, wantsHighlights, scrollTarget);
  }

//---------------------------------------------------------------------------

  /** Delivers computed paged (pdf.js) hit results for the intended file; a
   *  {@code null} JSON means the computation found no hits to apply. */
  public static void updateQueriesFtsHitsPaged(FilePath filePath, String hitsJson, int firstMatchPage)
  {
    hostFor(pvsQueriesTab).updateHitsPaged(filePath, hitsJson, firstMatchPage);
  }

//---------------------------------------------------------------------------

  /** Delivers computed direct-content hit results for the intended file. */
  public static void updateQueriesFtsHitsDirect(FilePath filePath, String hitsJson)
  {
    hostFor(pvsQueriesTab).updateHitsDirect(filePath, hitsJson);
  }

//---------------------------------------------------------------------------

  /** Reports that hit computation for the intended file failed; the pane
   *  degrades to an unhighlighted display rather than withholding it. */
  public static void updateQueriesFtsHitsFailed(FilePath filePath)
  {
    hostFor(pvsQueriesTab).updateHitsFailed(filePath);
  }

//---------------------------------------------------------------------------

  /** Clears the queries pane's FTS preview (intent = none). */
  public static void clearQueriesFtsPreview()
  {
    hostFor(pvsQueriesTab).clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Find or create the {@link ConversionSession} for the given source wrapper
   * and file. Returns {@code null} if the preview system is unavailable.
   *
   * <p>The caller attaches display or extraction subscribers to the returned
   * session to drive the UI and/or receive the converted path, and should call
   * {@link OfficePreviewer#enqueueForConversion} to actually queue the
   * conversion for the background thread.
   */
  public static ConversionSession getOrCreateSession(PreviewSource src, String mimetypeStr, FilePath filePath)
  {
    if (jxBrowserDisabled || (instance == null)) return null;

    PreviewWrapper wrapper = instance.srcToWrapper.get(src);
    if (wrapper == null) return null;

    if (wrapper.ensureInitialized() == false) return null;

    return OfficePreviewer.getOrCreateSession(filePath, mimetypeStr);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Enqueues a conversion for the session returned from {@link #getOrCreateSession}.
   * Separated from {@code getOrCreateSession} so callers can subscribe first,
   * then enqueue (ensuring no state transition fires before the subscriber
   * is ready to receive it).
   */
  public static void enqueueForConversion(PreviewSource src, ConversionSession session)
  {
    if ((instance == null) || (session == null)) return;

    nullSwitch(instance.srcToWrapper.get(src), _ ->
      OfficePreviewer.enqueueForConversion(session));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doSetPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
  {
    if (jxBrowserDisabled || disablePreviewUpdating) return;

    if ((record != null) && (record.getType () != hdtWork    ) && (record.getType() != hdtMiscFile) &&
                            (record.getType () != hdtWorkFile) && (record.getType() != hdtPerson  ))
      record = null;

    if (btnLock.isSelected() && (curSource() == src) && (hostFor(src).confirmedFile() != null))
    {
      HDT_Record lockedRecord = record;

      srcToLockedReplay.put(src, () -> doSetPreview(src, filePath, startPageNum, endPageNum, lockedRecord));
      return;
    }

    // Every pane routes through its reconciler host as intent: the host is that
    // source's single decision layer, deriving the display rather than being
    // commanded directly. Work-page numbers stay on the wrapper for the
    // Set-start/end chrome and the ContentsWindow.

    srcToWrapper.get(src).setWorkPageNums(startPageNum, endPageNum);

    // Directories clear the pane like empty paths do: nothing can preview a
    // folder (the File Manager passes one when a folder row is selected), and
    // the pre-host refresh path cleared the preview for them as well.

    if (FilePath.isEmpty(filePath) || filePath.isDirectory())
    {
      hostFor(src).clear();
      return;
    }

    if (isSourceActiveAndShowing(src) == false)
    {
      // Defer the load until the source is the active, showing one, then replay
      // through this same intent path (the File Manager's laziness, generalized).

      HDT_Record finalRecord = record;  // record is reassigned above, so the lambda needs a copy

      runWhenSourceActivates(src, () -> doSetPreview(src, filePath, startPageNum, endPageNum, finalRecord));
      return;
    }

    hostFor(src).setPreviewAuto(filePath, record, startPageNum);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  // To be called only from ContentsWindow

  void updatePageNumber(HDT_Work work, FilePath filePath, int pageNum, boolean isStart)
  {
    srcToWrapper.values().forEach(wrapper ->
    {
      if (FilePath.isEmpty(wrapper.getFilePath()) == false)
        if (wrapper.getFilePath().equals(filePath) && (wrapper.getRecord() == work))
        {
          wrapper.setWorkPageFromContentsWindow(pageNum, isStart);

          if (wrapper.getSource() == curSource())
          {
            if (isStart)
              updateStartBtn(pageNum);
            else
              updateEndBtn(pageNum);
          }
        }
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void switchTo(PreviewSource src)
  {
    if (curSource() == src) return;

    tpPreview.getSelectionModel().select(srcToWrapper.get(src).getTab());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void clearAll()
  {
    srcToHost.values().forEach(PreviewPaneHost::clear);
    pendingActivation.clear();
    tabToWrapper.values().forEach(PreviewWrapper::reset);
    instance().clearControls();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearControls()
  {
    stage.setTitle(dialogTitle);
    tfPath.setText("");
    setToolTip(tfPath, "");
    paneType.getChildren().clear();
    lblRecord.setText("");
    setToolTip(lblRecord, "");
    resetNavBtns();
    updateFileNavButtons();

    tfPreviewPage.setText("");

    setHiliteScanningIndicator(false);

    disableAll(tfPreviewPage, btnPreviewPrev, btnPreviewNext, btnHilitePrev, btnHiliteNext, btnPreviewBack, btnPreviewForward);

    sldPreview.setValue(1);
    lblPreviewPages.setText("");

    ContentsWindow.clear();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Swaps the annotation-navigation buttons' graphics for animated loading dots
   * while the current pane's annotation scan is running, and back to the normal
   * icons when it isn't. The buttons stay disabled while scanning (there is
   * nothing to navigate to yet); the inline opacity override keeps the
   * indicator from being washed out by the disabled-state dimming, which is the
   * point: a dimmed idle button reads as "no annotations", and until the scan
   * finishes that isn't known.
   */
  private void setHiliteScanningIndicator(boolean scanning)
  {
    if (scanning == (btnHilitePrev.getGraphic() == hiliteDotsPrev)) return;

    if (scanning)
    {
      btnHilitePrev.setGraphic(hiliteDotsPrev);
      btnHiliteNext.setGraphic(hiliteDotsNext);

      btnHilitePrev.setStyle("-fx-opacity: 1.0;");
      btnHiliteNext.setStyle("-fx-opacity: 1.0;");

      hiliteDotsPrev.play();
      hiliteDotsNext.play();
    }
    else
    {
      hiliteDotsPrev.stop();
      hiliteDotsNext.stop();

      btnHilitePrev.setGraphic(hiliteGraphicPrev);
      btnHiliteNext.setGraphic(hiliteGraphicNext);

      btnHilitePrev.setStyle("");
      btnHiliteNext.setStyle("");
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateFileNavButtons()
  {
    btnFileBack   .setDisable(curWrapper().enableFileNavButton(false) == false);
    btnFileForward.setDisable(curWrapper().enableFileNavButton(true ) == false);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void refreshControls(PreviewSource src)
  {
    if (curSource() == src)
      curWrapper().refreshControls();
  }

  void refreshControls(int pageNum, int numPages, PreviewWrapper previewWrapper)
  {
    if (curWrapper() != previewWrapper) return;

    FilePath filePath = previewWrapper.getFilePath();
    HDT_RecordWithPath record = previewWrapper.getRecord();

    refreshingControls = true;

    tfPreviewPage.setText(previewWrapper.getLabelByPage(pageNum));

    tfPreviewPage.setDisable(false);

    if (record == null)
    {
      paneType.getChildren().clear();
      lblRecord.setText("(No associated record)");
      setToolTip(lblRecord, "");
      resetNavBtns();
    }
    else
    {
      if (record.getType() == hdtWorkFile)
      {
        HDT_WorkFile workFile = (HDT_WorkFile)record;
        if (workFile.works.size() > 0)
          record = workFile.works.getFirst();
      }

      ImageView iv = imgViewForRecord(record, record.getType());

      paneType.getChildren().setAll(iv);

      if (record.getType() == hdtWork)
      {
        String recStr = record.defaultChoiceText();
        lblRecord.setText(recStr);
        setToolTip(lblRecord, recStr);

        enableAll(btnSetStart, btnStartPage, btnSetEnd, btnEndPage);

        updateStartBtn(previewWrapper.getWorkStartPageNum());
        updateEndBtn  (previewWrapper.getWorkEndPageNum  ());

        btnContents.setDisable(true);
        btnContents.setText("No other records...");

        HDT_RecordWithPath showingFile = HyperPath.getRecordFromFilePath(previewWrapper.getFilePath());

        if (showingFile == null) // External file (specified in URL field) is being previewed
        {
          btnContents.setDisable(false);
          btnContents.setText("Show contents");
        }
        else if (showingFile.getType() == hdtWorkFile)
        {
          HDT_WorkFile workFile = (HDT_WorkFile) showingFile;

          if (workFile.works.size() > 1)
          {
            btnContents.setDisable(false);
            btnContents.setText(workFile.works.size() == 2 ? "1 other record..." : ((workFile.works.size() - 1) + " other records..."));
          }
        }
      }
      else
      {
        String label = record.defaultCellText();

        lblRecord.setText(label);
        setToolTip(lblRecord, label);
        resetNavBtns();
      }
    }

    sldPreview.setMin(1);
    sldPreview.setMax(numPages);
    sldPreview.setValue(pageNum);

    tfPath.setText(filePath.toString());
    setToolTip(tfPath, filePath.toString());
    stage.setTitle(dialogTitle + " - " + filePath.getNameOnly());

    btnPreviewPrev.setDisable(pageNum == 1);
    btnPreviewNext.setDisable(pageNum == numPages);

    btnPreviewBack   .setDisable(previewWrapper.enableNavButton(false) == false);
    btnPreviewForward.setDisable(previewWrapper.enableNavButton(true ) == false);

    updateFileNavButtons();

    if (previewWrapper.annotScanInProgress())
    {
      setHiliteScanningIndicator(true);

      btnHilitePrev.setDisable(true);
      btnHiliteNext.setDisable(true);
    }
    else
    {
      setHiliteScanningIndicator(false);

      int lowest = previewWrapper.lowestHilitePage();

      if (lowest < 0)
      {
        btnHilitePrev.setDisable(true);
        btnHiliteNext.setDisable(true);
      }
      else
      {
        btnHilitePrev.setDisable(pageNum <= lowest);
        btnHiliteNext.setDisable(pageNum >= previewWrapper.highestHilitePage());
      }
    }

    lblPreviewPages.setText(pageNum + " / " + numPages);

    HDT_WorkFile workFile = null;

    if ((record != null) && (record.getType() == hdtWorkFile))
      workFile = (HDT_WorkFile) record;
    else
    {
      record = HyperPath.getRecordFromFilePath(filePath);

      if ((record != null) && (record.getType() == hdtWorkFile))
        workFile = (HDT_WorkFile) record;
    }

    if (workFile == null)
      ContentsWindow.instance().update(filePath, pageNum);
    else
      ContentsWindow.instance().update(workFile, pageNum);

    refreshingControls = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Runnable getDisposeHandler(Iterator<PreviewWrapper> it)
  {
    return () ->
    {
      synchronized(LOCK)
      {
        if (it.hasNext())
        {
          it.next().cleanup(getDisposeHandler(it));
          return;
        }
      }

      // Closing the shared engine terminates the Chromium process tree and closes any
      // browser still open; there is no straggler sweep and no per-browser process
      // management (JxBrowser 6 needed both; its exit-hang failure modes are gone).

      BrowserEngine.shutdown();

      Platform.runLater(() ->
      {
        if (app.debugging)
          System.out.println("Shutdown: closing main window");

        ui.getStage().close();

        ExitWatchdog.arm();  // Teardown is complete; anything still running after the grace period gets logged, then the process exits
      });
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void show()
  {
    show((PreviewSource) null);
  }

  public static void show(PreviewSource src)
  {
    if ((instance == null) || jxBrowserDisabled) return;

    if (src != null)
      instance.switchTo(src);

    show(instance);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void close(boolean exitingApp)
  {
    // On app exit, detach live BrowserViews from the scene graph before the stage
    // closes; otherwise JxBrowser's SceneTracker reacts to the closing window with
    // Platform.runLater callbacks that run after the native window peer is gone
    // ("Failed to get native widget ID" on the FX thread). The browsers themselves
    // are closed later by the cleanup() dispose chain.

    if (exitingApp && (instance != null))
      tabToWrapper.values().forEach(PreviewWrapper::detachBrowserView);

    close(instance, exitingApp);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void cleanup()
  {
    if (jxBrowserInitialized == false) return;

    if (app.debugging)
      System.out.println("Shutdown: disposing browser instances...");

    // The chain calls the blocking browser/engine close(), which must stay off the FX
    // thread (closing a browser whose view is in the scene graph can need the FX thread,
    // so blocking it there can deadlock). The chain's final step hops back onto the FX
    // thread itself for the window close.

    runOutsideFXThread(getDisposeHandler(tabToWrapper.values().iterator()));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
