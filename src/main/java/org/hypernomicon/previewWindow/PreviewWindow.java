/*
 * Copyright 2015-2025 Jason Winning
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
import static org.hypernomicon.util.MediaUtil.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;

import java.util.*;

import com.teamdev.jxbrowser.chromium.BrowserCore;
import com.teamdev.jxbrowser.chromium.internal.Environment;
import com.teamdev.jxbrowser.chromium.internal.ipc.IPCException;

import org.hypernomicon.Const.PrefKey;
import org.hypernomicon.dialogs.base.NonmodalWindow;
import org.hypernomicon.fileManager.FileManager;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.*;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Side;
import javafx.scene.control.*;
import javafx.scene.image.ImageView;
import javafx.scene.input.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;

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

  private static final Map<Tab, PreviewWrapper> tabToWrapper = new HashMap<>();

  private final Map<PreviewSource, PreviewWrapper> srcToWrapper = new EnumMap<>(PreviewSource.class);
  private final Map<PreviewSource, PreviewSetting> srcToSetting = new EnumMap<>(PreviewSource.class);

  public static boolean disablePreviewUpdating = false;

//---------------------------------------------------------------------------

  public static void clearAll()                  { tabToWrapper.values().forEach(PreviewWrapper::reset); instance().clearControls(); }
  public FilePath getFilePath(PreviewSource src) { return srcToWrapper.get(src).getFilePath(); }
  private PreviewWrapper curWrapper()            { return tabToWrapper.get(tpPreview.getSelectionModel().getSelectedItem()); }
  PreviewSource curSource()                      { return curWrapper().getSource(); }
  int curPage()                                  { return (int) sldPreview.getValue(); }
  int getMax()                                   { return (int) sldPreview.getMax(); }

  public static void close(boolean exitingApp)   { close(instance, exitingApp); }

  @Override protected void getDividerPositions() { }
  @Override protected void setDividerPositions() { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public enum PreviewSource { pvsPersonTab, pvsWorkTab, pvsQueriesTab, pvsManager, pvsTreeTab, pvsOther }

  private record PreviewSetting(FilePath filePath, int startPageNum, int endPageNum, HDT_Record record) { }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static PreviewWindow instance()
  {
    if (instance == null) instance = new PreviewWindow();

    return instance;
  }

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

    btnRefresh.setOnAction(event -> curWrapper().refreshPreview(true, false));

    btnLaunch.setOnAction(event ->
    {
      FilePath filePath = curWrapper().getFilePath();

      if (FilePath.isEmpty(filePath) == false)
        DesktopUtil.launchWorkFile(filePath, curWrapper().getPageNum());
    });

    btnLock.selectedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(newValue))
        btnLock.setGraphic(imgViewFromRelPath("resources/images/lock.png"));
      else
      {
        btnLock.setGraphic(imgViewFromRelPath("resources/images/lock_open.png"));
        srcToSetting.forEach(PreviewWindow::setPreview);
        srcToSetting.clear();
      }
    });

    sldPreview.valueProperty().addListener((ob, oldValue, newValue) ->
    {
      if (tfPreviewPage.isDisabled() == false)
      {
        tfPreviewPage.setText(curWrapper().getLabelByPage(newValue.intValue()));

        lblPreviewPages.setText(newValue.intValue() + " / " + curWrapper().getNumPages());

        if (sldPreview.isValueChanging() == false)
          curWrapper().updatePage(newValue.intValue());
      }
    });

    sldPreview.valueChangingProperty().addListener((ob, oldValue, newValue) ->
    {
      if (Boolean.TRUE.equals(oldValue) && Boolean.FALSE.equals(newValue) && (tfPreviewPage.isDisabled() == false))
        curWrapper().setPreview((int) sldPreview.getValue(), true);
    });

    btnHilitePrev.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled() == false)
        curWrapper().setPreview(curWrapper().getPrevHilite((int) sldPreview.getValue()), true);
    });

    btnHiliteNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled() == false)
        curWrapper().setPreview(curWrapper().getNextHilite((int) sldPreview.getValue()), true);
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

      int curPage = (int) sldPreview.getValue();

      if (curPage < 2) return;

      curWrapper().setPreview(--curPage, true);
    });

    btnPreviewNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      int curPage = (int) sldPreview.getValue();

      if (curPage >= sldPreview.getMax()) return;

      curWrapper().setPreview(++curPage, true);
    });

    btnStartPage.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      int curPage = (int) sldPreview.getValue(),
          workPage = curWrapper().getWorkStartPageNum();

      if (workPage < 0) workPage = 1;
      if (curPage == workPage) return;

      curWrapper().setPreview(workPage, true);
    });

    btnEndPage.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;

      int curPage = (int) sldPreview.getValue(),
          workPage = curWrapper().getWorkEndPageNum();

      if (workPage < 0) workPage = (int) sldPreview.getMax();
      if (curPage == workPage) return;

      curWrapper().setPreview(workPage, true);
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

      tabToWrapper.get(newValue).activate();
    });

    tfPreviewPage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      int pageNum = curWrapper().getPageNum();

      tfPreviewPage.setText(Boolean.TRUE.equals(newValue) ? "" : (pageNum == -1 ? "" : curWrapper().getLabelByPage(pageNum)));
    });

    tfPreviewPage.setOnAction(event -> curWrapper().updatePage(curWrapper().getPageByLabel(tfPreviewPage.getText())));

    onShown = () -> runDelayedInFXThread(1, 300, () -> curWrapper().activate());

    stage.setOnHiding(event -> srcToWrapper.values().forEach(PreviewWrapper::prepareToHide));

    onHidden = () -> srcToWrapper.values().forEach(PreviewWrapper::prepareToShow);

    btnContents.setOnAction(event -> ContentsWindow.show());

    stage.addEventFilter(ScrollEvent.SCROLL, event ->
    {
      double deltaY = event.getDeltaY();
      if ((event.isControlDown() == false) || (deltaY == 0)) return;

      if (curWrapper().zoom(deltaY > 0))
        event.consume();
    });

    stage.addEventFilter(KeyEvent.KEY_PRESSED, event ->
    {
      if (shortcutKeyIsDown(event))
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
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void goToPage(int pageNum)
  {
    if (pageNum < 0) pageNum = 1;

    curWrapper().setPreview(pageNum, true);
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

  public static void setPreview(PreviewSource src, HDT_WorkFile workFile, HDT_Work work)
  {
    instance.doSetPreview(src, workFile.filePath(), work.getStartPageNum(workFile), work.getEndPageNum(workFile), work);
  }

//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath)
  {
    setPreview(src, filePath, null);
  }

//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath, HDT_Record record)
  {
    if (record instanceof HDT_Work work)
      instance.doSetPreview(src, filePath, work.getStartPageNum(), work.getEndPageNum(), work);
    else
      instance.doSetPreview(src, filePath, -1, -1, record);
  }

//---------------------------------------------------------------------------

  public static void show(PreviewSource src, HDT_RecordWithPath record)
  {
    setPreview(src, record);
    show(src);
  }

//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, HDT_RecordWithPath record)
  {
    if (record instanceof HDT_Work work)
      setPreview(src, work.filePathIncludeExt(), work);
    else
      setPreview(src, record.filePath(), record);
  }

//---------------------------------------------------------------------------

  private static void setPreview(PreviewSource src, PreviewSetting setting)
  {
    setPreview(src, setting.filePath, setting.startPageNum, setting.endPageNum, setting.record);
  }

//---------------------------------------------------------------------------

  public static void setPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
  {
    instance.doSetPreview(src, filePath, startPageNum, endPageNum, record);
  }

  private void doSetPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
  {
    if (jxBrowserDisabled || disablePreviewUpdating) return;

    boolean previewAlreadySet = false;

    if ((record != null) && (record.getType () != hdtWork    ) && (record.getType() != hdtMiscFile) &&
                            (record.getType () != hdtWorkFile) && (record.getType() != hdtPerson  ))
      record = null;

    if (btnLock.isSelected() && (curSource() == src) && (curWrapper().getFilePathShowing() != null))
    {
      srcToSetting.put(src, new PreviewSetting(filePath, startPageNum, endPageNum, record));
      return;
    }

    for (PreviewWrapper wrapper : srcToWrapper.values())
    {
      if (FilePath.isEmpty(wrapper.getFilePath())           ||
          FilePath.isEmpty(filePath)                        ||
          (wrapper.getFilePath().equals(filePath) == false) ||
          (wrapper.getRecord() != record))
        continue;

      wrapper.setWorkPageNums(startPageNum, endPageNum);

      if ((wrapper == curWrapper()) && (src == curSource()))
      {
        wrapper.refreshControls();
        previewAlreadySet = true;
      }
    }

    if (previewAlreadySet) return;

    srcToWrapper.get(src).setWorkPageNums(startPageNum, endPageNum);

    if (startPageNum < 0) startPageNum = 1;

    srcToWrapper.get(src).setPreview(filePath, startPageNum, record);
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

    disableAll(tfPreviewPage, btnPreviewPrev, btnPreviewNext, btnHilitePrev, btnHiliteNext, btnPreviewBack, btnPreviewForward);

    sldPreview.setValue(1);
    lblPreviewPages.setText("/ 0");

    ContentsWindow.clear();
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

    disablePreviewUpdating = true;

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
          record = workFile.works.get(0);
      }

      ImageView iv = imgViewForRecord(record);

      paneType.getChildren().setAll(iv);

      if (record.getType() == hdtWork)
      {
        String recStr = record.getCBText();
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
        String label = record.listName();

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

    disablePreviewUpdating = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static Runnable getDisposeHandler(Iterator<PreviewWrapper> it)
  {
    return () ->
    {
      synchronized(tabToWrapper)
      {
        if (it.hasNext())
        {
          it.next().cleanup(getDisposeHandler(it));
          return;
        }
      }

      try
      {
        BrowserCore.shutdown();
      }
      catch (IPCException e)
      {
        errorPopup("An error occurred while shutting down preview window: " + getThrowableMessage(e));
      }

      Platform.runLater(() ->
      {
        ui.getStage().close();

        if (Environment.isMac())
          Platform.exit();
      });
    };
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  static void show()
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

  public static void cleanup()
  {
    if (jxBrowserInitialized)
      getDisposeHandler(tabToWrapper.values().iterator()).run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
