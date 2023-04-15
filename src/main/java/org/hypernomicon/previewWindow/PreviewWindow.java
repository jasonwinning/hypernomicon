/*
 * Copyright 2015-2023 Jason Winning
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
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;
import static org.hypernomicon.model.records.RecordType.*;
import static org.hypernomicon.previewWindow.PreviewWindow.PreviewSource.*;

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import com.teamdev.jxbrowser.chromium.BrowserCore;
import com.teamdev.jxbrowser.chromium.internal.Environment;
import com.teamdev.jxbrowser.chromium.internal.ipc.IPCException;

import org.hypernomicon.dialogs.HyperDlg;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Record;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.DesktopUtil;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.wrappers.ClickHoldButton;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Side;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleButton;
import javafx.scene.image.ImageView;
import javafx.scene.control.Slider;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.control.TextField;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.Pane;
import javafx.stage.Modality;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.StageStyle;

//---------------------------------------------------------------------------

public class PreviewWindow extends HyperDlg
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

  public enum PreviewSource { pvsPersonTab, pvsWorkTab, pvsQueriesTab, pvsManager, pvsTreeTab, pvsOther }

  private static final String dialogTitle = "Work Viewer",
                              TEXT_TO_SHOW_IF_NONE = "(none)";

  public boolean disablePreviewUpdating = false;

  private static final Map<Tab, PreviewWrapper> tabToWrapper = new HashMap<>();

  private final Map<PreviewSource, PreviewWrapper> srcToWrapper = new EnumMap<>(PreviewSource.class);
  private final Map<PreviewSource, PreviewSetting> srcToSetting = new EnumMap<>(PreviewSource.class);
  private final ClickHoldButton chbBack, chbForward;

  public void clearAll()                         { tabToWrapper.values().forEach(PreviewWrapper::reset); clearControls(); }
  public void clearPreview(PreviewSource src)    { setPreview(src, null, null); }
  public FilePath getFilePath(PreviewSource src) { return srcToWrapper.get(src).getFilePath(); }
  private PreviewWrapper curWrapper()            { return tabToWrapper.get(tpPreview.getSelectionModel().getSelectedItem()); }
  PreviewSource curSource()                      { return curWrapper().getSource(); }
  int curPage()                                  { return (int) sldPreview.getValue(); }
  int getMax()                                   { return (int) sldPreview.getMax(); }

  @Override protected boolean isValid()          { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final class PreviewSetting
  {
    private PreviewSetting(FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
    {
      this.filePath = filePath;
      this.startPageNum = startPageNum;
      this.endPageNum = endPageNum;
      this.record = record;
    }

    private final FilePath filePath;
    private final int startPageNum, endPageNum;
    private final HDT_Record record;
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

  public PreviewWindow()
  {
    super("previewWindow/PreviewWindow", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);

    addWrapper(pvsPersonTab , apPerson , tabPerson , btnPerson );
    addWrapper(pvsWorkTab   , apWork   , tabWork   , btnWorks  );
    addWrapper(pvsQueriesTab, apQuery  , tabQuery  , btnQueries);
    addWrapper(pvsManager   , apManager, tabManager, btnManager);
    addWrapper(pvsOther     , apOther  , tabOther  , btnOther  );
    addWrapper(pvsTreeTab   , apTree   , tabTree   , btnTree   );

    btnPerson.getToggleGroup().selectedToggleProperty().addListener((ob, oldVal, newVal) ->
    {
      if (newVal == null)
        oldVal.setSelected(true);
    });

    tabToWrapper.values().forEach(PreviewWrapper::clearPreview);

    lblRecord.setOnMouseClicked(event -> curWrapper().go());

    paneType.setOnMouseClicked(event -> curWrapper().go());

    btnGoToMain.setOnAction(event -> ui.windows.focusStage(ui.getStage()));
    btnGoToManager.setOnAction(event ->
    {
      if (fileManagerDlg.getStage().isShowing())
        ui.windows.focusStage(fileManagerDlg.getStage());
      else
        fileManagerDlg.showNonmodal();
    });

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
        srcToSetting.forEach((src, setting) -> setPreview(src, setting.filePath, setting.startPageNum, setting.endPageNum, setting.record));
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
      if ((oldValue == null) || (newValue == null)) return;

      if (oldValue && Boolean.FALSE.equals(newValue))
      {
        if (tfPreviewPage.isDisabled() == false)
          curWrapper().setPreview((int) sldPreview.getValue(), true);
      }
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

    chbBack    = new ClickHoldButton(btnPreviewBack   , Side.BOTTOM);
    chbForward = new ClickHoldButton(btnPreviewForward, Side.BOTTOM);

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
    setToolTip(sldPreview       , "Navigate to different page");

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

    btnFileBack.setOnAction(event -> curWrapper().fileNavClick(false));
    btnFileForward.setOnAction(event -> curWrapper().fileNavClick(true));

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

      PreviewWrapper wrapper = tabToWrapper.get(newValue);

      if (wrapper.needsRefresh())
        wrapper.refreshPreview(false, true);
      else
        wrapper.refreshControls();

      wrapper.getToggleButton().setSelected(true);
    });

    tfPreviewPage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      int pageNum = curWrapper().getPageNum();

      tfPreviewPage.setText(Boolean.TRUE.equals(newValue) ? "" : (pageNum == -1 ? "" : curWrapper().getLabelByPage(pageNum)));
    });

    tfPreviewPage.setOnAction(event -> curWrapper().updatePage(curWrapper().getPageByLabel(tfPreviewPage.getText())));

    onShown = () ->
    {
      Stage stage = getStage();

      if (stage.getX() < 5) stage.setX(5);
      if (stage.getY() < 5) stage.setY(5);

      Rectangle2D bounds = Screen.getPrimary().getBounds();

      if (stage.getWidth() >= bounds.getMaxX() - 100)
        stage.setWidth(bounds.getMaxX() - 100);

      if (stage.getHeight() >= bounds.getMaxY() - 100)
        stage.setHeight(bounds.getMaxY() - 100);

      ui.windows.push(dialogStage);

      PreviewWrapper wrapper = curWrapper();

      if (wrapper.needsRefresh())
        wrapper.refreshPreview(false, true);
    };

    dialogStage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if (ui.windows.getCyclingFocus()) return;

      if (Boolean.TRUE.equals(newValue) == false) return;

      ui.windows.push(dialogStage);
    });

    dialogStage.setOnHiding(event ->
    {
      srcToWrapper.values().forEach(PreviewWrapper::prepareToHide);
      ui.windows.focusStage(ui.getStage());
    });

    dialogStage.setOnHidden(event -> srcToWrapper.values().forEach(PreviewWrapper::prepareToShow));

    btnContents.setOnAction(event -> openContentsWindow());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void setPreview(PreviewSource src, FilePath filePath, HDT_Record record)
  {
    setPreview(src, filePath, -1, -1, record);
  }

  public void setPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Record record)
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

  public void switchTo(PreviewSource src)
  {
    if (curSource() == src) return;

    tpPreview.getSelectionModel().select(srcToWrapper.get(src).getTab());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  void clearControls()
  {
    dialogStage.setTitle(dialogTitle);
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

    if (contentsWindow != null)
      contentsWindow.clear();
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

  void refreshControls(int pageNum, int numPages, FilePath filePath, HDT_RecordWithPath record)
  {
    disablePreviewUpdating = true;

    tfPreviewPage.setText(curWrapper().getLabelByPage(pageNum));

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

        updateStartBtn(curWrapper().getWorkStartPageNum());
        updateEndBtn  (curWrapper().getWorkEndPageNum  ());

        btnContents.setDisable(true);
        btnContents.setText("No other records...");

        HDT_RecordWithPath showingFile = HyperPath.getRecordFromFilePath(curWrapper().getFilePath());

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
    dialogStage.setTitle(dialogTitle + " - " + filePath.getNameOnly());

    btnPreviewPrev.setDisable(pageNum == 1);
    btnPreviewNext.setDisable(pageNum == numPages);

    btnPreviewBack   .setDisable(curWrapper().enableNavButton(false) == false);
    btnPreviewForward.setDisable(curWrapper().enableNavButton(true ) == false);

    updateFileNavButtons();

    curWrapper().refreshNavMenu(chbBack.getMenu(), false);
    curWrapper().refreshNavMenu(chbForward.getMenu(), true);

    int lowest = curWrapper().lowestHilitePage();

    if (lowest < 0)
    {
      btnHilitePrev.setDisable(true);
      btnHiliteNext.setDisable(true);
    }
    else
    {
      btnHilitePrev.setDisable(pageNum <= lowest);
      btnHiliteNext.setDisable(pageNum >= curWrapper().highestHilitePage());
    }

    lblPreviewPages.setText(pageNum + " / " + numPages);

    record = HyperPath.getRecordFromFilePath(curWrapper().getFilePath());
    HDT_WorkFile workFile = null;

    if ((record != null) && (record.getType() == hdtWorkFile))
      workFile = (HDT_WorkFile) record;

    if (workFile == null)
      contentsWindow.update(curWrapper().getFilePath(), pageNum);
    else
      contentsWindow.update(workFile, pageNum);

    disablePreviewUpdating = false;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static void openContentsWindow()
  {
    if (contentsWindow.getStage().isShowing())
      ui.windows.focusStage(contentsWindow.getStage());
    else
      contentsWindow.showNonmodal();
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
        messageDialog("An error occurred while shutting down preview window: " + e.getMessage(), mtError);
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

  public static void cleanup()
  {
    if (jxBrowserInitialized)
      getDisposeHandler(tabToWrapper.values().iterator()).run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
