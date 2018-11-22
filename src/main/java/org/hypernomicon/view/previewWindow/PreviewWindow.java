/*
 * Copyright 2015-2018 Jason Winning
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

package org.hypernomicon.view.previewWindow;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.model.records.HDT_RecordType.*;
import static org.hypernomicon.view.previewWindow.PreviewWindow.PreviewSource.*;

import java.util.HashMap;
import java.util.Iterator;

import com.teamdev.jxbrowser.chromium.BrowserCore;
import com.teamdev.jxbrowser.chromium.internal.Environment;

import org.hypernomicon.App;
import org.hypernomicon.model.items.HyperPath;
import org.hypernomicon.model.records.HDT_Base;
import org.hypernomicon.model.records.SimpleRecordTypes.HDT_RecordWithPath;
import org.hypernomicon.model.records.HDT_Work;
import org.hypernomicon.model.records.HDT_WorkFile;
import org.hypernomicon.util.filePath.FilePath;
import org.hypernomicon.view.dialogs.HyperDialog;
import org.hypernomicon.view.wrappers.ClickHoldButton;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.geometry.Rectangle2D;
import javafx.geometry.Side;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ToggleButton;
import javafx.scene.control.Tooltip;
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

public class PreviewWindow extends HyperDialog
{
  public static final String TEXT_TO_SHOW_IF_NONE = "(none)";
  
  public static enum PreviewSource
  {
    pvsPersonTab,
    pvsWorkTab,
    pvsQueryTab,
    pvsManager,    
    pvsTreeTab,
    pvsOther
  } 
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  @FXML private AnchorPane apPerson;
  @FXML private AnchorPane apWork;
  @FXML private AnchorPane apQuery;
  @FXML private AnchorPane apManager;
  @FXML private AnchorPane apOther;
  @FXML private AnchorPane apTree;
  @FXML TabPane tpPreview;
  @FXML private Tab tabPerson;
  @FXML private Tab tabWork;
  @FXML private Tab tabQuery;
  @FXML private Tab tabManager;
  @FXML private Tab tabOther;
  @FXML private Tab tabTree;
  @FXML private Button btnPreviewPrev;
  @FXML private Button btnPreviewNext;
  @FXML private Button btnPreviewBack;
  @FXML private Button btnPreviewForward;
  @FXML private Button btnHilitePrev;
  @FXML private Button btnHiliteNext;
  @FXML private Button btnRefresh;
  @FXML private Button btnLaunch;
  @FXML private Button btnGoToMain;
  @FXML private Button btnGoToManager;
  @FXML private Button btnFileBack;
  @FXML private Button btnFileForward;  
  @FXML private ToggleButton btnPerson;
  @FXML private ToggleButton btnWorks;
  @FXML private ToggleButton btnQueries;
  @FXML private ToggleButton btnManager;
  @FXML private ToggleButton btnOther;
  @FXML private ToggleButton btnTree;  
  @FXML private ToggleButton btnLock;
  @FXML private TextField tfPreviewPage;
  @FXML private Slider sldPreview;
  @FXML private Label lblPreviewPages;
  @FXML private TextField tfPath;
  @FXML private Label lblRecord;
  @FXML private Button btnStartPage;
  @FXML private Button btnEndPage;
  @FXML private Button btnSetStart;
  @FXML private Button btnSetEnd;
  @FXML private Pane paneType;
  @FXML private Button btnContents;
  
  public static final String dialogTitle = "Work Viewer";
  
  public boolean disablePreviewUpdating = false;
  
  private HashMap<PreviewSource, PreviewWrapper> srcToWrapper = new HashMap<>();
  private static HashMap<Tab, PreviewWrapper> tabToWrapper = new HashMap<>();
  private ClickHoldButton chbBack, chbForward;
  
  public int curPage()                           { return (int) sldPreview.getValue(); }
  public int getMax()                            { return (int) sldPreview.getMax(); }
  public void clearAll()                         { tabToWrapper.values().forEach(PreviewWrapper::clearPreview); }
  public void clearPreview(PreviewSource src)    { srcToWrapper.get(src).clearPreview(); }
  private PreviewWrapper curWrapper()            { return tabToWrapper.get(tpPreview.getSelectionModel().getSelectedItem()); }
  public PreviewSource curSource()               { return curWrapper().getSource(); }
  public FilePath getFilePath(PreviewSource src) { return srcToWrapper.get(src).getFilePath(); }
  
  @Override protected boolean isValid()          { return true; }
  
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
    if (newVal < 0)
      btnStartPage.setText("Start p. " + TEXT_TO_SHOW_IF_NONE);
    else
      btnStartPage.setText("Start p. " + String.valueOf(newVal));    
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void updateEndBtn(int newVal)
  {
    if (newVal < 0)
      btnEndPage.setText("End p. " + TEXT_TO_SHOW_IF_NONE);
    else
      btnEndPage.setText("End p. " + String.valueOf(newVal));
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  private void resetNavBtns()
  {
    updateStartBtn(-1);
    updateEndBtn(-1);
    
    btnSetStart.setDisable(true);
    btnStartPage.setDisable(true);
    btnSetEnd.setDisable(true);
    btnEndPage.setDisable(true);
    btnContents.setDisable(true);
    btnContents.setText("No other records...");
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  public static PreviewWindow create()
  {
    PreviewWindow previewWindow = HyperDialog.createUsingFullPath("view/previewWindow/PreviewWindow.fxml", dialogTitle, true, StageStyle.DECORATED, Modality.NONE);
    previewWindow.init();
    return previewWindow;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------
  
  private void init()
  {
    addWrapper(pvsPersonTab, apPerson, tabPerson, btnPerson);
    addWrapper(pvsWorkTab, apWork, tabWork, btnWorks);
    addWrapper(pvsQueryTab, apQuery, tabQuery, btnQueries);
    addWrapper(pvsManager, apManager, tabManager, btnManager);
    addWrapper(pvsOther, apOther, tabOther, btnOther);
    addWrapper(pvsTreeTab, apTree, tabTree, btnTree);
        
    tabToWrapper.values().forEach(PreviewWrapper::clearPreview);

    lblRecord.setOnMouseClicked(event -> curWrapper().go());
    
    paneType.setOnMouseClicked(event -> curWrapper().go());

    btnGoToMain.setOnAction(event -> focusStage(app.getPrimaryStage()));
    btnGoToManager.setOnAction(event -> 
    {
      if (fileManagerDlg.getStage().isShowing())
        focusStage(fileManagerDlg.getStage());
      else
        fileManagerDlg.showNonmodal();
    });
    
    btnRefresh.setOnAction(event -> curWrapper().refreshPreview(true, false));
    
    btnLaunch.setOnAction(event ->
    {
      FilePath filePath = curWrapper().getFilePath();
      
      if (FilePath.isEmpty(filePath) == false)
        launchWorkFile(filePath, curWrapper().getPageNum());
    });
    
    btnLock.selectedProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue)
        btnLock.setGraphic(getImageViewForRelativePath("resources/images/lock.png"));        
      else
        btnLock.setGraphic(getImageViewForRelativePath("resources/images/lock_open.png"));
    });
    
    sldPreview.valueProperty().addListener((observable, oldValue, newValue) ->
    {
      if (tfPreviewPage.isDisabled() == false)
      {
        tfPreviewPage.setText(curWrapper().getLabelByPage(newValue.intValue()));
        
        lblPreviewPages.setText(newValue.intValue() + " / " + curWrapper().getNumPages());
        
        if (sldPreview.isValueChanging() == false)
          curWrapper().updatePage(newValue.intValue());
      }
    });
    
    sldPreview.valueChangingProperty().addListener((observable, oldValue, newValue) ->
    {
      if (oldValue == null) return;
      if (newValue == null) return;
      
      if (oldValue)
        if (newValue == false)
        {
          if (tfPreviewPage.isDisabled() == false)
            curWrapper().setPreview((int) sldPreview.getValue(), true);
        }
    });
    
    btnHilitePrev.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;
      
      int curPage = (int) sldPreview.getValue();
      
      curPage = curWrapper().getPrevHilite(curPage);
      
      curWrapper().setPreview(curPage, true);
    });
    
    btnHiliteNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;
      
      int curPage = (int) sldPreview.getValue();
      
      curPage = curWrapper().getNextHilite(curPage);
      
      curWrapper().setPreview(curPage, true);      
    });
       
    chbBack = new ClickHoldButton(btnPreviewBack, Side.BOTTOM);
    chbForward = new ClickHoldButton(btnPreviewForward, Side.BOTTOM);
    
    btnPreviewBack.setTooltip(new Tooltip("Click to go back, hold to see history"));
    btnPreviewForward.setTooltip(new Tooltip("Click to go forward, hold to see history"));
    
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
      
      curPage--;
      
      curWrapper().setPreview(curPage, true);
    });
    
    btnPreviewNext.setOnAction(event ->
    {
      if (tfPreviewPage.isDisabled()) return;
      
      int curPage = (int) sldPreview.getValue();
      
      if (curPage >= sldPreview.getMax()) return;
      
      curPage++;
      
      curWrapper().setPreview(curPage, true);
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
    
    tpPreview.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) ->
    {
      if (newValue == null) return;
      
      PreviewWrapper wrapper = tabToWrapper.get(newValue);
      
      if (wrapper.needsRefresh())
        wrapper.refreshPreview(false, true);
      else
        wrapper.refreshControls();
      
      wrapper.getToggleButton().setSelected(true);
    });
    
    tfPreviewPage.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      int pageNum = curWrapper().getPageNum();
      
      if (newValue) 
        tfPreviewPage.setText("");
      else
      {
        if (pageNum == -1)
          tfPreviewPage.setText("");
        else
          tfPreviewPage.setText(curWrapper().getLabelByPage(pageNum));
      }
    });
    
    tfPreviewPage.setOnAction(event -> curWrapper().updatePage(curWrapper().getPageByLabel(tfPreviewPage.getText())));
    
    onShown = () ->
    {
      Stage stage = getStage();
      
      if (stage.getX() < 5)
        stage.setX(5);
      
      if (stage.getY() < 5)
        stage.setY(5);
      
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
    
    dialogStage.focusedProperty().addListener((observable, oldValue, newValue) ->
    {
      if ((newValue == null) || (newValue == false)) return;
      
      ui.windows.push(dialogStage);
    });
    
    dialogStage.setOnHiding(event -> 
    {  
      srcToWrapper.values().forEach(PreviewWrapper::prepareToHide);
      
      focusStage(app.getPrimaryStage()); 
    });
    
    dialogStage.setOnHidden(event -> srcToWrapper.values().forEach(PreviewWrapper::prepareToShow));
    
    btnContents.setOnAction(event -> openContentsWindow());
  }
  
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------   

  public void setPreview(PreviewSource src, FilePath filePath, int startPageNum, int endPageNum, HDT_Base record)
  {
    if (App.jxBrowserDisabled || disablePreviewUpdating) return;
    
    boolean previewAlreadySet = false;   
    
    if (record != null)
      if ((record.getType() != hdtWork) && (record.getType() != hdtMiscFile))
        record = null;
    
    if (btnLock.isSelected())
      if (curSource() == src)
        if (curWrapper().getFilePathShowing() != null)
          return;
    
    for (PreviewWrapper wrapper : srcToWrapper.values())
    {
      if (FilePath.isEmpty(wrapper.getFilePath()) == false)
        if (FilePath.isEmpty(filePath) == false)
          if (wrapper.getFilePath().equals(filePath))
            if (wrapper.getRecord() == record)
            {
              wrapper.setWorkPageNums(startPageNum, endPageNum);
              
              if ((wrapper == curWrapper()) && (src == curSource()))
              {        
                wrapper.refreshControls();
                previewAlreadySet = true;
              }
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
  
  void updatePageNumber(HDT_Work work, HDT_WorkFile workFile, int pageNum, boolean isStart)
  {
    srcToWrapper.values().forEach(wrapper ->
    {
      if (FilePath.isEmpty(wrapper.getFilePath()) == false)
        if ((wrapper.getFilePath().equals(workFile.getPath().getFilePath())) &&
            (wrapper.getRecord() == work))
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
    tfPath.setTooltip(null);
    paneType.getChildren().clear();
    lblRecord.setText("");
    lblRecord.setTooltip(null);
    resetNavBtns();
    
    tfPreviewPage.setText("");
    tfPreviewPage.setDisable(true);
    btnPreviewPrev.setDisable(true);
    btnPreviewNext.setDisable(true);
    btnHilitePrev.setDisable(true);
    btnHiliteNext.setDisable(true);
    btnPreviewBack.setDisable(true);
    btnPreviewForward.setDisable(true);
    btnFileBack.setDisable(true);
    btnFileForward.setDisable(true);
    sldPreview.setValue(1);
    lblPreviewPages.setText("/ 0");
    
    if (contentsWindow != null)
      contentsWindow.clear();
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
      lblRecord.setText("(None)");
      lblRecord.setTooltip(null);
      resetNavBtns();
    }
    else
    {
      ImageView iv = getImageViewForRelativePath(ui.getGraphicRelativePathByType(record.getType()));
      paneType.getChildren().setAll(iv);
      
      if (record.getType() == hdtWork)
      {
        String recStr = HDT_Work.class.cast(record).getCBText();
        lblRecord.setText(recStr);
        lblRecord.setTooltip(new Tooltip(recStr));
        btnSetStart.setDisable(false);
        btnStartPage.setDisable(false);
        
        updateStartBtn(curWrapper().getWorkStartPageNum());

        btnSetEnd.setDisable(false);
        btnEndPage.setDisable(false);
        
        updateEndBtn(curWrapper().getWorkEndPageNum());

        HDT_WorkFile workFile = (HDT_WorkFile) HyperPath.getFileFromFilePath(curWrapper().getFilePath());
        
        if (workFile.works.size() > 1)
        {
          btnContents.setDisable(false);
          if (workFile.works.size() == 2)
            btnContents.setText("1 other record...");
          else
            btnContents.setText((workFile.works.size() - 1) + " other records...");
        }
        else
        {
          btnContents.setDisable(true);
          btnContents.setText("No other records...");
        }
      }
      else
      {
        lblRecord.setText(record.listName());
        lblRecord.setTooltip(new Tooltip(record.listName()));
        resetNavBtns();
      }
    }
    
    sldPreview.setMin(1);
    sldPreview.setMax(numPages);
    sldPreview.setValue(pageNum);    
    
    tfPath.setText(filePath.toString());
    tfPath.setTooltip(new Tooltip(filePath.toString()));
    dialogStage.setTitle(dialogTitle + " - " + filePath.getNameOnly());    
    
    btnPreviewPrev.setDisable(pageNum == 1);
    btnPreviewNext.setDisable(pageNum == numPages);
    
    btnPreviewBack.setDisable(curWrapper().enableNavButton(false) == false);
    btnPreviewForward.setDisable(curWrapper().enableNavButton(true) == false);
    
    btnFileBack.setDisable(curWrapper().enableFileNavButton(false) == false);
    btnFileForward.setDisable(curWrapper().enableFileNavButton(true) == false);
    
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
    
    record = HyperPath.getFileFromFilePath(curWrapper().getFilePath());
    HDT_WorkFile workFile = null;
    
    if (record != null)
      if (record.getType() == hdtWorkFile)
        workFile = (HDT_WorkFile) record;
    
    contentsWindow.update(workFile, pageNum, true);
    
    disablePreviewUpdating = false;
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void openContentsWindow()
  {
    if (contentsWindow.getStage().isShowing())
      focusStage(contentsWindow.getStage());
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

      BrowserCore.shutdown();

      Platform.runLater(() -> 
      {        
        app.getPrimaryStage().close();
        
        if (Environment.isMac())
          Platform.exit();
      });
    };
  }

//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

  public void cleanup()
  {
    if (browserCoreInitialized)
      getDisposeHandler(tabToWrapper.values().iterator()).run();
  }
   
//---------------------------------------------------------------------------  
//---------------------------------------------------------------------------

}
