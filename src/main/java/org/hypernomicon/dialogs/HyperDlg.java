/*
 * Copyright 2015-2020 Jason Winning
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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.App;

import java.io.IOException;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.Point2D;
import javafx.geometry.Rectangle2D;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.scene.robot.Robot;
import javafx.stage.Modality;
import javafx.stage.Screen;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.stage.Window;

//---------------------------------------------------------------------------

public abstract class HyperDlg
{
  protected boolean okClicked = false;
  protected Stage dialogStage;
  protected AnchorPane stagePane;
  protected Runnable onShown = null;
  private double initHeight = -1, initWidth = -1;
  private boolean shownAlready = false;

//---------------------------------------------------------------------------

  public final Stage getStage()       { return dialogStage; }
  public final boolean shownAlready() { return shownAlready; }

  protected abstract boolean isValid();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static final <T extends HyperDlg> T create(String loc, String title, boolean resizable)
  {
    return createUsingFullPath("dialogs/" + loc, title, resizable, StageStyle.UTILITY, Modality.APPLICATION_MODAL);
  }

  protected static final <T extends HyperDlg> T createUsingFullPath(String loc, String title, boolean resizable)
  {
    return createUsingFullPath(loc, title, resizable, StageStyle.UTILITY, Modality.APPLICATION_MODAL);
  }

  protected static final <T extends HyperDlg> T createUsingFullPath(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality)
  {
    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource(loc + ".fxml"));
      AnchorPane mainPane = (AnchorPane) loader.load();

      Stage dialogStage = new Stage();
      dialogStage.setTitle(title);
      dialogStage.initModality(modality);
      dialogStage.setResizable(resizable);
      dialogStage.initStyle(stageStyle);

      Window owner = null;
      if (modality != Modality.NONE)
      {
        owner = ui.windows.getOutermostStage();

        if ((owner != null) && (owner.isShowing() == false))
          owner = null;
      }

      dialogStage.initOwner(owner);
      dialogStage.getIcons().addAll(ui.getStage().getIcons());
      Scene scene = new Scene(mainPane);
      dialogStage.setScene(scene);

      if (safeStr(mainPane.getId()).equals("SpecialUI") == false)
        scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

      final T dlg = loader.getController();

      dlg.stagePane = mainPane;
      dlg.dialogStage = dialogStage;

      dialogStage.setOnShown(event -> dlg.doOnShown());

      return dlg;
    }
    catch (IOException e)
    {
      messageDialog("Internal error while initializing dialog window", mtError);
    }

    return null;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void initBounds(String prefKeyX, String prefKeyY, String prefKeyHeight, String prefKeyWidth)
  {
    double val = appPrefs.getDouble(prefKeyX, -1.0);
    if (val > 0)
      dialogStage.setX(val);
    
    val = appPrefs.getDouble(prefKeyY, -1.0);
    if (val > 0)
      dialogStage.setY(val);
    else if (SystemUtils.IS_OS_WINDOWS && (dialogStage.getY() < 30.0)) // Make sure Windows taskbar isn't at the top and covering the window controls
      dialogStage.setY(30.0);

    setInitHeight(prefKeyHeight);
    setInitWidth(prefKeyWidth);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------
  
  private void setInitHeight(String prefKey)
  {
    double defHeight = stagePane.getPrefHeight();
    Point2D point = new Robot().getMousePosition();
    Screen screen = Screen.getScreensForRectangle(new Rectangle2D(point.getX(), point.getY(), 1, 1)).stream().findFirst().orElse(null);
    
    if (screen != null)
    {
      double screenHeight = screen.getBounds().getHeight();
      if (defHeight > (screenHeight - 60.0))
        defHeight = screenHeight - 60.0;
    }
    
    initHeight = appPrefs.getDouble(prefKey, defHeight);
    
    if (initHeight < 350)
      initHeight = stagePane.getPrefHeight();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void setInitWidth(String prefKey)
  {
    initWidth = appPrefs.getDouble(prefKey, stagePane.getPrefWidth());

    if (initWidth < 350)
      initWidth = stagePane.getPrefWidth();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void doOnShown()
  {
    rescale();

    if (onShown != null) onShown.run();

    shownAlready = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final void rescale()
  {
    if (shownAlready == false)
    {
      if (safeStr(stagePane.getId()).equals("SpecialUI") == false)
      {
        scaleNodeForDPI(stagePane);
        setFontSize(stagePane);
      }
    }

    double diff = dialogStage.getHeight() - stagePane.getHeight();
    if (diff == 0.0) diff = 30.0;

    double val = stagePane.getMaxHeight();
    if (val > 0)
      dialogStage.setMaxHeight(val + diff);

    val = stagePane.getMinHeight();
    if (val > 0)
      dialogStage.setMinHeight(val + diff);

    val = stagePane.getMinWidth();
    if (val > 0)
      dialogStage.setMinWidth(val + diff);

    if (shownAlready == false)
    {
      if (initWidth <= 0)
      {
        val = stagePane.getPrefWidth();
        if (val > 0)
          dialogStage.setWidth(val + diff);
      }
      else
        dialogStage.setWidth(initWidth);

      if (initHeight <= 0)
      {
        val = stagePane.getPrefHeight();
        if (val > 0)
          dialogStage.setHeight(val + diff);
      }
      else
        dialogStage.setHeight(initHeight);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public final void showNonmodal()
  {
    dialogStage.show();

    ensureVisible(dialogStage, stagePane.getPrefWidth(), stagePane.getPrefHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean showModal()
  {
    ui.windows.push(dialogStage);

    dialogStage.showAndWait();

    ui.windows.pop();

    return okClicked;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML protected void btnOkClick()
  {
    if (isValid() == false) return;

    okClicked = true;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @FXML protected void btnCancelClick()
  {
    okClicked = false;
    dialogStage.close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
