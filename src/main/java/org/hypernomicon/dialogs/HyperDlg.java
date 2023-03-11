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

package org.hypernomicon.dialogs;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import org.apache.commons.lang3.SystemUtils;
import org.hypernomicon.App;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.geometry.BoundingBox;
import javafx.geometry.Bounds;
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
  protected final Stage dialogStage;
  protected final AnchorPane stagePane;
  protected Runnable onShown = null;
  private double initHeight = -1, initWidth = -1;
  private boolean shownAlready = false;

//---------------------------------------------------------------------------

  public final Stage getStage()       { return dialogStage; }
  public final boolean shownAlready() { return shownAlready; }

  protected abstract boolean isValid();

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected HyperDlg(String loc, String title, boolean resizable)
  {
    this(loc, title, resizable, false);
  }

  protected HyperDlg(String loc, String title, boolean resizable, boolean fullPath)
  {
    this(loc, title, resizable, StageStyle.UTILITY, Modality.APPLICATION_MODAL, fullPath);
  }

  protected HyperDlg(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality)
  {
    this(loc, title, resizable, stageStyle, modality, true);
  }

//---------------------------------------------------------------------------

  private HyperDlg(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality, boolean fullPath)
  {
    if (fullPath == false)
      loc = "dialogs/" + loc;

    Stage tmpDialogStage = null;
    AnchorPane tmpStagePane = null;

    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource(loc + ".fxml"), null, null, klass -> this);
      tmpStagePane = loader.load();

      tmpDialogStage = new Stage();
      tmpDialogStage.setTitle(title);
      tmpDialogStage.initModality(modality);
      tmpDialogStage.setResizable(resizable);
      tmpDialogStage.initStyle(stageStyle);

      Window owner = null;
      if (modality != Modality.NONE)
      {
        owner = ui.windows.getOutermostStage();

        if ((owner != null) && (owner.isShowing() == false))
          owner = null;
      }

      tmpDialogStage.initOwner(owner);
      tmpDialogStage.getIcons().addAll(ui.getStage().getIcons());
      Scene scene = new Scene(tmpStagePane);
      tmpDialogStage.setScene(scene);

      if ("SpecialUI".equals(tmpStagePane.getId()) == false)
        scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

      tmpDialogStage.setOnShown(event -> doOnShown());
    }
    catch (IOException e)
    {
      messageDialog("Internal error while initializing dialog window", mtError);
    }
    finally
    {
      dialogStage = tmpDialogStage;
      stagePane = tmpStagePane;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, Bounds> boundsMap = new HashMap<>();

  public final void initBounds(String prefKeyX, String prefKeyY, String prefKeyWidth, String prefKeyHeight)
  {
    double x = appPrefs.getDouble(prefKeyX, -1.0);
    if (x > 0)
      dialogStage.setX(x);

    double y = appPrefs.getDouble(prefKeyY, -1.0);
    if (y > 0)
      dialogStage.setY(y);
    else if (SystemUtils.IS_OS_WINDOWS && (dialogStage.getY() < 30.0)) // Make sure Windows taskbar isn't at the top and covering the window controls
    {
      y = 30.0;
      dialogStage.setY(30.0);
    }

    double h = setInitHeight(prefKeyHeight),
           w = setInitWidth(prefKeyWidth);

    boundsMap.put(prefKeyX, new BoundingBox(x, y, w, h));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void saveBoundPrefs(Stage stage, String prefKeyX, String prefKeyY, String prefKeyWidth, String prefKeyHeight)
  {
    Bounds b = new BoundingBox(stage.getX(), stage.getY(), stage.getWidth(), stage.getHeight());

    if (b.equals(boundsMap.get(prefKeyX)) == false)
    {
      appPrefs.putDouble(prefKeyX, b.getMinX());
      appPrefs.putDouble(prefKeyY, b.getMinY());
      appPrefs.putDouble(prefKeyWidth, b.getWidth());
      appPrefs.putDouble(prefKeyHeight, b.getHeight());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private double setInitHeight(String prefKey)
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

    return initHeight;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private double setInitWidth(String prefKey)
  {
    initWidth = appPrefs.getDouble(prefKey, stagePane.getPrefWidth());

    if (initWidth < 350)
      initWidth = stagePane.getPrefWidth();

    return initWidth;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doOnShown()
  {
    rescale();

    if (onShown != null) onShown.run();

    shownAlready = true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rescale()
  {
    if (shownAlready == false)
    {
      if ("SpecialUI".equals(stagePane.getId()) == false)
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

    if (shownAlready) return;

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
