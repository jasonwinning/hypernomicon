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

package org.hypernomicon.dialogs.base;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.SystemUtils;

import javafx.geometry.*;
import javafx.scene.robot.Robot;
import javafx.stage.*;

//---------------------------------------------------------------------------

public abstract class NonmodalWindow extends DialogBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private double initHeight = -1, initWidth = -1;

  private String prefKeyX, prefKeyY, prefKeyWidth, prefKeyHeight;

//---------------------------------------------------------------------------

  protected NonmodalWindow(String loc, String title, String prefKeyX, String prefKeyY, String prefKeyWidth, String prefKeyHeight)
  {
    super(loc, title, true, StageStyle.DECORATED, Modality.NONE, true);

    this.prefKeyX      = prefKeyX;
    this.prefKeyY      = prefKeyY;
    this.prefKeyWidth  = prefKeyWidth;
    this.prefKeyHeight = prefKeyHeight;

    initBounds();

    stage.focusedProperty().addListener((ob, oldValue, newValue) ->
    {
      if ((ui.windows.getCyclingFocus() == false) && Boolean.TRUE.equals(newValue))
        ui.windows.push(stage);
    });
  }

//---------------------------------------------------------------------------

  protected NonmodalWindow(String loc, String title)
  {
    super(loc, title, false, StageStyle.DECORATED, Modality.NONE, true);
  }

//---------------------------------------------------------------------------

  protected abstract void getDividerPositions();
  protected abstract void setDividerPositions();

  @Override protected double getInitHeight()  { return initHeight; }
  @Override protected double getInitWidth ()  { return initWidth;  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected final void doAdditionalOnShown()
  {
    if (shownAlready() == false)
      setDividerPositions();

    if (onShown != null) onShown.run();

    ui.windows.push(stage);
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected void doOnHidden()
  {
    ui.windows.focusStage(ui.getStage());

    if (onHidden != null) onHidden.run();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void showNonmodal()
  {
    stage.show();

    ensureVisible(stage, rootPane.getPrefWidth(), rootPane.getPrefHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static void show(NonmodalWindow instance)
  {
    if (instance == null) return;

    if (instance.stage.isShowing())
      ui.windows.focusStage(instance.stage);
    else
      instance.showNonmodal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private static final Map<String, Bounds> boundsMap = new HashMap<>();

  private void initBounds()
  {
    double x = app.prefs.getDouble(prefKeyX, -1.0);
    if (x > 0)
      stage.setX(x);

    double y = app.prefs.getDouble(prefKeyY, -1.0);
    if (y > 0)
      stage.setY(y);
    else if (SystemUtils.IS_OS_WINDOWS && (stage.getY() < 30.0)) // Make sure Windows taskbar isn't at the top and covering the window controls
    {
      y = 30.0;
      stage.setY(30.0);
    }

    double h = setInitHeight(prefKeyHeight),
           w = setInitWidth (prefKeyWidth);

    boundsMap.put(prefKeyX, new BoundingBox(x, y, w, h));
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private double setInitHeight(String prefKey)
  {
    double defHeight = rootPane.getPrefHeight();
    Point2D point = new Robot().getMousePosition();
    Screen screen = Screen.getScreensForRectangle(new Rectangle2D(point.getX(), point.getY(), 1, 1)).stream().findFirst().orElse(null);

    if (screen != null)
    {
      double screenHeight = screen.getBounds().getHeight();
      if (defHeight > (screenHeight - 60.0))
        defHeight = screenHeight - 60.0;
    }

    initHeight = app.prefs.getDouble(prefKey, defHeight);

    if (initHeight < 350)
      initHeight = rootPane.getPrefHeight();

    return initHeight;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private double setInitWidth(String prefKey)
  {
    initWidth = app.prefs.getDouble(prefKey, rootPane.getPrefWidth());

    if (initWidth < 350)
      initWidth = rootPane.getPrefWidth();

    return initWidth;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected static void close(NonmodalWindow instance, boolean exitingApp)
  {
    if (instance != null)
      instance.close(exitingApp);
  }

  private void close(boolean exitingApp)
  {
    if (stage.isShowing())
      stage.close();

    if ((exitingApp == false) || (shownAlready() == false)) return;

    getDividerPositions();

    if (strNullOrBlank(prefKeyX)) return;

    Bounds b = new BoundingBox(stage.getX(), stage.getY(), stage.getWidth(), stage.getHeight());

    if (b.equals(boundsMap.get(prefKeyX)) == false)
    {
      app.prefs.putDouble(prefKeyX     , b.getMinX  ());
      app.prefs.putDouble(prefKeyY     , b.getMinY  ());
      app.prefs.putDouble(prefKeyWidth , b.getWidth ());
      app.prefs.putDouble(prefKeyHeight, b.getHeight());
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
