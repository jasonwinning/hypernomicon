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

import org.hypernomicon.App;

import java.io.IOException;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.*;

//---------------------------------------------------------------------------

public abstract class DialogBase
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected final Stage stage;
  protected final AnchorPane rootPane;

  protected Runnable onShown, onHidden;

  private boolean shownAlready = false;

//---------------------------------------------------------------------------

  public final boolean shownAlready() { return shownAlready; }
  public final boolean isShowing   () { return (stage != null) && stage.isShowing(); }

  double getInitHeight() { return -1; }
  double getInitWidth () { return -1; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  DialogBase(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality, boolean fullPath)
  {
    if (fullPath == false)
      loc = "dialogs/" + loc;

    Stage tmpStage = null;
    AnchorPane tmpRootPane = null;

    try
    {
      FXMLLoader loader = new FXMLLoader(App.class.getResource(loc + ".fxml"), null, null, klass -> this);
      tmpRootPane = loader.load();

      tmpStage = new Stage();
      tmpStage.setTitle(title);
      tmpStage.initModality(modality);
      tmpStage.setResizable(resizable);
      tmpStage.initStyle(stageStyle);

      Window owner = null;
      if (modality != Modality.NONE)
      {
        owner = ui.windows.getOutermostStage();

        if ((owner != null) && (owner.isShowing() == false))
          owner = null;
      }

      tmpStage.initOwner(owner);
      tmpStage.getIcons().addAll(ui.getStage().getIcons());
      Scene scene = new Scene(tmpRootPane);
      tmpStage.setScene(scene);

      if (tmpRootPane.getStyleClass().contains("SpecialUI") == false)
        scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

      tmpStage.setOnShown (event -> doOnShown ());
      tmpStage.setOnHidden(event -> doOnHidden());
    }
    catch (IOException e)
    {
      logThrowable(e);
      errorPopup("Internal error while initializing dialog window");
    }
    finally
    {
      stage = tmpStage;
      rootPane = tmpRootPane;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doOnShown()
  {
    rescale();

    if (getInitHeight() <= 0)
      stage.centerOnScreen();

    doAdditionalOnShown();

    shownAlready = true;
  }

  protected abstract void doAdditionalOnShown();

  protected void doOnHidden() { if (onHidden != null) onHidden.run(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rescale()
  {
    if ((shownAlready == false) && (rootPane.getStyleClass().contains("SpecialUI") == false))
    {
      scaleNodeForDPI(rootPane);
      setFontSize(rootPane);
    }

    double diff = stage.getHeight() - rootPane.getHeight();
    if (diff == 0.0) diff = 30.0;

    double val = rootPane.getMaxHeight();
    if (val > 0)
      stage.setMaxHeight(val + diff);

    val = rootPane.getMinHeight();
    if (val > 0)
      stage.setMinHeight(val + diff);

    val = rootPane.getMinWidth();
    if (val > 0)
      stage.setMinWidth(val + diff);

    if (shownAlready) return;

    if (getInitWidth() <= 0)
    {
      val = rootPane.getPrefWidth();
      if (val > 0)
        stage.setWidth(val + diff);
    }
    else
      stage.setWidth(getInitWidth());

    if (getInitHeight() <= 0)
    {
      val = rootPane.getPrefHeight();
      if (val > 0)
        stage.setHeight(val + diff);
    }
    else
      stage.setHeight(getInitHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
