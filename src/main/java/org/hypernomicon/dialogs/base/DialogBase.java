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

  protected final Stage dialogStage;
  protected final AnchorPane stagePane;

  protected Runnable onShown, onHidden;

  private boolean shownAlready = false;

//---------------------------------------------------------------------------

  protected final Stage getStage   () { return dialogStage; }
  public final boolean shownAlready() { return shownAlready; }
  public final boolean isShowing   () { return (dialogStage != null) && dialogStage.isShowing(); }

  double getInitHeight() { return -1; }
  double getInitWidth () { return -1; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  DialogBase(String loc, String title, boolean resizable, StageStyle stageStyle, Modality modality, boolean fullPath)
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

      if (tmpStagePane.getStyleClass().contains("SpecialUI") == false)
        scene.getStylesheets().add(App.class.getResource("resources/css.css").toExternalForm());

      tmpDialogStage.setOnShown (event -> doOnShown ());
      tmpDialogStage.setOnHidden(event -> doOnHidden());
    }
    catch (IOException e)
    {
      errorPopup("Internal error while initializing dialog window");
    }
    finally
    {
      dialogStage = tmpDialogStage;
      stagePane = tmpStagePane;
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void doOnShown()
  {
    rescale();

    if (getInitHeight() <= 0)
      dialogStage.centerOnScreen();

    doAdditionalOnShown();

    shownAlready = true;
  }

  protected abstract void doAdditionalOnShown();

  protected void doOnHidden() { if (onHidden != null) onHidden.run(); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void rescale()
  {
    if ((shownAlready == false) && (stagePane.getStyleClass().contains("SpecialUI") == false))
    {
      scaleNodeForDPI(stagePane);
      setFontSize(stagePane);
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

    if (getInitWidth() <= 0)
    {
      val = stagePane.getPrefWidth();
      if (val > 0)
        dialogStage.setWidth(val + diff);
    }
    else
      dialogStage.setWidth(getInitWidth());

    if (getInitHeight() <= 0)
    {
      val = stagePane.getPrefHeight();
      if (val > 0)
        dialogStage.setHeight(val + diff);
    }
    else
      dialogStage.setHeight(getInitHeight());
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
