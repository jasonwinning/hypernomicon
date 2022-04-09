/*
 * Copyright 2015-2022 Jason Winning
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

import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;

import static org.hypernomicon.App.*;

import org.hypernomicon.HyperTask;

public class ProgressDlgCtrlr extends HyperDlg
{
  private HyperTask task;
  private boolean ownThread = true;
  private long lastPercent = -200;

  @FXML private ProgressBar progressBar;
  @FXML private Label lblTask, lblPercent;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ProgressDlgCtrlr build()
  {
    return ((ProgressDlgCtrlr) create("ProgressDlg", appTitle, true)).init();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ProgressDlgCtrlr init()
  {
    lblTask.setText("");
    lblPercent.setText("Progress: 0 %");

    progressBar.setProgress(0.0);

    dialogStage.setOnHiding(event ->
    {
      if (task.isRunning() && ownThread)
        task.cancelAndWait();

      lblTask.textProperty().unbind();
      progressBar.progressProperty().unbind();
    });

    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public State performTask(HyperTask task)
  {
    this.task = task;

    task.updateProgress(0, 1);

    lblTask.textProperty().bind(task.messageProperty());

    progressBar.progressProperty().bind(task.progressProperty());

    task.progressProperty().addListener((ob, oldValue, newValue) ->
    {
      long percent = Math.round(newValue.doubleValue() * 100.0);

      if (percent == lastPercent)
        return;

      lastPercent = percent;

      lblPercent.setText(percent < 0 ? "Working..." : "Progress: " + Math.round(newValue.doubleValue() * 100.0) + " %");
    });

    task.runWhenFinalStateSet(() -> getStage().close());

    onShown = () ->
    {
      ownThread = (task.isRunning() == false);

      if (ownThread)
        task.startWithNewThread();
    };

    showModal();

    return task.getState();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
