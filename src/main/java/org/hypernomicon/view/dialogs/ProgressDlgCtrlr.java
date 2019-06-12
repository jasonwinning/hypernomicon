/*
 * Copyright 2015-2019 Jason Winning
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

package org.hypernomicon.view.dialogs;

import javafx.concurrent.Task;
import javafx.concurrent.WorkerStateEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.HyperTask;

public class ProgressDlgCtrlr extends HyperDlg
{
  private Task<Boolean> task;
  private Thread thread;
  private boolean alreadyDone = false, ownThread = true;
  private long lastPercent = -200;

  @FXML private ProgressBar progressBar;
  @FXML private Label lblTask, lblPercent;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static ProgressDlgCtrlr create()
  {
    ProgressDlgCtrlr pdc = HyperDlg.create("ProgressDlg.fxml", appTitle, true);
    pdc.init();
    return pdc;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return false; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void init()
  {
    lblTask.setText("");
    lblPercent.setText("Progress: 0 %");

    progressBar.setProgress(0.0);

    dialogStage.setOnHiding(event ->
    {
      done();
      lblTask.textProperty().unbind();
      progressBar.progressProperty().unbind();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void performTask(HyperTask task)
  {
    this.task = task;

    task.updateProgress(0, 1);

    lblTask.textProperty().bind(task.messageProperty());

    progressBar.progressProperty().bind(task.progressProperty());
    task.progressProperty().addListener((ob, oldValue, newValue) ->
    {
      long percent = java.lang.Math.round(newValue.doubleValue() * 100.0);

      if (percent == lastPercent)
        return;

      lastPercent = percent;

      if (percent < 0)
        lblPercent.setText("Working...");
      else
        lblPercent.setText("Progress: " + java.lang.Math.round(newValue.doubleValue() * 100.0) + " %");
    });

    dialogStage.setOnShowing(event ->
    {
      EventHandler<WorkerStateEvent> successHndlr = task.getOnSucceeded(),
                                     failHndlr    = task.getOnFailed(),
                                     cancelHndlr  = task.getOnCancelled();
      task.setOnSucceeded(e ->
      {
        if (successHndlr != null) successHndlr.handle(e);
        done();
      });

      task.setOnFailed(e ->
      {
        if (failHndlr != null) failHndlr.handle(e);
        done();
      });

      task.setOnCancelled(e ->
      {
        if (cancelHndlr != null) cancelHndlr.handle(e);
        done();
      });
    });

    onShown = () ->
    {
      ownThread = !task.isRunning();

      if (ownThread)
      {
        thread = new Thread(task);
        task.setThread(thread);
        thread.start();
      }
    };

    showModal();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void done()
  {
    if (alreadyDone) return;

    if (task.isRunning() && ownThread)
    {
      task.cancel(true);
      try { thread.join(); } catch (Exception e) { noOp(); }
    }

    alreadyDone = true;
    getStage().close();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
