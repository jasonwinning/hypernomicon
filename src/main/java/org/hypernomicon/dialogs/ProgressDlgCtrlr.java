/*
 * Copyright 2015-2026 Jason Winning
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

import javafx.application.Platform;
import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.VBox;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.base.ModalDialog;

//---------------------------------------------------------------------------

public final class ProgressDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Label lblPercent;
  private boolean ownThread = true;
  private long lastPercent = -200;

  @FXML private Button btnSkip;
  @FXML private ProgressBar progressBar;
  @FXML private VBox vbox;

  private static final double ROW_HEIGHT = 30.0;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ProgressDlgCtrlr(HyperTask task)
  {
    super("ProgressDlg", appTitle, true);

    task.setInitialized();  // Prevent progress dialog configuration from being changed

    Label lblMessage = newLabelRow("");

    task.getAdditionalMessages().forEach(this::newLabelRow);

    if (task.willHaveProgressUpdates())
      lblPercent = newLabelRow("");

    if (task.getSkippable() == false)
      removeFromParent(btnSkip);

    updateProgressLabel(task.progressProperty().longValue());

    lblMessage .textProperty    ().bind(task.messageProperty ());
    progressBar.progressProperty().bind(task.progressProperty());

    setHeights(rootPane, ROW_HEIGHT * vbox.getChildren().size());

    task.progressProperty().addListener((ob, oldValue, newValue) ->
    {
      long percent = Math.round(newValue.doubleValue() * 100.0);

      if (percent == lastPercent)
        return;

      lastPercent = percent;

      updateProgressLabel(percent);
    });

    task.addDoneHandler(state -> stage.close());

    onShown = () -> Platform.runLater(() ->       // This needs to be done in a runLater because the task may have been started so recently that
    {                                             // the task's running property has not yet been set to true. It gets set to true by a runnable
      ownThread = (task.isRunning() == false);    // queued to run on the FX thread, so this runnable will run after that one.

      if (ownThread)
        task.startWithNewThread();
    });

    stage.setOnHiding(event ->
    {
      if (task.isRunning() && ownThread)
        task.cancelAndWait();

      lblMessage .textProperty    ().unbind();
      progressBar.progressProperty().unbind();
    });
  }

//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private Label newLabelRow(String message)
  {
    Label label = new Label(message);

    AnchorPane.setBottomAnchor(label, 5.0);
    AnchorPane.setLeftAnchor  (label, 6.0);

    AnchorPane ap = new AnchorPane(label);
    setHeights(ap, ROW_HEIGHT);

    vbox.getChildren().add(vbox.getChildren().size() - 2, ap);

    return label;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void updateProgressLabel(long percent)
  {
    if (lblPercent != null)
      lblPercent.setText(percent < 0 ? "Working..." : "Progress: " + percent + " %");
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run the specified task while showing progress updates in a progress dialog.
   * This method blocks execution in the calling thread until the task completes.
   *
   * <p>The task may already be running when this method is called.
   * If it is not, it will be started automatically.</p>
   *
   * <p>If the task was already running, and the user clicks Cancel or Skip,
   * this method does not tell the task to cancel or terminate; this should
   * be handled by the caller.</p>
   *
   * <p>To show an indeterminate animation in the progress dialog, pass <code>false</code> for the
   * <code>withProgressUpdates</code> parameter in the {@link HyperTask} constructor.</p>
   * @param task The task to execute
   * @return The final {@link State} of the task.
   */
  public static State performTask(HyperTask task)
  {
    return new ProgressDlgCtrlr(task).showModal() ? State.SUCCEEDED : task.getState();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
