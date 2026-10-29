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

import javafx.concurrent.Worker.State;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.VBox;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import org.hypernomicon.HyperTask;
import org.hypernomicon.dialogs.base.ModalDialog;
import org.hypernomicon.util.PopupRobot;

//---------------------------------------------------------------------------

public final class ProgressDlgCtrlr extends ModalDialog
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final HyperTask task;
  private final boolean startedByUs;

  private Label lblPercent;
  private long lastPercent = -200;

  @FXML private Button btnCancel, btnSkip;
  @FXML private ProgressBar progressBar;
  @FXML private VBox vbox;

  private static final double ROW_HEIGHT = 30.0;

  /**
   * Time to wait before showing the progress dialog. If the task completes
   * within this time, the dialog is not shown at all, avoiding UI flicker
   * for quick operations.
   */
  private static final long SHOW_DIALOG_THRESHOLD_MS = 300;

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private ProgressDlgCtrlr(HyperTask task, boolean startedByUs)
  {
    super("ProgressDlg", appTitle, true);

    this.task = task;
    this.startedByUs = startedByUs;

    task.setInitialized();  // Prevent progress dialog configuration from being changed

    Label lblMessage = newLabelRow("");

    task.getAdditionalMessages().forEach(this::newLabelRow);

    if (task.willHaveProgressUpdates())
      lblPercent = newLabelRow("");

    if (task.getSkippable() == false)
      removeFromParent(btnSkip);

    // A task we started refuses to be cancelled once it passes its point of no return, so the
    // buttons that would cancel it are disabled then. For any other task, they only end the wait.

    if (startedByUs)
    {
      btnCancel.disableProperty().bind(task.cancellableProperty().not());
      btnSkip  .disableProperty().bind(task.cancellableProperty().not());
    }

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

    stage.setOnCloseRequest(event ->
    {
      if (stopTask(task, startedByUs) == false)
        event.consume();
    });

    stage.setOnHiding(event ->
    {
      lblMessage .textProperty    ().unbind();
      progressBar.progressProperty().unbind();
      btnCancel  .disableProperty ().unbind();
      btnSkip    .disableProperty ().unbind();
    });
  }

//---------------------------------------------------------------------------

  @Override protected boolean isValid() { return true; }

  @Override protected void btnOkClick    () { if (stopTask(task, startedByUs)) super.btnOkClick    (); }  // The Skip button
  @Override protected void btnCancelClick() { if (stopTask(task, startedByUs)) super.btnCancelClick(); }

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
   * What Cancel, Skip and the window's close button do before the dialog closes, and what a
   * simulated Cancel does. If we started the task and it is still running, it is cancelled so the
   * caller can safely clean up data structures. By default we also wait for the thread to die;
   * tasks that opt out via setWaitOnCancel(false), typically because the underlying work doesn't
   * respond promptly to Thread.interrupt(), get a fire-and-forget cancel and the orphan continues
   * running until it naturally completes.
   * <p>
   * A task that has passed its point of no return refuses, and the dialog stays open: the
   * window's close button is ignored, and so is a click on Cancel or Skip made just before the
   * dialog disabled them.
   * @return false if the task refused, so the dialog must stay open
   */
  private static boolean stopTask(HyperTask task, boolean startedByUs)
  {
    if ((startedByUs == false) || (task.isRunning() == false)) return true;

    return task.getWaitOnCancel() ? task.cancelAndWait() : task.cancel();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Run the specified task while showing progress updates in a progress dialog.
   * This method blocks execution in the calling thread until the task completes.
   *
   * <p>To avoid UI flicker for quick operations, the dialog is not shown immediately.
   * Instead, the task is started first and the dialog only appears if the task is
   * still running after {@link #SHOW_DIALOG_THRESHOLD_MS} milliseconds.</p>
   *
   * <p>The task may already be running when this method is called.
   * If it is not, it will be started automatically.</p>
   *
   * <p>If the task was already running, and the user clicks Cancel or Skip,
   * this method does not tell the task to cancel or terminate; this should
   * be handled by the caller.</p>
   *
   * <p>Once a task this method started passes its point of no return (see
   * {@code HyperTask.passPointOfNoReturn}), Cancel and Skip are disabled and the
   * window's close button is ignored.</p>
   *
   * <p>To show an indeterminate animation in the progress dialog, pass <code>false</code> for the
   * <code>withProgressUpdates</code> parameter in the {@link HyperTask} constructor.</p>
   * @param task The task to execute
   * @return The final {@link State} of the task.
   */
  public static State performTask(HyperTask task)
  {
    // If task is already done, return immediately

    if (task.isDone())
      return task.getState();

    // Check both state and thread liveness: state might still be READY briefly
    // after an external caller starts the thread (SCHEDULED transition is deferred
    // via Platform.runLater)

    boolean startedByUs = (task.isRunning() == false) && (task.threadIsAlive() == false);

    // Start task first if not already running

    if (startedByUs)
      task.startWithNewThread();

    // Skip delay for tasks that opt out (e.g., tasks that queue many runLaters)

    if (task.getShowDialogImmediately() == false)
    {
      // Use custom threshold if set, otherwise use default

      long threshold = task.getDialogDelayMillis() >= 0 ? task.getDialogDelayMillis() : SHOW_DIALOG_THRESHOLD_MS;

      // Wait for task to complete or timeout. Each iteration pumps the FX event
      // queue so state property updates (via Platform.runLater) can be processed.

      long startTime = System.currentTimeMillis();

      do
      {
        sleepForMillis(50);
        pauseAndWaitForRunLaters();
      }
      while (task.isRunning() && ((System.currentTimeMillis() - startTime) < threshold));

      // If task completed within threshold, return its state

      if (task.isRunning() == false)
        return task.getState();
    }

    // Task still running; show dialog

    // PopupRobot interception: simulate Cancel on progress dialog. A task past its point of
    // no return refuses, as it would a click on Cancel, and the dialog is shown.

    if (PopupRobot.isActive() && PopupRobot.shouldCancelProgressDialog() && stopTask(task, startedByUs))
      return task.getState();

    boolean ok = new ProgressDlgCtrlr(task, startedByUs).showModal();

    return ok ? State.SUCCEEDED : task.getState();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
