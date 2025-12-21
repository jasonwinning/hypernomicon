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

package org.hypernomicon;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.Util.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;

import org.hypernomicon.dialogs.ProgressDlgCtrlr;
import org.hypernomicon.model.Exceptions.*;

import javafx.application.Platform;
import javafx.beans.property.*;
import javafx.concurrent.Task;
import javafx.concurrent.WorkerStateEvent;
import javafx.concurrent.Worker.State;
import javafx.event.EventHandler;

//---------------------------------------------------------------------------

public abstract class HyperTask
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HyperThread extends Thread
  {
    protected HyperThread(                   String name) { super(          newThreadName(name)); }
    public    HyperThread(Runnable runnable, String name) { super(runnable, newThreadName(name)); }

    protected HyperThread(HyperTask task)
    {
      super(task.innerTask, newThreadName(task.threadName));
      task.thread = this;
    }

    private static final ConcurrentHashMap<String, Integer> threadNameBaseToNum = new ConcurrentHashMap<>();

    private static synchronized String newThreadName(String base)
    {
      int num = threadNameBaseToNum.getOrDefault(base, -1) + 1;

      threadNameBaseToNum.put(base, num);

      return "Hypernomicon-" + base + '-' + num;
    }

    public static boolean isRunning(HyperThread thread)
    {
      return (thread != null) && thread.isAlive();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private final class InnerTask extends Task<Void>
  {
    private final String startingMessage;
    private final boolean withProgressUpdates;

    private boolean silent = false;

    private InnerTask(String startingMessage, boolean withProgressUpdates)
    {
      this.startingMessage = startingMessage;
      this.withProgressUpdates = withProgressUpdates;
    }

    @Override protected Void call() throws HyperDataException
    {
      updateMessage(startingMessage);
      updateProgress(withProgressUpdates ? 0 : -1, 1);

      try                              { HyperTask.this.call(); }
      catch (CancelledTaskException e) { cancel();              }
      return null;
    }

    @Override protected void updateProgress(double cur, double total) { super.updateProgress(cur, total); } // Make visible to outer class

    @Override protected void failed()
    {
      if (silent) return;

      Throwable e = getException();

      if (e instanceof HyperDataException)
        Platform.runLater(() -> errorPopup(e));
      else if (e != null)
        logThrowable(e);
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Create task
   * @param threadName Name to use for thread
   * @param withProgressUpdates If true, the progress bar will show the progress. If false, it will show the indeterminate animation.
   */
  public HyperTask(String threadName, boolean withProgressUpdates)
  {
    this(threadName, "Working...", withProgressUpdates);
  }

  /**
   * Create task
   * @param threadName Name to use for thread
   * @param message Description of work being done shown on progress dialog
   */
  public HyperTask(String threadName, String message)
  {
    this(threadName, message, true);
  }

  /**
   * Create task
   * @param threadName Name to use for thread
   * @param message Description of work being done shown on progress dialog
   * @param withProgressUpdates If true, the progress bar will show the progress. If false, it will show the indeterminate animation.
   */
  public HyperTask(String threadName, String message, boolean withProgressUpdates)
  {
    this(threadName, message, 1, withProgressUpdates);
  }

  /**
   * Create task
   * @param threadName Name to use for thread
   * @param message Description of work being done shown on progress dialog
   * @param totalCount Total progress units
   */
  public HyperTask(String threadName, String message, long totalCount)
  {
    this(threadName, message, totalCount, true);
  }

//---------------------------------------------------------------------------

  /**
   * Create task
   * @param threadName Name to use for thread
   * @param message Description of work being done shown on progress dialog
   * @param totalCount Total progress units
   * @param withProgressUpdates If true, the progress bar will show the progress. If false, it will show the indeterminate animation.
   */
  private HyperTask(String threadName, String message, long totalCount, boolean withProgressUpdates)
  {
    this.threadName = threadName;
    this.totalCount = totalCount;

    innerTask = new InnerTask(message, withProgressUpdates);
  }

//---------------------------------------------------------------------------

  private final InnerTask innerTask;
  private final List<String> additionalMessages = new ArrayList<>();
  private final String threadName;

  private HyperThread thread;
  private boolean skippable = false, initialized = false;

  /**
   * If completedCount is negative, then the progress will be shown as indeterminate, regardless of what totalCount is.
   * This is due to the behavior of how the Task class uses its corresponding properties.
   */
  public long completedCount = 0, totalCount;

  protected abstract void call() throws HyperDataException, CancelledTaskException;

  /**
   * Terminates execution of this task.
   *
   * @return returns true if the cancel was successful
   */
  public boolean cancel() { return innerTask.cancel(); }

  public Throwable getException()             { return innerTask.getException(); }
  public State getState()                     { return innerTask.getState(); }
  public List<String> getAdditionalMessages() { return Collections.unmodifiableList(additionalMessages); }

  public ReadOnlyStringProperty  messageProperty () { return innerTask.messageProperty (); }
  public ReadOnlyDoubleProperty  progressProperty() { return innerTask.progressProperty(); }

  public boolean getSkippable()            { return skippable; }
  public boolean threadIsAlive()           { return HyperThread.isRunning(thread); }
  public boolean isRunning()               { return innerTask.isRunning(); }
  public boolean isCancelled()             { return innerTask.isCancelled(); }
  public boolean isDone()                  { return innerTask.isDone(); }
  public boolean willHaveProgressUpdates() { return innerTask.withProgressUpdates; }

  protected void incrementAndUpdateProgress() throws CancelledTaskException { completedCount++; updateProgress(); }

  private void updateProgress() throws CancelledTaskException { updateProgress(completedCount, totalCount); }

  public void incrementAndUpdateProgress(int updateInterval) throws CancelledTaskException
  {
    if ((++completedCount % updateInterval) == 0)
      updateProgress();
  }

  public void updateProgress(double cur, double total) throws CancelledTaskException
  {
    innerTask.updateProgress(cur, total);
    throwExceptionIfCancelled(this);
  }

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
   * @return The final {@link State} of the task.
   */
  public State runWithProgressDialog() { return ProgressDlgCtrlr.performTask(this); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Prevent further changes to progress dialog configuration.
   * <p>
   * This should only be called from ProgressDlgCtrlr
   */
  public void setInitialized() { initialized = true; }

  /**
   * If true, then if an exception is thrown during the task's <code>call</code> method,
   * no popup will be shown and the exception will not automatically be logged.
   * @param silent Whether to report exceptions
   * @return This HyperTask
   */
  public HyperTask setSilent(boolean silent)
  {
    if (initialized) throw new IllegalStateException("Already initialized");

    innerTask.silent = silent;
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Add an additional message to be shown on the progress popup window
   * @param message The message
   * @return This HyperTask
   */
  public HyperTask addMessage(String message)
  {
    if (initialized) throw new IllegalStateException("Already initialized");

    additionalMessages.add(message);
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * If true, a "Skip" button will appear in addition to the Cancel button. If "Skip"
   * is pressed, the task will stop running early (the same as if Cancel was clicked)
   * but the progress dialog's showModal method will return true (as if OK was clicked).
   * @param skippable Whether the "Skip" button should be shown
   * @return This HyperTask
   */
  public HyperTask setSkippable(boolean skippable)
  {
    if (initialized) throw new IllegalStateException("Already initialized");

    this.skippable = skippable;
    return this;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private synchronized boolean waitUntilThreadDies()
  {
    if ((thread == null) || (thread.isAlive() == false)) return true;

    try
    {
      thread.join();
    }
    catch (InterruptedException e)
    {
      Thread.currentThread().interrupt();
      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Terminates execution of this task and waits for the thread to die.
   *
   * @return returns true if the cancel was successful
   */
  public boolean cancelAndWait()
  {
    cancel();

    return waitUntilThreadDies();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static void throwExceptionIfCancelled(HyperTask task) throws CancelledTaskException
  {
    if ((task != null) && task.isCancelled())
      throw new CancelledTaskException();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void startWithNewThreadAsDaemon()
  {
    initialized = true;

    if (getState() != State.READY)
      throw new IllegalStateException("Can only start a task in the READY state. Was in state " + getState());

    if (thread != null)
      throw new IllegalStateException("Task already has thread.");

    HyperThread newThread = new HyperThread(this);
    newThread.setDaemon(true);
    newThread.start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void startWithNewThread()
  {
    initialized = true;

    if (getState() != State.READY)
      throw new IllegalStateException("Can only start a task in the READY state. Was in state " + getState());

    if (thread != null)
      throw new IllegalStateException("Task already has thread.");

    new HyperThread(this).start();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  /**
   * Add a handler that will run after the task is finished, regardless of whether
   * it was cancelled, failed, or succeeded.
   * <p>
   * It will only run after both (1) a final
   * state is set, and (2) the task thread is no longer alive.
   * <p>
   * The final state is
   * passed as a parameter to the handler.
   * @param hndlr The handler
   */
  public void addDoneHandler(Consumer<State> hndlr)
  {
    EventHandler<WorkerStateEvent> successHndlr = innerTask.getOnSucceeded(),
                                   failHndlr    = innerTask.getOnFailed(),
                                   cancelHndlr  = innerTask.getOnCancelled();

    innerTask.setOnSucceeded(e ->
    {
      if (successHndlr != null)
        successHndlr.handle(e);
      else
        waitUntilThreadDies();

      hndlr.accept(State.SUCCEEDED);
    });

    innerTask.setOnFailed(e ->
    {
      if (failHndlr != null)
        failHndlr.handle(e);
      else
        waitUntilThreadDies();

      hndlr.accept(State.FAILED);
    });

    innerTask.setOnCancelled(e ->
    {
      if (cancelHndlr != null)
        cancelHndlr.handle(e);
      else
        waitUntilThreadDies();

      hndlr.accept(State.CANCELLED);
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
