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

package org.hypernomicon;

import static org.hypernomicon.util.UIUtil.*;
import static org.hypernomicon.util.UIUtil.MessageDialogType.*;

import java.util.concurrent.ConcurrentHashMap;

import org.hypernomicon.dialogs.ProgressDlgCtrlr;
import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.CancelledTaskException;

import javafx.application.Platform;
import javafx.beans.property.ReadOnlyBooleanProperty;
import javafx.beans.property.ReadOnlyDoubleProperty;
import javafx.beans.property.ReadOnlyStringProperty;
import javafx.concurrent.Task;
import javafx.concurrent.WorkerStateEvent;
import javafx.concurrent.Worker.State;
import javafx.event.EventHandler;

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

  private class InnerTask extends Task<Void>
  {
    @Override protected Void call() throws HyperDataException
    {
      try                              { HyperTask.this.call(); }
      catch (CancelledTaskException e) { cancel();              }
      return null;
    }

    @Override protected void updateMessage(String msg)                { super.updateMessage(msg); } // Make visible to outer class
    @Override protected void updateProgress(double cur, double total) { super.updateProgress(cur, total); } // Make visible to outer class
    @Override protected void done()                                   { HyperTask.this.done(); }

    @Override protected final void failed()
    {
      Throwable ex = getException();

      if (ex instanceof HyperDataException)
        Platform.runLater(() -> messageDialog(ex.getMessage(), mtError));
      else if (ex != null)
        ex.printStackTrace();
    }
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTask(String threadName)
  {
    this.threadName = threadName;

    innerTask = new InnerTask();
  }

  private final InnerTask innerTask;
  private final String threadName;
  private HyperThread thread;

  protected abstract void call() throws HyperDataException, CancelledTaskException;

  /**
   * Protected method invoked when this task transitions to state
   * {@code isDone} (whether normally or via cancellation). The
   * default implementation does nothing.  Subclasses may override
   * this method to invoke completion callbacks or perform
   * bookkeeping. Note that you can query status inside the
   * implementation of this method to determine whether this task
   * has been cancelled.
   */
  protected void done() { }

  /**
   * Terminates execution of this Worker. Calling this method will either
   * remove this Worker from the execution queue or stop execution.
   *
   * @return returns true if the cancel was successful
   */
  public boolean cancel() { return innerTask.cancel(); }

  public Throwable getException() { return innerTask.getException(); }
  public State getState()         { return innerTask.getState(); }

  public ReadOnlyStringProperty  messageProperty () { return innerTask.messageProperty (); }
  public ReadOnlyBooleanProperty runningProperty () { return innerTask.runningProperty (); }
  public ReadOnlyDoubleProperty  progressProperty() { return innerTask.progressProperty(); }

  public boolean threadIsAlive() { return HyperThread.isRunning(thread); }
  public boolean isRunning()     { return innerTask.isRunning(); }
  public boolean isCancelled()   { return innerTask.isCancelled(); }

  protected void updateMessage(String msg)             { innerTask.updateMessage(msg); }
  public void updateProgress(double cur, double total) { innerTask.updateProgress(cur, total); }

  public State runWithProgressDialog() { return ProgressDlgCtrlr.performTask(this); }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  protected synchronized boolean waitUntilThreadDies()
  {
    if (thread == null) return true;

    try { thread.join(); } catch (InterruptedException e) { return false; }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public boolean cancelAndWait()
  {
    if (cancel() == false)
      return false;

    return waitUntilThreadDies();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperThread startWithNewThreadAsDaemon()
  {
    if (getState() != State.READY)
      throw new IllegalStateException("Can only start a task in the READY state. Was in state " + getState());

    if (thread != null)
      throw new IllegalStateException("Task already has thread.");

    HyperThread newThread = new HyperThread(this);
    newThread.setDaemon(true);
    newThread.start();
    return newThread;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperThread startWithNewThread()
  {
    if (getState() != State.READY)
      throw new IllegalStateException("Can only start a task in the READY state. Was in state " + getState());

    if (thread != null)
      throw new IllegalStateException("Task already has thread.");

    HyperThread newThread = new HyperThread(this);
    newThread.start();
    return newThread;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public void runWhenFinalStateSet(Runnable runnable)
  {
    EventHandler<WorkerStateEvent> successHndlr = innerTask.getOnSucceeded(),
                                   failHndlr    = innerTask.getOnFailed(),
                                   cancelHndlr  = innerTask.getOnCancelled();
    innerTask.setOnSucceeded(e ->
    {
      if (successHndlr != null) successHndlr.handle(e);
      runnable.run();
    });

    innerTask.setOnFailed(e ->
    {
      if (failHndlr != null) failHndlr.handle(e);
      runnable.run();
    });

    innerTask.setOnCancelled(e ->
    {
      if (cancelHndlr != null) cancelHndlr.handle(e);
      runnable.run();
    });
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
