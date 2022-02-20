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
import org.hypernomicon.model.Exceptions.TerminateTaskException;

import javafx.concurrent.Task;

public abstract class HyperTask extends Task<Boolean>
{

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static class HyperThread extends Thread
  {
    public HyperThread(String name)                    { super(          newThreadName(name           )); }
    public HyperThread(HyperTask task)                 { super(task,     newThreadName(task.threadName)); }
    public HyperThread(Runnable runnable, String name) { super(runnable, newThreadName(name           )); }
  }

//---------------------------------------------------------------------------

  private static ConcurrentHashMap<String, Integer> threadNameBaseToNum = new ConcurrentHashMap<>();

  private static synchronized String newThreadName(String base)
  {
    int num = threadNameBaseToNum.getOrDefault(base, -1) + 1;

    threadNameBaseToNum.put(base, num);

    return "Hypernomicon-" + base + "-" + num;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTask(String threadName)
  {
    this.threadName = threadName;

    setOnFailed   (workerStateEvent -> handleException());
    setOnCancelled(workerStateEvent -> handleException());
  }

  private HyperThread thread;
  private final String threadName;

  @Override public void updateProgress(long   cur, long   total) { super.updateProgress((double)cur, (double)total); } // Increase visibility from protected
  @Override public void updateProgress(double cur, double total) { super.updateProgress(cur, total); }                 // to public for both of these functions

  public void setThread(HyperThread thread) { this.thread = thread; }
  protected HyperThread getThread()         { return thread; }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  private void handleException()
  {
    Throwable ex = getException();

    if ((ex == null) || (ex instanceof HyperDataException) || (ex instanceof TerminateTaskException)) return;

    ex.printStackTrace();
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public static boolean performTaskWithProgressDialog(HyperTask task)
  {
    ProgressDlgCtrlr.build().performTask(task);

    if ((task.getState() == State.FAILED) || (task.getState() == State.CANCELLED))
    {
      Throwable ex = task.getException();

      if (ex instanceof HyperDataException)
        messageDialog(ex.getMessage(), mtError);

      return false;
    }

    return true;
  }

//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

}
