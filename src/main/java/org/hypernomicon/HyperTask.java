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

package org.hypernomicon;

import static org.hypernomicon.App.*;
import static org.hypernomicon.util.Util.*;
import static org.hypernomicon.util.Util.MessageDialogType.*;

import org.hypernomicon.model.Exceptions.HyperDataException;
import org.hypernomicon.model.Exceptions.TerminateTaskException;
import org.hypernomicon.view.dialogs.ProgressDialogController;
import javafx.concurrent.Task;

public abstract class HyperTask extends Task<Boolean>
{
//---------------------------------------------------------------------------
//---------------------------------------------------------------------------

  public HyperTask()
  {
    setOnFailed   (workerStateEvent -> handleException());
    setOnCancelled(workerStateEvent -> handleException());
  }

  private Thread thread;

  @Override public void updateProgress(long cur, long total)     { super.updateProgress((double)cur, (double)total); } // This increases the visibility from protected
  @Override public void updateProgress(double cur, double total) { super.updateProgress(cur, total); }                 // to public for both of these functions

  public void setThread(Thread thread) { this.thread = thread; }
  public Thread getThread()            { return thread; }

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
    ProgressDialogController.create(appTitle).performTask(task);

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
